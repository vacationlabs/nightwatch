{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings     #-}
module Nightwatch.Websocket (startAria2WebsocketClient) where
import qualified Network.WebSockets  as WS
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Control.Concurrent.Async as A
import qualified Data.Map as M
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics (Generic)
import Control.Concurrent
import qualified Data.ByteString.Lazy as BL hiding (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL(pack, unpack)
import Control.Exception (catch, try, tryJust, bracketOnError, SomeException, Exception)
import Control.Monad (forever)
import Control.Concurrent.Chan
import Nightwatch.Telegram
import Nightwatch.Types hiding (message)
import qualified Nightwatch.Types as Ty(message)
import Data.Functor (void)
import qualified Data.List as DL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as TL
import Nightwatch.DBTypes as DB
import Text.Read (readMaybe)
import Control.Monad.IO.Class  (liftIO, MonadIO)


newtype Aria2MethodName = Aria2MethodName String deriving (Show, Eq)
-- type OutstandingRpcRequest = (aria2LogUserId, AuthNightwatchCommand)
-- type OutstandingRpcRequests = MVar [OutstandingRpcRequest]

data VersionResponse = VersionResponse {
  version :: String,
  enabledFeatures :: [String]
} deriving (Show)

instance FromJSON VersionResponse where
  parseJSON (Object v) = VersionResponse <$>
                         v .: "version" <*>
                         v .: "enabledFeatures"

data JsonRpcRequest p = JsonRpcRequest {
  request_id :: String,
  method :: String,
  params :: p 
} deriving (Show)

instance (ToJSON p) => ToJSON (JsonRpcRequest p) where
  toJSON (JsonRpcRequest request_id method params) = object ["jsonrpc" .= ("2.0"::String), "id" .= request_id, "method" .= method, "params" .= params]

data JsonRpcError = JsonRpcError {
  code :: Integer,
  message :: String
} deriving (Show, Generic)

data (FromJSON p) => JsonRpcResponse p = JsonRpcResponse {
   response_id :: Maybe Aria2LogId,
   result :: Maybe p,
   rpc_error :: Maybe JsonRpcError
} deriving (Show)

data AddUriResult = AddUriResult {
  addUriGid :: Aria2Gid
} deriving (Show, Generic)

instance FromJSON JsonRpcError
instance (FromJSON p) => FromJSON (JsonRpcResponse p) where
  parseJSON (Object v) = do
    response_id <- v .: "response_id"
    result <- v .: "result"
    rpc_error <- v .: "rpc_error"
    return JsonRpcResponse{response_id=response_id, result=result, rpc_error=rpc_error}
  
instance FromJSON AddUriResult where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = removePrefix "addUri",
    constructorTagModifier = T.unpack . T.toLower . T.pack -- TODO: Figure out the correct lowcase func
  }



-- instance FromJSON JsonRpcError where
--   parseJSON (Object v) =  
--     JsonRpcError  <$> v .: "code"
--                   <*> v .: "message"

-- instance (FromJSON p) => FromJSON (JsonRpcResponse p) where
--   parseJSON (Object v) =  
--     JsonRpcResponse p <$> v .:? "id"
--                       <*> v .:? "result"
--                       <*> v .:? "error"

-- addUri ==> GID (String)
-- pause ==> GID (String)
-- remove ==> GID (String)
-- tellStatus ==> Key-value Pair String:String for most of the cases except 'files', which is an array of key-value pairs
-- unpause ==> GID (String
jsonRpcRequest :: (ToJSON params) => WS.Connection -> Aria2MethodName -> Aria2LogId -> params -> IO (BL.ByteString)
jsonRpcRequest conn method request_id request = do
  let (Aria2MethodName mname)  = method
  let x = encode JsonRpcRequest{request_id=(show request_id), method=mname, params=request}
  putStrLn $ "===> SENDING TO Aria2: " ++ (show x)
  WS.sendTextData conn $ x
  return x

aria2WebsocketReceiver :: TelegramOutgoingChannel -> WS.Connection -> IO ()
aria2WebsocketReceiver tgOutChan conn = forever $ do
  msg <- WS.receiveData conn :: IO BL.ByteString
  case (decode msg :: Maybe Object) of
    Nothing -> putStrLn "Received blank ping from Aria2. Ignoring"
    Just obj -> case (HM.lookup "id" obj) of 
      Nothing -> putStrLn $ "RECEIVED NOTIFICATION: " ++ (show obj)
      Just (String x) -> case (readMaybe (T.unpack x) :: Maybe Integer) of
        Nothing -> putStrLn $ "Improper responseId received from Aria2. Ignoring=" ++ (show x)
        Just y -> aria2WebsocketReceiver_ msg y obj

  where
    aria2WebsocketReceiver_ :: BL.ByteString -> Integer -> Object -> IO ()
    aria2WebsocketReceiver_ msg requestId obj = runDb $ do
      let k = Aria2LogKey $ fromIntegral requestId
      aria2Log <- (fetchAria2LogById k)
      case aria2Log of
        Nothing -> liftIO $ putStrLn $ "Could not find Aria2Log with ID=" ++ (show requestId)
        Just l -> do
          updateAria2Log k l{aria2LogResponse=(Just $ BL.unpack msg)}
          liftIO $ handleAria2Response tgOutChan k msg l
          liftIO $ putStrLn $ "Received response to requestId " ++ (show requestId) ++ " and handled successfully."

valueToString :: Value -> String
valueToString value = case (fromJSON value :: Result String) of
  (Success s) -> s
  (Error s) -> s


-- aria2Response2TelegramMessage :: [OutstandingRpcRequest] -> Aria2LogId -> Object -> Maybe TelegramOutgoingMessage
-- aria2Response2TelegramMessage requestList response_id obj = do
--   (request_id, nwCommand) <- DL.find (findRequest response_id) requestList
--   case (command nwCommand) of
--     DownloadCommand url -> (HM.lookup "result" obj) >>= (\result -> Just $ TelegramOutgoingMessage {tg_chat_id=(chatId nwCommand), message=(T.pack $ "Download GID " ++ (valueToString result))})
--     _ -> Nothing


-- This method is called with we get a response with a response_id (as opposed
-- to a notification, that does NOT have a response_id)
--
-- We will look-up the request_id in the OutstandingRpcRequests, from which we
-- will get the original AuthNightwatchCommand. From the AuthNightwatchCommand
-- we'll get the NightwatchCommand, which will tell us how to parse this
-- response. Further, we will also get the UserId and ChatId allowing us to
-- send a tgram message to the user.
handleAria2Response :: TelegramOutgoingChannel -> Aria2LogId -> BL.ByteString -> Aria2Log -> IO ()
handleAria2Response tgOutChan responseId msg aria2Log = do
  let tgramLogId = aria2LogTelegramLogId aria2Log
  case tgramLogId of
    Nothing -> putStrLn $ "ERROR: Aria2Log does not have a correspndonding TelegramLogId. Ignoring=" ++ (show aria2Log)
    Just tgramLogId -> do
      tgramLog <- runDb $ fetchTelegramLogById tgramLogId
      case tgramLog of
        Nothing -> putStrLn $ "ERROR: Could not find corresponding telegramLog. Don't know what to do. Ignore. Telegram Log ID=" ++ (show tgramLogId)
        Just tgramLog -> do
          let nwCommand = (telegramLogNightwatchCommand tgramLog)
          case nwCommand of
            InvalidCommand -> putStrLn $ "ERROR: Very strange, how did an InvalidCommand get converted into an Aria2Log in the first place"
            DownloadCommand url -> handleAddUriResponse tgramLog aria2Log msg
            _ -> putStrLn $ "Have not implemented handling of such responses: (responseId, request)=" ++ show (responseId, nwCommand)


  where
    handleAddUriResponse :: TelegramLog -> Aria2Log -> BL.ByteString -> IO ()
    handleAddUriResponse tgramLog aria2Log msg = do
      let rpcResponse = decode msg :: Maybe (JsonRpcResponse AddUriResult)
      case (rpcResponse >>= result) of
        Nothing -> putStrLn $ "Error in parsing addUri response. Ignoring=" ++ (show msg)
        Just AddUriResult{addUriGid=gid} -> do
          (userEntity, (DownloadCommand url)) <- userAndCommand tgramLog
          runDb $ createDownload url gid responseId (entityKey userEntity)
          writeChan tgOutChan TelegramOutgoingMessage{tg_chat_id=(telegramLogTgramChatId tgramLog), Ty.message=(T.pack $ "Download GID " ++ (show gid))}

    userAndCommand :: TelegramLog -> IO ((Entity User, NightwatchCommand))
    userAndCommand tgramLog = do
      userEntity <- (runDb $ fetchUserByTelegramUserId $ telegramLogTgramUserId tgramLog)
      case userEntity of
        Nothing -> error $ "Could not find VL User against TelegramLog=" ++ (show tgramLog)
        Just userEntity -> return (userEntity, (telegramLogNightwatchCommand tgramLog))

sendNightwatchCommand :: WS.Connection -> Aria2LogId -> AuthNightwatchCommand -> IO (BL.ByteString)
sendNightwatchCommand conn requestId authNwCommand = do
  jsonRequest <- case (command authNwCommand) of
                  DownloadCommand url -> jsonRpcRequest conn (Aria2MethodName "aria2.addUri") requestId [[url]]
                  _ -> error "Not implemented yet"
  return jsonRequest
  -- TODO: we need to figure out how to get the telegramLogId here.
  -- createAria2Log requestId (T.unpack jsonRequest) Nothing (userId authNwCommand)

aria2WebsocketSender :: Aria2Channel -> TelegramOutgoingChannel ->  WS.Connection -> IO ()
aria2WebsocketSender aria2Chan tgOutChan conn = forever $ do
  authNwCommand <- (readChan aria2Chan)
  putStrLn $ "Nightwatch command received:" ++ (show authNwCommand) ++ " Sending to Aria2 and logging to DB"
  requestId <- runDb $ nextAria2LogId
  -- TODO: We're making a network call here. There should be a some sort of error handling...
  -- TODO: we need to figure out how to get the telegramLogId here.
  jsonRequest <- (sendNightwatchCommand conn requestId authNwCommand)
  runDb $ do
    aria2Log <- fetchAria2LogById requestId
    case aria2Log of
      Nothing -> error $ "ERROR: Super-weirdness / just got this aria2LogId from the DB and now it's mossing=" ++ (show requestId)
      Just aria2Log -> updateAria2Log requestId aria2Log{aria2LogRequest=(Just $ BL.unpack jsonRequest), aria2LogUserId=(Just $ userId authNwCommand)}
  

aria2WebsocketClient :: Aria2Channel -> TelegramOutgoingChannel -> WS.ClientApp ()
aria2WebsocketClient aria2Chan tgOutChan conn = do
  a1 <- A.async (aria2WebsocketReceiver tgOutChan conn)
  a2 <- A.async (aria2WebsocketSender aria2Chan tgOutChan conn)
  A.waitAnyCancel [a1, a2]
  return ()

startAria2WebsocketClient :: Aria2Channel -> TelegramOutgoingChannel -> IO ()
startAria2WebsocketClient aria2Chan tgOutChan = do 
  forkIO $ forever $ void (WS.runClient "localhost" 9999 "/jsonrpc" $ aria2WebsocketClient aria2Chan tgOutChan) `catch` (\e -> putStrLn $ "ERROR IN websocket client: " ++ (show (e :: Control.Exception.SomeException)))
  return ()
