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
import Control.Monad (forever)
import Control.Concurrent.Chan
import Nightwatch.Telegram
import Nightwatch.Types hiding (message)
import qualified Nightwatch.Types as Ty(message)
import qualified Data.List as DL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as TL
import Nightwatch.DBTypes as DB
import Text.Read (readMaybe)
import Control.Monad.IO.Class  (liftIO, MonadIO)
import Database.Persist.Sql (transactionSave)

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

data JsonRpcResponse p = JsonRpcResponse {
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


-- jsonRpcRequest :: (ToJSON params) => WS.Connection -> Aria2MethodName -> Aria2LogId -> params -> IO (BL.ByteString)
-- jsonRpcRequest conn method request_id request = do
--   let (Aria2MethodName mname)  = method
--   let x = 
--   putStrLn $ "===> SENDING TO Aria2: " ++ (show x)
--   WS.sendTextData conn $ x
--   return x

aria2WebsocketReceiver :: ConnectionPool -> TelegramOutgoingChannel -> WS.Connection -> IO ()
aria2WebsocketReceiver pool tgOutChan conn = forever $ logAllExceptions "Error in aria2WebsocketReceiver: " $ do
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
    aria2WebsocketReceiver_ msg requestId obj = do
      let k = mkRequestId requestId
      aria2Log <- runDb pool $ fetchAria2LogById k
      putStrLn $ "Received response to requestId " ++ (show requestId) ++ ". Retrieved log from the DB=" ++ (show aria2Log)
      case aria2Log of
        Nothing -> putStrLn $ "Could not find Aria2Log with ID=" ++ (show requestId)
        Just l -> do
          runDb pool $ updateAria2Log k l{aria2LogResponse=(Just $ BL.unpack msg)}
          handleAria2Response pool tgOutChan k msg l
          putStrLn $ "Received response to requestId " ++ (show requestId) ++ " and handled successfully."

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
handleAria2Response :: ConnectionPool -> TelegramOutgoingChannel -> Aria2LogId -> BL.ByteString -> Aria2Log -> IO ()
handleAria2Response pool tgOutChan responseId msg aria2Log = do
  let tgramLogId = aria2LogTelegramLogId aria2Log
  case tgramLogId of
    Nothing -> putStrLn $ "ERROR: Aria2Log does not have a correspndonding TelegramLogId. Ignoring=" ++ (show aria2Log)
    Just tgramLogId -> do
      tgramLog <- runDb pool $ fetchTelegramLogById tgramLogId
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
          runDb pool $ createDownload url gid responseId (entityKey userEntity)
          writeChan tgOutChan TelegramOutgoingMessage{tg_chat_id=(telegramLogTgramChatId tgramLog), Ty.message=(T.pack $ "Download GID " ++ (show gid))}

    userAndCommand :: TelegramLog -> IO ((Entity User, NightwatchCommand))
    userAndCommand tgramLog = do
      userEntity <- (runDb pool $ fetchUserByTelegramUserId $ telegramLogTgramUserId tgramLog)
      case userEntity of
        Nothing -> error $ "Could not find VL User against TelegramLog=" ++ (show tgramLog)
        Just userEntity -> return (userEntity, (telegramLogNightwatchCommand tgramLog))

prepareJsonRpcRequest :: Aria2LogId -> AuthNightwatchCommand -> BL.ByteString
prepareJsonRpcRequest requestId authNwCommand = case (command authNwCommand) of
  DownloadCommand url -> prepareJsonRpcRequest_ "aria2.addUri" [[url]]
  _ -> error "Not implemented yet"
  where
    prepareJsonRpcRequest_ :: (ToJSON param) => String -> param -> BL.ByteString
    prepareJsonRpcRequest_ method params = encode JsonRpcRequest{request_id=(show $ unMkRequestId requestId), method=method, params=params}

   -- TODO: we need to figure out how to get the telegramLogId here.
  -- createAria2Log requestId (T.unpack jsonRequest) Nothing (userId authNwCommand)

aria2WebsocketSender :: ConnectionPool -> Aria2Channel -> TelegramOutgoingChannel ->  WS.Connection -> IO ()
aria2WebsocketSender pool aria2Chan tgOutChan conn = forever $ logAllExceptions "Error in aria2WebsocketSender" $ do
  authNwCommand <- (readChan aria2Chan)
  putStrLn $ "Nightwatch command received:" ++ (show authNwCommand) ++ " Sending to Aria2 and logging to DB"
  runDb pool $ do
    requestId <- nextAria2LogId
    let jsonRequest = prepareJsonRpcRequest requestId authNwCommand
    logAria2Request requestId (Just $ BL.unpack jsonRequest) (Just $ userId authNwCommand)
    transactionSave
    liftIO $ WS.sendTextData conn jsonRequest

aria2WebsocketClient :: ConnectionPool -> Aria2Channel -> TelegramOutgoingChannel -> WS.ClientApp ()
aria2WebsocketClient pool aria2Chan tgOutChan conn = do
  a1 <- A.async (aria2WebsocketReceiver pool tgOutChan conn)
  a2 <- A.async (aria2WebsocketSender pool aria2Chan tgOutChan conn)
  A.waitAnyCancel [a1, a2]
  return ()

startAria2WebsocketClient :: ConnectionPool -> Aria2Channel -> TelegramOutgoingChannel -> IO ()
startAria2WebsocketClient pool aria2Chan tgOutChan = do 
  forkIO $ forever $ logAllExceptions "ERROR IN WS.runClient: " $ WS.runClient "localhost" 9999 "/jsonrpc" $ aria2WebsocketClient pool aria2Chan tgOutChan
  return ()

