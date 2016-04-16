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
import Data.Maybe (fromJust)
import Debug.Trace
import Data.Char(toLower)
import qualified Data.Aeson.Lens as L
import qualified Control.Lens as L
import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Monad.Trans.Maybe

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
   response_id :: Maybe Aria2RequestId,
   result :: Maybe p,
   rpc_error :: Maybe JsonRpcError
} deriving (Show, Generic)

instance FromJSON JsonRpcError
instance (FromJSON p) => FromJSON (JsonRpcResponse p) where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier=(\jsonKey -> if jsonKey=="response_id" then "id" else jsonKey)
  }

-- let r = "{\"jsonrpc\":\"2.0\",\"params\":[{\"gid\":\"9641d0fc8e4b424c\"}], \"method\":\"aria2.onDownloadStart\"}"

data JsonRpcNotification p = JsonRpcNotification {
  notifMethod :: String,
  notifParams :: p
} deriving (Show, Generic)

instance (FromJSON p) => FromJSON (JsonRpcNotification p) where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier=(removePrefix "notif") . (map toLower)
  }

--  let r = "{  \"id\":\"ce0ae9d8-0338-11e6-8001-3c075454c078\",  \"jsonrpc\":\"2.0\",  \"result\":{    \"bitfield\":\"80\",    \"completedLength\":\"12761\",    \"connections\":\"0\",    \"dir\":\"./downloads\",    \"downloadSpeed\":\"0\",    \"errorCode\":\"0\",    \"errorMessage\":\"\",    \"gid\":\"3e51037346ea55b6\",    \"numPieces\":\"1\",    \"pieceLength\":\"1048576\",    \"status\":\"complete\",    \"totalLength\":\"12761\",    \"uploadLength\":\"0\",    \"uploadSpeed\":\"0\",    \"files\":[        {          \"completedLength\":\"12761\",          \"index\":\"1\",          \"length\":\"12761\",          \"path\":\"./downloads/index.html.31\",          \"selected\":\"true\",          \"uris\":[            {              \"status\":\"used\",              \"uri\":\"http://www.google.com\"            },            {              \"status\":\"waiting\",              \"uri\":\"http://www.google.com\"            }            ]        }]    }  }"

-- let r = "[        {          \"completedLength\":\"12761\",          \"index\":\"1\",          \"length\":\"12761\",          \"path\":\"./downloads/index.html.31\",          \"selected\":\"true\",          \"uris\":[            {              \"status\":\"used\",              \"uri\":\"http://www.google.com\"            },            {              \"status\":\"waiting\",              \"uri\":\"http://www.google.com\"            }            ]        }]"

data GetUriResponse = GetUriResponse {
  gu_uri :: URL,
  gu_status :: String
 } deriving (Show, Generic)

instance FromJSON GetUriResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = (removePrefix "gu_") }

data GetFilesResponse = GetFilesResponse {
  gf_index :: Integer,
  gf_path :: String,
  gf_length :: Integer,
  gf_completedLength :: Integer,
  gf_selected :: Bool,
  gf_uris :: [GetUriResponse]
  } deriving (Show, Generic)

instance FromJSON GetFilesResponse where
  parseJSON (Object v) = GetFilesResponse <$>
    fmap read (v .: "index") <*>
    v .: "path" <*>
    fmap read (v .: "length") <*>
    fmap read (v .: "completedLength") <*>
    fmap parseBoolean (v .: "selected") <*>
    v .: "uris"

parseBoolean :: String -> Bool
parseBoolean "true" = True
parseBoolean "false" = False
parseBoolean x = error $ "Cannot parse this as a boolean: " ++ (show x)

data StatusResponse = StatusResponse {
  st_gid :: Aria2Gid,
  st_status :: String,
  st_totalLength :: Integer,
  st_completedLength :: Integer,
  st_uploadLength :: Integer,
  st_downloadSpeed :: Integer,
  st_uploadSpeed :: Integer,
  st_infoHash :: Maybe String,
  st_numSeeders :: Maybe Integer,
  st_seeder :: Maybe Bool,
  st_pieceLength :: Integer,
  st_numPieces :: Integer,
  st_connections :: Integer,
  st_errorCode :: Integer,
  st_errorMessage :: Maybe String,
  st_followedBy :: Maybe [Aria2Gid],
  st_following :: Maybe Aria2Gid,
  st_belongsTo :: Maybe Aria2Gid,
  st_dir :: String,
  st_files :: [GetFilesResponse],
  st_bittorrent :: Maybe Value
  } deriving (Show, Generic)

instance FromJSON StatusResponse where
  parseJSON (Object v) = StatusResponse <$>
    v .: "gid" <*>
    v .: "status" <*>
    fmap read (v .: "totalLength") <*>
    fmap read (v .: "completedLength") <*>
    fmap read (v .: "uploadLength") <*>
    fmap read (v .: "downloadSpeed") <*>
    fmap read (v .: "uploadSpeed") <*>
    v .:? "infoHash" <*>
    (fmap . fmap) read (v .:? "numSeeders") <*>
    (fmap . fmap) parseBoolean (v .:? "seeder") <*>
    fmap read (v .: "pieceLength") <*>
    fmap read (v .: "numPieces") <*>
    fmap read (v .: "connections") <*>
    fmap read (v .: "errorCode") <*>
    v .:? "errorMessage" <*>
    v .:? "followedBy" <*>
    v .:? "following" <*>
    v .:? "belongsTo" <*>
    v .: "dir" <*>
    v .: "files" <*>
    v .:? "bittorrent"

-- instance (FromJSON p) => FromJSON (JsonRpcResponse p) where
--   parseJSON (Object v) = do
--     response_id <- v .: "response_id"
--     result <- v .: "result"
--     rpc_error <- v .: "rpc_error"
--     return JsonRpcResponse{response_id=response_id, result=result, rpc_error=rpc_error}
  
-- instance FromJSON AddUriResult where
--   parseJSON = genericParseJSON defaultOptions {
--     fieldLabelModifier = removePrefix "addUri",
--     constructorTagModifier = T.unpack . T.toLower . T.pack -- TODO: Figure out the correct lowcase func
--   }



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
      Nothing -> websocketNotificationReceived_ msg obj 
      Just (String requestId) -> websocketResponseReceived_ msg (T.unpack requestId) obj
  where
    websocketResponseReceived_ :: BL.ByteString -> Aria2RequestId -> Object -> IO ()
    websocketResponseReceived_ msg requestId obj = do
      logEntity <- runDb pool $ fetchLogByRequestId requestId
      putStrLn $ "Received response to requestId " ++ (show requestId) ++ ". Retrieved log from the DB=" ++ (show logEntity)
      case logEntity of
        Nothing -> putStrLn $ "Could not find Aria2Log with ID=" ++ (show requestId)
        Just l -> do
          runDb pool $ updateWithAria2Response (entityKey l) (BL.unpack msg)
          handleAria2Response pool tgOutChan (entityKey l) msg (entityVal l)
          putStrLn $ "Received response to requestId " ++ (show requestId) ++ " and handled successfully."

    websocketNotificationReceived_ :: BL.ByteString -> Object -> IO ()
    websocketNotificationReceived_ msg obj = do
      logE <- runDb pool $ logAria2Notification (BL.unpack msg)
      let method = (HM.lookup "method" obj)
      let exceptT = case method of
            Just (String "aria2.onDownloadComplete") -> onDownloadComplete tgOutChan msg logE
            _ -> throwError $ "Don't know how to handle this notification. Ignoring=" ++ (show obj)
      r <- (runDb pool $ runExceptT exceptT)
      case r of
        Left s -> putStrLn $ "ERROR: " ++ s
        Right r -> putStrLn $ "Handled " ++ (show method) ++  "successfully"

onDownloadComplete :: TelegramOutgoingChannel -> BL.ByteString -> Entity Log -> ExceptT String NwApp ()
onDownloadComplete tgOutChan msg logE = do
  n <- case (eitherDecode msg :: Either String (JsonRpcNotification Value)) of
    Left s -> throwError s
    Right n -> return n
  gid <- maybeToExceptT ("GID not found in response" ++ (show n)) (MaybeT $ return $ (notifParams n) L.^? (L.nth 0) . (L.key "gid"))
  dloadE <- maybeToExceptT ("Download with this GID not found " ++ (show gid)) (MaybeT $ fetchDownloadByGid $ Aria2Gid $ valueToString gid)
  user <- maybeToExceptT ("User with this ID not found " ++ (show $ downloadUserId $ entityVal dloadE)) $ (MaybeT $ fetchUserById $ downloadUserId $ entityVal dloadE)
  chatId <- maybeToExceptT ("User does not have telegram chat ID " ++ (show user)) $ (MaybeT $ return $ userTgramChatId user)
  lift $ logAndSendTgramMessage (entityKey logE) TelegramOutgoingMessage{tg_chat_id=chatId, Ty.message=(TgramMsgText $ "Download completed. GID=" ++ (show $ downloadGid $ entityVal dloadE) ++ " URL=" ++ (show $ downloadUrl $ entityVal dloadE))} tgOutChan

-- withEitherHandling :: (MonadIO m) => Either String (m a) -> m a
-- withEitherHandling v = case v of
--   Left s -> putStrLn $ "ERROR: " ++ (show s)
--   Right m -> m

-- maybeToEither :: String -> Maybe a -> Either String a
-- maybeToEither s m = case m of
--   Nothing -> Left s
--   Just x -> Right x



  -- case (eitherDecode msg :: Either String (JsonRpcNotification Value)) of
  --   Left s -> error s
  --   Right n -> do
  --     let (String gid) = fromJust $ n ^? (nth 0) . (key "gid")
  --     dloadE <- runDb pool $ fetchDownloadByGid $ T.unpack gid
  --     case dloadE of
  --       Nothing -> putStrLn $ "Could not find download with GID" ++ (show gid)
  --       Just dloadE -> do
  --         userE <- runDb pool $ fetchUserById $ downloadUserId $ entityVal dloadE
  --         case (userTgramChatId $ entityVal userE) of
  --           Nothing -> putStrLn $ "User does not have a telegram chat ID, cannot notify. User=" ++ (show userE)
  --           Just chatId -> runDb pool $ logAndSendTgramMessage (entityKey logE) TelegramOutgoingMessage{tg_chat_id=chatId, message=(TgramMsgText "Download completed. GID=" ++ (show $ downloadGid $ entityVal dloadE) ++ " URL=" ++ (show $ downloadUrl entityVal dloadE))} tgOutChan
 


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
handleAria2Response :: ConnectionPool -> TelegramOutgoingChannel -> LogId -> BL.ByteString -> Log -> IO ()
handleAria2Response pool tgOutChan logId msg aria2Log = do
  let nwCommand = logNwCmd aria2Log
  case nwCommand of
    Nothing -> error $ "Not expecting nwCommand to be blank. log=" ++ (show aria2Log)
    Just InvalidCommand -> putStrLn $ "ERROR: Very strange, how did an InvalidCommand get converted into an Aria2Log in the first place"
    Just (DownloadCommand url) -> handleAddUriResponse
    Just (StatusCommand gid) -> runDb pool $ logAndSendTgramMessage logId TelegramOutgoingMessage{tg_chat_id=(fromJust $ logTgramChatId aria2Log), Ty.message=(TgramMsgText (BL.unpack msg))} tgOutChan
    _ -> putStrLn $ "Have not implemented handling of such responses: (logId, request)=" ++ show (logId, nwCommand)

  where
    handleAddUriResponse :: IO ()
    handleAddUriResponse = do
      putStrLn "===> about to handleAddUriResponse"
      traceShowM (decode msg :: Maybe (JsonRpcResponse Aria2Gid))
      let gid = fromJust $ result $ fromJust $ (decode msg :: Maybe (JsonRpcResponse Aria2Gid))
      putStrLn $ "===> PARSED" ++ (show gid)
      let (DownloadCommand url, userId, chatId) = (fromJust $ logNwCmd aria2Log, fromJust $ logUserId aria2Log, fromJust $ logTgramChatId aria2Log)
      dloadEntity <- runDb pool $ createDownload url gid logId userId
      let tgMsg = TelegramOutgoingMessage{tg_chat_id=chatId, Ty.message=(TgramMsgText $ "Download GID " ++ (show $ downloadGid $ entityVal dloadEntity))}
      runDb pool $ updateWithTgramOutgoingMsg logId tgMsg
      writeChan tgOutChan  tgMsg

    -- handleStatusResponse :: ExceptT String NwApp ()
    -- handleStatusResponse = do
    --   eitherDecode msg :: Either String (JsonRpcResponse Aria2Status)
      

prepareJsonRpcRequest :: Aria2RequestId -> NightwatchCommand -> BL.ByteString
prepareJsonRpcRequest requestId nwCommand = case nwCommand of
  DownloadCommand url -> prepareJsonRpcRequest_ "aria2.addUri" [[url]]
  StatusCommand gid -> prepareJsonRpcRequest_ "aria2.tellStatus" [gid]
  _ -> error "Not implemented yet"
  where
    prepareJsonRpcRequest_ :: (ToJSON param) => String -> param -> BL.ByteString
    prepareJsonRpcRequest_ method params = encode JsonRpcRequest{request_id=requestId, method=method, params=params}

   -- TODO: we need to figure out how to get the telegramLogId here.
  -- createAria2Log requestId (T.unpack jsonRequest) Nothing (userId authNwCommand)

aria2WebsocketSender :: ConnectionPool -> Aria2Channel -> TelegramOutgoingChannel ->  WS.Connection -> IO ()
aria2WebsocketSender pool aria2Chan tgOutChan conn = forever $ logAllExceptions "Error in aria2WebsocketSender" $ do
  authNwCmd <- (readChan aria2Chan)
  putStrLn $ "Nightwatch command received:" ++ (show authNwCmd) ++ " Sending to Aria2 and logging to DB"
  runDb pool $ do
    requestId <- liftIO nextRequestId
    let jsonRequest = prepareJsonRpcRequest requestId (command authNwCmd)
    updateWithAria2Request (logId authNwCmd) requestId (BL.unpack jsonRequest)
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

