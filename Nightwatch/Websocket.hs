{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings     #-}
module Nightwatch.Websocket (startAria2WebsocketClient) where
import           Control.Concurrent
import qualified Control.Concurrent.Async as A
import           Control.Concurrent.Chan
import           Control.Error.Util (hoistEither)
import qualified Control.Lens as L
ximport           Control.Monad (forever)
import           Control.Monad.Except
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.Aeson.Lens as L
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL hiding (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL (pack, unpack)
import           Data.Char (toLower)
import qualified Data.Foldable as F(foldl')
import qualified Data.HashMap.Strict as HM
import qualified Data.List as DL
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import           Database.Persist.Sql (transactionSave)
import           Debug.Trace
import           GHC.Generics (Generic)
import qualified Network.WebSockets as WS
import           Nightwatch.DBTypes as DB
import           Nightwatch.Telegram
import qualified Nightwatch.Types as Ty(message)
import           Nightwatch.Types hiding (message)
import           Text.Printf
import           Text.Read (readMaybe)
import           Data.Typeable
import           Control.Monad.Catch
import           Control.Exception
import           Safe (fromJustNote)

data JsonRpcException = JsonRpcParseException String | JsonRpcErrorException JsonRpcError deriving (Show, Typeable)
instance Exception JsonRpcException

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
    fieldLabelModifier = \jsonKey -> case jsonKey of
        "response_id" -> "id"
        "rpc_error" -> "error"
        _ -> jsonKey
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

decodeJsonRpcResponse :: (FromJSON a, MonadThrow m) => BL.ByteString -> m (JsonRpcResponse a)
decodeJsonRpcResponse x = case (eitherDecode x) of
  Right r -> return r
  Left s -> throwM $ JsonRpcParseException s

extractJsonRpcResult :: (FromJSON a, MonadThrow m) => BL.ByteString -> m a
extractJsonRpcResult x = do
  r <- decodeJsonRpcResponse x
  case (rpc_error r) of
    Just e -> throwM $ JsonRpcErrorException e
    Nothing -> case (result r) of
      Nothing -> throwM $ JsonRpcParseException "Did not receive either Result OR Error in JSONRpcRespons"
      Just y -> return y

extractJsonRpcNotification :: (FromJSON a, MonadThrow m) => BL.ByteString -> m a
extractJsonRpcNotification x = do
  case (eitherDecode x :: ((FromJSON a) => Either String (JsonRpcNotification a))) of
    Left s -> throwM $ JsonRpcParseException s
    Right r -> return (notifParams r)


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
  st_errorCode :: Maybe String, -- TODO Convert this to (Maybe Integer)
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
    v .:? "errorCode" <*>
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
      Nothing -> websocketNotificationReceived_ msg
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

    websocketNotificationReceived_ :: BL.ByteString -> IO ()
    websocketNotificationReceived_ msg = runDb pool $ do
      logE <- logAria2Notification (BL.unpack msg)
      handleAria2Notification tgOutChan msg logE


handleAria2Notification :: TelegramOutgoingChannel -> BL.ByteString -> Entity Log -> NwApp ()
handleAria2Notification tgOutChan msg logE = do
  case (decode msg :: Maybe (JsonRpcNotification Value)) of
    Nothing -> liftIO $ putStrLn "Received blank notification from Aria2. Ignoring."
    Just n ->  case (notifMethod n) of
      "aria2.onDownloadComplete" -> onDownloadComplete
      _ -> liftIO $ putStrLn $ "Don't know how to handle this notification. Ignoring" ++ (show n)

  where
    onDownloadComplete :: NwApp ()
    onDownloadComplete = do
      v <- extractJsonRpcNotification msg :: NwApp Value
      let gid = fromJustNote "GID not found in reponse" $ v L.^? (L.nth 0) . (L.key "gid")
      dloadE <- fmap (fromJustNote $ "Download not found with GID" ++ (show gid)) (fetchDownloadByGid $ Aria2Gid $ valueToString gid)
      user <- fmap (fromJustNote ("User with this ID not found " ++ (show $ downloadUserId $ entityVal dloadE))) (fetchUserById $ downloadUserId $ entityVal dloadE)
      let chatId = fromJustNote ("User does not have telegram chat ID " ++ (show user)) (userTgramChatId user)
      -- Now, making another tellStatus call to send the download stats along with this notification.
      logAndSendTgramMessage (entityKey logE) TelegramOutgoingMessage{tg_chat_id=chatId, Ty.message=(TgramMsgText $ "Download completed. " ++ (show $ downloadGid $ entityVal dloadE) ++ " URL=" ++ (show $ downloadUrl $ entityVal dloadE))} tgOutChan

      


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
      handler = case nwCommand of
        Nothing -> throwM $ AssertionFailed $ "Not expecting nwCommand to be blank. log=" ++ (show aria2Log)
        Just InvalidCommand -> throwM $ AssertionFailed "ERROR: Very strange, how did an InvalidCommand get converted into an Aria2Log in the first place"
        Just (DownloadCommand url) -> handleAddUriResponse
        Just (StatusCommand gid) -> handleStatusResponse
        Just (PauseCommand gid) -> handlePauseResponse
        _ -> throwM $ PatternMatchFail $ "Have not implemented handling of such responses: (logId, request)=" ++ show (logId, nwCommand)
  runDb pool handler

  where
    chatId = fromJust $ logTgramChatId aria2Log
    nwCmd = fromJust $ logNwCmd aria2Log
    userId = fromJust $ logUserId aria2Log

    handleAddUriResponse :: NwApp ()
    handleAddUriResponse = do
      let (DownloadCommand url) = nwCmd
      gid <- extractJsonRpcResult msg :: NwApp Aria2Gid
      dloadE <- createDownload url gid logId userId
      let (Aria2Gid gidStr) = gid
          tgMsg = TelegramOutgoingMessage{tg_chat_id=chatId, Ty.message=(TgramMsgText $ "Download GID " ++ gidStr)}
      logAndSendTgramMessage logId tgMsg tgOutChan

    handlePauseResponse :: NwApp ()
    handlePauseResponse = do
      res <- extractJsonRpcResult msg :: NwApp String
      logAndSendTgramMessage logId TelegramOutgoingMessage{tg_chat_id=chatId, Ty.message=(TgramMsgText res)} tgOutChan

    handleStatusResponse :: NwApp ()
    handleStatusResponse = do
      response <- extractJsonRpcResult msg :: NwApp StatusResponse
      logAndSendTgramMessage logId TelegramOutgoingMessage{tg_chat_id=chatId, Ty.message=(TgramMsgText $ humanizeStatusResponse response)} tgOutChan

humanizeStatusResponse :: StatusResponse -> String
humanizeStatusResponse res
  | percentDownloaded_  == (fromIntegral 100) = printf "Download completed (%s)" (humanizeBytes $ st_totalLength res)
  | (length $ st_files res) > 1 = joinStrings (map downloadFileSummary (st_files res)) "\n"
  | otherwise = (printf "%.0f%% downloaded. %s to go (%s at %s/s)..."
                 percentDownloaded_
                 (downloadEta downloadSpeed etaSeconds)
                 (humanizeBytes remainingLength)
                 (humanizeBytes downloadSpeed))
  where
    percentDownloaded_ = (percentDownloaded (st_completedLength res) (st_totalLength res))
    downloadSpeed = st_downloadSpeed res
    remainingLength = ((st_totalLength res) - (st_completedLength res))
    etaSeconds :: Integer
    etaSeconds = remainingLength `div` downloadSpeed

downloadEta :: Integer -> Integer -> String
downloadEta downloadSpeed etaSeconds
  | downloadSpeed < 1 = "A long time"
  | etaSeconds < 1*60 = "Under a minute"
  | otherwise = joinStrings (map (\(cnt, str) -> (show cnt) ++ " " ++ str) (take 2 x)) ", "
  where
    (tMin, sec) = divMod etaSeconds 60
    (tHr, min) = if tMin==0 then (0, 0) else divMod tMin 60
    (tDays, hr) = if tHr==0 then (0, 0) else divMod tHr 24
    (weeks, days) = if tDays==0 then (0, 0) else divMod tDays 7
    x = filter (\(cnt, _) -> cnt>0) [(weeks, "weeks"), (days, "days"), (hr, "hours"), (min, "minutes")]

downloadFileSummary :: GetFilesResponse -> String
downloadFileSummary res = (printf "[%.0f] %s"
                           (percentDownloaded (gf_completedLength res) (gf_length res))
                           (gf_path res))

percentDownloaded :: Integer -> Integer -> Float
percentDownloaded completed total = ((fromIntegral completed) / (fromIntegral total)) * 100.0

downloadUriSummary :: GetUriResponse -> String
downloadUriSummary res = printf "[%s] %s" (gu_status res) (show $ gu_uri res)

prepareJsonRpcRequest :: Aria2RequestId -> NightwatchCommand -> BL.ByteString
prepareJsonRpcRequest requestId nwCommand = case nwCommand of
  DownloadCommand url -> prepareJsonRpcRequest_ "aria2.addUri" [[url]]
  StatusCommand gid -> prepareJsonRpcRequest_ "aria2.tellStatus" [gid]
  PauseCommand gid -> prepareJsonRpcRequest_ "aria2.pause" [gid]
  _ -> error "Not implemented yet"
  where
    prepareJsonRpcRequest_ :: (ToJSON param) => String -> param -> BL.ByteString
    prepareJsonRpcRequest_ method params = encode JsonRpcRequest{request_id=requestId, method=method, params=params}

humanizeBytes :: Integer -> String
humanizeBytes b
  | b < 1024 = printf "%d bytes" b
  | b <= 1024*1024 = printf "%.2f KB" (((fromIntegral b) / (1024))::Float)
  | b <= 1024*1024*1024 = printf "%.2f MB" (((fromIntegral b) / (1024*1024))::Float)
  | otherwise = printf "%.2f GB" (((fromIntegral b) / (1024*1024*1024))::Float)

   -- TODO: we need to figure out how to get the telegramLogId here.
  -- createAria2Log requestId (T.unpack jsonRequest) Nothing (userId authNwCommand)

aria2WebsocketSender :: ConnectionPool -> Aria2Channel -> TelegramOutgoingChannel ->  WS.Connection -> IO ()
aria2WebsocketSender pool aria2Chan tgOutChan conn = forever $ logAllExceptions "Error in aria2WebsocketSender" $ do
  authwNCmd <- (readChan aria2Chan)
  putStrLn $ "Nightwatch command received:" ++ (show authNwCmd) ++ " Sending to Aria2 and logging to DB"
  runDb pool $ do
    requestId <- liftIO nextRequestId
    let jsonRequest = prepareJsonRpcRequest requestId (command authNwCmd)
    updateWithAria2Request (logId authNwCmd) requestId (BL.unpack jsonRequest)
    transactionSave
    liftIO $ WS.sendTextData conn jsonRequest

aria2WebsocketClient :: ConnectionPool -> Aria2Channel -> TelegramOutgoingChannel -> WS.ClientApp ()
aria2WebsocketClient pool aria2Chan tgOutChan conn = do
  c <- newChan
  a1 <- A.async (websocketReceiver c)
  a2 <- A.async (websocketSender c)
  a3 <- A.async (websocketOrchestrator c [])
  --   a1 <- A.async (aria2WebsocketReceiver pool tgOutChan conn)
  --  a2 <- A.async (aria2WebsocketSender pool aria2Chan tgOutChan conn)
  A.waitAnyCancel [a1, a2, a3]
  return ()

  where
    websocketReceiver :: Chan (Either BL.ByteString Aria2ChannelMessage) -> IO ()
    websocketReceiver c = forever $ do
      msg <- WS.receiveData conn :: BL.ByteString
      writeChan (Left msg) c

    websocketSender :: Chan (Either BL.ByteString Aria2ChannelMessage) -> IO ()
    websocketSender c = forever $ do
      authNwCmd <- readChan aria2Chan
      writeChan (Right authNwCmd) c

    websocketOrchestrator :: Chan (Either BL.ByteString Aria2ChannelMessage) -> [(Aria2RequestId, Aria2ChannelMessage)] -> IO ()
    websocketOrchestrator c osRpcReqs = do
      m <- readChan c
      case m of
        -- Received a notification or response
        Left msg -> 
        -- About to make a request
        Right authNwCmd -> do
          requestId <- nextRequestId
          sendAria2Message requestId authNwCmd
          websocketOrchestrator c (requestId, authNwCmd):osRpcReqs

    sendAria2Message :: Aria2RequestId -> AuthNightwatchCommand -> IO ()
    sendAria2Message requestId authNwCmd = runDb pool $ do
      let jsonRequest = prepareJsonRpcRequest requestId (command authNwCmd)
      updateWithAria2Request (logId authNwCmd) requestId (BL.unpack jsonRequest)
      transactionSave
      liftIO $ WS.sendTextData conn jsonRequest
      

startAria2WebsocketClient :: ConnectionPool -> Aria2Channel -> TelegramOutgoingChannel -> IO ()
startAria2WebsocketClient pool aria2Chan tgOutChan = do 
  forkIO $ forever $ logAllExceptions "ERROR IN WS.runClient: " $ WS.runClient "localhost" 9999 "/jsonrpc" $ aria2WebsocketClient pool aria2Chan tgOutChan
  return ()

