{-# LANGUAGE OverloadedStrings     #-}
module Nightwatch.Websocket (startAria2WebsocketClient) where
import qualified Network.WebSockets  as WS
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Control.Concurrent.Async as A
import qualified Data.Map as M
import Data.Aeson
import Data.Aeson.Types(Parser, parseMaybe)
import Control.Concurrent
import qualified Data.ByteString.Lazy.Internal as BS
import Data.ByteString.Lazy.Internal (ByteString)
import Control.Exception (catch, try, tryJust, bracketOnError, SomeException, Exception)
import Control.Monad (forever)
import Control.Concurrent.Chan
import Nightwatch.Telegram
import Nightwatch.Types
import Data.Functor (void)
import qualified Data.List as DL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as TL

newtype Aria2RequestID = Aria2RequestID String deriving (Show, Eq) 
newtype Aria2MethodName = Aria2MethodName String deriving (Show, Eq)
type OutstandingRpcRequest = (Aria2RequestID, AuthNightwatchCommand)
type OutstandingRpcRequests = MVar [OutstandingRpcRequest]

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

-- data JsonRpcError = JsonRpcError {
--   code :: Integer,
--   message :: String
-- } deriving (Show)

-- data JsonRpcResponse p = JsonRpcResponse {
--    response_id :: Maybe String,
--    result :: Maybe p,
--    rpc_error :: Maybe JsonRpcError
-- } deriving (Show)

-- instance FromJSON JsonRpcError where
--   parseJSON (Object v) =  
--     JsonRpcError  <$> v .: "code"
--                   <*> v .: "message"

-- instance (FromJSON p) => FromJSON (JsonRpcResponse p) where
--   parseJSON (Object v) =  
--     JsonRpcResponse p <$> v .:? "id"
--                       <*> v .:? "result"
--                       <*> v .:? "error"

jsonRpcRequest :: (ToJSON params) => WS.Connection -> Aria2MethodName -> Aria2RequestID -> params -> IO ()
jsonRpcRequest conn method request_id request = do
  let (Aria2MethodName mname)  = method
      (Aria2RequestID rid) = request_id
  let x = encode $ JsonRpcRequest {request_id=rid, method=mname, params=request}
  putStrLn $ "===> SENDING TO Aria2: " ++ (show x)
  WS.sendTextData conn $ x


  -- let v = do res <- decode msg
  --            flip parseMaybe res $ \o -> do
  --                                        r <- o .: "result"
  --                                        return $ "result=" ++ (show (r :: VersionResponse))



-- addUri ==> GID (String)
-- remove ==> GID (String)
-- pause ==> GID (String)
-- unpause ==> GID (String
-- tellStatus ==> Key-value Pair String:String for most of the cases except 'files', which is an array of key-value pairs
aria2WebsocketReceiver :: TelegramOutgoingChannel -> OutstandingRpcRequests -> WS.Connection -> IO ()
aria2WebsocketReceiver tgOutChan osRpcReqs conn = do
  let loop = do   msg <- WS.receiveData conn
                  let r = decode msg :: Maybe Object
                  case r of
                    Nothing -> return ()
                    Just res -> case (HM.lookup "id" res) of 
                                      -- Seems to be a notification
                                      Nothing -> putStrLn $ "RECEIVED NOTIFICATION: " ++ (show res)
                                      -- Seems to be a response to an outstanding RPC
                                      (Just (String response_id)) -> handleAria2Response osRpcReqs tgOutChan (Aria2RequestID $ T.unpack response_id) res
                                      (Just _) -> putStrLn $ "RANDOM data in 'id' field" ++ (show res)

  loop
  return ()

valueToString :: Value -> String
valueToString value = case (fromJSON value :: Result String) of
  (Success s) -> s
  (Error s) -> s


findRequest :: Aria2RequestID -> (OutstandingRpcRequest -> Bool)
findRequest r = (\(request_id, _) -> r==request_id)

aria2Response2TelegramMessage :: [OutstandingRpcRequest] -> Aria2RequestID -> Object -> Maybe TelegramOutgoingMessage
aria2Response2TelegramMessage requestList response_id obj = do
  (request_id, nwCommand) <- DL.find (findRequest response_id) requestList
  case (command nwCommand) of
    DownloadCommand{url=url} -> (HM.lookup "result" obj) >>= (\result -> Just $ TelegramOutgoingMessage {tg_chat_id=(chat_id nwCommand), message=(T.pack $ "Download GID " ++ (valueToString result))})
    _ -> Nothing


handleAria2Response :: OutstandingRpcRequests -> TelegramOutgoingChannel -> Aria2RequestID -> Object -> IO ()
handleAria2Response osRpcReqs tgOutChan response_id obj = do
  -- result <- HM.lookup "result" obj
  modifyMVar_ osRpcReqs (\requestList -> 
    case  (aria2Response2TelegramMessage requestList response_id obj) of
      Nothing -> return requestList
      Just tgMessage -> (writeChan tgOutChan tgMessage) >> (return $ DL.filter (findRequest response_id) requestList)
    )
  return ()

  
sendNightwatchCommand :: WS.Connection -> Aria2RequestID -> AuthNightwatchCommand -> IO ()
sendNightwatchCommand conn request_id (AuthNightwatchCommand{user=_, command=(DownloadCommand url)}) = jsonRpcRequest conn (Aria2MethodName "aria2.addUri") request_id [[url]]
sendNightwatchCommand _ _ _ = error "Not implemented yet"

aria2WebsocketSender :: Aria2Channel -> OutstandingRpcRequests -> TelegramOutgoingChannel ->  WS.Connection -> IO ()
aria2WebsocketSender aria2Chan osRpcReqs tgOutChan conn = do
  let loop rid = do   msg <- readChan aria2Chan
                      putStrLn $ "Nightwatch command received:" ++ (show msg)
                      let aria2RequestID = (Aria2RequestID $ show rid)
                      sendNightwatchCommand conn aria2RequestID msg
                      modifyMVar_ osRpcReqs (\a -> return $ (aria2RequestID, msg):a)
                      loop $ rid + 1
  loop 0
  return ()

aria2WebsocketClient :: Aria2Channel -> TelegramOutgoingChannel -> WS.ClientApp ()
aria2WebsocketClient aria2Chan tgOutChan conn = do
  osRpcReqs <- newMVar []
  a1 <- A.async (aria2WebsocketReceiver tgOutChan osRpcReqs conn)
  a2 <- A.async (aria2WebsocketSender aria2Chan osRpcReqs tgOutChan conn)
  A.waitAnyCancel [a1, a2]
  return ()

startAria2WebsocketClient :: Aria2Channel -> TelegramOutgoingChannel -> IO ()
startAria2WebsocketClient aria2Chan tgOutChan = do 
  forkIO $ forever $ void (WS.runClient "localhost" 9999 "/jsonrpc" $ aria2WebsocketClient aria2Chan tgOutChan) `catch` (\e -> putStrLn $ "ERROR IN websocket client: " ++ (show (e :: Control.Exception.SomeException)))
  return ()