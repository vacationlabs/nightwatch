{-# LANGUAGE OverloadedStrings     #-}
module Nightwatch.Websocket (startAria2WebsocketClient) where
import qualified Network.WebSockets  as WS
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Control.Concurrent.Async as A
import qualified Data.Map as M
import Data.Aeson
import Control.Concurrent
import qualified Data.ByteString.Lazy.Internal as BS
import Data.ByteString.Lazy.Internal (ByteString)
import Control.Exception (catch, try, tryJust, bracketOnError, SomeException, Exception)
import Control.Monad (forever)
import Control.Concurrent.Chan
import Nightwatch.Telegram
import Nightwatch.Types
import Data.Functor (void)

type OutstandingRpcRequest = (String, AuthNightwatchCommand)

data VersionResponse = VersionResponse {
  version :: String,
  enabledFeatures :: [String]
} deriving (Show)

instance FromJSON VersionResponse where
  parseJSON (Object v) = VersionResponse <$>
                         v .: "version" <*>
                         v .: "enabledFeatures"

data JsonRpcRequest p = JsonRpcRequest {
  _id :: String,
  method :: String,
  params :: p 
} deriving (Show)

instance (ToJSON p) => ToJSON (JsonRpcRequest p) where
  toJSON (JsonRpcRequest _id method params) = object ["jsonrpc" .= ("2.0"::String), "id" .= _id, "method" .= method, "params" .= params]

  -- toEncoding (JsonRpcRequest _id method params) = pairs ("jsonrpc" .= "2.0" <> "id" .= _id <> "method" .= method <> "params" .= params)

jsonRpcRequest :: (ToJSON params) => WS.Connection -> String -> Int -> params -> IO ()
jsonRpcRequest conn method _id request = do
  WS.sendTextData conn $ encode $ JsonRpcRequest {_id=(show _id), method=method, params=request}

aria2WebsocketReceiver :: Aria2Channel -> WS.Connection -> IO ()
aria2WebsocketReceiver aria2Chan conn = do
  msg <- WS.receiveData conn
  writeChan aria2Chan (Left (msg :: T.Text))
  aria2WebsocketReceiver aria2Chan conn

  -- let v = do res <- decode msg
  --            flip parseMaybe res $ \o -> do
  --                                        r <- o .: "result"
  --                                        return $ "result=" ++ (show (r :: VersionResponse))

sendNightwatchCommand :: WS.Connection -> Int -> AuthNightwatchCommand -> IO ()
sendNightwatchCommand conn _id (AuthNightwatchCommand{user=_, command=(DownloadCommand url)}) = jsonRpcRequest conn "aria2.addUri" _id [[url]]
sendNightwatchCommand _ _ _ = error "Not implemented yet"

aria2WebsocketSender :: Aria2Channel -> TelegramOutgoingChannel ->  WS.Connection -> [OutstandingRpcRequest] -> Int -> IO ()
aria2WebsocketSender aria2Chan tgOutChan conn outstandingRpc _id = do
  msg <- readChan aria2Chan
  newOutstandingRpc <-  case msg of
                        (Left websocketResponse) -> do
                                                    putStrLn $ "Websocket response received: " ++ (show websocketResponse)
                                                    writeChan tgOutChan $ TelegramOutgoingMessage {tg_chat_id=1, message=websocketResponse}
                                                    return $ filter (\(_i, _) -> _id /= 5) outstandingRpc
                                                      -- T.putStrLn ("websocket response received:" ++ (websocketResponse :: T.Text))
                                                      -- TODO: Do something with the the response
                                                      
                        (Right nwCommand) ->  do 
                                              putStrLn $ "Nightwatch command received:" ++ (show nwCommand)
                                              sendNightwatchCommand conn _id nwCommand
                                              return $ ((show _id), nwCommand):outstandingRpc
  putStrLn ("outstandingRpc ==> " ++ (show outstandingRpc))
  aria2WebsocketSender aria2Chan tgOutChan conn newOutstandingRpc (_id + 1)
  -- threadDelay (2*(10^6))
  -- jsonRpcRequest conn "aria2.addUri" _id   [["https://www.vacationlabs.com"::String]]
  -- WS.sendTextData conn (T.pack $ "{\"jsonrpc\":\"2.0\", \"method\":\"aria2.getVersion\", \"id\":\"1\"}")

aria2WebsocketClient :: Aria2Channel -> TelegramOutgoingChannel -> WS.ClientApp ()
aria2WebsocketClient aria2Chan tgOutChan conn = do
  a1 <- A.async (aria2WebsocketReceiver aria2Chan conn)
  a2 <- A.async (aria2WebsocketSender aria2Chan tgOutChan conn [] 1)
  A.waitAnyCancel [a1, a2]
  return ()

startAria2WebsocketClient :: Aria2Channel -> TelegramOutgoingChannel -> IO ()
startAria2WebsocketClient aria2Chan tgOutChan = do 
  forkIO $ forever $ void (WS.runClient "localhost" 9999 "/jsonrpc" $ aria2WebsocketClient aria2Chan tgOutChan) `catch` (\e -> putStrLn $ "ERROR IN websocket client: " ++ (show (e :: Control.Exception.SomeException)))
  return ()