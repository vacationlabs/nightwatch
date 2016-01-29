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

jsonRpcRequest :: (ToJSON params) => WS.Connection -> String -> params -> IO ()
jsonRpcRequest conn method request = do
  WS.sendTextData conn $ encode $ JsonRpcRequest {_id="1", method=method, params=request}

aria2WebsocketReceiver :: WS.Connection -> IO ()
aria2WebsocketReceiver conn = do
  msg <- WS.receiveData conn
  T.putStrLn (msg :: T.Text)
  aria2WebsocketReceiver conn

  -- let v = do res <- decode msg
  --            flip parseMaybe res $ \o -> do
  --                                        r <- o .: "result"
  --                                        return $ "result=" ++ (show (r :: VersionResponse))


aria2WebsocketSender :: WS.Connection -> Int -> IO ()
aria2WebsocketSender conn i = do
  -- WS.sendTextData conn (T.pack $ "{\"jsonrpc\":\"2.0\", \"method\":\"aria2.getVersion\", \"id\":\"1\"}")
  jsonRpcRequest conn "aria2.addUri" [["https://www.vacationlabs.com"::String]]
  threadDelay (2*(10^6))
  aria2WebsocketSender conn (i + 1)

aria2WebsocketClient :: WS.ClientApp ()
aria2WebsocketClient conn = do
  a1 <- A.async (aria2WebsocketReceiver conn)
  a2 <- A.async (aria2WebsocketSender conn 0)
  A.waitAnyCancel [a1, a2]
  return ()

startAria2WebsocketClient :: IO ()
startAria2WebsocketClient = do 
  forkIO $ forever $ (WS.runClient "localhost" 9999 "/jsonrpc" aria2WebsocketClient) `catch` (\e -> putStrLn $ "ERROR IN websocket client: " ++ (show (e :: Control.Exception.SomeException)))
  return ()