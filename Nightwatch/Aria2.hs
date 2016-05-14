{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings     #-}
module Nightwatch.Aria2(
  Aria2RpcEndpoint
  ,JsonRpcException(..)
  ,Aria2Callbacks(..)
  ,defaultAria2Callbacks
  ,addUri
  ,tellStatus
  ,startWebsocketClient
  ,ariaRPCUrl
  ) where

import           Control.Exception
import           Control.Monad.Catch
import qualified Data.ByteString.Lazy as BL hiding (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL (pack, unpack)
import           Data.Char (toLower)
import           Data.UUID
import qualified Data.UUID.V1 as UUIDv1
import qualified Network.Wreq as W
import           Nightwatch.DBTypes
import           Nightwatch.Types hiding (nextRequestId)
import           Safe (fromJustNote)
import           Data.Typeable
import           Control.Monad.Catch
import           Control.Exception
import           Data.Aeson
import           Data.Aeson.Types
import qualified Control.Lens as L
import qualified Data.Aeson.Lens as L
import           Control.Concurrent
import qualified Control.Concurrent.Async as A
import qualified Network.WebSockets as WS
import           Control.Monad (forever)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.Functor (void)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

ariaRPCPort = 9999
ariaRPCUrl = "http://localhost:" ++ (show ariaRPCPort) ++ "/jsonrpc"

type Aria2RpcEndpoint = String

data JsonRpcException = JsonRpcParseException String | JsonRpcErrorException JsonRpcError deriving (Show, Typeable)
instance Exception JsonRpcException

-- newtype Aria2MethodName = Aria2MethodName String deriving (Show, Eq)
data Aria2Callbacks = Aria2Callbacks {
  onDownloadStart :: Aria2Gid -> IO (),
  onDownloadPause :: Aria2Gid -> IO (),
  onDownloadComplete :: Aria2Gid -> IO (),
  onDownloadStop :: Aria2Gid -> IO (),
  onDownloadError :: Aria2Gid -> IO ()
  }

defaultAria2Callbacks :: Aria2Callbacks
defaultAria2Callbacks = Aria2Callbacks {
  onDownloadStart = (\_ -> return ()),
  onDownloadPause = (\_ -> return ()),
  onDownloadComplete = (\_ -> return ()),
  onDownloadStop = (\_ -> return ()),
  onDownloadError = (\_ -> return ())
  }

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
   response_id :: Aria2RequestId,
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

parseBoolean :: String -> Bool
parseBoolean "true" = True
parseBoolean "false" = False
parseBoolean x = error $ "Cannot parse this as a boolean: " ++ (show x)

decodeJsonRpcResponse :: (FromJSON a, MonadThrow m) => BL.ByteString -> m (JsonRpcResponse a)
decodeJsonRpcResponse x = case (eitherDecode x) of
  Right r -> return r
  Left s -> throwM $ JsonRpcParseException s

-- extractJsonRpcResult :: (FromJSON a, MonadThrow m) => BL.ByteString -> m a
-- extractJsonRpcResult x = do
--   r <- decodeJsonRpcResponse x
--   case (rpc_error r) of
--     Just e -> throwM $ JsonRpcErrorException e
--     Nothing -> case (result r) of
--       Nothing -> throwM $ JsonRpcParseException "Did not receive either Result OR Error in JSONRpcRespons"
--       Just y -> return y

nextRequestId :: IO Aria2RequestId
nextRequestId = UUIDv1.nextUUID >>= \uuid ->
  case uuid of
    Nothing -> nextRequestId
    Just uuid -> return $ toString uuid

prepareJsonRpcRequest :: (ToJSON params) => String -> params -> IO (JsonRpcRequest params)
prepareJsonRpcRequest method params = do
  requestId <- nextRequestId
  return $ JsonRpcRequest{request_id=requestId, method=method, params=params}

encodeJsonRpcRequest :: (ToJSON params) => String -> params -> IO (BL.ByteString)
encodeJsonRpcRequest method params = fmap encode  (prepareJsonRpcRequest method params)

makeJsonRpcAndExtractResult :: (FromJSON res, ToJSON params) => Aria2RpcEndpoint -> String -> params -> IO (res)
makeJsonRpcAndExtractResult rpcEndpoint method params = do
  req <- prepareJsonRpcRequest method params
  res <- W.post rpcEndpoint (toJSON req)
  resJson <- W.asJSON res
  return $ fromJustNote "Expecting to find a result in the JSON-RPC response" $ result $ resJson L.^. W.responseBody

-- TODO: Handle StatusCodeException gracefully
-- TODO: Use lenses
addUri :: Aria2RpcEndpoint -> String -> IO (Aria2Gid)
addUri rpcEndpoint url = makeJsonRpcAndExtractResult rpcEndpoint "aria2.addUri" [[url]]

tellStatus :: Aria2RpcEndpoint -> Aria2Gid -> IO (StatusResponse)
tellStatus rpcEndpoint gid = makeJsonRpcAndExtractResult rpcEndpoint "aria2.tellStatus" [gid]

startWebsocketClient :: String -> Int -> String -> Aria2Callbacks -> IO ()
startWebsocketClient host port path callbacks = do
  forever $ logAllExceptions "ERROR IN startWebsocketClient: " $ WS.runClient host port path clientApp
  where
    clientApp :: WS.Connection -> IO ()
    clientApp conn = do
      d <- WS.receiveData conn
      case ((decode d) :: Maybe Value) of
        Nothing -> putStrLn ("Unable to parse notification" ++ (BL.unpack d))
        (Just v) -> do
          let incomingMethod = fromJustNote "No method name in notification" (v L.^? (L.key "method") . L._String)
              p = fromJustNote "No params in notification" (v L.^? (L.key "params"))
          void $ forkIO $ case incomingMethod of
            "aria2.onDownloadStart" -> (onDownloadStart callbacks) (extractGidFromNotification p)
            "aria2.onDownloadPause" -> (onDownloadPause callbacks) (extractGidFromNotification p)
            "aria2.onDownloadComplete" -> (onDownloadComplete callbacks) (extractGidFromNotification p)
            "aria2.onDownloadStop" -> (onDownloadStop callbacks) (extractGidFromNotification p)
            "aria2.onDownloadError" -> (onDownloadError callbacks) (extractGidFromNotification p)

extractGidFromNotification :: Value -> Aria2Gid
extractGidFromNotification v = Aria2Gid $ T.unpack $ fromJustNote ("Unexpected params in notification " ++ (show v)) (v L.^? (L.nth 0) . (L.key "gid") . L._String)

extractJsonRpcResult :: (FromJSON a) => Value -> a
extractJsonRpcResult v = do
  case (fromJSON v) of
    Error s -> error s
    Success x -> x

