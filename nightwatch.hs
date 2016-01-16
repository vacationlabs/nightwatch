{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens
import Network.Wreq
--import Data.ByteString.Lazy as BS (p)
--import Data.Aeson (FromJSON, ToJSON, decode, encode, Value)
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics (Generic)
--import Data.Map as Map
import Control.Concurrent
import Control.Monad (forever)
import Control.Concurrent.Chan
import Data.List (isPrefixOf, drop)
type Resp = Response TelegramResponse

botToken = "151105940:AAEUZbx4_c9qSbZ5mPN3usjXVwGZzj-JtmI"
apiBaseUrl = "https://api.telegram.org/bot" ++ botToken


removePrefix :: String -> String -> String
removePrefix prefix input 
  | isPrefixOf prefix input = drop (length prefix) input
  | otherwise = input

data User = User {
  user_id :: Integer,
  user_first_name :: String,
  user_last_name :: Maybe String,
  user_username :: Maybe String
} deriving (Show, Generic)

instance ToJSON User
instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = removePrefix "user_"
  }

data Chat = Chat {
  chat_id :: Integer
  --username :: Maybe String,
  --first_name :: Maybe String,
  --last_name :: Maybe String
} deriving (Show, Generic)

instance ToJSON Chat
instance FromJSON Chat where 
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = removePrefix "chat_"
  }

data Message = Message {
  message_id :: Int,
  from :: User,
  date :: Integer,
  chat :: Chat,
  text :: [Char],
  forward_from :: Maybe User,
  forward_date :: Maybe Integer,
  reply_to_message :: Maybe Message
} deriving (Show, Generic)

instance FromJSON Message
instance ToJSON Message

data Update = Update {
  update_id :: Integer,
  message :: Message
} deriving (Show, Generic)

instance FromJSON Update
instance ToJSON Update

data TelegramResponse = TelegramResponse {
  ok :: Bool,
  result :: [Update]
} deriving (Show, Generic)

instance FromJSON TelegramResponse
instance ToJSON TelegramResponse

getUpdates = do
  get (apiBaseUrl ++ "/getUpdates")

getUpdatesAsJSON = do
  asJSON =<< getUpdates :: IO Resp

sendMessage update txt = do
  let cid = chat_id $ chat $ message update
  post (apiBaseUrl ++ "/sendMessage") [ "chat_id" := cid, "text" := txt ]

-- TODO: There's probably a better way to do this
--processUpdates :: [Integer] -> [Update] -> ([Integer], [Update])
--processUpdates processedUpdateIds [] = (processedUpdateIds, [])
--processUpdates processedUpdateIds all@(update:incomingUpdates)
--  | elem updt_id processedUpdateIds = processUpdates processedUpdateIds incomingUpdates
--  | otherwise = processUpdates (updt_id:processedUpdateIds) incomingUpdates
--  where updt_id = (update_id update)

--main = do 
--  r <- asJSON =<< getUpdates :: IO Resp
--  let incomingUpdates = result $ r ^. responseBody
--      processedUpdateIds = processUpdates processedUpdateIds incomingUpdates
--  putStrLn $ show $ processUpdates processedUpdateIds incomingUpdates

doPollLoop replyChan processedUpdateIds = do
  threadDelay (10^6)
  r <- asJSON =<< getUpdates :: IO Resp
  let incomingUpdates = (result $ r ^. responseBody)
      updatesToProcess = filter (\update -> not $ elem (update_id update) processedUpdateIds) incomingUpdates
  putStrLn $ "Will process " ++ (show updatesToProcess)
  writeList2Chan replyChan updatesToProcess
  doPollLoop replyChan $ (map update_id updatesToProcess) ++ processedUpdateIds

sendCannedResponse :: Chan Update -> IO ()
sendCannedResponse replyChan = do
  update <- readChan replyChan
  putStrLn $ "Will send a canned response to " ++ (show update)
  --sendMessage update "works"
  sendCannedResponse replyChan

main = do 
  replyChan <- newChan
  forkIO $ doPollLoop replyChan []
  forkIO $ sendCannedResponse replyChan
  getLine
  putStrLn "exiting now"
