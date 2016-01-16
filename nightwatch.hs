{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens
import Network.Wreq
--import Data.ByteString.Lazy as BS (p)
import Data.Aeson (FromJSON, ToJSON, decode, encode, Value)
import GHC.Generics (Generic)
import Data.Map as Map
import Control.Concurrent
import Control.Monad (forever)
type Resp = Response TelegramResponse

botToken = "151105940:AAEUZbx4_c9qSbZ5mPN3usjXVwGZzj-JtmI"
apiBaseUrl = "https://api.telegram.org/bot" ++ botToken

data User = User {
  id :: Integer,
  first_name :: [Char],
  last_name :: Maybe [Char],
  username :: Maybe [Char]
} deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

data Message = Message {
  message_id :: Int,
  from :: User,
  date :: Integer,
  --chat :: Chat,
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

processUpdates :: [Integer] -> [Update] -> [Integer]
processUpdates processedUpdateIds [] = processedUpdateIds
processUpdates processedUpdateIds all@(update:incomingUpdates)
  | elem updt_id processedUpdateIds = processUpdates processedUpdateIds incomingUpdates
  | otherwise = processUpdates (updt_id:processedUpdateIds) incomingUpdates
  where updt_id = (update_id update)

--main = do 
--  r <- asJSON =<< getUpdates :: IO Resp
--  let incomingUpdates = result $ r ^. responseBody
--      processedUpdateIds = processUpdates processedUpdateIds incomingUpdates
--  putStrLn $ show $ processUpdates processedUpdateIds incomingUpdates

doPollLoop processedUpdateIds = do
  threadDelay (10^6)
  r <- asJSON =<< getUpdates :: IO Resp
  putStrLn $ "Alredy procssed " ++ (show processedUpdateIds)
  doPollLoop (processUpdates processedUpdateIds (result $ r ^. responseBody))

main = do 
  forkIO $ doPollLoop []
  getLine
  putStrLn "exiting now"
