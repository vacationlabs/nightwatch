{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens
import Network.Wreq
import Data.ByteString.Lazy as BS
import Data.Aeson (FromJSON, ToJSON, decode, encode, Value)
import GHC.Generics (Generic)
import Data.Map as Map
type Resp = Response TelegramResponse

botToken = "151105940:AAEUZbx4_c9qSbZ5mPN3usjXVwGZzj-JtmI"
apiBaseUrl = "https://api.telegram.org/bot" ++ botToken
processedUpdateIds = []

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

--processUpdates :: [a] -> [b] -> [a]
--processUpdates processedUpdateIds [] = processedUpdateIds
--processUpdates processedUpdateIds (update:incomingUpdates) = fmap

main = do 
  --r <- getUpdates
  --BS.putStr (r ^. responseBody)
  r <- asJSON =<< getUpdates :: IO Resp
  Prelude.putStrLn (show r)