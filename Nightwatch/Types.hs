{-# LANGUAGE DeriveGeneric #-}
module Nightwatch.Types (VLUser(..)
  ,NightwatchCommand(..)
  ,AuthNightwatchCommand(..)
  ,Aria2Channel(..)
  ,Aria2ChannelMessage(..)
  ,TelegramOutgoingMessage(..)
  ,TelegramOutgoingChannel(..)
  ,Aria2Gid(..)
  ,URL(..)
  ,TgramUsername(..)
  ,TgramFirstName(..)
  ,TgramLastName(..)
  ,TgramUserId(..)
  ,TgramMsgText(..)
  ,TgramChatId(..)
  ,Aria2RequestID(..)) where

import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import Control.Concurrent.Chan(Chan)
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics (Generic)

-- TODO -- VLUser should be changed to UserId coming from the database
-- newtype VLUser = VLUser Integer deriving (Show, Eq)
newtype URL = URL String deriving (Show, Eq, Generic)
newtype Aria2Gid = Aria2Gid String deriving (Show, Eq, Generic)
instance ToJSON URL
instance ToJSON Aria2Gid

newtype TgramUserId = TgramUserId Integer deriving (Show, Eq, Generic)
newtype TgramFirstName = TgramFirstName String deriving (Show, Eq, Generic)
newtype TgramLastName = TgramLastName String deriving (Show, Eq, Generic)
newtype TgramUsername = TgramUsername String deriving (Show, Eq, Generic)
newtype TgramMsgText = TgramMsgText String deriving (Show, Eq, Generic)
newtype TgramChatId = TgramChatId Integer deriving (Show, Eq, Generic)
instance FromJSON TgramUserId
instance FromJSON TgramFirstName
instance FromJSON TgramLastName
instance FromJSON TgramUsername
instance FromJSON TgramMsgText
instance FromJSON TgramChatId
instance ToJSON TgramUserId
instance ToJSON TgramFirstName
instance ToJSON TgramLastName
instance ToJSON TgramUsername
instance ToJSON TgramMsgText
instance ToJSON TgramChatId

data NightwatchCommand = InvalidCommand | DownloadCommand URL | PauseCommand Aria2Gid | UnpauseCommand Aria2Gid | StatusCommand Aria2Gid deriving (Show, Eq)
data AuthNightwatchCommand = UnauthenticatedCommand | AuthNightwatchCommand {
  command :: NightwatchCommand,
  userId :: UserId,
  chat_id :: Integer
} deriving (Show, Eq)

-- (Websocket responses, Telegram commands) => Aria2Channel => Websocket [Aria2]
type Aria2ChannelMessage = AuthNightwatchCommand
type Aria2Channel = Chan Aria2ChannelMessage

data TelegramOutgoingMessage = TelegramOutgoingMessage {
  tg_chat_id :: Integer,
  message :: T.Text
} deriving (Show, Eq)

type TelegramOutgoingChannel = Chan TelegramOutgoingMessage

newtype Aria2RequestID = Aria2RequestId String deriving (Show, Eq, Generic)