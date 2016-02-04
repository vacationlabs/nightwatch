module Nightwatch.Types (VLUser(..)
  ,NightwatchCommand(..)
  ,AuthNightwatchCommand(..)
  ,Aria2Channel(..)
  ,Aria2ChannelMessage(..)
  ,TelegramOutgoingMessage(..)
  ,TelegramOutgoingChannel(..)
  ,Aria2Gid(..)) where

import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import Control.Concurrent.Chan(Chan)

newtype VLUser = VLUser Integer deriving (Show, Eq)
data NightwatchCommand = InvalidCommand | DownloadCommand { url :: String } | PauseCommand { gid :: String } | UnpauseCommand { gid :: String } | StatusCommand { gid :: String } deriving (Show, Eq)
data AuthNightwatchCommand = UnauthenticatedCommand | AuthNightwatchCommand {
  command :: NightwatchCommand,
  user :: VLUser,
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

newtype Aria2Gid = Aria2Gid String
