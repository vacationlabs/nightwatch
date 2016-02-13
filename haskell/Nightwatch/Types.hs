{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Nightwatch.Types (
  VlUser(..)
  ,NightwatchCommand(..)
  ,AuthNightwatchCommand(..)
  ,Aria2Channel(..)
  ,Aria2ChannelMessage(..)
  ,TelegramOutgoingMessage(..)
  ,TelegramOutgoingChannel(..)
  ,Aria2Gid(..)
  ,VlUserAuth
  ,chatId2User
  ,SqlPersistM(..)) where

import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import Control.Concurrent.Chan(Chan)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time
import Control.Monad.Trans.Resource(ResourceT)
import Control.Monad.Logger (NoLoggingT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
VlUser
  email String
  chatId Int64
  tgramUserId Int64
  tgramFirstName String
  tgramLastName String Maybe
  tgramUsername String Maybe
  oauthToken String Maybe
  UniqueChatId chatId
  deriving Show Eq

Download
  userId VlUserId
  url String
  gid String
  completedAt UTCTime Maybe
  deriving Show
|]


newtype VlUserAuth = VlUserAuth VlUser deriving (Show, Eq)
mkVlUserAuth :: VlUser -> Maybe VlUserAuth
mkVlUserAuth x = case (vlUserOauthToken x) of
  Nothing -> Nothing
  Just y -> Just $ VlUserAuth x

data NightwatchCommand = InvalidCommand | DownloadCommand { url :: String } | PauseCommand { gid :: String } | UnpauseCommand { gid :: String } | StatusCommand { gid :: String } deriving (Show, Eq)
data AuthNightwatchCommand = UnauthenticatedCommand | AuthNightwatchCommand {
  command :: NightwatchCommand,
  user :: VlUserAuth,
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


chatId2User cId = getBy $ UniqueChatId cId
