{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Nightwatch.DBTypes(User(..)
  ,TelegramLog(..)
  ,Aria2Log(..)
  ,Download(..)
  ,AuthNightwatchCommand(..)
  ,Aria2Channel(..)
  ,Aria2ChannelMessage(..)
  ,NightwatchCommand(..)
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
  ,Aria2RequestId(..)
  ,UserId(..)
  ,TelegramLogId(..)
  ,Aria2LogId(..)
  ,DownloadId(..)
  ,createAria2Log
  ,recordAria2Response
  ,createDownload
  ,fetchDownloadByGid
  ,fetchAria2LogByRequestId
  ,authenticateChat
  ,SqlPersistM
  ,Entity(..)
  ) where
import Control.Monad.IO.Class  (liftIO, MonadIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time (UTCTime, getCurrentTime)
import Nightwatch.Types
import Control.Concurrent.Chan(Chan)

-- import Nightwatch.DBInternal

-- NOTE: This cannot be implemented because the accessors are not createdAt
-- for all the Entity classes. They are userCreatedAt, downloadCreatedAt, etc.

-- class (MonadIO m) => Timestamped a where
--   assignCreatedAt :: a -> m a
--   assignCreatedAt x = getCurrentTime >>= (\time -> x{createdAt=time})

--   assignUpdatedAt :: a -> m a
--   assignUpdatedAt x = getCurrentTime >>= (\time -> x{updatedAt=time})

--   assignTimestamps :: a -> m a
--   assignTimestamps x = getCurrentTime >>= (\time -> x{createdAt=time, updatedAt=time})

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name String Maybe
  email String
  tgramUserId TgramUserId Maybe
  tgramUsername TgramUsername Maybe
  tgramChatId TgramChatId Maybe -- The last known ChatId from this user
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show

TelegramLog
  tgramUserId TgramUserId
  tgramChatId TgramChatId
  tgramMsgText TgramMsgText Maybe
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show

Aria2Log
  requestId Aria2RequestId Maybe
  request String Maybe
  response String Maybe
  telegramLogId TelegramLogId Maybe
  userId UserId Maybe
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show

Download
  userId UserId
  gid Aria2Gid
  url URL
  aria2LogId Aria2LogId
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show
|]

-- NOTE: see note above.
-- instance Timestamped User
-- instance Timestamped Aria2Log
-- instance Timestamped Download

data AuthNightwatchCommand = UnauthenticatedCommand | AuthNightwatchCommand {
  command :: NightwatchCommand,
  userId :: UserId,
  chatId :: TgramChatId
} deriving (Show, Eq)

type Aria2ChannelMessage = AuthNightwatchCommand
type Aria2Channel = Chan Aria2ChannelMessage

createAria2Log :: Aria2RequestId -> String -> Maybe TelegramLogId -> Maybe UserId -> SqlPersistM (Entity Aria2Log)
createAria2Log requestId request telegramLogId userId = (liftIO getCurrentTime) >>= (\time -> insertEntity $ Aria2Log{aria2LogRequestId=(Just requestId), aria2LogRequest=(Just request), aria2LogTelegramLogId=telegramLogId, aria2LogUserId=userId, aria2LogCreatedAt=time, aria2LogUpdatedAt=time, aria2LogResponse=Nothing})

recordAria2Response :: Aria2RequestId -> String -> SqlPersistM ()
recordAria2Response requestId response = (liftIO getCurrentTime) >>= (\time -> updateWhere [Aria2LogRequestId ==. (Just requestId)] [Aria2LogResponse =. (Just response), Aria2LogUpdatedAt =. time])

createDownload :: Download -> SqlPersistM (Entity Download)
createDownload d = (liftIO getCurrentTime) >>= (\time -> insertEntity d{downloadCreatedAt=time, downloadUpdatedAt=time})

fetchDownloadByGid :: Aria2Gid -> SqlPersistM (Maybe (Entity Download))
fetchDownloadByGid gid = selectFirst [ DownloadGid ==. gid ] []

fetchAria2LogByRequestId :: Aria2RequestId -> SqlPersistM (Maybe (Entity Aria2Log))
fetchAria2LogByRequestId requestId = selectFirst [Aria2LogRequestId ==. (Just requestId)] [Desc Aria2LogCreatedAt]

-- TODO: Lookup (chat_id $ chat $ msg) in DB to ensure that this chat has been
-- authenticated in the past
authenticateChat :: TgramChatId -> SqlPersistM (Maybe (Entity User))
authenticateChat chatId = selectFirst [UserTgramChatId ==. (Just chatId)] []
