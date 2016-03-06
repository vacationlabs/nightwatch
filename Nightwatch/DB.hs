{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Nightwatch.DB where
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Nightwatch.Types
import Data.Time (UTCTime, getCurrentTime)
import Nightwatch.DBInternal

class (MonadIO m) => Timestamped a where
  assignCreatedAt :: a -> m a
  assignCreatedAt x = getCurrentTime >>= (\time -> x{createdAt=time})

  assignUpdatedAt :: a -> m a
  assignUpdatedAt x = getCurrentTime >>= (\time -> x{updatedAt=time})

  assignTimestamps :: a -> m a
  assignTimestamps x = getCurrentTime >>= (\time -> x{createdAt=time, updatedAt=time})

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

instance Timestamped User
instance Timestamped Aria2Log
instance Timestamped Download

createAria2Log :: Aria2RequestId -> String -> Maybe TelegramLogId -> Maybe UserId -> SqlPersistM (Entity Aria2Log)
createAria2Log requestId request telegramLogId userId = getCurrentTime >>= (\time -> insertEntity $ Aria2Log{requestId=requestId request=request telegramLogId=telegramLogId userId=userId createdAt=time updatedAt=time})

recordAria2Response :: Aria2RequestId -> String -> SqlPersistM ()
recordAria2Response requestId response = getCurrentTime >>= (\time -> updateWhere [Aria2LogRequestId ==. requestId] [Aria2LogResponse =. response, Aria2LogUpdatedAt =. time])

createDownload :: Download -> SqlPersistM (Entity Download)
createDownload d = (assignTimestamps d) >>= (\val -> insertEntity val)

fetchDownloadByGid :: Aria2Gid -> SqlPersistM (Maybe (Entity Download))
fetchDownloadByGid gid = selectFirst [ DownloadGid ==. gid ] []

assignCreatedAt :: ()