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
  ,UserId(..)
  ,TelegramLogId(..)
  ,Aria2LogId(..)
  ,Key(..)
  ,DownloadId(..)
  ,ConnectionPool
  ,createUser
  ,updateAria2Log
  ,logAria2Request
  ,nextAria2LogId
  ,createDownload
  ,fetchDownloadByGid
  ,fetchAria2LogById
  ,authenticateChat
  ,fetchTelegramLogById
  ,fetchUserByTelegramUserId
  ,createTelegramLog
  ,SqlPersistM
  ,NwApp(..)
  ,Entity(..)
  ,runDb
  ,runMigrations
  ,mkRequestId
  ,unMkRequestId
  ) where
import Control.Monad.IO.Class  (liftIO, MonadIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time (UTCTime, getCurrentTime)
import Nightwatch.Types
import Control.Concurrent.Chan(Chan)
import qualified Data.Text as T
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

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
  nightwatchCommand NightwatchCommand Maybe
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show

Aria2Log
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
type NwApp = SqlPersistT IO

createUser :: String -> Maybe String -> Maybe TgramUserId -> Maybe TgramUsername -> Maybe TgramChatId -> NwApp (Entity User)
createUser email name tgramUserId tgramUsername tgramChatId = (liftIO getCurrentTime) >>= (\time -> insertEntity User{userName=name, userEmail=email, userTgramUserId=tgramUserId, userTgramUsername=tgramUsername, userTgramChatId=tgramChatId, userCreatedAt=time, userUpdatedAt=time})

-- createAria2Log :: String -> Maybe TelegramLogId -> Maybe UserId -> NwApp (Entity Aria2Log)
 -- createAria2Log request telegramLogId userId = (liftIO getCurrentTime) >>= (\time -> insecreateEntity $ Aria2Log{aria2LogRequest=(Just request), aria2LogTelegramLogId=telegramLogId, aria2LogUserId=userId, aria2LogCreatedAt=time, aria2LogUpdatedAt=time, aria2LogResponse=Nothing})

logAria2Request :: Aria2LogId -> Maybe String -> Maybe UserId -> NwApp ()
logAria2Request aria2LogId request userId = (liftIO getCurrentTime) >>= (\time -> update aria2LogId [ Aria2LogRequest =. request, Aria2LogUserId =. userId ])

updateAria2Log :: Aria2LogId -> Aria2Log -> NwApp (Aria2Log)
updateAria2Log aria2LogId aria2Log = do 
  time <- (liftIO getCurrentTime) 
  let newLog = aria2Log{aria2LogUpdatedAt=time}
  repsert aria2LogId newLog
  return newLog

-- recordAria2Response :: Aria2RequestId -> String -> NwApp ()
-- recordAria2Response requestId response = (liftIO getCurrentTime) >>= (\time -> updateWhere [Aria2LogRequestId ==. (Just requestId)] [Aria2LogResponse =. (Just response), Aria2LogUpdatedAt =. time])

createDownload :: URL -> Aria2Gid -> Aria2LogId -> UserId -> NwApp (Entity Download)
createDownload url gid logId userId = (liftIO getCurrentTime) >>= (\time -> insertEntity Download{downloadUrl=url, downloadGid=gid, downloadAria2LogId=logId, downloadUserId=userId, downloadCreatedAt=time, downloadUpdatedAt=time})

fetchDownloadByGid :: Aria2Gid -> NwApp (Maybe (Entity Download))
fetchDownloadByGid gid = selectFirst [ DownloadGid ==. gid ] []

fetchAria2LogById :: Aria2LogId -> NwApp (Maybe Aria2Log)
fetchAria2LogById requestId = get requestId

fetchTelegramLogById :: TelegramLogId -> NwApp (Maybe TelegramLog)
fetchTelegramLogById tgramLogId = get tgramLogId

nextAria2LogId :: NwApp (Aria2LogId)
nextAria2LogId = (liftIO getCurrentTime) >>= (\time -> insert Aria2Log{aria2LogRequest=Nothing, aria2LogResponse=Nothing, aria2LogTelegramLogId=Nothing, aria2LogUserId=Nothing, aria2LogCreatedAt=time, aria2LogUpdatedAt=time})

fetchUserByTelegramUserId :: TgramUserId -> NwApp (Maybe (Entity User))
fetchUserByTelegramUserId tgramUserId = selectFirst [ UserTgramUserId ==. (Just tgramUserId) ] []

-- fetchLargestAria2RequestId :: NwApp (Aria2RequestId)
-- fetchLargestAria2RequestId = do 
--   res <- runQuery
--   return $ if (length res) == 0 
--     then (Aria2RequestId 0)
--     else (unSingle (head res))
--   where 
--     runQuery :: NwApp [Single Aria2RequestId]
--     runQuery = rawSql "select max(request_id) from aria2_log" []

-- incrementAria2RequestId :: Aria2RequestId -> Aria2RequestId
-- incrementAria2RequestId (Aria2RequestId x) = Aria2RequestId $ 1+ x

-- TODO: Lookup (chat_id $ chat $ msg) in DB to ensure that this chat has been
-- authenticated in the past
authenticateChat :: TgramChatId -> NwApp (Maybe (Entity User))
authenticateChat chatId = selectFirst [UserTgramChatId ==. (Just chatId)] []

runDb :: ConnectionPool -> NwApp a -> IO a
runDb pool operation = runSqlPool operation pool

runMigrations :: ConnectionPool -> IO ()
runMigrations pool = runDb pool $ runMigration migrateAll

--mkRequestId :: Integer -> Aria2LogId
mkRequestId x = Aria2LogKey $ fromIntegral x

--unMkRequestId :: Aria2LogId -> Integer
unMkRequestId x = unSqlBackendKey $ unAria2LogKey x


-- SqlPersistT (NoLoggingT (ResourceT IO))
-- ReaderT SqlBackend (NoLoggingT (ResourceT IO))

createTelegramLog :: TgramUserId -> TgramChatId -> Maybe TgramMsgText -> Maybe NightwatchCommand -> NwApp (Entity TelegramLog)
createTelegramLog tgramUserId tgramChatId msg nwCommand = (liftIO getCurrentTime) >>= (\time -> insertEntity TelegramLog{telegramLogTgramUserId=tgramUserId, telegramLogTgramChatId=tgramChatId, telegramLogTgramMsgText=msg, telegramLogNightwatchCommand=nwCommand, telegramLogCreatedAt=time, telegramLogUpdatedAt=time})
