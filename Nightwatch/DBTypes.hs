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
  ,Key(..)
  ,DownloadId(..)
  ,ConnectionPool
  ,File(..)
  ,Url(..)
  ,createUser
  ,createDownload
  ,fetchDownloadByGid
  ,authenticateChat
  ,fetchUserByTelegramUserId
  ,fetchUserById
  ,fetchLogById
  ,fetchLogByRequestId
  ,updateWithAria2Request
  ,updateWithAria2Response
  ,logIncomingTelegramMessage
  ,updateWithTgramOutgoingMsg
  ,logAria2Notification
  ,updateDownloadWithFiles
  ,getDownloadsByUserId
  ,SqlPersistM
  ,NwApp(..)
  ,Entity(..)
  ,runDb
  ,runMigrations
  ,Log(..)
  ,LogId(..)
  ,joinStrings
  ) where
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Time (UTCTime, getCurrentTime)
import           Nightwatch.Types
import           Control.Concurrent.Chan (Chan)
import qualified Nightwatch.TelegramTypes as TT
import           Data.List(foldl', unfoldr, partition)

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
  accessToken OAuthAccessToken
  refreshToken OAuthRefreshToken
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show

Log
  requestId Aria2RequestId Maybe
  userId UserId Maybe
  tgramUserId TgramUserId Maybe
  tgramChatId TgramChatId Maybe
  tgramIncomingText TgramMsgText Maybe
  tgramOutgoingText TgramMsgText Maybe
  nwCmd NightwatchCommand Maybe
  aria2Request String Maybe
  aria2Response String Maybe
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show

Download
  userId UserId
  gid Aria2Gid
  logId LogId
  parentId DownloadId Maybe
  createdAt UTCTime
  updatedAt UTCTime
  deriving Show

File
  downloadId DownloadId
  path String
  length Int
  deriving Show

Url
  downloadId DownloadId
  fileId FileId
  url URL
  deriving Show
|]


type ParentDownloadId = DownloadId

-- NOTE: see note above.
-- instance Timestamped User
-- instance Timestamped Aria2Log
-- instance Timestamped Download

data AuthNightwatchCommand = AuthNightwatchCommand {
  command :: NightwatchCommand,
  userId :: UserId,
  chatId :: TgramChatId,
  logId :: LogId
} deriving (Show, Eq)

type Aria2ChannelMessage = AuthNightwatchCommand
type Aria2Channel = Chan Aria2ChannelMessage
type NwApp = SqlPersistT IO

createUser :: String -> OAuthAccessToken -> OAuthRefreshToken -> Maybe String -> Maybe TgramUserId -> Maybe TgramUsername -> Maybe TgramChatId -> NwApp (Entity User)
createUser email accessToken refreshToken name tgramUserId tgramUsername tgramChatId = (liftIO getCurrentTime) >>= (\time -> insertEntity User{userName=name, userEmail=email, userTgramUserId=tgramUserId, userTgramUsername=tgramUsername, userTgramChatId=tgramChatId, userCreatedAt=time, userUpdatedAt=time, userAccessToken=accessToken, userRefreshToken=refreshToken})

-- createAria2RequestLog :: Maybe String -> Maybe UserId -> NwApp ()
-- createAria2RequestLog request userId = (liftIO getCurrentTime) >>= (\time -> update logId [ LogAria2Request =. request, LogUserId =. userId ])

createDownload :: Aria2Gid -> LogId -> UserId -> [(String, Integer, [URL])] -> Maybe ParentDownloadId -> NwApp (Entity Download)
createDownload gid logId_ userId_ files parentId = do
  time <- liftIO getCurrentTime
  dloadE <- insertEntity Download{downloadGid=gid, downloadLogId=logId_, downloadUserId=userId_, downloadParentId=parentId, downloadCreatedAt=time, downloadUpdatedAt=time}
  _ <- updateDownloadWithFiles (entityKey dloadE) files
  return dloadE

updateDownloadWithFiles :: DownloadId -> [(String, Integer, [URL])] -> NwApp([Entity File])
updateDownloadWithFiles dloadId files = (sequence . (map $ insertFile dloadId)) files
  where
    insertFile :: DownloadId -> (String, Integer, [URL]) -> NwApp (Entity File)
    insertFile dloadId (p, l, urls) = do
      fileE <- insertEntity File{filePath=p, fileLength=(fromIntegral l), fileDownloadId=dloadId}
      _ <- (sequence . (map (\u -> insertEntity Url{urlUrl=u, urlDownloadId=dloadId, urlFileId=(entityKey fileE)}))) urls
      return fileE

fetchDownloadByGid :: Aria2Gid -> NwApp (Maybe (Entity Download))
fetchDownloadByGid gid = selectFirst [ DownloadGid ==. gid ] []

fetchLogById :: LogId -> NwApp (Maybe Log)
fetchLogById requestId = get requestId

fetchLogByRequestId :: Aria2RequestId -> NwApp (Maybe (Entity Log))
fetchLogByRequestId requestId = selectFirst [ LogRequestId ==. (Just requestId) ] []

fetchUserByTelegramUserId :: TgramUserId -> NwApp (Maybe (Entity User))
fetchUserByTelegramUserId tgramUserId = selectFirst [ UserTgramUserId ==. (Just tgramUserId) ] []

fetchUserById :: UserId -> NwApp (Maybe User)
fetchUserById userId_ = get userId_

-- TODO: Lookup (chat_id $ chat $ msg) in DB to ensure that this chat has been
-- authenticated in the past
authenticateChat :: TgramChatId -> NwApp (Maybe (Entity User))
authenticateChat chatId_ = selectFirst [UserTgramChatId ==. (Just chatId_)] []

runDb :: ConnectionPool -> NwApp a -> IO a
runDb pool operation = runSqlPool operation pool

runMigrations :: ConnectionPool -> IO ()
runMigrations pool = runDb pool $ runMigrationUnsafe migrateAll

-- Sqlpersistti (NoLoggingT (ResourceT IO))
-- ReaderT SqlBackend (NoLoggingT (ResourceT IO))

blankLog :: IO Log
blankLog = getCurrentTime >>= \time -> return Log{logRequestId=Nothing, logUserId=Nothing, logTgramUserId=Nothing, logTgramChatId=Nothing, logTgramIncomingText=Nothing, logTgramOutgoingText=Nothing, logNwCmd=Nothing, logAria2Request=Nothing, logAria2Response=Nothing, logCreatedAt=time, logUpdatedAt=time}

logIncomingTelegramMessage :: TT.Message -> Maybe UserId -> Maybe NightwatchCommand -> NwApp (Entity Log)
logIncomingTelegramMessage tgramMsg userId_ nwCmd = (liftIO blankLog) >>= (\log_ -> insertEntity log_{logTgramUserId=(Just $ TT.user_id $ TT.from tgramMsg), logTgramChatId=(Just $ TT.chat_id $ TT.chat tgramMsg), logTgramIncomingText=(TT.text tgramMsg), logNwCmd=nwCmd, logUserId=userId_})

updateWithAria2Request :: LogId -> Aria2RequestId -> String -> NwApp ()
updateWithAria2Request logId_ requestId req = (liftIO getCurrentTime) >>= (\time -> update logId_ [ LogRequestId =. (Just requestId), LogAria2Request =. (Just req), LogUpdatedAt =. time ])

updateWithAria2Response :: LogId -> String -> NwApp ()
updateWithAria2Response requestId r = (liftIO getCurrentTime) >>= (\time -> update requestId [ LogAria2Response =. (Just r), LogUpdatedAt =. time ])

updateWithTgramOutgoingMsg :: LogId -> TelegramOutgoingMessage -> NwApp()
updateWithTgramOutgoingMsg requestId msg = (liftIO getCurrentTime) >>= (\time -> update requestId [ LogTgramOutgoingText =. (Just $ message msg), LogTgramChatId =. (Just $ tg_chat_id msg), LogUpdatedAt =. time])

logAria2Notification :: String -> NwApp (Entity Log)
logAria2Notification notif = (liftIO blankLog) >>= (\log_ -> insertEntity log_{logAria2Response=(Just notif)})

getDownloadsByUserId :: Key User -> NwApp ([(Entity Download, [Entity File])])
getDownloadsByUserId userId = do
  dloadsE <- selectList [DownloadUserId ==. userId] [Desc DownloadUpdatedAt]
  let dloadIds = map entityKey dloadsE
  filesE <- selectList [FileDownloadId <-. dloadIds] [Asc FileDownloadId]
  return $ unfoldr unfoldrHelper (dloadsE, filesE)
  where
    partitionChildren :: Key Download -> [Entity File] -> ([Entity File], [Entity File])
    partitionChildren parentId children = partition (\childE -> (fileDownloadId $ entityVal childE) == parentId) children

    unfoldrHelper :: ([Entity Download], [Entity File]) -> Maybe ((Entity Download, [Entity File]), ([Entity Download], [Entity File]))
    unfoldrHelper ([], _) = Nothing
    unfoldrHelper ((dloadE:dloadsE), filesE) = Just ((dloadE, matchingFiles), (dloadsE, nonMatchingFiles))
      where (matchingFiles, nonMatchingFiles) = partitionChildren (entityKey dloadE) filesE

    
    -- return $ map (\dloadE -> (dloadE, selectList [FileDownloadId ==. (entityKey dloadE)] [Asc FileId])) dloadsE

-- loadEfficiently :: (PersistStore backend, PersistEntity parent, PersistEntity child, backend ~ PersistEntityBackend child, MonadIO m) => (parent -> Maybe (Key child)) -> parents -> ReaderT backend m child
-- loadEfficiently

