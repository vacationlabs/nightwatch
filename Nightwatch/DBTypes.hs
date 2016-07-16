{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}

module Nightwatch.DBTypes(User(..)
  ,NightwatchCommand(..)
  ,TelegramOutgoingMessage(..)
  ,TelegramOutgoingChannel
  ,Aria2Gid(..)
  ,URL(..)
  ,TgramUsername(..)
  ,TgramFirstName(..)
  ,TgramLastName(..)
  ,TgramUserId(..)
  ,TgramMsgText(..)
  ,TgramChatId(..)
  ,ConnectionPool
  ,File(..)
  ,Url(..)
  ,createDownload
  ,createUser
  ,createOrUpdateUser
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
  ,NwApp
  ,Entity(..)
  ,runDb
  ,runMigrations
  ,joinStrings
  ,googleClientId
  ,googleClientSecret
  ,tgramBotToken
  ,dbPool
  ,tgramOutgoingChannel
  ,aria2Command
  ,aria2DownloadDir
  ,NwConfig
  ,def
  ,module Model
  ) where
 
import Prelude
import           Database.Persist
import           Database.Persist.Sqlite
import           Data.Time (getCurrentTime)
import           Nightwatch.Types
import qualified Nightwatch.TelegramTypes as TT
import           Data.List(unfoldr, partition)
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Default
import Control.Lens
import Model

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

type ParentDownloadId = DownloadId

-- NOTE: see note above.
-- instance Timestamped User
-- instance Timestamped Aria2Log
-- instance Timestamped Download

-- data AuthNightwatchCommand = AuthNightwatchCommand {
--   command :: NightwatchCommand,
--   userId :: UserId,
--   chatId :: TgramChatId,
--   logId :: LogId
-- } deriving (Show, Eq)

data NwConfig = NwConfig {
  _googleClientId :: T.Text,
  _googleClientSecret :: T.Text,
  _tgramBotToken :: String,
  _dbPool :: ConnectionPool,
  _tgramOutgoingChannel :: TelegramOutgoingChannel,
  _aria2Command :: String,
  _aria2DownloadDir :: String
  }
$(makeLenses ''NwConfig)
instance Default NwConfig where
  def = NwConfig{}

type NwApp  = SqlPersistT IO
--type NwApp = ReaderT NwConfig (SqlPersistT IO)
-- type NwAppWithConfig = ReaderT NwConfig NwApp

createOrUpdateUser :: (MonadIO m) => User -> SqlPersistT m (Entity User)
createOrUpdateUser userE = do
  let e = userEmail userE
  u <- selectFirst [UserEmail ==. e] []
  case u of
    Just (Entity existingUid _) -> do
      userWithTimeE <- updateTs userE
      replace existingUid userWithTimeE
      return (Entity existingUid userWithTimeE)
    Nothing -> insertEntity =<< (assignTs userE)


createUser :: Maybe String -> String -> Maybe TgramUserId -> Maybe TgramUsername -> Maybe TgramChatId -> OAuthAccessToken -> OAuthRefreshToken -> NwApp (Entity User)
createUser userName userEmail userTgramUserId userTgramUsername userTgramChatId userAccessToken userRefreshToken =
  insertEntity =<< assignTs User{..}

-- createAria2RequestLog :: Maybe String -> Maybe UserId -> NwApp ()
-- createAria2RequestLog request userId = (liftIO getCurrentTime) >>= (\time -> update logId [ LogAria2Request =. request, LogUserId =. userId ])

createDownload :: Aria2Gid -> LogId -> UserId -> [(String, Integer, [URL])] -> Maybe ParentDownloadId -> NwApp (Entity Download)
createDownload gid logId_ userId_ files parentId = do
  dloadE <- insertEntity =<< (assignTs Download{downloadGid=gid, downloadLogId=logId_, downloadUserId=userId_, downloadParentId=parentId})
  _ <- updateDownloadWithFiles (entityKey dloadE) files
  return dloadE

updateDownloadWithFiles :: DownloadId -> [(String, Integer, [URL])] -> NwApp([Entity File])
updateDownloadWithFiles dloadId files = (sequence . (map $ insertFile dloadId)) files
  where
    insertFile :: DownloadId -> (String, Integer, [URL]) -> NwApp (Entity File)
    insertFile dloadId (p, l, urls) = do
      fileE <- insertEntity File{fileFpath=p, fileLen=(fromIntegral l), fileDownloadId=dloadId}
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

