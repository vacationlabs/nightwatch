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
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
  ,createOrReplaceUser
  ,fetchDownloadByGid
  ,authenticateChat
  ,fetchUserByTelegramUserId
  ,fetchUserById
  ,fetchLogById
  ,fetchLogByRequestId
  ,updateWithAria2Request
  ,markDownloadAsComplete
  ,updateWithAria2Response
  ,logIncomingTelegramMessage
  ,updateWithTgramOutgoingMsg
  ,logAria2Notification
  ,updateDownloadWithFiles
  ,getDownloadsByUserId
  ,hasIncompleteChildren
  ,updateDownload
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
  ,aggregateChildrenStatus
  ,nestedTuples
  ) where
 
import ClassyPrelude
import           Database.Persist
import           Database.Persist.Sqlite
import           Data.Time (getCurrentTime)
import           Nightwatch.Types
import qualified Nightwatch.TelegramTypes as TT
import           Data.List(unfoldr)
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Default
import Control.Lens
import Model
import           Safe (fromJustNote)
import Data.Maybe (isJust)
import Data.Tree
import qualified Database.Esqueleto as E
import Data.List (mapAccumL)
import qualified Data.Map.Strict as Map
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

type ParentDownloadId = DownloadId

-- data 

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


newtype DownloadTree = Tree Download
type DownloadObject = (Entity Download, [(Entity File, [Entity Url])])

--type NwApp  = SqlPersistT (NoLoggingT (ResourceT IO))
type NwApp = SqlPersistT IO
--type NwApp = ReaderT NwConfig (SqlPersistT IO)
-- type NwAppWithConfig = ReaderT NwConfig NwApp

-- upsertEntity :: (PersistEntity a, PersistField fd) => EntityField a fd -> a -> m (Entity a)
-- upsertEntity keyCol rcrd = do
--   let keyVal = keyCol rcrd
--   r <- selectFirst []

createOrReplaceUser :: (MonadIO m) => String ->
  (User -> SqlPersistT m User) ->
  User ->
  SqlPersistT m (Entity User)
createOrReplaceUser email replaceFn newUser = do
  u <- selectFirst [UserEmail ==. email] []
  case u of
    Just (Entity existingUid existingUser) -> do

      userWithTime <- updateTs existingUser
      replace existingUid =<< replaceFn userWithTime
      return $ (Entity existingUid userWithTime)
    Nothing -> insertEntity =<< (assignTs newUser)


createUser :: Maybe String -> String -> Maybe TgramUserId -> Maybe TgramUsername -> Maybe TgramChatId -> OAuthAccessToken -> OAuthRefreshToken -> NwApp (Entity User)
createUser userName userEmail userTgramUserId userTgramUsername userTgramChatId userAccessToken userRefreshToken =
  insertEntity =<< assignTs User{..}

-- createAria2RequestLog :: Maybe String -> Maybe UserId -> NwApp ()
-- createAria2RequestLog request userId = (liftIO getCurrentTime) >>= (\time -> update logId [ LogAria2Request =. request, LogUserId =. userId ])

createDownload :: Aria2Gid -> LogId -> UserId -> [(String, Integer, [URL])] -> Maybe ParentDownloadId -> NwApp (Entity Download)
createDownload gid logId_ userId_ files pid = do
  pid <- ensureParentIsComplete pid
  dloadE@(Entity dloadId dload) <- insertEntity =<< (assignTs Download{downloadGid=gid, downloadLogId=logId_, downloadUserId=userId_, downloadParentId=pid, downloadStatus=DownloadIncomplete})
  _ <- updateDownloadWithFiles dloadId files
  recomputeDownloadStatus pid
  return (Entity dloadId dload)
  where
    ensureParentIsComplete pid = case pid of
      Nothing -> return Nothing
      Just x -> get x >>= \case
        Nothing -> throwM . DownloadStatusException $ "No such parent exists. ID " ++ (tshow x)
        Just parent -> if (not $ isDownloadComplete (parent ^. status))
          then throwM . DownloadStatusException $ "Attempt to create a child for incomplete download ID " ++ (tshow x)
          else return pid

-- createChildDownload :: Aria2Gid -> LogId -> UserId -> [(String, Integer, [URL])] -> ParentDownloadId -> NwApp (Entity Download, Entity Download)
-- createChildDownload gid logId_ userId_ files pid = do
--   pid <- ensureParentIsComplete pid
--   dloadE@(Entity dloadId dload) <- insertEntity =<< (assignTs Download{downloadGid=gid, downloadLogId=logId_, downloadUserId=userId_, downloadParentId=(Just pid), downloadStatus=DownloadIncomplete})
--   _ <- updateDownloadWithFiles dloadId files
--   recomputeDownloadStatus (Just pid)
--   return (Entity dloadId dload, undefined)
--   where
--     ensureParentIsComplete pid = get pid >>= \case
--         Nothing -> throwM . DownloadStatusException $ "No such parent exists. ID " ++ (tshow pid)
--         Just parent -> if (not $ isDownloadComplete (parent ^. status))
--           then throwM . DownloadStatusException $ "Attempt to create a child for incomplete download ID " ++ (tshow pid)
--           else return pid




markDownloadAsComplete :: DownloadId -> NwApp (Download)
markDownloadAsComplete dloadId = do
  cStatus <- aggregateChildrenStatus dloadId
  dload <- updateGet dloadId [DownloadStatus =. (DownloadComplete cStatus)]
  recomputeDownloadStatus (dload ^. parentId)
  transactionSave
  return dload


aggregateChildrenStatus :: DownloadId -> NwApp ChildrenStatus
aggregateChildrenStatus pid = do
  (selectList [DownloadParentId ==. (Just pid)] []) >>= \case
    [] -> return ChildrenNone
    children -> case (all (\(Entity _ x) -> isDownloadComplete (x ^. status)) children) of
      -- We have at least one incomplete child. 
      False -> return ChildrenIncomplete
      
      -- All children are compliete
      True -> return ChildrenComplete

-- changeDownloadStatus :: DownloadId -> DownloadStatus -> NwApp (Download)
-- changeDownloadStatus dloadId dloadStatus = do
--   dload <- updateGet dloadId [DownloadStatus =. dloadStatus]
--   aggregateChildrenStatus (dload ^. parentId)
--   transactionSave
--   return dload

recomputeDownloadStatus :: Maybe DownloadId -> NwApp ()
recomputeDownloadStatus Nothing = return ()
recomputeDownloadStatus (Just pid) = do
  get pid >>= \case
    Nothing -> return ()
    Just parent -> do
      aggregateChildrenStatus pid >>= \case
        ChildrenNone -> return () 
        ChildrenIncomplete -> update pid [DownloadStatus =. (DownloadComplete ChildrenIncomplete)]
        ChildrenComplete -> update pid [DownloadStatus =. (DownloadComplete ChildrenComplete)]

      -- Walk-up the downlaod-tree, if required
      case (parent ^. parentId) of
        Nothing -> return ()
        Just ppid -> recomputeDownloadStatus (Just ppid)


updateDownload :: Entity Download -> NwApp(Download)
updateDownload (Entity dloadId dload) = do
  d <- updateTs dload
  replace dloadId d
  return d
 
hasIncompleteChildren :: DownloadId -> NwApp (Bool)
hasIncompleteChildren dloadId = do
  selectList [DownloadParentId ==. (Just dloadId)] [] >>=  \case
    [] -> return False
    children -> return $ any (\(Entity _ x) -> isDownloadComplete (x ^. status)) children

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

-- fetchDownloadTrees :: E.SqlExpr (E.Value Bool) -> [E.SqlExpr E.OrderBy] -> NwApp (Forest DownloadObject)
-- fetchDownloadTrees whereClause orderClause = do
--   -- ds <- selectList filters []
--   tuples <- downloadObjects whereClause orderClause
--   buildTree $ dloadsToForest tuples
--   where

--     buildTree :: Forest DownloadObject ->  NwApp (Forest DownloadObject)
--     buildTree forest = do
--       let pids = map (Just . entityKey . (view _1) . rootLabel)  forest
--       children <- downloadObjects (E.where_ (\d -> d E.^. DownloadId E.in_ E.valList pids)) []
--       let newForest = map
--                       (\tree@Node{rootLabel=(Entity pid _), subForest=_} ->
--                          tree{subForest=dloadsToForest $ childrendWithParent pid children})
--                       forest

--       mapM
--         (\tree -> do
--             newSubForest <- buildTree $ subForest tree
--             return tree{subForest=newSubForest}
--         )
--         newForest

--     dloadsToForest :: [DownloadObject] -> Forest DownloadObject
--     dloadsToForest dloads = map (\d -> Node{rootLabel=d, subForest=[]}) dloads

--     childrendWithParent :: DownloadId -> [DownloadObject] -> [DownloadObject]
--     childrendWithParent pid children = filter (\(dloadE, _) -> (entityKey dloadE) == pid) children


downloadObjects :: E.SqlExpr (E.Value Bool) -> [E.SqlExpr E.OrderBy] -> NwApp [DownloadObject]
downloadObjects whereClause orderClause = do
    result <- E.select $ E.from $
      \ (dload `E.LeftOuterJoin` file `E.LeftOuterJoin` url) -> do
        E.on (dload E.^. DownloadId E.==. file E.^. FileDownloadId)
        E.on (file E.^. FileId E.==. url E.^. UrlFileId)
        E.where_ whereClause
        E.orderBy orderClause
        return (dload, file, url)
    return $ nestedTuples
      (\(d, _, _) -> d) -- extract the first-level-group's key from each tuple, i.e. Entity Download
      (\(_, f, u) -> (f, u)) -- extract the first-level-group's value from each tuple, i.e. (Entity File, Entity Url)
      (\tpl -> nestedTuples (view _1) (view _2) id tpl) -- Transform the first-level-group's value to a second-level-group
      result


-- dbHarness = runStderrLoggingT $ withSqliteConn "nightwatch.db" $ \conn -> liftIO $ do
--   flip runSqlConn conn $ do
--     runMigration migrateAll
--     downloadObjects (\ d _ _ -> d E.^. DownloadStatus E.==. E.val DownloadIncomplete) (\ _ _ _ -> [])


-- groupBy :: ((Entity Download, Entity File, Entity Url) -> (Entity Download))
--   -> ((Entity Download, Entity File, Entity Url) -> (Entity File, Entity Url))
--   -> [(Entity Download, Entity File, Entity Url)]
--   -> [(Entity Download, [(Entity File, Entity Url)])]

-- groupBy :: Ord b => (a -> b) -> (a -> c) -> [a] -> [(b, [c])]
-- groupBy f g arr = Map.toList $ foldl' (\m kv -> insertWith (++) (f kv) [g kv] m) Map.empty arr


nestedTuples :: Ord b => (a -> b) -> (a -> c) -> ([c] -> d) -> [a] -> [(b, d)]
nestedTuples f g h arr = Map.toList $
                         Map.map h $
                         foldl' (\m kv -> insertWith (++) (f kv) [g kv] m) Map.empty arr

