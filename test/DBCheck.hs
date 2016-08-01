{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables            #-}

module DBCheck where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Monadic as QCM
import Nightwatch.DBTypes 
import Import
import Nightwatch.Types
import Test.QuickCheck.Instances
import Control.Lens hiding (elements)
import Model
import Database.Persist.Sql
import Data.Traversable (forM)
import qualified Prelude as P
import Control.Monad.Catch
import           Database.Persist.Sqlite
import Control.Monad.Logger (runStderrLoggingT)


-- instance (Monad a) => MonadThrow (PropertyM a) 
-- instance (MonadThrow a) => MonadCatch (PropertyM a)

instance Arbitrary TgramUserId where
  arbitrary = TgramUserId <$> arbitrary
  
instance Arbitrary TgramUsername where
  arbitrary = TgramUsername <$> arbitrary

instance Arbitrary TgramChatId where
  arbitrary = TgramChatId <$> arbitrary

-- instance Arbitrary UTCTime where
--   arbitrary =
--     getCurrentTime

-- instance Arbitrary OAuthAccessToken where
--   arbitrary = OAuthAccessToken <$> arbitrary

-- instance Arbitrary OAuthRefreshToken where
--   arbitrary = OAuthRefreshToken <$> arbitrary

arbitraryPositiveInt :: (Num a, Ord a, Arbitrary a) => Gen a
arbitraryPositiveInt = arbitrary `suchThat` (\x -> x > 1)

arbitraryEmail :: Gen String
arbitraryEmail = do
  n <- arbitrary
  d <- arbitrary
  tld <- arbitrary
  return (n ++ "@" ++ d ++ "." ++ tld)

-- arbitraryDbId :: (PersistEntity val)  => Gen (Key val)
-- arbitraryDbId = 

instance Arbitrary (BackendKey SqlBackend) where
  arbitrary = SqlBackendKey <$> arbitraryPositiveInt

instance Arbitrary User where
  arbitrary = User <$> arbitrary
    <*> arbitraryEmail
    <*> (Just . TgramUserId <$> arbitraryPositiveInt)
    <*> arbitrary
    <*> (Just . TgramChatId <$> arbitraryPositiveInt)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

-- TODO: Figure out how to pick the element list automatically. Or this can go
-- out of wync with the actual ADT
instance Arbitrary DownloadStatus where
  arbitrary = elements [DownloadIncomplete, DownloadComplete ChildrenNone, DownloadComplete ChildrenComplete, DownloadComplete ChildrenIncomplete]

instance Arbitrary Download where
  arbitrary = Download
    -- <$> (UserKey <$> arbitraryPositiveInt)
      <$> (UserKey <$> arbitrary)
      <*> (Aria2Gid <$> arbitrary)
      <*> (LogKey <$> arbitrary)
      <*> (Just . DownloadKey <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

--prop_downloadCompletion depth fileCount = monadicIO $

prop_childCreation :: PropertyM NwApp ()
prop_childCreation = do
  uid <- pick $ UserKey <$> arbitrary
  lid <- pick $ LogKey <$> arbitrary
  gid <- pick $ Aria2Gid <$> arbitrary
  let createDownload_  = createDownload gid lid uid []
  (Entity pid _) <- run $ createDownload_ Nothing
  dstatus <- pick arbitrary
  parent <- run $ updateGet pid [DownloadStatus =. dstatus]

  let test = do
        (Entity cid child) <- createDownload_ (Just pid)
        parent <- fmap (fromJustNote "How can the parent disappear?") (get pid)
        case (parent ^. status, child ^. status) of
          (DownloadIncomplete, _) -> return False
          (DownloadComplete ChildrenComplete, DownloadComplete ChildrenNone) -> return True
          (DownloadComplete ChildrenIncomplete, DownloadIncomplete) -> return True
          (a, b) -> return False

  let test2 = test `catches` [
        Handler (\ (e :: SanityException) -> return True),
        Handler (\ (e :: SomeException) -> return False)
        ]

  result <- run test2
  QCM.assert result

runTests = runStderrLoggingT $ withSqliteConn ":memory:" $ \conn -> liftIO $ do
  let runSqlProperty :: NwApp Property -> Property
      runSqlProperty action = ioProperty $ runSqlConn action conn

  runSqlConn (runMigration migrateAll) conn
  quickCheck $ monadic runSqlProperty prop_childCreation

  -- parent <- set parentId Nothing <$> (pick arbitrary)
  -- child <- set parentId


  -- pick arbitrary
  
-- createDownloadTree :: Int -> Int -> NwApp (Entity Download)
-- createDownloadTree depth fileCount = do

-- prop_downloadComplete :: Int -> PropertyM NwApp Bool
-- prop_downloadComplete depth = do
--   uid <-pick $ UserKey <$> arbitrary
--   lid <- pick $ LogKey <$> arbitrary
--   gid <- pick $ Aria2Gid <$> arbitrary
--   x <- pick arbitrary
--   topLevelPid <- pick $ elements [Just $ DownloadKey x, Nothing]
--   let createDownload_  = createDownload gid lid uid []

--       generateDownloadTree :: Entity Download -> Int -> PropertyM NwApp ()
--       generateDownloadTree _ 0 = return ()
--       generateDownloadTree (Entity pid parent) d
--         | (dloadComplete == False) = return ()

  --         | (dloadComplete == True) = do
--             numChildren <- pick $ arbitraryPositiveInt `suchThat` (\x -> x < 5)
--             children <- mapM (\ _ -> run $ createDownload_ pid) [1..numChildren]
--             newParentIndex <- pick $ arbitraryPositiveInt `suchThat` (\x -> x < numChildren)
--             let (Entity newParentId _) = children P.!! newParentIndex
--             generateDownloadTree (Just newParentId)  (d - 1)

--         where
--           dloadComplete = isDownloadComplete (parent ^. status) 


--   dloadE@(Entity dloadId dload) <- run $ createDownload_ topLevelPid
--   generateDownloadTree dloadE depth

--   -- Now we'll walk down the downlaod tree and do the checking
--   let checkStatusInvariant :: DownloadId -> NwApp Bool
--       checkStatusInvariant dlid = do
--         r <- aggregateChildrenStatus dlid >>= \case
--           ChildrenNone -> return True
--           ChildrenComplete -> fmap $
--             (\(Entity _ x) -> isDownloadComplete (x ^. status))
--             (selectList [DownloadParentId ==. dlid] [])


