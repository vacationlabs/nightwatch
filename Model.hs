{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}


module Model where

import Nightwatch.Types
import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Control.Lens

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings,
        mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")

$(makeLensesWith abbreviatedFields ''User)
$(makeLensesWith abbreviatedFields ''Log)
$(makeLensesWith abbreviatedFields ''Download)
$(makeLensesWith abbreviatedFields ''File)
$(makeLensesWith abbreviatedFields ''Url)

-- genericCompare :: (PersistEntity record) => record -> record -> Ordering
-- genericCompare (Entity d1 _) (Entity d2 _)
--   | d1 == d2 = EQ
--   | d1 < d2 = LT
--   | d1 > d2 = GT

-- instance {-# OVERLAPPING #-}  Ord (Entity Download) where
--   compare = genericCompare

-- instance {-# OVERLAPPING #-} Ord (Entity File) where
--   compare = genericCompare

-- instance {-# OVERLAPPING #-} Ord (Entity Url) where
--   compare = genericCompare 
 
defWithTs :: (MonadIO m, Default a, HasCreatedAt a UTCTime, HasUpdatedAt a UTCTime) => m a
defWithTs = do
  time <- liftIO getCurrentTime
  return $ def & (createdAt .~ time) & (updatedAt .~ time)

assignTs :: (MonadIO m, HasCreatedAt a UTCTime, HasUpdatedAt a UTCTime) => a -> m a
assignTs obj = do
  time <- liftIO getCurrentTime
  return $ obj & (createdAt .~ time) & (updatedAt .~ time)

updateTs :: (MonadIO m, HasUpdatedAt a UTCTime) => a -> m a
updateTs obj = do
  time <- liftIO getCurrentTime
  return $ obj & (updatedAt .~ time)
