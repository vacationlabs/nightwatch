{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveAnyClass #-}

module JsonTypes where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing
import qualified Model as M
import qualified Nightwatch.Types as T
import GHC.Generics()
import Control.Lens
import Data.List.NonEmpty
import Yesod.Core.Content
import Yesod.Core.Types

data DownloadResponse = DownloadResponse {
  downloads :: [Download]
  } deriving (Show, Eq, Generic, ToJSON)

data Download = Download {
  downloadKey :: M.DownloadId
  ,downloadUserId :: M.UserId
  ,downloadGid :: T.Aria2Gid
  ,downloadStatus :: T.DownloadStatus
  ,downloadCreatedAt :: UTCTime
  ,downloadUpdatedAt :: UTCTime
  ,downloadFiles :: NonEmpty File
  } deriving (Show, Eq, Generic)


data File = File {
  fileKey :: M.FileId
  ,fileFpath :: String
  ,fileLen :: Int
  ,fileUrls :: NonEmpty Url
  } deriving (Show, Eq, Generic)

data Url = Url {
  urlKey :: M.UrlId
  ,urlUrl :: T.URL
  } deriving (Show, Eq, Generic)

instance ToJSON T.DownloadStatus where
  toJSON = toJSON . show

-- instance ToJSON x => ToJSON (NonNull x) where
--   toJSON = toJSON . toNullable

instance ToJSON Download where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance ToJSON File where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance ToJSON Url where
  toJSON = genericToJSON $ aesonPrefix camelCase

$(makeLensesWith abbreviatedFields ''Download)
$(makeLensesWith abbreviatedFields ''File)
$(makeLensesWith abbreviatedFields ''Url)

instance ToContent DownloadResponse where
  toContent = toContent . toJSON

instance ToTypedContent DownloadResponse where
  toTypedContent x = TypedContent "application/json" (toContent x)
