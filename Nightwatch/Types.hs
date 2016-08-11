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
 
module Nightwatch.Types where

import Prelude 
import qualified Data.Text           as T
import Text.Printf
-- -- -- -- import qualified Data.Text.IO        as T
import Control.Concurrent.Chan(Chan)
import Data.Aeson
-- -- -- -- import Data.Aeson.Types
import GHC.Generics (Generic)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Network.Wreq as W(FormValue)
import Data.List (isPrefixOf, drop)
import qualified GHC.Stack as Stk
import Data.Functor (void)
import Data.UUID
import qualified Data.UUID.V1 as UUIDv1
import Data.Foldable(foldl')
import Control.Monad.Catch
import Data.Typeable
import Text.Blaze (ToMarkup, toMarkup)
-- -- -- -- import Data.String(IsString)


-- googleClientId :: T.Text
-- googleClientId = "1045667944271-dnh31h9n4ul2i0q42tjirc7n7tk7k9jq.apps.googleusercontent.com"

-- googleClientSecret :: T.Text
-- googleClientSecret = "GcX5OviTB0uMjQHKpEXZaF4A"

data SanityException = DownloadStatusException T.Text
  deriving (Show, Typeable)

instance Exception SanityException

-- TODO -- VLUser should be changed to UserId coming from the database
-- newtype VLUser = VLUser Integer deriving (Show, Eq)
newtype URL = URL String deriving (Show, Eq, Generic, Read, FromJSON, ToJSON, Ord)
instance ToMarkup URL where
  toMarkup (URL u) = toMarkup u

newtype Aria2Gid = Aria2Gid String deriving (Show, Eq, Generic, Read, FromJSON, ToJSON,Ord)
instance ToMarkup Aria2Gid where
  toMarkup (Aria2Gid gid) = toMarkup gid

data ChildrenStatus = ChildrenNone | ChildrenComplete | ChildrenIncomplete deriving (Show, Eq, Generic, Read, Ord)
data DownloadStatus = DownloadComplete ChildrenStatus | DownloadIncomplete deriving (Show, Eq, Generic, Read, Ord)
derivePersistField "DownloadStatus"
derivePersistField "ChildrenStatus"
instance ToMarkup DownloadStatus where
  toMarkup DownloadIncomplete = "Incomplete"
  toMarkup (DownloadComplete ChildrenNone) = "Complete"
  toMarkup (DownloadComplete ChildrenComplete) = "Complete"
  toMarkup (DownloadComplete ChildrenIncomplete) = "Waiting for children"

isDownloadComplete :: DownloadStatus -> Bool
isDownloadComplete DownloadIncomplete = False
isDownloadComplete (DownloadComplete ChildrenNone) = True
isDownloadComplete (DownloadComplete ChildrenComplete) = True
isDownloadComplete (DownloadComplete ChildrenIncomplete) = False


type Aria2RequestId = String
type OAuthRefreshToken = String
type OAuthAccessToken = String

newtype TgramUserId = TgramUserId Integer deriving (Show, Eq, Generic, Read, FromJSON, ToJSON)
newtype TgramFirstName = TgramFirstName String deriving (Show, Eq, Generic, Read, FromJSON, ToJSON)
newtype TgramLastName = TgramLastName String deriving (Show, Eq, Generic, Read, FromJSON, ToJSON)
newtype TgramUsername = TgramUsername String deriving (Show, Eq, Generic, Read, FromJSON, ToJSON)
newtype TgramMsgText = TgramMsgText String deriving (Show, Eq, Generic, Read, W.FormValue, FromJSON, ToJSON)
newtype TgramChatId = TgramChatId Integer deriving (Show, Eq, Generic, Read, W.FormValue, FromJSON, ToJSON)

data NightwatchCommand = InvalidCommand | DownloadCommand URL | PauseCommand Aria2Gid | UnpauseCommand Aria2Gid | StatusCommand Aria2Gid deriving (Show, Eq, Generic, Read)

data TelegramOutgoingMessage = TelegramOutgoingMessage {
  tg_chat_id :: TgramChatId,
  message :: TgramMsgText
  } deriving (Show, Eq)

type TelegramOutgoingChannel = Chan TelegramOutgoingMessage

-- newtype Aria2RequestId = Aria2RequestId Integer deriving (Show, Eq, Generic, Read, Num)

instance PersistField URL where
  toPersistValue (URL url) = PersistText (T.pack url)
  fromPersistValue (PersistText url) = Right (URL (T.unpack url))
  fromPersistValue x = Left $ T.pack $ "Error in de-serializing URL from the database. Found " ++ (show x)

instance PersistFieldSql URL where
  sqlType _ = SqlString

instance PersistField Aria2Gid where
  toPersistValue (Aria2Gid gid) = PersistText (T.pack gid)
  fromPersistValue (PersistText gid) = Right (Aria2Gid (T.unpack gid))
  fromPersistValue x = Left $ T.pack $ "Error in de-serializing Aria2GID from the database. Found " ++ (show x)

instance PersistFieldSql Aria2Gid where
  sqlType _ = SqlString

instance PersistField TgramUserId where
  toPersistValue (TgramUserId x) = PersistInt64 (fromIntegral x)
  fromPersistValue (PersistInt64 x) = Right (TgramUserId (fromIntegral x))
  fromPersistValue x = Left $ T.pack $ "Error in de-serializing TgramUserId from the database. Found " ++ (show x)

instance PersistFieldSql TgramUserId where
  sqlType _ = SqlInt64

instance PersistField TgramChatId where
  toPersistValue (TgramChatId x) = PersistInt64 (fromIntegral x)
  fromPersistValue (PersistInt64 x) = Right (TgramChatId (fromIntegral x))
  fromPersistValue x = Left $ T.pack $ "Error in de-serializing TgramChatId from the database. Found " ++ (show x)

instance PersistFieldSql TgramChatId where
  sqlType _ = SqlInt64


derivePersistField "TgramUsername"
derivePersistField "TgramFirstName"
derivePersistField "TgramLastName"
derivePersistField "TgramMsgText"
-- derivePersistField "URL"
-- derivePersistField "Aria2Gid"
derivePersistField "NightwatchCommand"
derivePersistField "UUID"

removePrefix :: String -> String -> String
removePrefix prefix input 
  | isPrefixOf prefix input = drop (length prefix) input
  | otherwise = input

logAllExceptions logMarker fn = (void fn) `catch` (\e -> Stk.currentCallStack >>= (\stack -> putStrLn $ logMarker ++ (show (e::SomeException)) ++ "\nSTACKTRACE\n" ++ (show stack)))

-- TODO: This has the potential of going into an infinite loop. Break after
-- N-tries
nextRequestId :: IO Aria2RequestId
nextRequestId = UUIDv1.nextUUID >>= \uuid ->
  case uuid of
    Nothing -> nextRequestId
    Just uuid -> return $ toString uuid

-- parseRequestId :: String -> Maybe Aria2RequestId
-- parseRequestId = Just . id


joinStrings :: String -> [String] -> String
joinStrings sep lst = drop (length sep) $ foldl' (\memo x -> memo ++ sep ++ x) "" lst

ariaRPCPort :: Int
ariaRPCPort = 9999

ariaRPCHost :: String
ariaRPCHost = "localhost"

ariaRPCUrl :: String
ariaRPCUrl = printf "http://%s:%d/jsonrpc" ariaRPCHost ariaRPCPort
