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

module Nightwatch.Types (NightwatchCommand(..)
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
  ,removePrefix
  ,logAllExceptions
  ,Aria2RequestId
  ,nextRequestId
  ) where

import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import Control.Concurrent.Chan(Chan)
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics (Generic)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Network.Wreq as W(FormValue)
import Data.List (isPrefixOf, drop)
import qualified GHC.Stack as Stk
import Data.Functor (void)
import Control.Exception (catch, try, tryJust, bracketOnError, SomeException, Exception)
import Data.UUID
import qualified Data.UUID.V1 as UUIDv1

-- TODO -- VLUser should be changed to UserId coming from the database
-- newtype VLUser = VLUser Integer deriving (Show, Eq)
newtype URL = URL String deriving (Show, Eq, Generic, Read)
newtype Aria2Gid = Aria2Gid String deriving (Show, Eq, Generic, Read)
instance ToJSON URL
instance ToJSON Aria2Gid
instance FromJSON Aria2Gid

type Aria2RequestId = String

newtype TgramUserId = TgramUserId Integer deriving (Show, Eq, Generic, Read)
newtype TgramFirstName = TgramFirstName String deriving (Show, Eq, Generic, Read)
newtype TgramLastName = TgramLastName String deriving (Show, Eq, Generic, Read)
newtype TgramUsername = TgramUsername String deriving (Show, Eq, Generic, Read)
newtype TgramMsgText = TgramMsgText String deriving (Show, Eq, Generic, Read, W.FormValue)
newtype TgramChatId = TgramChatId Integer deriving (Show, Eq, Generic, Read, W.FormValue)
instance FromJSON TgramUserId
instance FromJSON TgramFirstName
instance FromJSON TgramLastName
instance FromJSON TgramUsername
instance FromJSON TgramMsgText
instance FromJSON TgramChatId
instance ToJSON TgramUserId
instance ToJSON TgramFirstName
instance ToJSON TgramLastName
instance ToJSON TgramUsername
instance ToJSON TgramMsgText
instance ToJSON TgramChatId

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

logAllExceptions logMarker fn = (void fn) `catch` (\e -> Stk.currentCallStack >>= (\stack -> putStrLn $ logMarker ++ (show (e :: Control.Exception.SomeException)) ++ "\nSTACKTRACE\n" ++ (show stack)))

-- TODO: This has the potential of going into an infinite loop. Break after
-- N-tries
nextRequestId :: IO Aria2RequestId
nextRequestId = UUIDv1.nextUUID >>= \uuid ->
  case uuid of
    Nothing -> nextRequestId
    Just uuid -> return $ toString uuid

-- parseRequestId :: String -> Maybe Aria2RequestId
-- parseRequestId = Just . id
