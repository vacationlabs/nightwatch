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

module Nightwatch.TelegramTypes (
  User(..)
  ,Chat(..)
  ,Message(..)
  ,Update(..)
  ,TelegramResponse(..)
  ,OAuthCodeResponse
  ,deviceCode
  ,userCode
  ,verificationUrl
  ,expiresIn
  ,interval
  ,accessToken
  ,refreshToken
  ,tokenType
  ,OAuthTokenResponse
)where

import Prelude
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Casing
import GHC.Generics
import Nightwatch.Types
import Control.Lens

data User = User {
  user_id :: TgramUserId,
  user_first_name :: TgramFirstName,
  user_last_name :: Maybe TgramLastName,
  user_username :: Maybe TgramUsername
} deriving (Show, Generic)

instance ToJSON User
instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = removePrefix "user_"
  }

data Chat = Chat {
  chat_id :: TgramChatId
  --username :: Maybe String,
  --first_name :: Maybe String,
  --last_name :: Maybe String
} deriving (Show, Generic)

instance ToJSON Chat
instance FromJSON Chat where 
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = removePrefix "chat_"
  }

data Message = Message {
  message_id :: Int,
  from :: User,
  date :: Integer,
  chat :: Chat,
  text :: Maybe TgramMsgText,
  forward_from :: Maybe User,
  forward_date :: Maybe Integer,
  reply_to_message :: Maybe Message
} deriving (Show, Generic)

instance ToJSON Message
instance FromJSON Message


data Update = Update {
  update_id :: Integer,
  message :: Message
} deriving (Show, Generic)

instance ToJSON Update
instance FromJSON Update

data TelegramResponse = TelegramResponse {
  ok :: Bool,
  result :: [Update]
} deriving (Show, Generic)

instance ToJSON TelegramResponse
instance FromJSON TelegramResponse


data OAuthCodeResponse = OAuthCodeResponse {
  codeDeviceCode :: String,
  codeUserCode :: String,
  codeVerificationUrl :: String,
  codeExpiresIn :: Int,
  codeInterval :: Int
  } deriving (Show, Generic)

instance FromJSON OAuthCodeResponse where 
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

$(makeLensesWith abbreviatedFields ''OAuthCodeResponse)

data OAuthTokenResponse = OAuthTokenReponse {
  tokenAccessToken :: OAuthAccessToken,
  tokenTokenType :: String,
  tokenExpiresIn :: Int,
  tokenRefreshToken :: OAuthRefreshToken
  } deriving (Show, Generic)

instance FromJSON OAuthTokenResponse where 
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

$(makeLensesWith abbreviatedFields ''OAuthTokenResponse)

