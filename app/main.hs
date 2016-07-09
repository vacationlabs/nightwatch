{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import Import hiding (googleClientId, googleClientSecret, tgramBotToken, aria2Command, aria2DownloadDir)
import Application (startWebapp)
import Nightwatch.Types
import Nightwatch.DBTypes
import Nightwatch.Telegram
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.IO.Class  (liftIO)
import System.Environment(getEnv)
import Control.Lens
import qualified Data.Text as T
import Application (makeFoundation)

-- onMessage :: Aria2Gid -> IO ()
-- onMessage gid = putStrLn $ "Received response " ++ (show gid)


-- main :: IO ()
-- main = runStderrLoggingT $ withSqlitePool "nightwatch.db" 5 $ \pool -> liftIO $ do
--   [cId, cSecret, botToken] <- sequence [getEnv "GOOGLE_CLIENT_ID", getEnv "GOOGLE_CLIENT_SECRET", getEnv "TELEGRAM_TOKEN"]
--   tgOutChan <- newChan
--   let nwConfig = (def :: NwConfig)
--         & googleClientId .~ (T.pack cId)
--         & googleClientSecret .~ (T.pack cSecret)
--         & tgramBotToken .~ botToken
--         & dbPool .~ pool
--         & tgramOutgoingChannel .~ tgOutChan
--         & aria2Command .~ "/Users/saurabhnanda/projects/nightwatch/aria2-1.19.3/bin/aria2c"
--         & aria2DownloadDir .~ "/Users/saurabhnanda/projects/nightwatch/downloads"
--   startWebapp
--   -- runMigrations pool
--   startAria2 nwConfig
--   startTelegramBot nwConfig


main :: IO ()
main = do
  -- Get the settings from all relevant sources
  settings <- loadYamlSettingsArgs
    -- fall back to compile-time values, set to [] to require values at runtime
    [configSettingsYmlValue]

    -- allow environment variables to override
    useEnv

  [cId, cSecret, botToken] <- sequence [getEnv "GOOGLE_CLIENT_ID", getEnv "GOOGLE_CLIENT_SECRET", getEnv "TELEGRAM_TOKEN"]

  -- Generate the foundation from the settings
  foundation <- makeFoundation (settings
                                 & googleClientIdL .~ (pack cId)
                                 & googleClientSecretL .~ (pack cSecret)
                                 & tgramBotTokenL .~ botToken)

  putStrLn "creating channel..."
  tgOutChan <- newChan
  let nwConfig = (def :: NwConfig)
        & googleClientId .~ (foundation ^. appSettingsL ^. googleClientIdL)
        & googleClientSecret .~ (foundation ^. appSettingsL ^. googleClientSecretL)
        & tgramBotToken .~ (foundation ^. appSettingsL ^. tgramBotTokenL)
        & dbPool .~ (foundation ^. appConnPoolL)
        & tgramOutgoingChannel .~ tgOutChan
        & aria2Command .~ (foundation ^. appSettingsL ^. aria2CommandL)
        & aria2DownloadDir .~ (foundation ^. appSettingsL ^. aria2DownloadDirL)
  putStrLn "starting Aria2..."
  startAria2 nwConfig
  putStrLn "starting telegram bot..."
  startTelegramBot nwConfig

  putStrLn "Starting webapp..."
  startWebapp foundation
