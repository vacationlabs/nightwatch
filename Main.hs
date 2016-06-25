{-# LANGUAGE OverloadedStrings          #-}
import Nightwatch.Types
import Nightwatch.DBTypes
import Nightwatch.Telegram
import Nightwatch.Webapp
-- import Nightwatch.Websocket
-- import Nightwatch.Aria2
-- import Control.Concurrent.Async
import Control.Concurrent.Chan
-- import Database.Persist.Sql
import Database.Persist.Sqlite
-- import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.IO.Class  (liftIO)
import System.Environment(getEnv)
import Control.Lens
import qualified Data.Text as T

onMessage :: Aria2Gid -> IO ()
onMessage gid = putStrLn $ "Received response " ++ (show gid)


-- myTest :: IO ([(Entity Download, [Entity File])])
-- myTest = runStderrLoggingT $ withSqlitePool "nightwatch.db" 5 $ \pool -> liftIO $ do 
--   runDb pool $ getDownloadsByUserId $ UserKey 1

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "nightwatch.db" 5 $ \pool -> liftIO $ do
  [cId, cSecret, botToken] <- sequence [getEnv "GOOGLE_CLIENT_ID", getEnv "GOOGLE_CLIENT_SECRET", getEnv "TELEGRAM_TOKEN"]
  tgOutChan <- newChan
  let nwConfig = (def :: NwConfig)
        & googleClientId .~ (T.pack cId)
        & googleClientSecret .~ (T.pack cSecret)
        & tgramBotToken .~ botToken
        & dbPool .~ pool
        & tgramOutgoingChannel .~ tgOutChan
        & aria2Command .~ "/Users/saurabhnanda/projects/nightwatch/aria2-1.19.3/bin/aria2c"
        & aria2DownloadDir .~ "/Users/saurabhnanda/projects/nightwatch/downloads"
  -- runMigrations pool
  startAria2 nwConfig
  startTelegramBot nwConfig
  startWebapp nwConfig

-- main = do
--   a <- async $ startWebsocketClient "localhost" 9999 "/jsonrpc" defaultAria2Callbacks{onDownloadStart=onMessage, onDownloadComplete=onMessage, onDownloadError=onMessage}
--   addUri ariaRPCUrl "http://www.vacationlabs.com"
--   wait a
--   return ()


