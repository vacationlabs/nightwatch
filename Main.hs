{-# LANGUAGE OverloadedStrings          #-}
import Nightwatch.Types
import Nightwatch.DBTypes
import Nightwatch.Telegram
import Nightwatch.Webapp
-- import Nightwatch.Websocket
import Nightwatch.Aria2
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Database.Persist.Sql
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.IO.Class  (liftIO, MonadIO)

onMessage :: Aria2Gid -> IO ()
onMessage gid = putStrLn $ "Received response " ++ (show gid)


main :: IO ()
main = runStderrLoggingT $ withSqlitePool "nightwatch.db" 5 $ \pool -> liftIO $ do 
  runMigrations pool
  aria2Chan <- newChan
  tgOutChan <- newChan
  startTelegramBot pool aria2Chan tgOutChan
  startAria2
  -- startAria2WebsocketClient pool aria2Chan tgOutChan
  startWebapp

-- main = do
--   a <- async $ startWebsocketClient "localhost" 9999 "/jsonrpc" defaultAria2Callbacks{onDownloadStart=onMessage, onDownloadComplete=onMessage, onDownloadError=onMessage}
--   addUri ariaRPCUrl "http://www.vacationlabs.com"
--   wait a
--   return ()


