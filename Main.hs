{-# LANGUAGE OverloadedStrings          #-}
import Nightwatch.Types
import Nightwatch.DBTypes
import Nightwatch.Telegram
import Nightwatch.Webapp
import Nightwatch.Websocket
import Control.Concurrent.Chan
import Database.Persist.Sql
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.IO.Class  (liftIO, MonadIO)

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "nightwatch.db" 5 $ \pool -> liftIO $ do 
  runMigrations pool
  aria2Chan <- newChan
  tgOutChan <- newChan
  startTelegramBot pool aria2Chan tgOutChan
  startAria2
  startAria2WebsocketClient pool aria2Chan tgOutChan
  startWebapp

