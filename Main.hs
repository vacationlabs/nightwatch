{-# LANGUAGE OverloadedStrings          #-}
import Nightwatch.Types
import Nightwatch.DBTypes
import Nightwatch.Telegram
import Nightwatch.Webapp
import Nightwatch.Websocket
import Control.Concurrent.Chan
import Database.Persist.Sql
import Database.Persist.Sqlite

main :: IO ()
main = do
  _withSqlitePool "nightwatch.db" 5 $ \pool ->
    runMigrations pool
    aria2Chan <- newChan
    tgOutChan <- newChan
    startTelegramBot pool aria2Chan tgOutChan
    startAria2
    startAria2WebsocketClient pool aria2Chan tgOutChan
    startWebapp
