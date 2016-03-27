import Nightwatch.Types
import Nightwatch.DBTypes
import Nightwatch.Telegram
import Nightwatch.Webapp
import Nightwatch.Websocket
import Control.Concurrent.Chan
import Database.Persist.Sql

main :: IO ()
main = do
  runMigrations
  aria2Chan <- newChan
  tgOutChan <- newChan
  startTelegramBot aria2Chan tgOutChan
  startAria2
  startAria2WebsocketClient aria2Chan tgOutChan
  startWebapp
