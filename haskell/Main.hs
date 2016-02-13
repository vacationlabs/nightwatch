import Nightwatch.Types
import Nightwatch.Telegram
import Nightwatch.Webapp
import Nightwatch.Websocket
import Control.Concurrent.Chan

main = do 
  aria2Chan <- newChan
  tgOutChan <- newChan
  startTelegramBot aria2Chan tgOutChan
  startAria2
  startAria2WebsocketClient aria2Chan tgOutChan
  startWebapp
