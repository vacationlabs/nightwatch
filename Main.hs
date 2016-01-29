import Nightwatch.Telegram
import Nightwatch.Webapp
import Nightwatch.Websocket

main = do 
  startTelegramBot
  startAria2
  startAria2WebsocketClient
  startWebapp
