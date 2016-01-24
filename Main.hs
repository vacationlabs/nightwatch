import Nightwatch.Telegram

main = do 
  startTelegramBot
  startAria2
  getLine
  putStrLn "exiting now"
