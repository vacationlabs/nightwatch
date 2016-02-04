{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Nightwatch.Telegram (ensureAria2Running, startAria2, startTelegramBot, NightwatchCommand(..), AuthNightwatchCommand(..)) where
import Control.Lens
import Network.Wreq
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics (Generic)
import Control.Concurrent
import Control.Monad (forever, guard, liftM)
import Control.Concurrent.Chan
import Data.List (isPrefixOf, drop)
import Data.Text (Text, pack)
import Control.Exception (catch, try, tryJust, bracketOnError, SomeException, Exception)
import Data.Functor (void)
import Network.XmlRpc.Client
import Data.Time
import Data.Time.Clock.POSIX
import Data.ByteString.Lazy.Internal (ByteString)
import Text.Regex.Posix
import System.IO.Error
import Text.Read (readMaybe)
import System.Process (proc, createProcess, getProcessExitCode, ProcessHandle, waitForProcess)
import qualified Network.WebSockets  as WS
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Control.Concurrent.Async as A
import qualified Data.Map as M
import Nightwatch.Types hiding (chat_id, message)
import qualified Nightwatch.Types as NT (chat_id, message)

type Resp = Response TelegramResponse

botToken = "151105940:AAEUZbx4_c9qSbZ5mPN3usjXVwGZzj-JtmI"
apiBaseUrl = "https://api.telegram.org/bot" ++ botToken
ariaRPCPort = 9999
ariaRPCUrl = "http://localhost:" ++ (show ariaRPCPort) ++ "/rpc"
aria2Command = "./aria2-1.19.3/bin/aria2c"
aria2DownloadDir = "./downloads"
aria2Args = ["--enable-rpc=true", "--rpc-listen-port=" ++ (show ariaRPCPort), "--rpc-listen-all=false", "--dir=" ++ aria2DownloadDir]


parseIncomingMessage :: Maybe String -> NightwatchCommand
parseIncomingMessage Nothing = InvalidCommand
parseIncomingMessage (Just inp)
  | length matches == 0 = InvalidCommand
  | (head matches) == "download" = DownloadCommand (head $ tail matches)
  | (head matches) == "pause" = PauseCommand (head $ tail matches)
  | (head matches) == "unpause" = PauseCommand (head $ tail matches)
  | (head matches) == "status" = StatusCommand (head $ tail matches)
  | otherwise = InvalidCommand
  where (_, _, _, matches) = ((inp :: String) =~ ("^(download|pause|cancel|status)[ \t\r\n\v\f]+(.*)" :: String) :: (String, String, String, [String]))

-- TODO: Lookup (chat_id $ chat $ msg) in DB to ensure that this chat has been
-- authenticated in the past
authenticateCommand :: Message -> IO (AuthNightwatchCommand)
authenticateCommand msg = return $ AuthNightwatchCommand {command=(parseIncomingMessage $ text msg), user=(VLUser 1), NT.chat_id=(chat_id $ chat $ msg)}

removePrefix :: String -> String -> String
removePrefix prefix input 
  | isPrefixOf prefix input = drop (length prefix) input
  | otherwise = input

data User = User {
  user_id :: Integer,
  user_first_name :: String,
  user_last_name :: Maybe String,
  user_username :: Maybe String
} deriving (Show, Generic)

instance ToJSON User
instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = removePrefix "user_"
  }

data Chat = Chat {
  chat_id :: Integer
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
  text :: Maybe String,
  forward_from :: Maybe User,
  forward_date :: Maybe Integer,
  reply_to_message :: Maybe Message
} deriving (Show, Generic)

instance FromJSON Message
instance ToJSON Message

data Update = Update {
  update_id :: Integer,
  message :: Message
} deriving (Show, Generic)

instance FromJSON Update
instance ToJSON Update

data TelegramResponse = TelegramResponse {
  ok :: Bool,
  result :: [Update]
} deriving (Show, Generic)

instance FromJSON TelegramResponse
instance ToJSON TelegramResponse

getUpdates :: (Num a, Show a) => Maybe a -> IO (Response Data.ByteString.Lazy.Internal.ByteString)
getUpdates Nothing = getUpdates (Just 0)
getUpdates (Just offset) = do
  putStrLn (apiBaseUrl ++ "/getUpdates?offset=" ++ (show offset))
  get (apiBaseUrl ++ "/getUpdates?offset=" ++ (show offset))

getUpdatesAsJSON offset = do
  asJSON =<< (getUpdates offset) :: IO Resp

sendMessage :: TelegramOutgoingMessage -> IO ()
sendMessage tgMsg = do
  putStrLn $ "Sending to " ++ (show tgMsg)
  void (post (apiBaseUrl ++ "/sendMessage") ["chat_id" := (tg_chat_id tgMsg), "text" := (NT.message tgMsg)]) `catch` (\e -> putStrLn $ "ERROR in sending to " ++ (show $ tg_chat_id tgMsg) ++ ": " ++ (show (e :: Control.Exception.SomeException)))
    

-- TODO: There's probably a better way to do this
--processUpdates :: [Integer] -> [Update] -> ([Integer], [Update])
--processUpdates processedUpdateIds [] = (processedUpdateIds, [])
--processUpdates processedUpdateIds all@(update:incomingUpdates)
--  | elem updt_id processedUpdateIds = processUpdates processedUpdateIds incomingUpdates
--  | otherwise = processUpdates (updt_id:processedUpdateIds) incomingUpdates
--  where updt_id = (update_id update)

--main = do 
--  r <- asJSON =<< getUpdates :: IO Resp
--  let incomingUpdates = result $ r ^. responseBody
--      processedUpdateIds = processUpdates processedUpdateIds incomingUpdates
--  putStrLn $ show $ processUpdates processedUpdateIds incomingUpdates


findLastUpdateId :: Integer -> [Update] -> Integer
findLastUpdateId lastUpdateId [] = lastUpdateId
findLastUpdateId lastUpdateId updates = (1+) $ foldl (\m update -> if (update_id update) > m then (update_id update) else m) lastUpdateId updates

doPollLoop tgIncomingChan lastUpdateId = do
  threadDelay (10^6)
  r <- asJSON =<< (getUpdates (Just lastUpdateId)) :: IO Resp
  let incomingUpdates = (result $ r ^. responseBody)
  putStrLn $ "Will process " ++ (show incomingUpdates)
  writeList2Chan tgIncomingChan incomingUpdates
  doPollLoop tgIncomingChan =<< setLastUpdateId (findLastUpdateId lastUpdateId incomingUpdates)

processIncomingMessages :: Chan Update -> Aria2Channel -> IO ()
processIncomingMessages tgIncomingChan aria2Chan = do
  putStrLn "STARTING processIncomingMessages"
  update <- readChan tgIncomingChan
  nwCommand <- authenticateCommand $ message update
  putStrLn $ "nwCommand received: " ++ (show nwCommand)
  case (command nwCommand) of
    (DownloadCommand url) -> writeChan aria2Chan nwCommand
    _ -> sendMessage $ TelegramOutgoingMessage {tg_chat_id=(chat_id $ chat $ message update), NT.message="What language, dost thou speaketh? Command me with: download <url>"} 
  processIncomingMessages tgIncomingChan aria2Chan

--sendCannedResponse :: Chan Update -> IO ()
--sendCannedResponse tgIncomingChan = do
--  update <- readChan tgIncomingChan
--  putStrLn $ "=====> SENDING: " ++ (show update)
--  let funkyMsg = "Night gathers, and now my download begins. It shall not end until the morn. I shall play no games, watch no videos, read no blogs. I shall get no rest and get no sleep. I shall live and die at my download queue. I am the leech on the network. I pledge my life and honor to the Night's Watch, for this night and all the nights to come."
--  void (sendMessage update funkyMsg) `catch` (\e -> do putStrLn (show (e :: Control.Exception.SomeException)))
--  sendCannedResponse tgIncomingChan

setLastUpdateId updateId = do
  writeFile "./last-update-id" (show updateId)
  return updateId

getLastUpdateId = do
  x <- readFile "./last-update-id"
  case (reads x :: [(Integer, String)]) of
    [] -> return 0 -- Probably not a good idea
    [(y, s)] -> return y

startAria2_ = do
  putStrLn "==> Starting Aria2"
  (_, _, _, processHandle) <- createProcess (proc aria2Command aria2Args)
  return processHandle

ensureAria2Running :: IO a
ensureAria2Running = do
  ph <- startAria2_
  exitCode <- waitForProcess ph
  putStrLn $ "ERROR: Aria2 process died mysteriously: " ++ (show exitCode)
  ensureAria2Running

processOutgoingMessages :: TelegramOutgoingChannel -> IO ()
processOutgoingMessages tgOutChan = do 
  tgMsg <- readChan tgOutChan
  sendMessage tgMsg
  processOutgoingMessages tgOutChan


startTelegramBot aria2Chan tgOutChan = do
  tgIncomingChan <- newChan
  forkIO $ forever $ void (doPollLoop tgIncomingChan =<< getLastUpdateId) `catch` (\e -> putStrLn $ "ERROR IN doPollLoop: " ++ (show (e :: Control.Exception.SomeException)))
  forkIO $ forever $ void (processIncomingMessages tgIncomingChan aria2Chan) `catch` (\e -> putStrLn $ "ERROR IN processIncomingMessages: " ++ (show (e :: Control.Exception.SomeException)))
  forkIO $ forever $ void (processOutgoingMessages tgOutChan) `catch` (\e -> putStrLn $ "ERROR IN processOutgoingMessages: " ++ (show (e :: Control.Exception.SomeException)))

startAria2 = do
  forkIO $ forever $ ensureAria2Running

