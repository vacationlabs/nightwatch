{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Nightwatch.Telegram (ensureAria2Running, startAria2, startTelegramBot, NightwatchCommand(..), AuthNightwatchCommand(..)) where
import Control.Lens hiding(from)
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
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL(pack, unpack)
import Text.Regex.Posix
import System.IO.Error
import Text.Read (readMaybe)
import System.Process (proc, createProcess, getProcessExitCode, ProcessHandle, waitForProcess)
import qualified Network.WebSockets  as WS
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Control.Concurrent.Async as A
import qualified Data.Map as M
import Nightwatch.Types hiding (message)
import qualified Nightwatch.Types as Ty(message)
import Nightwatch.DBTypes hiding (message, chatId, User(..))
import qualified Nightwatch.DBTypes as DB (message, chatId, User(..), authenticateChat)
import Control.Monad.IO.Class  (liftIO, MonadIO)
import Nightwatch.TelegramTypes

type Resp = Response TelegramResponse

botToken = "151105940:AAEUZbx4_c9qSbZ5mPN3usjXVwGZzj-JtmI"
apiBaseUrl = "https://api.telegram.org/bot" ++ botToken
ariaRPCPort = 9999
ariaRPCUrl = "http://localhost:" ++ (show ariaRPCPort) ++ "/rpc"
aria2Command = "./aria2-1.19.3/bin/aria2c"
aria2DownloadDir = "./downloads"
aria2Args = ["--enable-rpc=true", "--rpc-listen-port=" ++ (show ariaRPCPort), "--rpc-listen-all=false", "--dir=" ++ aria2DownloadDir]


parseIncomingMessage :: Maybe TgramMsgText -> NightwatchCommand
parseIncomingMessage Nothing = InvalidCommand
parseIncomingMessage (Just (TgramMsgText inp))
  | length matches == 0 = InvalidCommand
  | (head matches) == "download" = DownloadCommand $ URL (head $ tail matches)
  | (head matches) == "pause" = PauseCommand $ Aria2Gid (head $ tail matches)
  | (head matches) == "unpause" = PauseCommand $ Aria2Gid (head $ tail matches)
  | (head matches) == "status" = StatusCommand $ Aria2Gid (head $ tail matches)
  | otherwise = InvalidCommand
  where (_, _, _, matches) = ((inp :: String) =~ ("^(download|pause|cancel|status)[ \t\r\n\v\f]+(.*)" :: String) :: (String, String, String, [String]))

-- authenticateCommand :: Message -> NwApp (AuthNightwatchCommand)
-- authenticateCommand msg = do
--   let chatId = chat_id $ chat msg
--   user <- DB.authenticateChat chatId
--   case user of
--     Nothing -> return UnauthenticatedCommand
--     Just u -> return AuthNightwatchCommand{command=(parseIncomingMessage $ text msg), userId=(entityKey u), DB.chatId=chatId}

getUpdates :: (Num a, Show a) => Maybe a -> IO (Response BL.ByteString)
getUpdates Nothing = getUpdates (Just 0)
getUpdates (Just offset) = do
  putStrLn (apiBaseUrl ++ "/getUpdates?offset=" ++ (show offset))
  get (apiBaseUrl ++ "/getUpdates?offset=" ++ (show offset))

getUpdatesAsJSON offset = do
  asJSON =<< (getUpdates offset) :: IO Resp

sendMessage :: TelegramOutgoingMessage -> IO ()
sendMessage tgMsg = do
  putStrLn $ "Sending to " ++ (show tgMsg)
  (void (post (apiBaseUrl ++ "/sendMessage") ["chat_id" := (tg_chat_id tgMsg), "text" := (DB.message tgMsg)])) `catch` (\e -> putStrLn $ "ERROR in sending to " ++ (show $ tg_chat_id tgMsg) ++ ": " ++ (show (e :: Control.Exception.SomeException)))
    

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

processIncomingMessages :: ConnectionPool -> Chan Update -> Aria2Channel -> IO ()
processIncomingMessages pool tgIncomingChan aria2Chan = forever $ do
  putStrLn "STARTING processIncomingMessages"
  update <- readChan tgIncomingChan
  let msg = message update
  let chatId = chat_id $ chat $ msg
  user <- runDb pool $ DB.authenticateChat chatId
  case user of
    Nothing -> sendMessage TelegramOutgoingMessage{tg_chat_id=chatId, DB.message=(TgramMsgText "Cannot process command. You are not an authenticated user.")}
    Just userE -> do
      let nwCmd = parseIncomingMessage (text msg)
          logTgram :: IO (Entity Log)
          logTgram = (runDb pool $ logIncomingTelegramMessage msg  (Just $ entityKey userE) (Just nwCmd))
          sendToAria2 :: Entity Log -> IO ()
          sendToAria2 logE = writeChan aria2Chan AuthNightwatchCommand{command=nwCmd, userId=(entityKey userE), DB.chatId=chatId, logId=(entityKey logE)}
      case nwCmd of
        (DownloadCommand url) -> logTgram >>= sendToAria2
        (StatusCommand gid) -> logTgram >>= sendToAria2
        _ -> sendMessage $ TelegramOutgoingMessage {tg_chat_id=(chat_id $ chat $ message update), DB.message=(TgramMsgText "What language, dost thou speaketh? Command me with: download <url>")}


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


startTelegramBot :: ConnectionPool -> Aria2Channel -> TelegramOutgoingChannel -> IO ()
startTelegramBot pool aria2Chan tgOutChan = do
  tgIncomingChan <- newChan
  forkIO $ forever $ logAllExceptions "Error in processIncomingMEssages: " (processIncomingMessages pool tgIncomingChan aria2Chan)
  forkIO $ forever $ logAllExceptions "Error in doPollLoop: " (doPollLoop tgIncomingChan =<< getLastUpdateId)
  forkIO $ forever $ logAllExceptions "Error in processOutgoingMessages:" (processOutgoingMessages tgOutChan)
  return ()

startAria2 = forkIO $ forever  ensureAria2Running
