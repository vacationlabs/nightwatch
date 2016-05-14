{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Nightwatch.Telegram (ensureAria2Running, startAria2, startTelegramBot, NightwatchCommand(..), AuthNightwatchCommand(..)) where
import           Control.Concurrent
import qualified Control.Concurrent.Async as A
import           Control.Concurrent.Chan
import           Control.Exception (catch, try, tryJust, bracketOnError, SomeException, Exception)
import Control.Lens hiding(from)
import           Control.Monad (forever, guard, liftM)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL (pack, unpack)
import           Data.Functor (void)
import           Data.List (isPrefixOf, drop)
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Text (Text, pack)
import qualified Data.Text.IO as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           GHC.Generics (Generic)
import           Network.Wreq
import           Network.XmlRpc.Client
import qualified Nightwatch.DBTypes as DB (message, chatId, User(..), authenticateChat)
import           Nightwatch.DBTypes hiding (message, chatId, User(..))
import           Nightwatch.TelegramTypes
import qualified Nightwatch.Types as Ty(message)
import           Nightwatch.Types hiding (message)
import           System.IO.Error
import           System.Process (proc, createProcess, getProcessExitCode, ProcessHandle, waitForProcess)
import           Text.Read (readMaybe)
import           Text.Regex.Posix
import qualified Nightwatch.Aria2 as A2
import           Safe (fromJustNote)
import           Text.Printf

type Resp = Response TelegramResponse

botToken = "151105940:AAEUZbx4_c9qSbZ5mPN3usjXVwGZzj-JtmI"
apiBaseUrl = "https://api.telegram.org/bot" ++ botToken
ariaRPCPort = 9999
ariaRPCUrl = "http://localhost:" ++ (show ariaRPCPort) ++ "/jsonrpc"
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
          logTgram :: NwApp (Entity Log)
          logTgram = (logIncomingTelegramMessage msg  (Just $ entityKey userE) (Just nwCmd))
      void $ forkIO $ runDb pool $ case nwCmd of
        (DownloadCommand url) -> logTgram >>= (\logE -> onDownloadCommand userE logE url)
        (StatusCommand gid) -> logTgram >>= (\logE -> onStatusCommand userE logE gid)
        _ -> liftIO $ sendMessage $ TelegramOutgoingMessage {tg_chat_id=(chat_id $ chat $ message update), DB.message=(TgramMsgText "What language, dost thou speaketh? Command me with: download <url>")}


onStatusCommand :: Entity DB.User -> Entity Log -> Aria2Gid -> NwApp ()
onStatusCommand userE logE gid = do
  let (Aria2Gid gidS) = gid
      logId = entityKey logE
      log = entityVal logE
      userId = entityKey userE
      user = entityVal userE
      chatId = (fromJustNote "Expecting Log to have telegram chat ID" $ logTgramChatId log)
  statusResponse <- liftIO $ A2.tellStatus ariaRPCUrl gid
  updateWithAria2Response logId (show statusResponse)
  logAndSendTgramMessage logId TelegramOutgoingMessage{tg_chat_id=chatId, DB.message=(TgramMsgText $ humanizeStatusResponse statusResponse)}


onDownloadCommand :: Entity DB.User -> Entity Log -> URL -> NwApp ()
onDownloadCommand userE logE url = do
  let (URL urlS) = url
      logId = entityKey logE
      log = entityVal logE
      userId = entityKey userE
      user = entityVal userE
      chatId = (fromJustNote "Expecting Log to have telegram chat ID" $ logTgramChatId log)
  gid <- liftIO $ A2.addUri ariaRPCUrl urlS
  let (Aria2Gid gidS) = gid
  updateWithAria2Response logId gidS
  dloadE <- createDownload url gid logId userId
  logAndSendTgramMessage logId TelegramOutgoingMessage{tg_chat_id=chatId, DB.message=(TgramMsgText $ "Download queued. ID=" ++ gidS ++ ". You can use that to check the status, pause, or stop the download. eg. status " ++ gidS)}

logAndSendTgramMessage :: LogId -> TelegramOutgoingMessage -> NwApp()
logAndSendTgramMessage logId msg = do
  updateWithTgramOutgoingMsg logId msg
  liftIO $ sendMessage msg


humanizeBytes :: Integer -> String
humanizeBytes b
  | b < 1024 = printf "%d bytes" b
  | b <= 1024*1024 = printf "%.2f KB" (((fromIntegral b) / (1024))::Float)
  | b <= 1024*1024*1024 = printf "%.2f MB" (((fromIntegral b) / (1024*1024))::Float)
  | otherwise = printf "%.2f GB" (((fromIntegral b) / (1024*1024*1024))::Float)

humanizeStatusResponse :: A2.StatusResponse -> String
humanizeStatusResponse res
  | percentDownloaded_  == (fromIntegral 100) = printf "Download completed (%s)" (humanizeBytes $ A2.st_totalLength res)
  | (length $ A2.st_files res) > 1 = joinStrings (map downloadFileSummary (A2.st_files res)) "\n"
  | otherwise = (printf "%.0f%% downloaded. %s to go (%s at %s/s)..."
                 percentDownloaded_
                 (downloadEta downloadSpeed etaSeconds)
                 (humanizeBytes remainingLength)
                 (humanizeBytes downloadSpeed))
  where
    percentDownloaded_ = (percentDownloaded (A2.st_completedLength res) (A2.st_totalLength res))
    downloadSpeed = A2.st_downloadSpeed res
    remainingLength = ((A2.st_totalLength res) - (A2.st_completedLength res))
    etaSeconds :: Integer
    etaSeconds = remainingLength `div` downloadSpeed

downloadEta :: Integer -> Integer -> String
downloadEta downloadSpeed etaSeconds
  | downloadSpeed < 1 = "A long time"
  | etaSeconds < 1*60 = "Under a minute"
  | otherwise = joinStrings (map (\(cnt, str) -> (show cnt) ++ " " ++ str) (take 2 x)) ", "
  where
    (tMin, sec) = divMod etaSeconds 60
    (tHr, min) = if tMin==0 then (0, 0) else divMod tMin 60
    (tDays, hr) = if tHr==0 then (0, 0) else divMod tHr 24
    (weeks, days) = if tDays==0 then (0, 0) else divMod tDays 7
    x = filter (\(cnt, _) -> cnt>0) [(weeks, "weeks"), (days, "days"), (hr, "hours"), (min, "minutes")]

downloadFileSummary :: A2.GetFilesResponse -> String
downloadFileSummary res = (printf "[%.0f] %s"
                           (percentDownloaded (A2.gf_completedLength res) (A2.gf_length res))
                           (A2.gf_path res))

percentDownloaded :: Integer -> Integer -> Float
percentDownloaded completed total = ((fromIntegral completed) / (fromIntegral total)) * 100.0

downloadUriSummary :: A2.GetUriResponse -> String
downloadUriSummary res = printf "[%s] %s" (A2.gu_status res) (show $ A2.gu_uri res)

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
  -- forkIO $ forever $ logAllExceptions "Error in processOutgoingMessages:" (processOutgoingMessages tgOutChan)
  return ()

startAria2 = forkIO $ forever  ensureAria2Running
