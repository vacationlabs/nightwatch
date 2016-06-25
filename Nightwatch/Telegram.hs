{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Nightwatch.Telegram (ensureAria2Running, startAria2, startTelegramBot, NightwatchCommand(..), AuthNightwatchCommand(..)) where
import           Control.Concurrent
import qualified Control.Concurrent.Async as A
-- import           Control.Concurrent.Chan
import           Control.Exception (catch, try, tryJust, bracketOnError, SomeException, Exception)
import Control.Lens hiding(from)
import Data.Aeson.Lens
import           Control.Monad (forever, guard, liftM, when, zipWithM)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.Aeson
-- import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS (pack, unpack)
-- import qualified Data.ByteString.Lazy.Char8 as BL (pack, unpack)
import           Data.Functor (void)
-- import           Data.List (isPrefixOf, drop)
-- import qualified Data.Map as M
import qualified Data.Text as T
-- import           Data.Text (Text, pack)
-- import qualified Data.Text.IO as T
-- import           Data.Time
-- import           Data.Time.Clock.POSIX
-- import           GHC.Generics (Generic)
import           Network.Wreq hiding (defaults)
import qualified Network.Wreq as W(defaults)
-- import Network.HTTP.Conduit(HttpException)
-- import           Network.XmlRpc.Client
import qualified Nightwatch.DBTypes as DB (message, chatId, User(..), authenticateChat)
import           Nightwatch.DBTypes hiding (message, chatId, User(..))
import           Nightwatch.TelegramTypes
-- import qualified Nightwatch.Types as Ty(message)
import           Nightwatch.Types hiding (message)
-- import           System.IO.Error
import           System.Process (proc, createProcess, getProcessExitCode, ProcessHandle, waitForProcess)
-- import           Text.Read (readMaybe)
import           Text.Regex.Posix
import qualified Nightwatch.Aria2 as A2
import           Safe (fromJustNote)
import           Text.Printf
import           Data.Char (toUpper)
import           Database.Persist.Sql (transactionSave)
import           Data.List (foldl')

type Resp = Response TelegramResponse

-- botToken = "151105940:AAEUZbx4_c9qSbZ5mPN3usjXVwGZzj-JtmI"
apiBaseUrl :: NwConfig -> String
apiBaseUrl nwConfig = "https://api.telegram.org/bot" ++ (nwConfig ^. tgramBotToken)

ariaRPCPort = 9999
ariaRPCHost = "localhost"
ariaRPCUrl = printf "http://%s:%d/jsonrpc" ariaRPCHost ariaRPCPort
-- aria2Command = 
-- aria2DownloadDir = "./downloads"
-- aria2Args = ["--enable-rpc=true", "--rpc-listen-port=" ++ (show ariaRPCPort), "--rpc-listen-all=false", "--dir=" ++ aria2DownloadDir]


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

getUpdates :: (Num a, Show a) => NwConfig -> Maybe a -> IO (Response BL.ByteString)
getUpdates nwConfig Nothing = getUpdates nwConfig (Just 0)
getUpdates nwConfig (Just offset) = do
--  putStrLn (apiBaseUrl ++ "/getUpdates?offset=" ++ (show offset))
  get $ (apiBaseUrl nwConfig) ++ "/getUpdates?offset=" ++ (show offset)

-- getUpdatesAsJSON offset = do
--  asJSON =<< (getUpdates nwConfig offset) :: IO Resp

sendMessage :: NwConfig -> TelegramOutgoingMessage -> IO ()
sendMessage nwConfig tgMsg = do
  putStrLn $ "Sending to " ++ (show tgMsg)
  (void (post ((apiBaseUrl nwConfig) ++ "/sendMessage") ["chat_id" := (tg_chat_id tgMsg), "text" := (DB.message tgMsg)])) `catch` (\e -> putStrLn $ "ERROR in sending to " ++ (show $ tg_chat_id tgMsg) ++ ": " ++ (show (e :: Control.Exception.SomeException)))
    

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
findLastUpdateId lastUpdateId updates = (1+) $ foldl' (\m update -> if (update_id update) > m then (update_id update) else m) lastUpdateId updates

doPollLoop nwConfig tgIncomingChan lastUpdateId = do
  threadDelay (10^6)
  r <- asJSON =<< (getUpdates nwConfig (Just lastUpdateId)) :: IO Resp
  let incomingUpdates = (result $ r ^. responseBody)
  putStrLn $ "Will process " ++ (show incomingUpdates)
  writeList2Chan tgIncomingChan incomingUpdates
  doPollLoop nwConfig tgIncomingChan =<< setLastUpdateId (findLastUpdateId lastUpdateId incomingUpdates)

processIncomingMessages :: NwConfig -> Chan Update -> IO ()
processIncomingMessages nwConfig tgIncomingChan = forever $ do
  let pool = nwConfig ^. dbPool
  update <- readChan tgIncomingChan
  let msg = message update
  let chatId = chat_id $ chat $ msg
  user <- runDb pool $ DB.authenticateChat chatId
  case user of
    Nothing -> void $ forkIO $ runDb pool $ oAuthProcess nwConfig chatId (user_id $ from $ msg) (user_username $ from $ msg)
    Just userE -> do
      let nwCmd = parseIncomingMessage (text msg)
          logTgram :: NwApp (Entity Log)
          logTgram = (logIncomingTelegramMessage msg  (Just $ entityKey userE) (Just nwCmd))
      void $ forkIO $ runDb pool $ case nwCmd of
        (DownloadCommand url) -> logTgram >>= (\logE -> onDownloadCommand nwConfig userE logE url)
        (StatusCommand gid) -> logTgram >>= (\logE -> onStatusCommand nwConfig userE logE gid)
        (PauseCommand gid) -> logTgram >>= (\logE -> onPauseCommand nwConfig userE logE gid)
        _ -> liftIO $ sendMessage nwConfig $ TelegramOutgoingMessage {tg_chat_id=(chat_id $ chat $ message update), DB.message=(TgramMsgText "What language, dost thou speaketh? Command me with: download <url>")}

oAuthProcess :: NwConfig -> TgramChatId -> TgramUserId -> Maybe TgramUsername -> NwApp ()
oAuthProcess nwConfig chatId tgramUserId tgramUsername = do
  (accessToken, refreshToken, e, d, n) <- liftIO oAuthProcess_
  case (e, d) of
    (Just email, Just "vacationlabs.com") -> do
      createUser email accessToken refreshToken n (Just tgramUserId) tgramUsername (Just chatId)
      liftIO $ sendMsg_ $ "Welcome " ++ email
    (Just email, _) -> liftIO $ sendMsg_ "Sorry, can't let you in. Doesn't look like you authenticated yourself with a VL email ID."
    (Nothing, _) -> liftIO $ sendMsg_ "Whoops! Something's gone wrong. I didn't get access to your email ID after authentication."
    (_, _) -> liftIO $ sendMsg_ "Whoops! Something's wrong. An expected field from the authentication response is missing."
      
  where
    sendMsg_ :: String -> IO ()
    sendMsg_ msg = sendMessage nwConfig TelegramOutgoingMessage{tg_chat_id=chatId, DB.message=TgramMsgText msg}
    
    oAuthProcess_ :: IO (OAuthAccessToken, OAuthRefreshToken, Maybe String, Maybe String, Maybe String)
    oAuthProcess_ = do 
      codeResp <- generateOAuthUserCode nwConfig
      let msg = (printf
                 "Cannot process command because you have not authenticated yourself.\n===\nPlease visit %s, signin with your VL account and enter the code: %s\n---\nGo ahead, I'll wait for another %d minutes for you to complete this."
                 (codeResp ^. verificationUrl)
                 (codeResp ^. userCode)
                 (div (codeResp ^. expiresIn) 60))
      sendMessage nwConfig TelegramOutgoingMessage{tg_chat_id=chatId, DB.message=TgramMsgText msg}
      (accessToken, refreshToken) <- pollForOAuthTokens nwConfig codeResp
      resp <- asValue =<< getWith (W.defaults & header "Authorization" .~ [BS.pack $ "Bearer " ++ accessToken]) "https://www.googleapis.com/plus/v1/people/me"
      let body = resp ^. responseBody
          domain = body ^? (key "domain") . _String
          name = body ^? (key "displayName") . _String
          -- TODO: This might not get the right email
          email = body ^? (key "emails") . (nth 0) . (key "value") . _String
      return (accessToken, refreshToken, fmap T.unpack email, fmap T.unpack domain, fmap T.unpack name)

onPauseCommand :: NwConfig -> Entity DB.User -> Entity Log -> Aria2Gid -> NwApp ()
onPauseCommand nwConfig userE logE gid = do
  let (Aria2Gid gidS) = gid
      logId = entityKey logE
      log = entityVal logE
      userId = entityKey userE
      user = entityVal userE
      chatId = (fromJustNote "Expecting Log to have telegram chat ID" $ logTgramChatId log)
  -- NOTE: Not bothered about the respone of the pause command. It's not
  -- interesting.
  _ <- liftIO $ A2.pause ariaRPCUrl gid
  onStatusCommand nwConfig userE logE gid

onStatusCommand :: NwConfig -> Entity DB.User -> Entity Log -> Aria2Gid -> NwApp ()
onStatusCommand nwConfig userE logE gid = do
  dloadE <- fmap (fromJustNote $ "Could not find download by GID in DB " ++ (show gid)) (fetchDownloadByGid gid)
  let (Aria2Gid gidS) = gid
      logId = entityKey logE
      log = entityVal logE
      userId = entityKey userE
      user = entityVal userE
      chatId = (fromJustNote "Expecting Log to have telegram chat ID" $ logTgramChatId log)
      -- url = downloadUrl $ entityVal dloadE
  statusResponse <- liftIO $ A2.tellStatus ariaRPCUrl gid
  updateWithAria2Response logId (show statusResponse)
  logAndSendTgramMessage nwConfig logId TelegramOutgoingMessage{tg_chat_id=chatId, DB.message=(TgramMsgText $ humanizeStatusResponse statusResponse)}


onDownloadCommand :: NwConfig -> Entity DB.User -> Entity Log -> URL -> NwApp ()
onDownloadCommand nwConfig userE logE url = do
  let (URL urlS) = url
      logId = entityKey logE
      log = entityVal logE
      userId = entityKey userE
      user = entityVal userE
      chatId = (fromJustNote "Expecting Log to have telegram chat ID" $ logTgramChatId log)
  gid <- liftIO $ A2.addUri ariaRPCUrl urlS
  let (Aria2Gid gidS) = gid
  updateWithAria2Response logId gidS
  dloadE <- createDownload gid logId userId [] Nothing
  transactionSave
  (statusResponse, _, _, _, _) <- statusHelper_ gid
  _ <- updateDownloadWithFiles (entityKey dloadE) $ fileHelper_ (A2.st_files statusResponse)
  transactionSave
  logAndSendTgramMessage nwConfig logId TelegramOutgoingMessage{tg_chat_id=chatId, DB.message=(TgramMsgText $ "Download queued. ID=" ++ gidS ++ ". You can use that to check the status, pause, or stop the download. eg. status " ++ gidS)}


fileHelper_ :: [A2.GetFilesResponse] -> [(String, Integer, [URL])]
fileHelper_ fileResponse = map (\f -> (A2.gf_path f, A2.gf_length f, map A2.gu_uri $ A2.gf_uris f)) fileResponse

logAndSendTgramMessage :: NwConfig -> LogId -> TelegramOutgoingMessage -> NwApp()
logAndSendTgramMessage nwConfig logId msg = do
  updateWithTgramOutgoingMsg logId msg
  liftIO $ sendMessage nwConfig msg


humanizeBytes :: Integer -> String
humanizeBytes b
  | b < 1024 = printf "%d bytes" b
  | b <= 1024*1024 = printf "%.2f KB" (((fromIntegral b) / (1024))::Float)
  | b <= 1024*1024*1024 = printf "%.2f MB" (((fromIntegral b) / (1024*1024))::Float)
  | otherwise = printf "%.2f GB" (((fromIntegral b) / (1024*1024*1024))::Float)

humanizeStatusResponse :: A2.StatusResponse -> String
humanizeStatusResponse res = (printf "%s | %s | %s%s\n===\n%s"
                              gidS
                              statusUpcase
                              downloadedStringFragment
                              etaStringFragment
                              (joinStrings "\n" $ map downloadFileSummary (A2.st_files res)))
  where
    percentDownloaded_ :: Float
    percentDownloaded_ = (percentDownloaded (A2.st_completedLength res) (A2.st_totalLength res))
    statusUpcase = (map toUpper (A2.st_status res))
    downloadSpeed = A2.st_downloadSpeed res
    remainingLength = ((A2.st_totalLength res) - (A2.st_completedLength res))
    etaSeconds :: Integer 
    etaSeconds = remainingLength `div` downloadSpeed
    (Aria2Gid gidS) = (A2.st_gid res)
    downloadedStringFragment :: String
    downloadedStringFragment = if percentDownloaded_ == (fromIntegral 100) then
                                 "Download complete"
                               else
                                 (printf "%.2f%% downloaded" percentDownloaded_)
    etaStringFragment :: String
    etaStringFragment = if percentDownloaded_ == (fromIntegral 100) then
                          ""
                        else
                          (printf "\n===\n%s to go (%s at %s/s)..."
                           (downloadEta downloadSpeed etaSeconds)
                           (humanizeBytes remainingLength)
                           (humanizeBytes downloadSpeed))

downloadEta :: Integer -> Integer -> String
downloadEta downloadSpeed etaSeconds
  | downloadSpeed < 1 = "A long time"
  | etaSeconds < 1*60 = "Under a minute"
  | otherwise = joinStrings ", " (map (\(cnt, str) -> (show cnt) ++ " " ++ str) (take 2 x))
  where
    (tMin, sec) = divMod etaSeconds 60
    (tHr, min) = if tMin==0 then (0, 0) else divMod tMin 60
    (tDays, hr) = if tHr==0 then (0, 0) else divMod tHr 24
    (weeks, days) = if tDays==0 then (0, 0) else divMod tDays 7
    x = filter (\(cnt, _) -> cnt>0) [(weeks, "weeks"), (days, "days"), (hr, "hours"), (min, "minutes")]

downloadFileSummary :: A2.GetFilesResponse -> String
downloadFileSummary res = (printf "[%.2f%%] %s"
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
  writeFile "/Users/saurabhnanda/projects/nightwatch/xslast-update-id" (show updateId)
  return updateId

getLastUpdateId = do
  x <- readFile "./last-update-id"
  case (reads x :: [(Integer, String)]) of
    [] -> return 0 -- Probably not a good idea
    [(y, s)] -> return y

startAria2_ nwConfig = do
  putStrLn "==> Starting Aria2"
  let aria2Args = ["--enable-rpc=true", "--rpc-listen-port=" ++ (show ariaRPCPort), "--rpc-listen-all=false", "--dir=" ++ (nwConfig ^. aria2DownloadDir)] 
  (_, _, _, processHandle) <- createProcess $ proc (nwConfig ^. aria2Command) aria2Args
  return processHandle

ensureAria2Running :: NwConfig -> IO a
ensureAria2Running nwConfig = forever $ do
  ph <- startAria2_ nwConfig
  exitCode <- waitForProcess ph
  putStrLn $ "ERROR: Aria2 process died mysteriously: " ++ (show exitCode)

processOutgoingMessages :: NwConfig -> IO ()
processOutgoingMessages nwConfig = do
  let tgOutChan = nwConfig ^. tgramOutgoingChannel
  tgMsg <- readChan tgOutChan
  sendMessage nwConfig tgMsg
  processOutgoingMessages nwConfig

-- sendDownloadLinkToUser :: NwConfig -> A2.StatusResponse -> DB.User -> Download -> IO ()
-- sendDownloadLinkToUser nwConfig statusResponse user dload = do
--   let tgramChatId = fromJustNote "Cannot notify user because he/she doesn't have a tgramChatId" $ DB.userTgramChatId user
--       msg = TgramMsgText (printf "%s\n===\nDownload it from: %s"
--                           (humanizeStatusResponse statusResponse)
--                           _fetchDownloadLink)
--   sendMessage nwConfig TelegramOutgoingMessage{tg_chat_id=tgramChatId, DB.message=msg}

onDownloadComplete :: NwConfig -> Aria2Gid -> IO ()
onDownloadComplete nwConfig gid = do
  let pool = nwConfig ^. dbPool
      sendMsg = sendMessage nwConfig

  -- First, let's check what this GID was all about, read from the DB, and make
  -- a tellStatus call to check what were the results of this download (we're
  -- interested in seeing if it resulted in more downloads being queued)
  (statusResponse, userId, user, dloadId, dload) <- runDb pool $ statusHelper_ gid
  let tgramChatId = fromJustNote "Cannot notify user because he/she doesn't have a tgramChatId" $ DB.userTgramChatId user
      (Aria2Gid gidS) = gid

  -- Notiy the user that this download has been completed
  sendMessage nwConfig TelegramOutgoingMessage{tg_chat_id=tgramChatId, DB.message=(TgramMsgText $ humanizeStatusResponse statusResponse)}
  case (A2.st_followedBy statusResponse) of
    Nothing -> return ()
    Just [] -> return ()
    Just gids -> do

      -- Now, make tellStatus calls on any downloads that have been created as a
      -- result of this download being finished (happens mostly with torrent
      -- files)
      statusResponses <- A.mapConcurrently (A2.tellStatus ariaRPCUrl) gids
      newDloadsE <- runDb pool $ mapM (saveDownload userId dloadId (downloadLogId dload)) statusResponses
      sendMsg TelegramOutgoingMessage{tg_chat_id=tgramChatId, DB.message=(TgramMsgText $ printf "%s | %d downloads queued" gidS (length newDloadsE))}
      void $ zipWithM (\sr idx -> sendMsg TelegramOutgoingMessage{tg_chat_id=tgramChatId, DB.message=(TgramMsgText $ printf "%d of %d\n===\n%s" idx (length statusResponses) (humanizeStatusResponse sr))}) statusResponses ([1..]::[Int])
    where
      saveDownload :: UserId -> DownloadId -> LogId -> A2.StatusResponse -> NwApp(Entity Download)
      saveDownload userId parentDloadId logId sr = do
        dloadE <- createDownload (A2.st_gid sr) logId userId [] (Just parentDloadId)
        _ <- updateDownloadWithFiles (entityKey dloadE) $ fileHelper_ (A2.st_files sr)
        return dloadE

onAria2Notification :: NwConfig -> Aria2Gid -> IO ()
onAria2Notification nwConfig gid = do
  (statusResponse, userId, user, dloadId, dload) <- runDb (nwConfig ^. dbPool) $ statusHelper_ gid
  sendMessage nwConfig TelegramOutgoingMessage{tg_chat_id=(fromJustNote "Cannot notify user because he/she doesn't have a tgramChatId" $ DB.userTgramChatId user), DB.message=(TgramMsgText $ humanizeStatusResponse statusResponse)}

statusHelper_ :: Aria2Gid -> NwApp (A2.StatusResponse, Key DB.User, DB.User, Key Download, Download)
statusHelper_ gid = do
  statusResponse <- liftIO $ A2.tellStatus ariaRPCUrl gid
  dloadE <- fmap (fromJustNote $ "Could not find GID " ++ (show gid) ++ " in DB") (fetchDownloadByGid gid)
  let dload = entityVal dloadE
      dloadId = entityKey dloadE
      userId = (downloadUserId dload)
  user <- fmap (fromJustNote $ "Could not find User ID " ++ (show userId) ++ " in DB") (fetchUserById userId)
  return (statusResponse, userId, user, dloadId, dload)

generateOAuthUserCode :: NwConfig -> IO OAuthCodeResponse
generateOAuthUserCode nwConfig = do
  r <- asJSON =<< post "https://accounts.google.com/o/oauth2/device/code" ["client_id" := (nwConfig ^. googleClientId), "scope" := ("email profile" :: T.Text)]
  return $ r ^. responseBody

pollForOAuthTokens :: NwConfig -> OAuthCodeResponse -> IO (OAuthAccessToken, OAuthRefreshToken)
pollForOAuthTokens nwConfig codeResponse = if (codeResponse ^. expiresIn) < 0 then
                                    error $ "OAuth token polling timed out " ++ (show codeResponse)
                                  else
                                    catch pollForOAuthTokens_ errorHandler 

  where
    errorHandler :: SomeException -> IO (OAuthAccessToken, OAuthRefreshToken)
    errorHandler e = do
      putStrLn $ "Error while polling OAuth tokens (probably non-HTTP related) "  ++ (show e)
      retryPoll

    pollForOAuthTokens_ :: IO (OAuthAccessToken, OAuthRefreshToken)
    pollForOAuthTokens_ = do
      r <- (postWith
            -- NOTE: This is required so that Wreq doesn't throw it's stupid
            -- exception for non-200 responses.
            (W.defaults & checkStatus .~ (Just (\ _ _ _ -> Nothing))) 
            "https://www.googleapis.com/oauth2/v4/token"
            ["client_id" := (nwConfig ^. googleClientId),
             "client_secret" := (nwConfig ^. googleClientSecret),
             "code" := (codeResponse ^. deviceCode),
             "user_code" := (codeResponse ^. userCode),
             "verification_url" := (codeResponse ^. verificationUrl),
             "grant_type" := ("http://oauth.net/grant_type/device/1.0" :: T.Text)])
      case (r ^. responseStatus ^. statusCode) of
        200 -> do
          j <- asJSON r
          let pollResponse = (j ^. responseBody) :: OAuthTokenResponse
          return (pollResponse ^. accessToken, pollResponse ^. refreshToken)
  
        _ -> do
          j <- asJSON r :: IO (Response Value)
          let err = j ^. responseBody ^? (key "error")
          case err of
            Just "expired_token" -> error $ "OAuth token polling timed out " ++ (show codeResponse)
            Just "authorization_pending" -> return ()
            Just "slow_down" -> return ()
            _ -> putStrLn $ "Unexpected response while polling for OAuth tokens " ++ (show r)
          retryPoll

    retryPoll = do
      threadDelay $ 1000 * (codeResponse ^. interval)
      pollForOAuthTokens nwConfig (over expiresIn (\x -> x - (codeResponse ^. interval)) codeResponse)


startTelegramBot :: NwConfig -> IO ()
startTelegramBot nwConfig = do
  let pool = nwConfig ^. dbPool
  tgIncomingChan <- newChan
  _ <- forkIO $ forever $ logAllExceptions "Error in processIncomingMEssages: " (processIncomingMessages nwConfig tgIncomingChan)
  _ <- forkIO $ forever $ logAllExceptions "Error in doPollLoop: " (doPollLoop nwConfig tgIncomingChan =<< getLastUpdateId)
  _ <- forkIO $ A2.startWebsocketClient ariaRPCHost ariaRPCPort A2.defaultAria2Callbacks{
    A2.onDownloadStart=(onAria2Notification nwConfig),
    A2.onDownloadPause=(onAria2Notification nwConfig),
    A2.onDownloadComplete=(onDownloadComplete nwConfig),
    A2.onDownloadStop=(onAria2Notification nwConfig),
    A2.onDownloadError=(onAria2Notification nwConfig)
    }
  return ()

startAria2 :: NwConfig -> IO ()
startAria2 nwConfig = void $ forkIO $ forever $ ensureAria2Running nwConfig
