module Handler.Home where

import Import hiding (downloadStatus)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Text.Julius (RawJS (..))
import Nightwatch.Types
import Nightwatch.DBTypes hiding (downloadStatus)
import qualified Database.Esqueleto as E
import Data.Default
import Control.Lens hiding ((<|))
import Text.Blaze(toMarkup, Markup)
import qualified JsonTypes as J
import qualified Database.Esqueleto as E
import qualified Data.Map as Map
import Data.List.NonEmpty

-- data DownloadFilter = DownloadFilter {
--   _whereClause :: E.SqlExpr (E.Value Bool)
--   ,_userIds :: [UserId]
--   ,_downloadStatus :: [DownloadStatus]
--   ,_limitOffset :: Maybe (Int, Int)
--   ,_orderBy :: [E.SqlExpr E.OrderBy]
--   }

-- $(makeLenses ''DownloadFilter)

-- instance Default DownloadFilter where
--   def = DownloadFilter{
--     _whereClause=(E.val True)
--     ,_userIds=[]
--     ,_downloadStatus=[]
--     ,_limitOffset=Nothing
--     ,_orderBy=[]
--     }

-- fetchDownloadTrees :: DownloadFilter -> NwApp [(Download, [(File, [Url])])]
-- fetchDownloadTrees f = undefined

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  (Entity uId user) <- fmap (fromJustNote "Not expecting maybeAuth to result in Nothing")  maybeAuth
  -- let downloadsOfUser = (def :: DownloadFilter) & userIds .~ [uId]
  -- completeDownloads:incompleteDownloads:[] <- mapM (runDB . liftPersist) $
  --   [fetchDownloadTrees (downloadsOfUser & downloadStatus .~ [DownloadComplete ChildrenNone, DownloadComplete ChildrenComplete]) 
  --   ,fetchDownloadTrees (downloadsOfUser & downloadStatus .~ [DownloadIncomplete, DownloadComplete ChildrenIncomplete])
  --   ]

  defaultLayout $(widgetFile "index2")

-- postHomeR :: Handler Html
-- postHomeR = do
--     ((result, formWidget), formEnctype) <- runFormPost sampleForm
--     let handlerName = "postHomeR" :: Text
--         submission = case result of
--             FormSuccess res -> Just res
--             _ -> Nothing

--     defaultLayout $ do
--         let (commentFormId, commentTextareaId, commentListId) = commentIds
--         aDomId <- newIdent
--         setTitle "Welcome To Yesod!"
--         $(widgetFile "homepage")

-- sampleForm :: Form (FileInfo, Text)
-- sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
--     <$> fileAFormReq "Choose a file"
--     <*> areq textField (withSmallInput "What's on the file?") Nothing

-- commentIds :: (Text, Text, Text)
-- commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

-- humanizeUrls :: [Url] -> Markup
-- humanizeUrls urls
--   | (length urls == 0) = ""
--   | (length urls == 1) = toMarkup $ "<div class='muted'>" ++ (toMarkup $ firstUrl ^. url) ++  "</div>"
--   | (length urls >1) = toMarkup $ "<div class='muted'>" ++ (toMarkup $ firstUrl ^. url) ++ " and " ++ (toMarkup $ length urls) ++ " more</div>"
--   where
--     firstUrl = unsafeHead urls


nonempty :: a -> NonEmpty a
nonempty x = x :| []

getDownloadR :: Handler J.DownloadResponse
getDownloadR = do
  cd <- runDB $ E.select incompleteDownloads
  return $ J.DownloadResponse{downloads=mergeJson jsonMerger cd}
  where

    jsonMerger :: [J.Download] -> (Entity Download, Entity File, Entity Url) -> [J.Download]
    jsonMerger [] tuple = [tupleToJson tuple]
    jsonMerger jsons@(jsonHead:jsonTail) tuple@(d@(Entity did dval), f@(Entity fid fval), u@(Entity uid uval))
      -- The following should never happen, but just for the sake of completeness
      | sameDownload && sameFile && sameUrl = jsons
      | sameDownload && sameFile && (not sameUrl) = jsons & (ix 0 . J.files . ix 0 . J.urls) %~ (\us -> (urlEntityToJson u) <| us)
      | sameDownload && (not sameFile) && (not sameUrl) = jsons & (ix 0 . J.files) %~ (\fs -> (fileEntityToJson f $ nonempty $ urlEntityToJson u) <| fs)
      | (not sameDownload) && (not sameFile) && (not sameUrl) = (tupleToJson tuple):jsons
      where
        -- TODO: figure out how to avoid use of `headEx`
        j1 = jsonHead
        f1 = headEx (j1 ^. J.files)
        u1 = headEx (f1 ^. J.urls)
        sameDownload = (j1 ^. J.key) == did
        sameFile = f1 ^. J.key == fid
        sameUrl = u1 ^. J.key == uid

    tupleToJson :: (Entity Download, Entity File, Entity Url) -> J.Download
    tupleToJson (d, f, u) = downloadEntityToJson d (nonempty (fileEntityToJson f (nonempty (urlEntityToJson u))))

    -- tuplesToJson tuples = map
    --   (\(d, fs) -> downloadEntityToJson d $
    --     map (\(f, us) -> fileEntityToJson f $
    --           map urlEntityToJson us) fs)
    --   (nestedTuples_ tuples)

    nestedTuples_ :: (Ord a, Ord b) => [(a, b, c)] -> [(a, [(b, [c])])]
    nestedTuples_ tuples = nestedTuples
      -- extract the first-level-group's key from each tuple, i.e. Entity Download
      (\(d, _, _) -> d)
       -- extract the first-level-group's value from each tuple, i.e. (Entity File, Entity Url)
      (\(_, f, u) -> (f, u))
       -- Transform the first-level-group's value to a second-level-group
      (\tpl -> nestedTuples (view _1) (view _2) id tpl)
      tuples

    urlEntityToJson :: Entity Url -> J.Url
    urlEntityToJson (Entity uid u) = J.Url{
      urlKey=uid
      ,urlUrl=u ^. url
      }

    fileEntityToJson (Entity fid f) us = J.File{
      fileKey=fid
      ,fileFpath=f ^. fpath
      ,fileLen=f ^. len
      ,fileUrls=us
      }

    downloadEntityToJson (Entity dloadid d) fs = J.Download{
      downloadKey=dloadid
      ,downloadUserId=d ^. userId
      ,downloadGid=d ^. gid
      ,downloadStatus=d ^. status
      ,downloadCreatedAt=d ^. createdAt
      ,downloadUpdatedAt=d ^. updatedAt
      ,downloadFiles=fs
      }

    baseQuery = E.from $ \(dload `E.InnerJoin` file `E.InnerJoin` url) -> do
      E.on (file E.^. FileId E.==. url E.^. UrlFileId)
      E.on (dload E.^. DownloadId E.==. file E.^. FileDownloadId)
      E.orderBy [E.desc (dload E.^. DownloadUpdatedAt), E.asc (dload E.^. DownloadId), E.asc (file E.^. FileId), E.asc (url E.^. UrlId)]
      return (dload, file, url)

    completedDownloads = do
      (dload, file, query) <- baseQuery
      -- E.where_ $ (dload E.^. DownloadStatus) E.in_ (E.valList [DownloadComplete ChildrenNone, DownloadComplete ChildrenComplete])
      E.where_ $ (dload E.^. DownloadStatus E.==. E.val (DownloadComplete ChildrenNone)) E.||. (dload E.^. DownloadStatus E.==. E.val (DownloadComplete ChildrenComplete))
      return (dload, file, query)

    incompleteDownloads = do
      (dload, file, query) <- baseQuery
      E.where_ $ (dload E.^. DownloadStatus E.==. E.val (DownloadIncomplete)) E.||. (dload E.^. DownloadStatus E.==. E.val (DownloadComplete ChildrenIncomplete))
      return (dload, file, query)


-- nestedTuples :: Ord b => (a -> b) -> (a -> c) -> ([c] -> d) -> [a] -> [(b, d)]
-- nestedTuples f g h arr = Map.toList $
--                          Map.map h $
--                          foldl' (\m kv -> insertWith (++) (f kv) [g kv] m) Map.empty arr



-- type X = Map.Map DownloadId

-- tuplesToJson2 :: [(a, b)] -> ((a, b) -> c) -> [c]
-- tuplesToJson2 x y = undefined

mergeJson :: ([memo] -> json -> [memo]) -> [json] -> [memo]
mergeJson combinerFn jsons = foldl' combinerFn [] jsons
