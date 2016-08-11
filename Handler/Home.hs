module Handler.Home where

import Import hiding (downloadStatus)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Text.Julius (RawJS (..))
import Nightwatch.Types
import Nightwatch.DBTypes hiding (downloadStatus)
import qualified Database.Esqueleto as E
import Data.Default
import Control.Lens
import Text.Blaze(toMarkup, Markup)

data DownloadFilter = DownloadFilter {
  _whereClause :: E.SqlExpr (E.Value Bool)
  ,_userIds :: [UserId]
  ,_downloadStatus :: [DownloadStatus]
  ,_limitOffset :: Maybe (Int, Int)
  ,_orderBy :: [E.SqlExpr E.OrderBy]
  }

$(makeLenses ''DownloadFilter)

instance Default DownloadFilter where
  def = DownloadFilter{
    _whereClause=(E.val True)
    ,_userIds=[]
    ,_downloadStatus=[]
    ,_limitOffset=Nothing
    ,_orderBy=[]
    }

fetchDownloadTrees :: DownloadFilter -> NwApp [(Download, [(File, [Url])])]
fetchDownloadTrees f = undefined

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
  let downloadsOfUser = (def :: DownloadFilter) & userIds .~ [uId]
  completeDownloads:incompleteDownloads:[] <- mapM (runDB . liftPersist) $
    [fetchDownloadTrees (downloadsOfUser & downloadStatus .~ [DownloadComplete ChildrenNone, DownloadComplete ChildrenComplete]) 
    ,fetchDownloadTrees (downloadsOfUser & downloadStatus .~ [DownloadIncomplete, DownloadComplete ChildrenIncomplete])
    ]

  defaultLayout $(widgetFile "index")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

humanizeUrls :: [Url] -> Markup
humanizeUrls urls
  | (length urls == 0) = ""
  | (length urls == 1) = toMarkup $ "<div class='muted'>" ++ (toMarkup $ firstUrl ^. url) ++  "</div>"
  | (length urls >1) = toMarkup $ "<div class='muted'>" ++ (toMarkup $ firstUrl ^. url) ++ " and " ++ (toMarkup $ length urls) ++ " more</div>"
  where
    firstUrl = unsafeHead urls
