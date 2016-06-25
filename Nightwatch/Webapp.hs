{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Nightwatch.Webapp (startWebapp) where
import Yesod
import Yesod.Auth
import Yesod.Auth.GoogleEmail2
import Network.HTTP.Client.Conduit (Manager, newManager)
import qualified Data.Text as T
import Nightwatch.Types
import Nightwatch.DBTypes
import Control.Lens

data Nightwatch = Nightwatch {
  httpManager :: Manager,
  clientId :: T.Text,
  clientSecret :: T.Text
  }

mkYesod "Nightwatch" [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod Nightwatch

instance YesodAuth Nightwatch where
  type AuthId Nightwatch = T.Text
  getAuthId = return . Just . credsIdent
  loginDest _ = HomeR
  logoutDest _ = HomeR
  authPlugins app = [
    authGoogleEmail (clientId app) (clientSecret app)
    ]
  authHttpManager = httpManager
  maybeAuthId = lookupSession "_ID"

instance RenderMessage Nightwatch FormMessage where
  renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
                                 Hello World!
                                 <a href=@{AuthR LoginR}>Go to the login page
|]

startWebapp :: NwConfig -> IO ()
startWebapp nwConfig = do
  mgr <- newManager
  warp 3000 Nightwatch{
    httpManager=mgr,
    clientId=(nwConfig ^. googleClientId),
    clientSecret = (nwConfig ^. googleClientSecret)
    }


-- https://accounts.google.com/o/oauth2/auth?scope=email%20profile&state=rSFE1az77Up4gmD156336Vxz&redirect_uri=%2Fauth%2Fpage%2Fgoogleemail2%2Fcomplete&response_type=code&client_id=1045667944271-dnh31h9n4ul2i0q42tjirc7n7tk7k9jq.apps.googleusercontent.com&access_type=offline

