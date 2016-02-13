{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Nightwatch.Webapp (startWebapp) where
import Yesod

data Nightwatch = Nightwatch

mkYesod "Nightwatch" [parseRoutes|
/ HomeR GET
|] 

instance Yesod Nightwatch

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

startWebapp :: IO ()
startWebapp = do
  warp 3000 Nightwatch
