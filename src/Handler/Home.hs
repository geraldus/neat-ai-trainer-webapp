{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Import
import           XOR    (createPopulation)


getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")


postRunNewPopR :: Handler TypedContent
postRunNewPopR = do
    ch <- appTasks <$> getYesod
    task <- liftIO createPopulation
    liftIO . atomically $ writeTChan ch task
    selectRep . provideRep . pure $ object ["status" .= ("ok" :: Text)]
