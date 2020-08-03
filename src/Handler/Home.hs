{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Import
import           XOR    (createBasicPopulation)


getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")


postRunNewPopR :: Handler TypedContent
postRunNewPopR = do
    y  <- getYesod
    let ch = appTasks y
        st = appLastPopulationResult y
    pop <- atomically $ tryTakeTMVar st
    case pop of
        Nothing -> do
            task <- liftIO createBasicPopulation
            liftIO . atomically $ writeTChan ch task
        Just p  -> liftIO . atomically $ writeTChan ch p
    selectRep . provideRep . pure $ object ["status" .= ("ok" :: Text)]
