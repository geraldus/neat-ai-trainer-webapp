{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Import

import           Solve
import           Text.Pretty.Simple (pPrint)
import           Type.Genetics
import           XOR


getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")


postRunNewPopR :: Handler TypedContent
postRunNewPopR = do
    ch <- appTasks <$> getYesod
    pop <- liftIO $ createPopulation (fromIntegral populationSize)
    liftIO $ pPrint pop
    let ais = map
            (\(i, g) -> IndividualAI { aiId = i, aiFitness = 0.0, genome = g })
            (zip [1 .. fromIntegral populationSize] pop)
    let task = AIPopulation
            { populationNiches = [Species { individuals = ais }] }
    liftIO . atomically $ writeTChan ch task
    selectRep . provideRep . pure $ object ["status" .= ("ok" :: Text)]


isActiveOutput :: Node -> Bool
isActiveOutput (Node _ Output True) = True
isActiveOutput _                    = False
