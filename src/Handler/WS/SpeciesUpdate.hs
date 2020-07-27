module Handler.WS.SpeciesUpdate where

import           Data.Aeson       (encode)
import           Import
import           Yesod.WebSockets

-- * Constants

getSpeciesUpdateNoticesWS :: Handler TypedContent
getSpeciesUpdateNoticesWS = do
    chan <- appSpeciesUpdateChannel <$> getYesod
    source <- atomically $ dupTChan chan
    webSockets $ forever $ do
        v <- atomically $ readTChan source
        sendTextData (encode v)
    notFound
