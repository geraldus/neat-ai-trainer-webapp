{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    -- * for tests
    , runPopulation
    ) where

import           Control.Concurrent                   (forkIO)
import           Control.Monad.Logger                 (liftLoc, runLoggingT)
import           Database.Persist.Postgresql          (createPostgresqlPool,
                                                       pgConnStr, pgPoolSize,
                                                       runSqlPool)
import           Genetics.Crossover                   (selectiveCrossover)
import           Genetics.Defaults                    (populationSize)
import           Genetics.Niches
import           Genetics.Type
import           Import
import           Language.Haskell.TH.Syntax           (qLocation)
import           Network.HTTP.Client.TLS              (getGlobalManager)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (Settings,
                                                       defaultSettings,
                                                       defaultShouldDisplayException,
                                                       getPort, runSettings,
                                                       setHost, setOnException,
                                                       setPort)
import           Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                                       IPAddrSource (..),
                                                       OutputFormat (..),
                                                       destination,
                                                       mkRequestLogger,
                                                       outputFormat)
import           Solve                                (runGen)
import           System.Log.FastLogger                (defaultBufSize,
                                                       newStdoutLoggerSet,
                                                       toLogStr)
import           Text.Pretty.Simple                   (pPrint)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import           Handler.Comment
import           Handler.Common
import           Handler.Home
import           Handler.Profile
import           Handler.WS.SpeciesUpdate

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- getGlobalManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)
    appSpeciesUpdateChannel <- newBroadcastTChanIO
    appTasks                <- newBroadcastTChanIO
    appLastPopulationResult <- newEmptyTMVarIO

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    _ <- forkIO $ aiPopulationRunner appLastPopulationResult appTasks appSpeciesUpdateChannel

    -- Return the foundation
    return $ mkFoundation pool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend Handler a -> IO a
db = handler . runDB


aiPopulationRunner :: TMVar AIPopulation -> TChan AIPopulation -> TChan Value -> IO ()
aiPopulationRunner mv readCh noticeCh = do
    hSetBuffering stdout LineBuffering
    ch <- atomically $ dupTChan readCh
    forever $ do
        p <- atomically $ readTChan ch
        scoredPop <- runPopulation noticeCh p
        let nodeMV      = nextNodeId p
            innovMV     = nextInnovationId p
            connInnovMV = connMap p
            aiIdMV      = nextAIIdMV p
            nicheMV     = nextNicheId p
        niches' <- selection nodeMV innovMV connInnovMV aiIdMV nicheMV (populationNiches scoredPop)
        atomically $ writeTChan noticeCh (object
            [ "size"    .= toJSON (length (concat (map nicheIndividuals niches')))
            , "fitness" .= toJSON (sum (map aiFitness (concatMap nicheIndividuals niches')))
            , "niches"  .= toJSON niches'
            ])
        atomically $ putTMVar mv (p { populationNiches = niches' })
        return ()
    return ()

runPopulation :: TChan Value -> AIPopulation -> IO AIPopulation
runPopulation noticeCh p = do
    scoredPop <- distributeFitness <$> runGen noticeCh p
    let scoredNiches = map reScoreNiche (populationNiches scoredPop)
    return $ scoredPop { populationNiches = scoredNiches }
  where

reScoreNiche :: Species -> Species
reScoreNiche n = n
    { nicheLastFitness      = lastFitness
    , nicheFitness          = f
    , nicheCorrectedFitness = f'
    , nicheStagnantGens     = s
    }
  where
    lastFitness = nicheFitness n
    f           = sum (map aiFitness (nicheIndividuals n))
    f'          = sum (map aiCorrectedFitness (nicheIndividuals n))
    stg         = nicheStagnantGens n
    s           = if lastFitness > f then stg + 1 else 0

selection :: MVar Int
          -> MVar Int
          -> MVar (Map (Int, Int) Int)
          -> MVar Int
          -> MVar Int
          -> [Species]
          -> IO [Species]
selection nodeMV innovMV connInnovMV aiIdMV nicheMV scoredNiches = do
    ais <- concat <$> mapM sel (zip scoredNiches [0..])
    ais' <- if length ais < fromIntegral populationSize
        then do
            selectiveCrossover' (length ais)
        else return ais
    recreateNiches nicheMV ais' scoredNiches
  where
    sel (n, i) = do
        let fPop = sum $ map
                (\n' ->
                    (/) (nicheCorrectedFitness n')
                        (fromIntegral (length (nicheIndividuals n'))))
                scoredNiches
        let (nInit, nTail) = splitAt i scoredNiches
            otherSpecies =
                map genome . concatMap nicheIndividuals $ nInit ++ drop 1 nTail
        crossoverNiche nodeMV innovMV connInnovMV aiIdMV fPop otherSpecies n

    selectiveCrossover' d = do
        let all' = map genome $ concat (map nicheIndividuals scoredNiches)
        gs <- selectiveCrossover (fromIntegral populationSize - d) all' all'
        ids <- mapM mkIndividual gs
        return ids

    mkIndividual g = do
        aiId' <- takeMVar aiIdMV
        putMVar aiIdMV (aiId' + 1)
        return $ IndividualAI
            { aiId = aiId'
            , aiFitness = 0
            , aiCorrectedFitness = 0
            , genome = g
            }
