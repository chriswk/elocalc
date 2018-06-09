{-# LANGUAGE OverloadedStrings #-}

module Init where

import           Api                         (app)
import           Api.User                    (generateJavaScript)
import           Config                      (Config (..), Environment (..),
                                              makePool, setLogger)
import           Control.Concurrent          (killThread)
import           Control.Exception           (bracket)
import qualified Control.Monad.Metrics       as M
import qualified Data.Pool                   as Pool
import           Database.Persist.Postgresql (runSqlPool)
import qualified Katip
import           Lens.Micro                  ((^.))
import           Logger                      (defaultLogEnv)
import           Models                      (doMigrations)
import           Network.Wai                 (Application)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Metrics         (metrics, registerWaiMetrics)
import           Safe                        (readMay)
import           System.Environment          (lookupEnv)
import           System.Remote.Monitoring    (forkServer, serverMetricStore,
                                              serverThreadId)

runApp :: IO ()
runApp = bracket acquireConfig shutdownApp runApp
  where
    runApp config = run (configPort config) =<< initialize config

initialize :: Config -> IO Application
initialize cfg = do
  waiMetrics <- registerWaiMetrics (configMetrics cfg ^. M.metricsStore)
  let logger = setLogger (configEnv cfg)
  runSqlPool doMigrations (configPool cfg)
  generateJavaScript
  pure . logger . metrics waiMetrics . app $ cfg

acquireConfig :: IO Config
acquireConfig = do
  port <- lookupSetting "PORT" 8081
  metricsPort <- lookupSetting "METRICS" 8000
  env <- lookupSetting "ENV" Development
  logEnv <- defaultLogEnv
  pool <- makePool env logEnv
  ekgServer <- forkServer "localhost" metricsPort
  let store = serverMetricStore ekgServer
  _ <- registerWaiMetrics store
  metr <- M.initializeWith store
  pure Config { configPool = pool
              , configEnv = env
              , configMetrics = metr
              , configLogEnv = logEnv
              , configPort = port
              , configEkgServer = serverThreadId ekgServer
              }

shutdownApp :: Config -> IO ()
shutdownApp cfg = do
  _ <- Katip.closeScribes (configLogEnv cfg)
  Pool.destroyAllResources (configPool cfg)
  killThread (configEkgServer cfg)
  pure ()

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeVal <- lookupEnv env
    case maybeVal of
      Nothing  -> return def
      Just str -> maybe (handleFailedRead str) return (readMay str)
    where handleFailedRead str = error $ mconcat
              [ "Failed to read [[", str, "]] for environment variable ", env]
