{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config where

import           Control.Concurrent                   (ThreadId)
import           Control.Exception                    (throwIO)
import           Control.Monad.Except                 (ExceptT, MonadError)
import           Control.Monad.IO.Class
import           Control.Monad.Logger                 (MonadLogger (..))
import           Control.Monad.Metrics                (Metrics, MonadMetrics,
                                                       getMetrics)
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT, asks)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Char8                as BS
import           Data.Monoid                          ((<>))
import           Database.Persist.Postgresql          (ConnectionPool,
                                                       ConnectionString,
                                                       createPostgresqlPool)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (Port)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Servant                              (ServantErr)
import           System.Environment                   (lookupEnv)

import           Logger


-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT Config', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.

newtype AppT m a
    = AppT
    { runApp :: ReaderT Config (ExceptT ServantErr m) a
    } deriving
    ( Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr
    , MonadIO
    )

type App = AppT IO
-- | The Config for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data Config = Config
  { configPool      :: ConnectionPool
  , configEnv       :: Environment
  , configMetrics   :: Metrics
  , configEkgServer :: ThreadId
  , configLogEnv    :: LogEnv
  , configPort      :: Port
  }


instance Monad m => MonadMetrics (AppT m) where
  getMetrics = asks Config.configMetrics

instance MonadIO m => Katip (AppT m) where
  getLogEnv = asks configLogEnv
  localLogEnv = error "not implemented"

instance MonadIO m => MonadLogger (AppT m) where
  monadLoggerLog = adapt logMsg

instance MonadIO m => MonadLogger (KatipT m) where
  monadLoggerLog = adapt logMsg


data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)

-- | This returns a 'Middleware' based on the environment that we're in.
setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout

katipLogger :: LogEnv -> Middleware
katipLogger env app req respond = runKatipT env $ do
  logMsg "web" InfoS "todo: received some request"
  liftIO $ app req respond

-- | This function creates a 'ConnectionPool' for the given environment.
-- For 'Development' and 'Test' environments, we use a stock and highly
-- insecure connection string. The 'Production' environment acquires the
-- information from environment variables that are set by the keter
-- deployment application.
makePool :: Environment -> LogEnv -> IO ConnectionPool
makePool Test env =
    runKatipT env (createPostgresqlPool (connStr "-test") (envPool Test))
makePool Development env =
    runKatipT env $ createPostgresqlPool (connStr "") (envPool Development)
makePool Production env
    -- This function makes heavy use of the 'MaybeT' monad transformer, which
    -- might be confusing if you're not familiar with it. It allows us to
    -- combine the effects from 'IO' and the effect of 'Maybe' into a single
    -- "big effect", so that when we bind out of @MaybeT IO a@, we get an
    -- @a@. If we just had @IO (Maybe a)@, then binding out of the IO would
    -- give us a @Maybe a@, which would make the code quite a bit more
    -- verbose.
 = do
  pool <-
    runMaybeT $ do
      let keys = ["host=", "port=", "user=", "password=", "dbname="]
          envs = ["PGHOST", "PGPORT", "PGUSER", "PGPASS", "PGDATABASE"]
      envVars <- traverse (MaybeT . lookupEnv) envs
      let prodStr = BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> envVars
      lift $ runKatipT env $ createPostgresqlPool prodStr (envPool Production)
  case pool
        -- If we don't have a correct database configuration, we can't
        -- handle that in the program, so we throw an IO exception. This is
        -- one example where using an exception is preferable to 'Maybe' or
        -- 'Either'.
        of
    Nothing ->
      throwIO (userError "Database Configuration not present in environment.")
    Just a -> return a

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8

-- | A basic 'ConnectionString' for local/test development. Pass in either
-- @""@ for 'Development' or @"test"@ for 'Test'.
connStr :: BS.ByteString -> ConnectionString
connStr sfx =
  "host=localhost dbname=elocalc" <> sfx <>
  " user=elocalc password=elocalc port=5432"
