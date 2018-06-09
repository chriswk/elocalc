{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Api.User where

import           Config                      (AppT (..))
import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Logger        (logDebugNS)
import           Control.Monad.Metrics       (increment, metricsCounters)
import qualified Control.Monad.Metrics       as Metrics
import           Data.HashMap.Lazy           (HashMap)
import           Data.Int                    (Int64)
import           Data.IORef                  (readIORef)
import           Data.Text                   (Text)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Lens.Micro                  ((^.))
import           Models                      (User (User), runDb, userEmail,
                                              userName)
import           Servant
import           Servant.JS                  (vanillaJS, writeJSForAPI)

import qualified Models                      as Md
import qualified System.Metrics.Counter      as Counter

userApi :: Proxy UserAPI
userApi = Proxy


type UserAPI = "users" :> Get '[JSON] [Entity User]
  :<|> "users" :> Capture "name" Text :> Get '[JSON] (Entity User)
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
  :<|> "metrics" :> Get '[JSON] (HashMap Text Int64
                                )


userServer :: MonadIO m => ServerT UserAPI (AppT m)
userServer = allUsers :<|> singleUser :<|> createUser:<|> waiMetrics

allUsers :: MonadIO m => AppT m [Entity User]
allUsers = do
  increment "allUsers"
  logDebugNS "web" "allUsers"
  runDb (selectList [] [])

singleUser :: MonadIO m => Text -> AppT m (Entity User)
singleUser str = do
  increment "singleUser"
  logDebugNS "web" "singleUser"
  maybeUser <- runDb (selectFirst [Md.UserName ==. str] [])
  case maybeUser of
    Nothing ->
      throwError err404
    Just person ->
      return person


createUser :: MonadIO m => User -> AppT m Int64
createUser p = do
  increment "createUser"
  logDebugNS "web" "creating a user"
  newUser <- runDb (insert (User (userName p) (userEmail p)))
  return $ fromSqlKey newUser

waiMetrics :: MonadIO m => AppT m (HashMap Text Int64)
waiMetrics = do
  increment "metrics"
  logDebugNS "web" "metrics"
  metr <- Metrics.getMetrics
  liftIO $ mapM Counter.read =<< readIORef (metr ^. metricsCounters)

generateJavaScript :: IO ()
generateJavaScript = writeJSForAPI (Proxy :: Proxy UserAPI) vanillaJS "assets/api.js"
