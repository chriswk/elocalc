{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.User where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Aeson.Encode.Pretty    (encodePretty)
import qualified Data.ByteString.Lazy.Char8  as BL8
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant
import           Servant.JS                  (vanillaJS, writeJSForAPI)
import           Servant.Swagger

import           Config                      (App (..), Config (..))
import           Models

userApi :: Proxy UserAPI
userApi = Proxy

type UserAPI
   = "users" :> Get '[ JSON] [Entity User] :<|> "users" :> Capture "name" String :> Get '[ JSON] (Entity User) :<|> "users" :> ReqBody '[ JSON] User :> Post '[ JSON] Int64

userServer :: ServerT UserAPI App
userServer = allUsers :<|> singleUser :<|> createUser

allUsers :: App [Entity User]
allUsers = runDb (selectList [] [])

singleUser :: String -> App (Entity User)
singleUser str = do
  mayUs <- runDb (selectFirst [UserName ==. str] [])
  case mayUs of
    Nothing     -> throwError err404
    Just person -> return person

createUser :: User -> App Int64
createUser p = do
  newUser <- runDb (insert (User (userName p) (userEmail p)))
  return $ fromSqlKey newUser

generateJavaScript :: IO ()
generateJavaScript =
  writeJSForAPI (Proxy :: Proxy UserAPI) vanillaJS "./assets/api.js"

userSwagger :: Swagger
userSwagger =
  toSwagger userApi & info . title .~ "Elo Rank API" & info . version .~ "1.0" &
  info .
  description ?~
  "Rank your games" &
  info .
  license ?~
  ("MIT" & url ?~ URL "http://mit.com")

writeSwaggerJSON :: IO ()
writeSwaggerJSON =
  BL8.writeFile "./assets/swagger.json" (encodePretty userSwagger)
