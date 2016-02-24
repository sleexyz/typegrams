{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import Data.Aeson.Types
import Data.Aeson
import Servant
import GHC.Generics
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class

import Interp (getType, getType', TypeSig)




type Cool = String

type TypingAPI = "dumb" :> QueryParam "src" String :> Get '[JSON] Cool
                 :<|> "api" :> QueryParam "src" String :> Get '[JSON] (Maybe TypeSig)

typingApi :: Proxy TypingAPI
typingApi = Proxy

-- data Cool = Cool
--             { input :: String
--             , output :: String
--             } deriving (Eq, Show, Generic)


dumb :: MonadIO m1 => Maybe String -> m1 String
dumb Nothing = return "error!"
dumb (Just str) = do
  poop <- liftIO . getType $ str
  return $ f poop where
    f (Left err) = show err
    f (Right r) = r

api :: (MonadIO m) => Maybe String -> m (Maybe TypeSig)
api Nothing = return Nothing
api (Just str) = do
  poop <- liftIO . getType' $ str
  return $ f poop where
    f (Left err) = Nothing
    f (Right r) = Just r

server :: Server TypingAPI
server  = dumb :<|> api


test :: Application
test = serve typingApi server


main :: IO ()
main = run 8080 test
