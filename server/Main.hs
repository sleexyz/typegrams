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

import Interp (getType)




type Cool = String

type TypingAPI = "api" :> QueryParam "src" String :> Get '[JSON] Cool

typingApi :: Proxy TypingAPI
typingApi = Proxy

-- data Cool = Cool
--             { input :: String
--             , output :: String
--             } deriving (Eq, Show, Generic)



server :: Server TypingAPI
server Nothing = return "error!"
server (Just str) = do
  poop <- liftIO . getType $ str
  return $ f poop where
    f (Left err) = show err
    f (Right r) = r

test :: Application
test = serve typingApi server


main :: IO ()
main = run 8080 test
