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

type TypingAPI = "api" :> Get '[JSON] Cool

-- data Cool = Cool
--             { input :: String
--             , output :: String
--             } deriving (Eq, Show, Generic)

typingApi :: Proxy TypingAPI
typingApi = Proxy

type Cool = String

server :: Server TypingAPI
server =  return "hello world"

test :: Application
test = serve typingApi server


main :: IO ()
main = run 8080 test
