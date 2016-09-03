{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude ()
import Prelude.Compat
import Servant

import Network.Wai (Application)
import Network.Wai.Handler.Warp (runEnv)

type StaticAPI = "static" :> Raw

staticAPI :: Proxy StaticAPI
staticAPI = Proxy

server :: Server StaticAPI
server = serveDirectory "res"

app :: Application
app = serve staticAPI server

main :: IO ()
main = runEnv 8080 app
