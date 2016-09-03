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
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (runEnv)
import qualified Data.ByteString as BS

-- TODO: Text?
type API = "roundel" :> Capture "filename" String :> Get '[OctetStream] BS.ByteString

api :: Proxy API
api = Proxy

roundelHandler :: String -> Handler BS.ByteString
roundelHandler name = do
  file <- liftIO $ BS.readFile "res/roundel-no-text.svg.tpl"
  return file

server :: Server API
server = roundelHandler

app :: Application
app = serve api server

main :: IO ()
main = runEnv 8080 app
