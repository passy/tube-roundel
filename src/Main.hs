{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude ()
import Prelude.Compat
import Servant
import qualified Data.Text as T

import Network.Wai (Application)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (runEnv)
import qualified Data.ByteString as BS

-- TODO: Text?
type API = "roundel" :> Capture "filename" T.Text :> Get '[OctetStream] BS.ByteString

api :: Proxy API
api = Proxy

roundelHandler :: T.Text -> Handler BS.ByteString
roundelHandler name =
  liftIO $ BS.readFile "res/roundel-no-text.svg.tpl"

server :: Server API
server = roundelHandler

app :: Application
app = serve api server

main :: IO ()
main = runEnv 8080 app
