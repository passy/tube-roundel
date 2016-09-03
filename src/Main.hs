{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude ()
import Prelude.Compat
import Servant

import Debug.Trace.Compat

import Network.Wai (Application)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (runEnv)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Template as TT

-- SVG/PNG Mimetypes?
type API = "roundel" :> "no-text" :> Capture "color" T.Text :> Get '[OctetStream] BS.ByteString

api :: Proxy API
api = Proxy

roundelHandler :: T.Text -> Handler BS.ByteString
roundelHandler color = do
  -- TODO: Validate color. No joke, forget this and we're in big trouble, bro.
  tmpl <- liftIO $ TIO.readFile "res/roundel-no-text.svg.tpl"
  -- TODO: Avoid partial function.
  let context "colorHex" = color
  -- TODO: Consider using templateSafe.
  let res = BSL.toStrict . TL.encodeUtf8 $ TT.substitute tmpl context
  return $ traceShowId res

server :: Server API
server = roundelHandler

app :: Application
app = serve api server

main :: IO ()
main = runEnv 8080 app
