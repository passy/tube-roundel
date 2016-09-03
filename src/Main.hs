{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude ()
import Prelude.Compat
import Servant

import Network.Wai (Application)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (runEnv)
import GHC.Generics (Generic)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Template as TT
import qualified Data.Char as Char

newtype ColorHex = ColorHex T.Text
  deriving (Eq, Show, Generic)

instance FromHttpApiData ColorHex where
  parseUrlPiece param =
    let digits = T.takeWhile Char.isHexDigit $ T.take 6 param
    in if T.null digits
      then Left "Invalid color provided. Only hex digits allowed."
      else Right $ ColorHex $ T.toLower digits

-- SVG/PNG Mimetypes?
type API = "roundel" :> "no-text" :> Capture "color" ColorHex :> Get '[OctetStream] BS.ByteString

api :: Proxy API
api = Proxy

roundelHandler :: ColorHex -> Handler BS.ByteString
roundelHandler (ColorHex color) = do
  tmpl <- liftIO $ TIO.readFile "res/roundel-no-text.svg.tpl"
  -- TODO: Avoid partial function.
  let context "colorHex" = color
  -- TODO: Consider using templateSafe.
  return $ BSL.toStrict . TL.encodeUtf8 $ TT.substitute tmpl context

server :: Server API
server = roundelHandler

app :: Application
app = serve api server

main :: IO ()
main = runEnv 8080 app
