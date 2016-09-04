{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude ()
import Prelude.Compat
import Servant

import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Network.HTTP.Media ((//))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (runEnv)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Template as TT
import qualified Data.Char as Char

--
-- ## Custom Types ##
--
newtype ColorHex =
  ColorHex T.Text
  deriving (Eq, Show, Generic)

instance FromHttpApiData ColorHex where
  parseUrlPiece param =
    let digits = T.takeWhile Char.isHexDigit $ T.take 6 param
    in if T.null digits
         then Left "Invalid color provided. Only hex digits allowed."
         else Right $ ColorHex $ T.toLower digits

data SVG

instance Accept SVG where
  contentType _ = "image" // "svg+xml"

instance MimeRender SVG TL.Text where
  mimeRender _ = TL.encodeUtf8

--
-- ## API ##
--
type API = "roundel" :> "no-text" :> Capture "color" ColorHex :> "image.svg" :> Get '[SVG] TL.Text

api :: Proxy API
api = Proxy

roundelHandler :: ColorHex -> Handler TL.Text
roundelHandler (ColorHex color) = do
  tmpl <- liftIO $ TIO.readFile "res/roundel-no-text.svg.tpl"
  -- TODO: Avoid partial function.
  let context "colorHex" = color
  -- TODO: Consider using templateSafe.
  return $ TT.substitute tmpl context

server :: Server API
server = roundelHandler

app :: Application
app = serve api server

main :: IO ()
main = runEnv 8080 app
