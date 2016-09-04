{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude ()
import Prelude.Compat
import Servant

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Except (ExceptT, throwError, withExceptT)
import GHC.Generics (Generic)
import Graphics.Svg (parseSvgFile)
import Graphics.Rasterific.Svg
       (renderSvgDocument, loadCreateFontCache)
import Network.HTTP.Media ((//))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (runEnv)
import Data.Monoid ((<>))
import Codec.Picture.Types
       (Image, PixelRGBA8, DynamicImage(ImageRGBA8))
import Codec.Picture.Saving (imageToPng)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Template as TT
import qualified Data.Char as Char

--
-- ## Constants ##
--
pngDpi :: Int
pngDpi = 96

fontCacheDirectory :: FilePath
fontCacheDirectory = "fonty-texture-cache"

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

data PNG

instance Accept SVG where
  contentType _ = "image" // "svg+xml"

instance Accept PNG where
  contentType _ = "image" // "png"

instance MimeRender SVG TL.Text where
  mimeRender _ = TL.encodeUtf8

instance MimeRender PNG DynamicImage where
  mimeRender _ = imageToPng

--
-- ## API ##
--
type API = "roundel" :> "no-text" :> Capture "color" ColorHex :> "image.svg" :> Get '[SVG] TL.Text :<|> "roundel" :> "no-text" :> Capture "color" ColorHex :> "image.png" :> Get '[PNG] DynamicImage

api :: Proxy API
api = Proxy

renderRoundelTemplate
  :: MonadIO m
  => ColorHex -> ExceptT T.Text m TL.Text
renderRoundelTemplate (ColorHex color) = do
  tmpl <- liftIO $ TIO.readFile "res/roundel-no-text.svg.tpl"
  let context "colorHex" = color
      context v =
        error $ "Undefined template variable '" <> T.unpack v <> "' requested."
  -- TODO: Consider using templateSafe/substituteA
  return $ TT.substitute tmpl context

custom500 :: T.Text -> ServantErr
custom500 t =
  ServantErr
  { errHTTPCode = 500
  , errReasonPhrase = T.unpack t
  , errBody = ""
  , errHeaders = []
  }

with500
  :: MonadIO m
  => ExceptT T.Text m a -> ExceptT ServantErr m a
with500 = withExceptT custom500

roundelSvgHandler :: ColorHex -> Handler TL.Text
roundelSvgHandler = with500 . renderRoundelTemplate

roundelPngHandler :: ColorHex -> Handler DynamicImage
roundelPngHandler color = do
  tmpl <- with500 $ renderRoundelTemplate color
  img <- with500 . renderSvgToPng $ BSL.toStrict $ TL.encodeUtf8 tmpl
  return $ ImageRGBA8 img

renderSvgToPng
  :: MonadIO m
  => BS.ByteString -> ExceptT T.Text m (Image PixelRGBA8)
renderSvgToPng input =
  case parseSvgFile "/dev/null" input of
    Nothing -> throwError "Error while loading SVG"
    Just doc -> do
      cache <- liftIO $ loadCreateFontCache fontCacheDirectory
      (finalImage, _) <- liftIO $ renderSvgDocument cache Nothing 96 doc
      return finalImage

server :: Server API
server = roundelSvgHandler :<|> roundelPngHandler

app :: Application
app = serve api server

main :: IO ()
main = runEnv 8080 app
