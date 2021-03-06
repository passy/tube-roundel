{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude
import Servant

import Graphics.Svg (parseSvgFile)
import Graphics.Rasterific.Svg
       (renderSvgDocument, loadCreateFontCache)
import Network.HTTP.Media ((//))
import Network.Wai.Handler.Warp (runEnv)
import Codec.Picture.Types
       (Image, PixelRGBA8, DynamicImage(ImageRGBA8))
import Servant.JuicyPixels (PNG)

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

instance Accept SVG where
  contentType _ = "image" // "svg+xml"

instance MimeRender SVG TL.Text where
  mimeRender _ = TL.encodeUtf8

--
-- ## API ##
--
type API =
  "roundel" :> "no-text" :> Capture "color" ColorHex :>
    (    "image.svg" :> Get '[SVG] TL.Text
    :<|> "image.png" :> Get '[PNG] DynamicImage )

api :: Proxy API
api = Proxy

renderRoundelTemplate
  :: MonadIO m
  => ColorHex -> ExceptT T.Text m TL.Text
renderRoundelTemplate (ColorHex color) = do
  file <- liftIO $ TIO.readFile "res/roundel-no-text.svg.tpl"
  tmpl <-
    case TT.templateSafe file of
      Left _ -> throwError "Template parsing failed."
      Right a -> return a
  let table "colorHex" = pure color
      table _ = Nothing
      context key =
        case table key of
          Just a -> return a
          Nothing ->
            throwError $
            "Undefined template variable '" <> key <> "' requested."
  TT.renderA tmpl context

custom500 :: T.Text -> ServerError
custom500 t =
  ServerError
  { errHTTPCode = 500
  , errReasonPhrase = T.unpack t
  , errBody = ""
  , errHeaders = []
  }

with500
  :: ExceptT T.Text IO a -> Handler a
with500 = Handler . withExceptT custom500

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
server color = roundelSvgHandler color :<|> roundelPngHandler color

app :: Application
app = serve api server

main :: IO ()
main = runEnv 8080 app
