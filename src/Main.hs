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

import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import GHC.Generics (Generic)
import Graphics.Svg (parseSvgFile)
import Graphics.Rasterific.Svg
       (renderSvgDocument, pdfOfSvgDocument)
import System.FilePath ((</>))
import Network.HTTP.Media ((//))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (runEnv)
import Data.Monoid ((<>))
import System.Directory (getTemporaryDirectory)
import Codec.Picture.Types
       (Image, PixelRGBA8, DynamicImage(ImageRGBA8))
import Codec.Picture.Saving (imageToPng)

import Graphics.Rasterific.Svg (loadCreateFontCache)
import Graphics.Text.TrueType (FontCache)

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

roundelSvgHandler :: ColorHex -> Handler TL.Text
roundelSvgHandler (ColorHex color) = do
  tmpl <- liftIO $ TIO.readFile "res/roundel-no-text.svg.tpl"
  -- TODO: Avoid partial function.
  let context "colorHex" = color
      context v =
        error $ "Undefined template variable '" <> T.unpack v <> "' requested."
  -- TODO: Consider using templateSafe.
  return $ TT.substitute tmpl context

roundelPngHandler :: ColorHex -> Handler DynamicImage
roundelPngHandler (ColorHex color) = do
  tmpl <- liftIO $ TIO.readFile "res/roundel-no-text.svg.tpl"
  -- TODO: Get rid of "error"
  let context "colorHex" = color
      context v =
        error $ "Undefined template variable '" <> T.unpack v <> "' requested."
  let rendered = TT.substitute tmpl context
  img <- liftIO . renderSvgToPng $ BSL.toStrict $ TL.encodeUtf8 rendered
  case img of
    Left err -> error $ T.unpack err
    Right img' -> return $ ImageRGBA8 img'

renderSvgToPng :: BS.ByteString -> IO (Either T.Text (Image PixelRGBA8))
renderSvgToPng input =
  case parseSvgFile "/dev/null" input of
    Nothing -> return $ Left "Error while loading SVG"
    Just doc -> do
      cache <- loadCreateFontCache "fonty-texture-cache"
      (finalImage, _) <- renderSvgDocument cache Nothing 96 doc
      return $ Right finalImage

server :: Server API
server = roundelSvgHandler :<|> roundelPngHandler

app :: Application
app = serve api server

main :: IO ()
main = runEnv 8080 app
