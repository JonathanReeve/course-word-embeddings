{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Clay (Css, em, px, sym, (?))
import qualified Clay as C
import Control.Monad
import Data.Aeson (FromJSON, fromJSON)
import Data.Maybe (fromMaybe)
import qualified Data.Aeson as Aeson
import Data.Text (Text, intercalate, strip)
import qualified Data.Text as T
import Development.Shake
import GHC.Generics (Generic)
import Lucid
import Main.Utf8
import Rib (IsRoute, Pandoc)
import qualified Rib
import qualified Rib.Parser.Pandoc as Pandoc

-- | Route corresponding to each generated static page.
--
-- The `a` parameter specifies the data (typically Markdown document) used to
-- generate the final page text.
data Route a where
  Route_Index :: FilePath -> Route Pandoc
  Route_Article :: FilePath -> Route Pandoc

-- | The `IsRoute` instance allows us to determine the target .html path for
-- each route. This affects what `routeUrl` will return.
instance IsRoute Route where
  routeFile = \case
    Route_Index _ -> pure "index2.html"
    Route_Article _ -> pure $ "index.html"

-- | Main entry point to our generator.
--
-- `Rib.run` handles CLI arguments, and takes three parameters here.
--
-- 1. Directory `content`, from which static files will be read.
-- 2. Directory `dest`, under which target files will be generated.
-- 3. Shake action to run.
--
-- In the shake action you would expect to use the utility functions
-- provided by Rib to do the actual generation of your static site.
main :: IO ()
main = withUtf8 $ do
  Rib.run "content" "dest" generateSite

-- | Shake action for generating the static site
generateSite :: Action ()
generateSite = do
  -- Copy over the static files
  Rib.buildStaticFiles ["static/**"]
  -- Build individual sources, generating .html for each.
  Rib.forEvery ["*.md"] $ \srcPath -> do
    let r = Route_Article srcPath
    doc <- Pandoc.parse Pandoc.readMarkdown srcPath
    Rib.writeRoute r . Lucid.renderText . renderPage r
    pure (r, doc)

-- | Define your site HTML here
renderPage :: Route a -> a -> Html ()
renderPage _ val = html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    title_ $ toHtml $ title $ getMeta val
    link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/tufte-css/1.7.2/tufte.min.css"]
    style_ [type_ "text/css"] $ C.render pageStyle
  body_ $ do
    h1_ routeTitle
    article_ $ Pandoc.render val

-- | Define your site CSS here
pageStyle :: Css
pageStyle =
  C.body ? do
    -- C.margin (em 4) (pc 20) (em 1) (pc 20)
    ".header" ? do
      C.marginBottom $ em 2
    "li.pages" ? do
      C.listStyleType C.none
      C.marginTop $ em 1
      C.fontSize (em 2)
      "p" ? sym C.margin (px 0)

-- | Metadata in our markdown sources
data SrcMeta = SrcMeta
  { title :: Text,
    -- | Description is optional, hence `Maybe`
    description :: Maybe Text,
    date :: Maybe Text,
    authors :: [Text],
    github :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON)

-- | Get metadata from Markdown's YAML block
getMeta :: Pandoc -> SrcMeta
getMeta src = case Pandoc.extractMeta src of
  Nothing -> error "No YAML metadata"
  Just (Left e) -> error $ T.unpack e
  Just (Right val) -> case fromJSON val of
    Aeson.Error e -> error $ "JSON error: " <> e
    Aeson.Success v -> v
