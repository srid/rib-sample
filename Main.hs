{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Clay ((?), Css, em, pc, px, sym)
import qualified Clay as C
import Control.Monad
import Data.Aeson (FromJSON, fromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Data.Text (Text)
import Development.Shake
import GHC.Generics
import Lucid
import Path
import Rib (MMark, Source)
import qualified Rib
import qualified Rib.Parser.MMark as MMark

-- | This will be our type representing generated pages.
--
-- Each `Source` specifies the parser type to use. Rib provides `MMark` and
-- `Pandoc`; but you may define your own as well.
data Page
  = Page_Index [Source MMark]
  | Page_Single (Source MMark)

-- | Main entry point to our generator.
--
-- `Rib.run` handles CLI arguments, and takes three parameters here.
--
-- 1. Directory `a`, from which static files will be read.
-- 2. Directory `b`, under which target files will be generated.
-- 3. Shake action to run.
--
-- In the shake build action you would expect to use the utility functions
-- provided by Rib to do the actual generation of your static site.
main :: IO ()
main = Rib.run [reldir|a|] [reldir|b|] generateSite

-- | Shake action for generating the static site
generateSite :: Action ()
generateSite = do
  -- Copy over the static files
  Rib.buildStaticFiles [[relfile|static/**|]]
  -- Build individual sources, generating .html for each.
  -- The function `buildHtmlMulti` takes the following arguments:
  -- - Function that will parse the file (here we use mmark)
  -- - File patterns to build
  -- - Function that will generate the HTML (see below)
  srcs <-
    Rib.forEvery [[relfile|*.md|]] $ \srcPath ->
      Rib.buildHtml srcPath MMark.parse $ renderPage . Page_Single
  -- Write an index.html linking to the aforementioned files.
  Rib.writeHtml [relfile|index.html|] $
    renderPage (Page_Index srcs)

-- | Define your site HTML here
renderPage :: Page -> Html ()
renderPage page = with html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    title_ $ case page of
      Page_Index _ -> "My website!"
      Page_Single src -> toHtml $ title $ getMeta src
    style_ [type_ "text/css"] $ C.render pageStyle
  body_ $ do
    with div_ [id_ "thesite"] $ do
      with div_ [class_ "header"] $
        with a_ [href_ "/"] "Back to Home"
      case page of
        Page_Index srcs -> div_ $ forM_ srcs $ \src ->
          with li_ [class_ "pages"] $ do
            let meta = getMeta src
            b_ $ with a_ [href_ (Rib.sourceUrl src)] $ toHtml $ title meta
            maybe mempty renderMarkdown $ description meta
        Page_Single src ->
          with article_ [class_ "post"] $ do
            h1_ $ toHtml $ title $ getMeta src
            MMark.render $ Rib.sourceVal src
  where
    renderMarkdown =
      MMark.render . either (error . T.unpack) id . MMark.parsePure "<none>"

-- | Define your site CSS here
pageStyle :: Css
pageStyle = "div#thesite" ? do
  C.margin (em 4) (pc 20) (em 1) (pc 20)
  ".header" ? do
    C.marginBottom $ em 2
  "li.pages" ? do
    C.listStyleType C.none
    C.marginTop $ em 1
    "b" ? C.fontSize (em 1.2)
    "p" ? sym C.margin (px 0)

-- | Metadata in our markdown sources
data SrcMeta
  = SrcMeta
      { title :: Text,
        -- | Description is optional, hence `Maybe`
        description :: Maybe Text
      }
  deriving (Show, Eq, Generic, FromJSON)

-- | Get metadata from Markdown's YAML block
getMeta :: Source MMark -> SrcMeta
getMeta src = case MMark.projectYaml (Rib.sourceVal src) of
  Nothing -> error "No YAML metadata"
  Just val -> case fromJSON val of
    Aeson.Error e -> error $ "JSON error: " <> e
    Aeson.Success v -> v
