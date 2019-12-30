{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Clay hiding (id, meta, src, title, type_)
import Control.Monad
import Data.Aeson (FromJSON, fromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Data.Text (Text)
import Development.Shake
import GHC.Generics
import Lucid
import Path
import Rib (Source)
import qualified Rib
import qualified Rib.Parser.MMark as M

-- | A generated page corresponds to either an index of sources, or an
-- individual source.
--
-- Each `Source` specifies the parser type to use. Rib provides `MMark` and
-- `Pandoc`; but you may define your own as well.
data Page
  = Page_Index [Source M.MMark]
  | Page_Single (Source M.MMark)

-- | Metadata in our markdown sources. Parsed as JSON.
data SrcMeta
  = SrcMeta
      { title :: Text,
        -- | Description is optional, hence it is a `Maybe`
        description :: Maybe Text
      }
  deriving (Show, Eq, Generic, FromJSON)

-- | Main entry point to our generator.
--
-- `Rib.run` handles CLI arguments, and takes three parameters here.
--
-- 1. Directory `a`, from which static files will be read.
-- 2. Directory `b`, under which target files will be generated.
-- 3. Shake build action to run.
--
-- In the shake build action you would expect to use the utility functions
-- provided by Rib to do the actual generation of your static site.
main :: IO ()
main = Rib.run [reldir|a|] [reldir|b|] generateSite
  where
    -- Shake Action for generating the static site
    generateSite :: Action ()
    generateSite = do
      -- Copy over the static files
      Rib.buildStaticFiles [[relfile|static/**|]]
      -- Build individual sources, generating .html for each.
      -- The function `buildHtmlMulti` takes the following arguments:
      -- - File patterns to build
      -- - Function that will parse the file (here we use mmark)
      -- - Function that will generate the HTML (see below)
      srcs <-
        Rib.buildHtmlMulti [[relfile|*.md|]] M.parseIO $
          renderPage . Page_Single
      -- Build an index.html linking to the aforementioned files.
      Rib.buildHtml [relfile|index.html|] $
        renderPage (Page_Index srcs)
    -- Define your site HTML here
    renderPage :: Page -> Html ()
    renderPage page = with html_ [lang_ "en"] $ do
      head_ $ do
        meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
        title_ $ case page of
          Page_Index _ -> "My website!"
          Page_Single src -> toHtml $ title $ getMeta src
        style_ [type_ "text/css"] $ Clay.render pageStyle
      body_
        $ with div_ [id_ "thesite"]
        $ do
          with a_ [href_ "/"] "Back to Home"
          hr_ []
          case page of
            Page_Index srcs ->
              div_ $ forM_ srcs $ \src -> with li_ [class_ "links"] $ do
                let meta = getMeta src
                b_ $ with a_ [href_ (Rib.sourceUrl src)] $ toHtml $ title meta
                maybe mempty (M.render . either (error . T.unpack) id . M.parsePure "<desc>") $ description meta
            Page_Single src ->
              with article_ [class_ "post"] $ do
                h1_ $ toHtml $ title $ getMeta src
                M.render $ Rib.sourceVal src
    -- Get metadata from Markdown YAML block
    getMeta :: Source M.MMark -> SrcMeta
    getMeta src = case M.projectYaml (Rib.sourceVal src) of
      Nothing -> error "No YAML metadata"
      Just val -> case fromJSON val of
        Aeson.Error e -> error $ "JSON error: " <> e
        Aeson.Success v -> v
    -- Define your site CSS here
    pageStyle :: Css
    pageStyle = "div#thesite" ? do
      margin (em 4) (pc 20) (em 1) (pc 20)
      "li.links" ? do
        listStyleType none
        marginTop $ em 1
        "b" ? fontSize (em 1.2)
        "p" ? sym margin (px 0)
