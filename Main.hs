{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Clay hiding (title, type_)
import Control.Monad
import Data.Aeson (FromJSON)
import Data.Maybe
import Data.Text (Text)
import Development.Shake
import GHC.Generics
import Lucid
import Path
import Rib (Document, MMark, Markup)
import qualified Rib

-- First we shall define two datatypes to represent our pages. One, the page
-- itself. Second, the metadata associated with each document.

-- | A generated page is either an index of documents, or an individual document.
data Page doc
  = Page_Index [Document doc]
  | Page_Doc (Document doc)

-- | Type representing the metadata in our Markdown documents
--
-- Note that if a field is not optional (i.e., not Maybe) it must be present.
data Meta
  = Meta
      { title :: Text,
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
main = Rib.run [reldir|a|] [reldir|b|] $ do
  -- Copy over the static files
  Rib.buildStaticFiles [[relfile|static/**|]]
  -- Build individual markdown files, generating .html for each.
  --
  -- NOTE: We use TypeApplications to specify the type of the `doc` type
  -- variable, as used in the `Markup doc` constraint in the functions below.
  -- There are currently two possible values: `MMark` (if you choose to use the
  -- `mmark` parser) and `Pandoc` (if using pandoc).
  posts <- Rib.buildHtmlMulti @MMark [relfile|*.md|] (renderPage . Page_Doc)
  -- Build an index.html linking to the aforementioned files.
  Rib.buildHtml [relfile|index.html|]
    $ renderPage
    $ Page_Index posts
  where
    -- Define your site HTML here
    renderPage :: Markup doc => Page doc -> Html ()
    renderPage page = with html_ [lang_ "en"] $ do
      head_ $ do
        meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
        title_ $ case page of
          Page_Index _ -> "My website!"
          Page_Doc doc -> toHtml $ title $ Rib.getDocumentMeta doc
        style_ [type_ "text/css"] $ Clay.render pageStyle
      body_
        $ with div_ [id_ "thesite"]
        $ do
          with a_ [href_ "/"] "Back to Home"
          hr_ []
          case page of
            Page_Index docs ->
              div_ $ forM_ docs $ \doc -> with li_ [class_ "links"] $ do
                let meta = Rib.getDocumentMeta doc
                b_ $ with a_ [href_ (Rib.getDocumentUrl doc)] $ toHtml $ title meta
                maybe mempty Rib.renderMarkdown $
                  description meta
            Page_Doc doc ->
              with article_ [class_ "post"] $ do
                h1_ $ toHtml $ title $ Rib.getDocumentMeta doc
                Rib.renderDoc doc
    -- Define your site CSS here
    pageStyle :: Css
    pageStyle = "div#thesite" ? do
      margin (em 4) (pc 20) (em 1) (pc 20)
      "li.links" ? do
        marginTop $ em 1
        "b" ? fontSize (em 1.2)
        listStyleType none
        "p" ? sym margin (px 0)
