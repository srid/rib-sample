{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude hiding (div, (**))

import Control.Monad
import Data.Aeson (FromJSON)
import Data.Maybe
import Data.Text (Text)
import GHC.Generics

import Clay hiding (title, type_)
import Development.Shake
import Lucid

import Rib (Document, MMark, Markup)
import qualified Rib

-- | A generated page is either an index of documents, or an individual document.
data Page doc
  = Page_Index [Document doc]
  | Page_Doc (Document doc)

-- | Type representing the metadata in our Markdown documents
--
-- Note that if a field is not optional (i.e., not Maybe) it must be present.
data Meta = Meta
  { title :: Text
  , description :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON)

main :: IO ()
main = Rib.run "a" "b" $ do
  -- Copy over the static files
  Rib.buildStaticFiles ["static/**"]
  -- Build individual markdown files, generating .html for each.
  --
  -- NOTE: We use TypeApplications to specify the type of the `doc` type
  -- variable, as used in the `Markup doc` constraint in the functions below.
  -- There are currently two possible values: `MMark` (if you choose to use the
  -- `mmark` parser) and `Pandoc` (if using pandoc).
  posts <- Rib.buildHtmlMulti @MMark "*.md" (renderPage . Page_Doc)
  -- Build an index.html linking to the aforementioned files.
  Rib.buildHtml "index.html" $
    renderPage $ Page_Index posts

-- | Render the given page as HTML
renderPage :: Markup doc => Page doc -> Html ()
renderPage page = with html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    title_ pageTitle
    style_ [type_ "text/css"] $ Clay.render pageStyle
  body_ $
    with div_ [id_ "thesite"] $ do
      -- Main content
      with a_ [href_ "/"] "Back to Home"
      hr_ []
      case page of
        Page_Index docs ->
          div_ $ forM_ docs $ \doc -> li_ $ do
            let meta = Rib.getDocumentMeta doc
            b_ $ with a_ [href_ (Rib.getDocumentUrl doc)] $ toHtml $ title meta
            case description meta of
              Just s -> em_ $ small_ $ toHtml s
              Nothing -> mempty
        Page_Doc doc ->
          with article_ [class_ "post"] $ do
            h1_ $ toHtml $ title $ Rib.getDocumentMeta doc
            Rib.renderDoc doc
  where
    pageTitle = case page of
      Page_Index _ -> "My website!"
      Page_Doc doc -> toHtml $ title $ Rib.getDocumentMeta doc

    -- | CSS
    pageStyle :: Css
    pageStyle = div # "#thesite" ? do
      marginLeft $ pct 20
      marginTop $ em 4
      "h1" ? do
        fontSize $ em 2.3
