{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Dhall.TH
import GHC.Generics
import Lucid
import Path
import Rib (MMark, Target)
import qualified Rib
import qualified Rib.Parser.Dhall as Dhall
import qualified Rib.Parser.MMark as MMark

-- | This will be our type representing generated pages.
--
-- Each `Source` specifies the parser type to use. Rib provides `MMark` and
-- `Pandoc`; but you may define your own as well.
data Page
  = Page_Index [Target (Path Rel File) MMark]
  | Page_Single (Target (Path Rel File) MMark)

-- | The "Config" type generated from the Dhall type.
--
-- Use `Rib.Parser.Dhall` to parse it (see below). We will need Generic and
-- FromDhall instances for it.
makeHaskellTypes
  [ SingleConstructor "Config" "Config" "./src-dhall/Config.dhall"
  ]

deriving instance Show Config

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
main = Rib.run [reldir|content|] [reldir|dest|] generateSite

-- | Shake action for generating the static site
generateSite :: Action ()
generateSite = do
  -- Copy over the static files
  Rib.buildStaticFiles [[relfile|static/**|]]
  -- Read the site config
  config :: Config <-
    Rib.readSource
      (Dhall.parse [[relfile|src-dhall/Config.dhall|]])
      [relfile|config.dhall|]
  -- Build individual sources, generating .html for each.
  --
  -- `Rib.forEvery` iterates over a pattern of files.
  -- For each such file, we use `Rib.loadTarget`, passing it a parser function
  -- (`MMark.parse` in this case), to load the source file. This returns a
  -- `Target` type can be used in rendering stage.
  -- Finally, `Rib.writeTarget` is called to write the rendered HTML.
  srcs <-
    Rib.forEvery [[relfile|*.md|]] $ \srcPath -> do
      targetPath <- liftIO $ replaceExtension ".html" srcPath
      target <- Rib.loadTarget srcPath MMark.parse targetPath
      Rib.writeTarget target $ renderPage config . Page_Single
      pure target
  -- Write an index.html linking to the aforementioned files.
  Rib.writeHtml [relfile|index.html|] $
    renderPage config (Page_Index srcs)

-- | Define your site HTML here
renderPage :: Config -> Page -> Html ()
renderPage config page = with html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    title_ $ case page of
      Page_Index _ -> toHtml $ siteTitle config
      Page_Single src -> toHtml $ title $ getMeta src
    style_ [type_ "text/css"] $ C.render pageStyle
  body_ $ do
    with div_ [id_ "thesite"] $ do
      with div_ [class_ "header"] $
        with a_ [href_ "/"] "Back to Home"
      case page of
        Page_Index srcs -> do
          h1_ $ toHtml $ siteTitle config
          div_ $ forM_ srcs $ \src ->
            with li_ [class_ "pages"] $ do
              let meta = getMeta src
              b_ $ with a_ [href_ (Rib.targetUrl src)] $ toHtml $ title meta
              maybe mempty renderMarkdown $ description meta
        Page_Single src ->
          with article_ [class_ "post"] $ do
            h1_ $ toHtml $ title $ getMeta src
            MMark.render $ Rib.targetVal src
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
getMeta :: Target src MMark -> SrcMeta
getMeta src = case MMark.projectYaml (Rib.targetVal src) of
  Nothing -> error "No YAML metadata"
  Just val -> case fromJSON val of
    Aeson.Error e -> error $ "JSON error: " <> e
    Aeson.Success v -> v
