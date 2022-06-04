{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Functor                  ((<&>))
import qualified Data.List                     as List
import qualified Data.Maybe                    as Maybe
import           Data.String                   (IsString (..))
import           GHC.IO.Encoding
import           Hakyll
import           Hakyll.Core.Compiler.Internal as CI
import           Document

main :: IO ()
main = do
  setLocaleEncoding utf8
  hakyllWith config $ do
    match "templates/**" $ compile templateBodyCompiler

    match "styles/_*.scss" $ return ()
    match "styles/*.scss" $ do
        route   $ setExtension "css"
        compile $ do
          source <- getResourceString
          makePatternDependency "styles/_*.scss"
          compiled <- withItemBody (unixFilter "sass" ["--stdin", "-I", "content/styles"]) source
          return $ compressCss <$> compiled
    match "styles/*.css" $ do
      route idRoute
      compile compressCssCompiler

    match "static/**" $ do
      route $ stripPrefixRoute "static/"
      compile copyFileCompiler

    match "**.md" $ do
      route $ setExtension "html"
      compile $ loadBody "templates/page.html" >>= documentCompiler >>= relativizeUrls

config :: Configuration
config = defaultConfiguration {providerDirectory = "content"}

stripPrefixRoute :: String -> Routes
stripPrefixRoute p =
  customRoute $ Maybe.fromJust . List.stripPrefix p . toFilePath
