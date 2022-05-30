{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List       as List
import qualified Data.Maybe      as Maybe
import           Debug.Trace     (traceId)
import           GHC.IO.Encoding
import           Hakyll
import           Markdown

main :: IO ()
main = do
    setLocaleEncoding utf8
    hakyllWith config $ do
        match "static/*" $ do
            route $ stripPrefixRoute "static/"
            compile copyFileCompiler

        match "**/*.md" $ do
            route $ setExtension "html"
            compile $ documentCompiler >>= relativizeUrls

config :: Configuration
config = defaultConfiguration {providerDirectory = "content"}

stripPrefixRoute :: String -> Routes
stripPrefixRoute p =
  customRoute $ Maybe.fromJust . List.stripPrefix p . toFilePath
