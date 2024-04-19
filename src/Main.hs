{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll
import Data.Maybe (fromJust)

main :: IO ()
main = hakyll $ do
    match "content/*" $ do
        route   $ metadataRoute $ \meta ->
            constRoute $ fromJust (lookupString "slug" meta) <> ".html"
        compile $ pandocCompiler
