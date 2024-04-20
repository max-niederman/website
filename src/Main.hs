{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll
import Data.Maybe (fromJust)

main :: IO ()
main = hakyll $ do
    match "content/*.md" $ do
        route   $ metadataRoute $ \meta ->
            constRoute $ fromJust (lookupString "slug" meta) <> ".html"
        compile $ pandocCompiler
    
    create ["content/index.html"] $ do
        route $ constRoute "index.html"
        compile copyFileCompiler