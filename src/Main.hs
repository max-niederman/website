{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll
import Data.Maybe (fromJust)

main :: IO ()
main = hakyll $ do    
    match "styles/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "scripts/*" $ do
        route   idRoute
        compile copyFileCompiler

    create ["content/index.html"] $ do
        route $ constRoute "index.html"
        compile copyFileCompiler

    match "content/*.md" $ do
        route   $ metadataRoute $ \meta ->
            constRoute $ fromJust (lookupString "slug" meta) <> ".html"
        compile $ pandocCompiler