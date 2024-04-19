module Main where

import Hakyll

main :: IO ()
main = hakyll $ do
    match "content/*" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
