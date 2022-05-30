{-# LANGUAGE OverloadedStrings #-}

module Markdown where

import qualified Hakyll                   as H
import           Text.Pandoc
import           Text.Pandoc.Highlighting (pygments)

documentCompiler :: H.Compiler (H.Item String)
documentCompiler = H.pandocCompilerWith readerOpts writerOpts

readerOpts :: ReaderOptions
readerOpts = def {
    readerExtensions = pandocExtensions
}

writerOpts :: WriterOptions
writerOpts = def {
    writerExtensions = pandocExtensions,
    writerHTMLMathMethod = MathML,
    writerHighlightStyle = Just pygments
}
