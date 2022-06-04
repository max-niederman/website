{-# LANGUAGE OverloadedStrings #-}

module Document (documentCompiler) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer
import           Data.Functor               ((<&>))
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as Text
import           Debug.Trace                (traceShowId)
import qualified Hakyll                     as H
import           Text.Pandoc
import           Text.Pandoc.Highlighting
import           Text.Pandoc.Shared         (stringify)

documentCompiler :: H.Template -> H.Compiler (H.Item String)
documentCompiler template = runWriterT documentCompiler' >>= uncurry (flip (H.applyTemplate template))

type CompilerM = WriterT (H.Context String) H.Compiler

documentCompiler' :: CompilerM (H.Item String)
documentCompiler' = lift (H.getResourceBody >>= H.readPandocWith readerOpts) >>= transforms <&> H.writePandocWith writerOpts

readerOpts :: ReaderOptions
readerOpts = def {readerExtensions = pandocExtensions}

writerOpts :: WriterOptions
writerOpts =
  def
    { writerExtensions = pandocExtensions
    , writerHTMLMathMethod = MathML
    -- , writerHighlightStyle = Just pygments
    }

transforms :: H.Item Pandoc -> CompilerM (H.Item Pandoc)
transforms = foldl1 (>=>) [
    -- traverse highlight,
    (tell H.defaultContext >>) . return
  ]

