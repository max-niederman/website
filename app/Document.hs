{-# LANGUAGE OverloadedStrings #-}

module Document (documentCompiler) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer
import           Data.Functor               ((<&>))
import qualified Data.List                  as List
import qualified Data.Maybe                 as Maybe
import qualified Hakyll                     as H
import           Text.Pandoc
import           Text.Pandoc.Highlighting
import           Text.Pandoc.Shared         (stringify)
import           Text.Printf                (printf)

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
    (tell basicContext >>) . return,
    (tell tagsContext >>) . return
  ]

basicContext :: H.Context String
basicContext = mconcat [
    H.bodyField "body",
    H.urlField "url",
    H.pathField "path",
    H.metadataField
  ]

tagsContext :: H.Context String
tagsContext = H.field "tags" $ \i -> do
  metadata <- H.getMetadata $ H.itemIdentifier i
  case H.lookupStringList "tags" metadata of
    Just tags -> return $ List.intercalate ", " $ map formatTag tags
    Nothing   -> H.noResult "no tags present"
  where
    formatTag :: String -> String
    formatTag tag = "<a href=\"/tags/" ++ tag ++ "\">" ++ tag ++ "</a>"
