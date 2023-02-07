module QuoteOfTheDay where

-- A simple 2-tuple database for storing text snippets which can be transcluded. The initial use of this was for 'quote of the day' snippet generation.
--
-- 'Quote of the day' is an old website feature where, for visitors' edification or amusement, a random quote from a list of quotes would be display, often in the website footer or homepage.
-- An example is <https://en.wikiquote.org/wiki/Wikiquote:Quote_of_the_day> which is transcluded in the middle of <https://en.wikiquote.org/wiki/Main_Page>, or <https://web.archive.org/web/20150410044208/http://bbs.stardestroyer.net/SiteBanner.php?display=history>.
-- A common source of quotes used to be <https://en.wikipedia.org/wiki/Fortune_(Unix)>; see also <https://www.lesswrong.com/tag/rationality-quotes>.
--
-- A QOTD database is a `[Quote]` Haskell file, read with read/show. A `Quote` is a 3-tuple `(String, String, Bool)`: HTML "quote", HTML "attribution [or other commentary/metadata]", and whether it has been "used yet" (not entirely necessary, since one could sample randomly, but tracked to minimize reuse of quotes).† When all quotes have been used and are `True`, they get reset to `False` and the cycle begins again (with, presumably, new quotes added since the last time).
-- Quotes & attributions do not contain double-quote delimiters or other HTML wrapping; that will be added when they are formatted as Gwern.net-style 'epigraphs' to be written.
--
-- The QOTD db is used by read in the QOTDB, selecting the first unused quote, marking it used & writing out the updated db, and then writing out the quote to a particular file in a HTML format; that file is used by downstream users such as Hakyll websites which template or transclude it.
-- With the use of `transclude.js`, this can be as simple as:
--
-- `<div class="qotd"><a class="include" href="/metadata/today-quote.html">Quote Of The Day</a></div>`
--
-- † The probability that a daily visitor would see a duplicate quote under simple random sampling grows rapidly with time; see <https://en.wikipedia.org/wiki/Birthday_problem>. If eg. there were 366 quotes, then after only 23 visits, the reader would have a 50-50 chance of seeing ≥1 duplicate!

import Control.Monad (unless, when)
import qualified Data.Set as S (delete, empty, filter, fromList, toList, insert, map)
import System.Directory (doesFileExist)
import Text.Show.Pretty (ppShow)

import LinkMetadata (typesetHtmlField)

type TTDB = [Snippet]
type Snippet = (String, String, Bool)

quoteDBPath, quotePath :: FilePath
quoteDBPath = "metadata/quotes.hs"
quotePath   = "metadata/today-quote.html"
quoted :: Snippet -> String
quoted (quote,attribution,_) = "<div class=\"epigraph\">\n<blockquote><p>" ++ typesetHtmlField quote ++ "</p>" ++ if null attribution then "" else ("\n<p>" ++ typesetHtmlField attribution ++ "</p>") ++ "</blockquote>\n</div>"

linkDBPath, linkPath :: FilePath
linkDBPath = "metadata/links.hs"
linkPath   = "metadata/today-link.html"
linked :: Snippet -> String
linked (link,attribution,_) = "<div class=\"link-of-the-day\">\n<blockquote><p><a href=\"" ++ attribution ++ "\">" ++ typesetHtmlField link ++ "</a></p>" ++ "</blockquote>\n</div>"

readTTDB :: FilePath -> IO TTDB
readTTDB path = do exists <- doesFileExist path
                   if not exists then return [] else fmap read $ readFile path

writeTTDB :: FilePath -> TTDB -> IO ()
writeTTDB path = writeFile path . ppShow

writeSnippet :: FilePath -> (Snippet -> String) -> Snippet -> IO ()
writeSnippet path formatter x = writeFile path (formatter x)

generateSnippetAndWriteTTDB :: FilePath -> FilePath -> (Snippet -> String) -> IO ()
generateSnippetAndWriteTTDB dbpath path formatter =
  do dblist <- readTTDB dbpath
     when (null dblist) $ error $ "Fatal error: tuple database " ++ path ++ " is empty?"
     unless (not $ any (\(q,_,_) -> null q) dblist) $ error $ "Fatal error: tuple database has empty first-fields? " ++ show dblist
     let db = S.fromList dblist

     -- get set of usable quotes, and if there are none, reset the entire set and use that:
     let dbUnused = S.filter (\(_,_,status) -> not status) db
     let dbReset = if dbUnused /= S.empty then db else S.map snegate db
     let dbUnused' = S.filter (\(_,_,status) -> not status) dbReset

     let snippet = head $ S.toList dbUnused'
     writeSnippet path formatter snippet

     let db'' = S.insert (snegate snippet) $ S.delete snippet dbReset -- update the now-used quote
     writeTTDB dbpath $ S.toList db''

 where snegate :: Snippet -> Snippet
       snegate (a,b,s) = (a,b,not s)

qotd, lotd :: IO ()
qotd = generateSnippetAndWriteTTDB quoteDBPath quotePath quoted
lotd = generateSnippetAndWriteTTDB linkDBPath linkPath linked
