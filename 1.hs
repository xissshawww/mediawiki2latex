import UrlAnalyse
import ImperativeState
import UrlAnalyse
import qualified Data.ByteString as BStr
import Network.URL as URL
import qualified Data.ByteString.UTF8 as UTF8Str
import Data.List.Split (splitOn)
import Control.Monad
import Data.Maybe
import Control.Monad.State
import Data.List
import System.FilePath
import Codec.Binary.UTF8.String
import Control.Concurrent.MVar
import Parallel
import Data.Map (member)
import MediaWikiParser
import MediaWikiParseTree
import WikiHelper
import Verben 
import Nomen 
import Adjektive
import Adverbien
import Gradpartikel
import Hilfsverben
deepGet2 :: [Char] -> [Anything a] -> [Anything a]
deepGet2 tag ll = concat $ map go ll
  where
          | t == tag =
            [Environment Tag (TagAttr tag m) l] ++ (deepGet2 tag l)
        go (Environment _ _ l) = (deepGet2 tag l)
        go _ = []

deepGet3 :: [Char] -> [Char] -> [Anything Char] -> [Anything Char]
deepGet3 tag k ll  = concat ( map go ll) 
  where go (Environment Tag (TagAttr t m) l) 
          | ((t == tag) && (member k m)) =
            [Environment Tag (TagAttr tag m) l] ++ (deepGet3 tag k l)
        go (Environment _ _ l) = (deepGet3 tag k l)
        go _ = []

deepGo :: [Anything Char] -> [[Char]]
deepGo ((Environment Tag (TagAttr t m) l):xs) = (shallowFlatten l):(deepGo xs)
deepGo (_:xs) = deepGo xs
deepGo [] = [] 

--       yy <- geturl theUrl3
--       let gg = (deepGet "li" "id" "ca-history" (parseit minparsers yy))
gourl url  = do x<-geturl url
                return (reverse ( (reverse ( (deepGo (deepGet2 "a" (deepGet "div" "class" "mw-category-group" (deepGet "div" "id" "mw-pages"(parseit minparsers x)))))))))
          
gourl2 url  = do x<-geturl url
                 return (reverse ((reverse ( (deepGo (deepGet2 "a" (deepGet "div" "class" "mw-category-group" (deepGet "div" "id" "mw-pages"(parseit minparsers x)))))))))

gourls x = do y<-gourl2 ("https://de.wiktionary.org/w/index.php?title=Kategorie:Adverb_(Deutsch)&pagefrom="++(decodeString (last x))++"#mw-pages")
              if (length y)>0 then (print (last x))>>(print (last y))>> gourls (x++y) else return (x++y) 

main = do x <- gourl ("https://de.wiktionary.org/wiki/Kategorie:Adverb_%28Deutsch%29")
          y <- gourls x
          print y









