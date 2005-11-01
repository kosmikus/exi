{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Support for package-specific keyword settings.
-}

module Portage.PackageKeywords
  where

import System.IO.Unsafe

import Portage.Keyword
import Portage.Utilities
import Portage.Constants
import Portage.Tree
import Portage.Match
import Portage.Dependency
import Portage.Ebuild

data KeywordsForPackage  =  KeywordsForPackage
                              {
                                 kkeywords  ::  [Keyword],
                                 kdepatom   ::  DepAtom
                              }

-- | Parse a @package.keywords@ file.
parseKeywords :: String -> [KeywordsForPackage]
parseKeywords = map parseKeywordsLine . lines . stripComments 

parseKeywordsLine :: String -> KeywordsForPackage
parseKeywordsLine x =
    case words x of
      []      ->  error $ "parseKeywordsLine: parse error in package.keywords file on line '" ++ x ++ "'"
      (d:ks)  ->  KeywordsForPackage ks (getDepAtom d)

readKeywordsFile :: FilePath -> IO [KeywordsForPackage]
readKeywordsFile f = fmap parseKeywords (strictReadFile f)

localKeywords  ::  IO [KeywordsForPackage]
localKeywords  =   unsafeInterleaveIO $
                   readKeywordsFile localKeywordsFile

performKeywords :: KeywordsForPackage -> Tree -> Tree
performKeywords (KeywordsForPackage ks d) =
    modifyTreeForDepAtom d (\v -> v { meta =  case meta v of
                                                me -> me { lockey = ks } })
