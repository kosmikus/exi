{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Support for package-specific keyword settings.
-}

module Portage.PackageKeywords
  where

import System.IO.Unsafe

import Portage.Config
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
  deriving (Eq,Show)

-- | Performs a keyword modification.
mapKeywordsForPackage :: ([Keyword] -> [Keyword]) -> KeywordsForPackage -> KeywordsForPackage
mapKeywordsForPackage f k = k { kkeywords = f (kkeywords k) }

-- | Parse a @package.keywords@ file.
parseKeywords :: Config -> String -> [KeywordsForPackage]
parseKeywords cfg = map (parseKeywordsLine cfg) . lines . stripComments 

parseKeywordsLine :: Config -> String -> KeywordsForPackage
parseKeywordsLine cfg x =
    case words x of
      []      ->  error $ "parseKeywordsLine: internal error, empty line in package.keywords file"
      (d:ks)  ->  let  ks' =  case ks of
                                []  ->  ["~" ++ arch cfg]
                                _   ->  ks
                  in   KeywordsForPackage ks' (getDepAtom d)

readKeywordsFile :: Config -> FilePath -> IO [KeywordsForPackage]
readKeywordsFile cfg f = fmap (parseKeywords cfg) (strictReadFileIfExists f)

userKeywords      ::  Config -> IO [KeywordsForPackage]
userKeywords cfg  =   unsafeInterleaveIO $ readKeywordsFile cfg localKeywordsFile

performKeywords :: KeywordsForPackage -> Tree -> Tree
performKeywords (KeywordsForPackage ks d) =
    modifyTreeForDepAtom d (\v -> v { meta =  case meta v of
                                                me -> me { lockey = ks } })
