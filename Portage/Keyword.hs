{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Keyword utilities.
-}

module Portage.Keyword where

import qualified Data.Set as S

type Keyword = String

-- | Splits a string of keyowrds.
splitKeywords :: String -> [Keyword]
splitKeywords = words

-- | Merges keywords; this is similar to merging USE flags
mergeKeywords :: [Keyword] -> [Keyword] -> [Keyword]
mergeKeywords xs []  =  xs
mergeKeywords xs ys  =  S.elems $
                        foldl (\s k -> if k == "-*"  then  S.empty
                                                     else  S.insert k s)
                              (S.fromList xs)
                              ys

-- | Filters a list of keywords for keywords that belong to a
--   certain architecture. The format of an architecture is like
--   a keyword, but without an initial modifier.
archFilter  ::  Keyword     -- the architecture
            ->  [Keyword] -> [Keyword]
archFilter arch ks = filter (isArch arch) ks

isArch :: Keyword -> Keyword -> Bool
isArch arch "*"      =  True
isArch arch ('-':k)  =  isArch arch k
isArch arch ('~':k)  =  isArch arch k
isArch arch k        =  arch == k
