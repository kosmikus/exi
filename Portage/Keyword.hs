{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Keyword utilities.
-}

module Portage.Keyword where

import Data.Set

type Keyword = String

-- | Splits a string of keyowrds.
splitKeywords :: String -> [Keyword]
splitKeywords = words

-- | Merges keywords; this is similar to merging USE flags
mergeKeywords :: [Keyword] -> [Keyword] -> [Keyword]
mergeKeywords xs []  =  xs
mergeKeywords xs ys  =  elems $
                        foldl (\s k -> if k == "-*"  then  empty
                                                     else  insert k s)
                              (fromList xs)
                              ys

