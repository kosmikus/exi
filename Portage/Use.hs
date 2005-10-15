{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    USE flag utilities.
-}

module Portage.Use where

import Data.Set

type UseFlag = String

-- | Splits a string of USE flags.
splitUse :: String -> [UseFlag]
splitUse = words

-- | Merges use flags; the first list is assumed to be okay
-- | already (no duplicates, no negatives)
mergeUse :: [UseFlag] -> [UseFlag] -> [UseFlag]
mergeUse xs []  =  xs
mergeUse xs ys  =  elems $
                   foldl  (\s u@(x:n) ->  if u == "-*"      then  empty
                                          else if x == '-'  then  delete n s
                                                            else  insert u s
                          )
                          (fromList xs)
                          ys

