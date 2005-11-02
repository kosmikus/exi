{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    USE flag utilities.
-}

module Portage.Use where

import Data.Char
import qualified Data.Set as S

type UseFlag = String

-- | Splits a string of USE flags.
splitUse :: String -> [UseFlag]
splitUse = words

-- | Merges USE flags; the first list is assumed to be okay
--   already (no duplicates, no negatives).
mergeUse :: [UseFlag] -> [UseFlag] -> [UseFlag]
mergeUse xs []  =  xs
mergeUse xs ys  =  S.elems $
                   foldl  (\s u@(x:n) ->  if u == "-*"      then  S.empty
                                          else if x == '-'  then  S.delete n s
                                                            else  S.insert u s
                          )
                          (S.fromList xs)
                          ys

-- | Computes a difference of USE flags. Both lists are
--   assumed to contain only positives. The resulting list
--   contains modifiers though.
diffUse :: [UseFlag] -> [UseFlag] -> [UseFlag]
diffUse xs ys =  let  sx     =  S.fromList xs 
                      sy     =  S.fromList ys
                      plus   =  S.intersection sx sy
                      minus  =  sy S.\\ plus
                 in   S.elems plus ++ map ('-':) (S.elems minus)

-- | Perform USE flag expansion.
expandUse :: [(String,String)] -> [UseFlag]
expandUse = concatMap expand
  where  expand (v,c)   =  [map toLower v ++ "_" ++ w | w <- words c ]
