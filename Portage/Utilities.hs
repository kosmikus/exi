{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Common utilities module.
-}

module Portage.Utilities
  (
    splitAtLast,
    strictReadFile
  )
  where

import Data.List

-- | Split a list at the last occurrence of an element.
splitAtLast :: (Eq a) => a -> [a] -> ([a],[a])
splitAtLast s xs  =   splitAt (maximum (elemIndices s xs)) xs

-- | Reads a file completely into memory.
strictReadFile :: FilePath -> IO String
strictReadFile f  =   do  f <- readFile f
                          f `stringSeq` return f

-- | Completely evaluates a string.
stringSeq :: String -> b -> b
stringSeq []      c  =  c
stringSeq (x:xs)  c  =  stringSeq xs c
