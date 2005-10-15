{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Common utilities module.
-}

module Portage.Utilities
  where

import Data.List

-- | Split a list at the last occurrence of an element.
splitAtLast :: (Eq a) => a -> [a] -> ([a],[a])
splitAtLast s xs  =   splitAt (maximum (elemIndices s xs)) xs
