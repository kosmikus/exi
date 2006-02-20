{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    World target.
-}

module Portage.World
  where

import System.IO.Unsafe

import Portage.Constants
import Portage.Utilities
import Portage.Dependency

worldTarget :: IO [DepAtom]
worldTarget =  unsafeInterleaveIO $
               fmap  (map getDepAtom . lines . stripComments)
                     (strictReadFileIfExists worldFile)
