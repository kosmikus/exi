{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    World target and world file.
-}

module Portage.World
  where

import System.IO
import System.IO.Unsafe
import Control.Monad

import Portage.Package
import Portage.PortageConfig.Type
import Portage.Constants
import Portage.Utilities
import Portage.Dependency

-- | Returns the contents of the world file.
worldTarget :: IO [DepAtom]
worldTarget =  unsafeInterleaveIO $
               fmap  (map getDepAtom . lines . stripComments)
                     (strictReadFileIfExists worldFile)

-- | Potentially updates the world file. According to
--   "man 5 portage" from 05/2006, the world file may only
--   contain depend atom bases (i.e., values of type P),
--   therefore we write only a P (even though we allow to
--   read arbitrary DepAtom's). Returns whether a change
--   was made. System packages are not added to the world
--   file.
addToWorldFile :: PortageConfig -> P -> IO Bool
addToWorldFile pc p =
  if    (p `elem` map pFromDepAtom (depStringAtoms $ system pc))
  then  return False
  else  do  w <- worldTarget
            let matches = filter (\x -> pFromDepAtom x == p) w
            case matches of
              []  ->  -- no entry yet; we update the file
                      do  normalizeEOF worldFile -- make sure we write a new line
                          f <- openFile worldFile AppendMode
                          hPutStrLn f (showP p)
                          hClose f
                          return True
              _   ->  -- there's an entry already; do nothing
                      return False
