{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Handle masked USE flags.
-}

module Portage.UseMask
  where

import System.IO.Unsafe

import Portage.Utilities
import Portage.Profile
import Portage.Constants
import Portage.Use

-- | Parse a @use.mask@ file.
parseUseMask :: String -> [UseFlag]
parseUseMask = map ('-':) . mergeUse [] . lines . stripComments


readUseMaskFile :: FilePath -> IO [UseFlag]
readUseMaskFile f = fmap parseUseMask (strictReadFileIfExists f)

profileUseMask :: IO [UseFlag]
profileUseMask  =  unsafeInterleaveIO $
                   fmap concat (readProfileFile useMask readUseMaskFile)
