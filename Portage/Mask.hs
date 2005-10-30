{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Masking (for instance, hardmask or keyword-mask) of packages.
-}

module Portage.Mask
  where

import Data.Char

import Portage.Dependency
import Portage.Utilities
import Portage.Tree
import Portage.Constants
import Portage.Config
import Portage.Profile

type MaskReason = [String]

-- | Parse a package.mask (or package.unmask) file.
parseMask :: String -> [(DepAtom, MaskReason)]
parseMask = parseMaskByLine [] [] . lines

parseMaskByLine :: MaskReason -> MaskReason -> [String] -> [(DepAtom, MaskReason)]
parseMaskByLine acc facc (l@('#':_) : ls)  =  let  nacc = l : acc
                                              in   parseMaskByLine nacc (reverse nacc) ls
parseMaskByLine acc facc (l : ls)
  | all isSpace l                          =  parseMaskByLine [] [] ls
  | otherwise                              =  (getDepAtom l, facc) : parseMaskByLine [] facc ls
parseMaskByLine acc facc []                =  []


readMaskFile :: FilePath -> IO [(DepAtom, MaskReason)]
readMaskFile f = fmap parseMask (strictReadFile f)


globalMask :: Config -> IO [(DepAtom, MaskReason)]
globalMask cfg = do r <- findOverlayFile cfg (\pt -> profilesDir pt ./. packageMask) parseMask (++)
                    return $ maybe [] id r

profileMask :: IO [(DepAtom, MaskReason)]
profileMask = fmap concat (readProfileFile packageMask readMaskFile)

userMask :: IO [(DepAtom, MaskReason)]
userMask = readMaskFile (localConfigDir ./. packageMask)

userUnMask :: IO [(DepAtom, MaskReason)]
userUnMask = readMaskFile (localConfigDir ./. packageUnMask)

-- TODO: record filename in MaskReason
-- TODO: packages file, slightly different syntax

