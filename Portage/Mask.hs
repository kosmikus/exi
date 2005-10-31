{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Masking (for instance, hardmask or keyword-mask) of packages.
-}

module Portage.Mask
  where

import Data.Char
import qualified Data.Set as S
import Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Token

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

parsePackages :: String -> [ProfilePackage]
parsePackages = map getProfilePackage . lines . stripComments

readPackages :: FilePath -> IO [ProfilePackage]
readPackages f = fmap parsePackages (strictReadFile f)

data ProfilePackage  =  ProfilePackage
                          {
                             pnegate  ::  Bool,
                             psystem  ::  Bool,
                             depatom  ::  DepAtom
                          }
  deriving (Show,Eq,Ord)

-- | Get a dependency atom which can be modified by an initial *,
--   indicating a base system package, and by an additional initial -,
--   indicating removal of a package from the profile.
getProfilePackage :: String -> ProfilePackage
getProfilePackage p   =  case parseProfilePackage p of
                           Left   e  ->  error $ "getProfilePackage: " ++ show e
                           Right  x  ->  x

parseProfilePackage   =  parse readProfilePackage "<-*depatom>"

readProfilePackage    =  do  neg  <-  optchar '-'
                             sys  <-  optchar '*'
                             d    <-  readDepAtom
                             return $ ProfilePackage neg sys d

-- | Merges profile packages; the first list is assumed to be okay
--   already (no duplicates, no negatives).
mergeProfilePackages :: [ProfilePackage] -> [ProfilePackage] -> [ProfilePackage]
mergeProfilePackages xs []  =  xs
mergeProfilePackages xs ys  =  S.elems $
                               foldl  (\s p -> if    pnegate p
                                               then  S.delete (p {pnegate = False}) s
                                               else  S.insert p s
                                      )
                                      (S.fromList xs)
                                      ys

profilePackages :: IO [ProfilePackage]
profilePackages = fmap  (foldl1 mergeProfilePackages)
                        (readProfileFile packages readPackages)
