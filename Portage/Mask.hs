{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Masking (for instance, hardmask or keyword-mask) of packages.
-}

module Portage.Mask
  where

import System.IO.Unsafe
import Data.Char
import Data.List
import qualified Data.Set as S
import Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Token

import Portage.Dependency
import Portage.Utilities
import Portage.Tree
import Portage.Constants
import Portage.Config
import Portage.Profile
import Portage.Match
import Portage.Ebuild
import Portage.Keyword

data Masking  =  Masking
                   {
                      mreason   ::  [String],
                      mfile     ::  FilePath,
                      mdepatom  ::  DepAtom,
                      mnegate   ::  Bool
                   }
  deriving (Eq,Ord,Show)

-- | Parse a @package.mask@ (or @package.unmask@) file.
parseMask :: FilePath -> String -> [Masking]
parseMask f = parseMaskByLine f [] [] . lines

parseMaskByLine :: FilePath -> [String] -> [String] -> [String] -> [Masking]
parseMaskByLine f acc facc (l@('#':_) : ls)  =  let  nacc = l : acc
                                                in   parseMaskByLine f nacc (reverse nacc) ls
parseMaskByLine f acc facc (l@('-':_) : ls)  =  Masking facc f (getDepAtom l) True  : parseMaskByLine f [] facc ls
parseMaskByLine f acc facc (l : ls)
  | all isSpace l                            =  parseMaskByLine f [] [] ls
  | otherwise                                =  Masking facc f (getDepAtom l) False : parseMaskByLine f [] facc ls
parseMaskByLine f acc facc []                =  []


readMaskFile :: FilePath -> IO [Masking]
readMaskFile f = fmap (parseMask f) (strictReadFileIfExists f)

normalizeMasking :: [Masking] -> [Masking]
normalizeMasking xs  =  S.elems $
                        foldl  (\s m -> if mnegate m  then  S.delete m s
                                                      else  S.insert m s
                               )
                               (S.empty)
                               xs


globalMask   ::  Config -> IO [Masking]
profileMask  ::  IO [Masking]
userMask     ::  IO [Masking]
userUnMask   ::  IO [Masking]


globalMask cfg  =
    unsafeInterleaveIO $
    do  r <- findOverlayFile cfg (\pt -> profilesDir pt ./. packageMask) (readMaskFile) (++)
        return $ maybe [] normalizeMasking r

profileMask     =  unsafeInterleaveIO $ 
                   fmap (normalizeMasking . concat) (readProfileFile packageMask readMaskFile)

userMask        =  unsafeInterleaveIO $
                   fmap normalizeMasking (readMaskFile (localConfigDir ./. packageMask))

userUnMask      =  unsafeInterleaveIO $
                   fmap normalizeMasking (readMaskFile (localConfigDir ./. packageUnMask))


performProfilePackage :: ProfilePackage -> Tree -> Tree
performProfilePackage m@(ProfilePackage { pnegate = False, pdepatom = d }) =
    modifyTreeForNegDepAtom d (\v -> v { meta =  case meta v of
                                                  me -> me { masked = NotInProfile : masked me } })
performProfilePackage m@(ProfilePackage { pnegate = True }) =
    error "performProfilePackage: illegal negated profile package"

performMask :: Masking -> Tree -> Tree
performMask m@(Masking { mdepatom = d }) = 
    modifyTreeForDepAtom d (\v -> v { meta =  case meta v of
                                                me -> me { masked = HardMasked (mfile m) (mreason m) : masked me } })

performUnMask :: Masking -> Tree -> Tree
performUnMask m@(Masking { mdepatom = d }) =
    modifyTreeForDepAtom d (\v -> v { meta =  case meta v of
                                                me -> me { masked = filter notHardMasked (masked me) } })
  where
    notHardMasked (HardMasked _ _)  =  False
    notHardMasked _                 =  True

parsePackages :: String -> [ProfilePackage]
parsePackages = map getProfilePackage . lines . stripComments

readPackages :: FilePath -> IO [ProfilePackage]
readPackages f = fmap parsePackages (strictReadFile f)

data ProfilePackage  =  ProfilePackage
                          {
                             pnegate   ::  Bool,
                             psystem   ::  Bool,
                             pdepatom  ::  DepAtom
                          }
  deriving (Show,Eq,Ord)

-- | Get a dependency atom which can be modified by an initial @*@,
--   indicating a base system package, and by an additional initial @-@,
--   indicating removal of a package from the profile.
getProfilePackage :: String -> ProfilePackage
getProfilePackage p   =  case parseProfilePackage p of
                           Left   e  ->  error $ "getProfilePackage: " ++ show e
                           Right  x  ->  x

parseProfilePackage   =  parse readProfilePackage "<-*depatom>"

readProfilePackage    =  do  neg  <-  optchar '-'
                             sys  <-  optchar '*'
                             d    <-  readDepAtom (const [])
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
profilePackages =  unsafeInterleaveIO $
                   fmap  (foldl1 mergeProfilePackages)
                         (readProfileFile packages readPackages)


-- | Tree traversal that performs keyword-based masking. To be used with
--   traverseTree.
keywordMask :: Config -> Variant -> Variant
keywordMask cfg v@(Variant m e) =  let  ekey      =  keywords e
                                        finalkey  =  mergeKeywords (acceptedKeywords cfg) (lockey m)
                                   in   if null (intersect ekey finalkey)
                                        then  Variant m { masked = KeywordMasked (archFilter (arch cfg) ekey) : masked m } e
                                        else  v
