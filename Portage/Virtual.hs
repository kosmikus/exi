{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Handling of virtual packages.
-}

module Portage.Virtual
  where

import System.IO.Unsafe
import Data.Map (Map(..))
import Data.List
import qualified Data.Map as M

import Portage.Utilities
import Portage.Package
import Portage.Tree
import Portage.Ebuild
import Portage.Dependency
import Portage.Profile
import Portage.Constants

data Virtual  =  Virtual
                   {
                      vcategory  ::  Category,
                      vpackage   ::  Package,
                      vdefaults  ::  [DepAtom]
                   }
  deriving (Show,Eq)

-- | Parse a @virtuals@ file.
parseVirtuals :: String -> [Virtual]
parseVirtuals = map parseVirtualLine . lines . stripComments

parseVirtualLine :: String -> Virtual
parseVirtualLine x =
    case words x of
      []       ->  error $ "parseVirtualLine: internal error, empty line in virtuals file"
      (cp:ds)  ->  let  (cat,pkg) = getCatPkg cp
                   in   Virtual cat pkg (map getDepAtom ds)

readVirtuals :: FilePath -> IO [Virtual]
readVirtuals f = fmap parseVirtuals (strictReadFile f)

profileVirtuals :: IO [Virtual]
profileVirtuals  =  unsafeInterleaveIO $
                    fmap concat (readProfileFile virtuals readVirtuals)

-- | Compute the virtuals that are provided by a tree.
providedByTree :: Tree -> Map (Category,Package) [DepAtom]
providedByTree t = M.fold (\p r -> M.fold (\vs s -> foldr addProvide s vs) r p) M.empty (ebuilds t)
  where
    addProvide :: Variant -> Map (Category,Package) [DepAtom] -> Map (Category,Package) [DepAtom]
    addProvide (Variant m e) t =  case provide e of
                                    Just d   ->  updateWithDefault (Just . (depAtomFromPV (pv m):)) (catpkgFromDepAtom d) [] t
                                    Nothing  ->  t
    -- We ignore the version:
    depAtomFromPV :: PV -> DepAtom
    depAtomFromPV (PV cat pkg _) = DepAtom False False DNONE cat pkg NoVer

-- | Compute the dependency that corresponds to each virtual. This function
--   is supposed to be partially applied to the first two arguments.
computeVirtuals :: [Virtual] -> Tree -> (DepAtom -> Maybe DepTerm)
computeVirtuals vs t =
    let
      -- add the virtuals as defaults to the map
      vm :: Map (Category,Package) [DepAtom]
      vm = foldr (\(Virtual cat pkg ds) -> updateWithDefault (\x -> Just ( ds ++ nub x)) (cat,pkg) []) (providedByTree t) vs
      -- note that there can still be duplicates in the map, but we avoid doing a nub on the whole thing for
      -- efficiency reasons
    in
      \d ->  fmap  (\cp -> Or (map (Plain . mergeWithTemplate d) cp))
                   (M.lookup (catpkgFromDepAtom d) vm)

-- | The first atom is the virtual. It serves as a template w.r.t. modifiers,
--   version etc.
mergeWithTemplate :: DepAtom -> DepAtom -> DepAtom
mergeWithTemplate (DepAtom _ False DNONE _ _ _) d = d
mergeWithTemplate (DepAtom n r m _ _ v) (DepAtom _ _ _ c p _) = DepAtom n r m c p v

-- Note about efficiency: The default of a virtual is cheap to compute,
-- the rest is expensive. Laziness should take care that the expensive
-- part of the computation is only performed when the default of a virtual
-- is not installed.

