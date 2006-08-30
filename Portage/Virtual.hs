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
import qualified Portage.Constants as C
import Portage.PortageConfig.Type

data Virtual  =  Virtual
                   {
                      vp         ::  P,
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
      (cp:ds)  ->  let  p = getP cp
                   in   Virtual p (map getDepAtom ds)

readVirtuals :: FilePath -> IO [Virtual]
readVirtuals f = fmap parseVirtuals (strictReadFile f)

profileVirtuals :: IO [Virtual]
profileVirtuals  =  unsafeInterleaveIO $
                    fmap concat (readProfileFile C.virtuals readVirtuals)

-- | Compute the virtuals that are provided by a tree.
providedByTree :: Tree -> Map P [DepAtom]
providedByTree t =  (  M.fromListWith (++) . 
                       concatMap providedBy . concat . concatMap (M.elems) .
                       M.elems) (ebuilds t)
  where
    -- There used to be a bug in |addProvide|, because the call to |depStringAtoms| was
    -- missing, as interpreted PROVIDE strings can still contain |And| constructors. Note
    -- that we implicitly assume that at this point, there are no |Or|s or |Use|s left ...
    providedBy :: Variant -> [(P,[DepAtom])]
    providedBy (Variant m e) =  let  da = [depAtomFromPV (pv m)]
                                in   map  (\p -> (p,da))
                                          (map pFromDepAtom . depStringAtoms . provide $ e)
    -- We ignore the version:
    depAtomFromPV :: PV -> DepAtom
    depAtomFromPV (PV cat pkg _) = DepAtom False False DNONE cat pkg NoVer

-- | Compute the dependency that corresponds to each virtual. This function
--   is supposed to be partially applied to the first two arguments.
computeVirtuals :: [Virtual] -> Tree -> (DepAtom -> Maybe DepTerm)
computeVirtuals vs t =
    let
      -- add the virtuals as defaults to the map
      vm :: Map P [DepAtom]
      vm = foldr (\(Virtual p ds) -> updateWithDefault (\x -> Just ( ds ++ nub x)) p []) (providedByTree t) vs
      -- note that there can still be duplicates in the map, but we avoid doing a nub on the whole thing for
      -- efficiency reasons
    in
      \d ->  fmap  (\cp -> (if blocking d then And else Or) (map (Plain . (if blocking d then block else id) . mergeWithTemplate d) cp))
                   (M.lookup (pFromDepAtom d) vm)

-- | The first atom is the virtual. It serves as a template w.r.t. modifiers,
--   version etc.
mergeWithTemplate :: DepAtom -> DepAtom -> DepAtom
mergeWithTemplate (DepAtom _ False DNONE _ _ _) d = d
mergeWithTemplate (DepAtom n r m _ _ v) (DepAtom _ _ _ c p _) = DepAtom n r m c p v

-- | Looks up a virtual depatom in the precomputed table
--   of virtuals. Virtuals are usually mapped to or-dependencies.
resolveVirtuals :: PortageConfig -> DepTerm -> DepTerm
resolveVirtuals pc (Plain d)  =  maybe (Plain d) id (virtuals pc d)
resolveVirtuals _  dt         =  dt

-- Note about efficiency: The default of a virtual is cheap to compute,
-- the rest is expensive. Laziness should take care that the expensive
-- part of the computation is only performed when the default of a virtual
-- is not installed.

