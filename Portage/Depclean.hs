{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Performing depclean calculation to determine packages that
    can safely be removed.
-}

module Portage.Depclean
  where

import Data.Graph.Inductive
import Data.List
import Data.Maybe
import qualified Data.Map as M

import Portage.PortageConfig
import Portage.Tree
import Portage.Ebuild
import Portage.World
import Portage.Dependency
import Portage.Match
import Portage.Package
import Portage.Virtual
import Portage.AnsiColor

-- Assumes that dependencies of installed packages are already interpreted,
-- look in |Portage.Ebuild| in function |getInstalledVariantFromDisk|.

-- Why we need a graph:
--
-- First, think of cyclic dependency groups that aren't referenced from world
-- or system. All dependencies of all installed packages do currently count,
-- that's bad.
--
-- Second, we'd need a fixpoint computation otherwise.
--
-- How to proceed? We populate a graph with all installed packages. We enter
-- dependencies as arcs (as before) between the nodes. We create a world meta-node
-- and add system and world depencies. Finally, we check what isn't reachable
-- from the world node.

depcleanGr rdepclean pc =
    do  let all = concat . concat . map snd . M.toList . M.map (map snd . M.toList) . ebuilds $ i  -- flattened list of all installed variants
        putStrLn $ "building graph with " ++ show (length all) ++ " nodes"
        let (g,nm)  =  mkMapGraph
                         (Nothing : map Just all)  -- all nodes plus world node
                         (handleDeps Nothing (world pc ++ system pc) ++ concatMap (\v -> handleDeps (Just v) (let e = ebuild v in rdepend e ++ pdepend e ++ if rdepclean then [] else depend e)) all)
        putStrLn $ nodes g `seq` "done"
        let flagged' = reachable (fst $ mkNode_ nm Nothing) (g :: Gr (Maybe Variant) ())
        putStrLn $ "flagged " ++ show (length flagged') ++ " nodes"
        let flagged = map (fromJust) (filter isJust (map (fromJust . lab g) flagged'))
        return (all \\ flagged)
  where
    i = inst pc
    handleDeps :: Maybe Variant -> DepString -> [(Maybe Variant, Maybe Variant, ())]
    handleDeps s ds = map (\t -> (s,Just t,())) $ concatMap (\d -> matchDepAtomTree d i) $ depStringAtoms $ map (resolveVirtuals pc . Plain) $ depStringAtoms ds

{-
depclean :: Bool -> PortageConfig -> IO [Variant]
depclean rdepclean pc =
    do  let i = inst pc  -- tree of installed packages
        let all = concat . concat . map snd . M.toList . M.map (map snd . M.toList) . ebuilds $ i  -- flattened list of all installed variants
        depcleanfp rdepclean all [] pc
{- (non-fixpoint version)
        let conservativeDeps = depStringAtoms $ map (resolveVirtuals pc . Plain) $ depStringAtoms $ world pc ++ system pc ++ concatMap (\v -> let e = ebuild v in rdepend e ++ pdepend e ++ if rdepclean then [] else depend e) all
        let flagged = concatMap (\d -> matchDepAtomTree d i) conservativeDeps
        return $ all \\ flagged
-}

-- depcleanfp :: Bool -> PortageConfig -> IO [Variant]
depcleanfp rdepclean all removed pc =
    do  let i = inst pc
        putStrLn $ "started with " ++ show (length all) ++ " packages"
        let conservativeDeps = depStringAtoms $ map (resolveVirtuals pc . Plain) $ depStringAtoms $ world pc ++ system pc ++ concatMap (\v -> let e = ebuild v in rdepend e ++ pdepend e ++ if rdepclean then [] else depend e) all
        let flagged = concatMap (\d -> matchDepAtomTree d i) conservativeDeps
        let removable = all \\ flagged
        putStrLn $ "removing " ++ show (length removable) ++ " packages"
        if null removable then return removed else depcleanfp rdepclean (intersect all flagged) (removable ++ removed) pc
-}

depclean rdepclean pc =
    do  vs  <-  depcleanGr rdepclean pc
        putStr (unlines (map (showUnmergeLine pc) vs))

-- | Is similar to |showMergeLine| from |Portage.Merge| and to |showStatus|
--   from |Portage.Ebuild|. Refactoring necessary ...

showUnmergeLine :: PortageConfig -> Variant -> String
showUnmergeLine pc v =
    inColor (config pc) Red True Default "X " ++
    " " ++ showVariant pc v