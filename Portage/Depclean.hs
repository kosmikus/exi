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
        let gnodes   =  Nothing : map Just all  -- all nodes plus world node
        putStrLn $ "building graph with " ++ show (length gnodes) ++ " nodes"
        let gedges   =  nub $
                        handleDeps Nothing (world pc ++ system pc) ++
                        concatMap
                          (\v -> handleDeps  (Just v)
                                             (  let  e = ebuild v
                                                in   rdepend e ++ pdepend e ++
                                                     if rdepclean then [] else depend e))
                          all
        putStrLn $ "there are " ++ show (length gedges) ++ " edges"
        -- putStrLn $ unlines $ map show $ map (\ (v1,v2,()) -> let { m (Just v) = showPVS (pvs v); m Nothing = "world" } in (m v1, m v2)) gedges
        let (g,nm)  =  mkMapGraph gnodes gedges
        putStrLn $ nodes g `seq` "done"
        -- print g
        let flagged' = reachable (fst $ mkNode_ nm Nothing) (g :: Gr (Maybe Variant) ())
        putStrLn $ "flagged " ++ show (length flagged') ++ " nodes"
        let flagged = map (fromJust) (filter isJust (map (fromJust . lab g) flagged'))
        return (all \\ flagged)
  where
    i = inst pc
    handleDeps :: Maybe Variant -> DepString -> [(Maybe Variant, Maybe Variant, ())]
    handleDeps s ds =
      map (\t -> (s,Just t,())) $
      concatMap (\d -> matchDepAtomTree d i) $
      depStringAtoms $
      -- We keep the virtual itself during resolution of virtuals, because there
      -- are new-style virtual packages which are still PROVIDEd. assuming worst
      -- case, the PROVIDEs could have been installed later than the new-style virtual,
      -- and of course we don't know if the new-style virtual doesn't provide any
      -- functionality itself, so it shouldn't be removed.
      concatMap (\v -> let pv = Plain v in [pv,resolveVirtuals pc pv]) $
      depStringAtoms ds

depclean rdepclean pc =
    do  vs  <-  depcleanGr rdepclean pc
        putStr (unlines (map (showUnmergeLine pc) vs))

-- | Is similar to |showMergeLine| from |Portage.Merge| and to |showStatus|
--   from |Portage.Ebuild|. Refactoring necessary ...

showUnmergeLine :: PortageConfig -> Variant -> String
showUnmergeLine pc v =
    inColor (config pc) Red True Default "X " ++
    " " ++ showVariant pc v