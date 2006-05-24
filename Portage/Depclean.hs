{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Performing depclean calculation to determine packages that
    can safely be removed.
-}

module Portage.Depclean
  where

import Data.List
import qualified Data.Map as M

import Portage.PortageConfig
import Portage.Tree
import Portage.Ebuild
import Portage.World
import Portage.Dependency
import Portage.Match
import Portage.Package

depclean :: PortageConfig -> IO [PV]
depclean pc =
    do  let i = inst pc  -- tree of installed packages
        let all = concat . concat . map snd . M.toList . M.map (map snd . M.toList) . ebuilds $ i  -- flattened list of all installed variants
        world <- worldTarget
        let conservativeRdeps = depStringAtoms $ map Plain world ++ concatMap (rdepend . ebuild) all
        let flagged = concatMap (\d -> matchDepAtomTree d i) conservativeRdeps
        return $ map (pv . meta) (all \\ flagged)
