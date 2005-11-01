{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Generation of dependency graph.
-}

module Portage.Graph
  where

import Data.Graph.Inductive

import Portage.Tree
import Portage.Match
import Portage.Dependency
import Portage.Ebuild
import Portage.Package
import Portage.PortageConfig

findVersions :: Tree -> DepAtom -> [Variant]
findVersions = flip matchDepAtomTree

showVariant :: Variant -> String
showVariant (Variant m e)  =  showPV (pv m) ++ showLocation (location m) 
                              ++ " " ++ unwords (map showMasked (masked m))

showLocation :: TreeLocation -> String
showLocation Installed = " (installed)"
showLocation (PortageTree t) = " [" ++ t ++ "]"

showMasked :: Mask -> String
showMasked KeywordMasked = "(masked by keyword)"
showMasked (HardMasked f r) = "(hardmasked in " ++ f ++ ")"
showMasked (ProfileMasked f) = "(excluded from profile in " ++ f ++")"
showMasked (Shadowed t) = "(shadowed by " ++ showLocation t ++ ")"


