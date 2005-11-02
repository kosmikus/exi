{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Generation of dependency graph.
-}

module Portage.Graph
  where

import Data.List (sortBy)
import Data.Graph.Inductive hiding (version, Graph())
import Control.Monad.Identity
import Control.Monad.State

import Portage.Tree
import Portage.Match
import Portage.Dependency
import Portage.Ebuild
import Portage.Package
import Portage.PortageConfig
import Portage.Config
import Portage.Use

findVersions :: Tree -> DepAtom -> [Variant]
findVersions = flip matchDepAtomTree

showVariant :: Config -> Variant -> String
showVariant cfg (Variant m e)  =  showPV (pv m) ++ showLocation (location m) 
                                  ++ " " ++ unwords (map showMasked (masked m))
                                  ++ "\n" ++ concatMap hardMask (masked m) ++ unwords (diffUse (use cfg) (iuse e))

showLocation :: TreeLocation -> String
showLocation Installed = " (installed)"
showLocation (Provided f) = " (provided in " ++ f ++ ")"
showLocation (PortageTree t) = " [" ++ t ++ "]"

hardMask :: Mask -> String
hardMask (HardMasked f r) = unlines r
hardMask _                = ""

showMasked :: Mask -> String
showMasked (KeywordMasked xs) = "(masked by keyword: " ++ show xs ++ ")"
showMasked (HardMasked f r) = "(hardmasked in " ++ f ++ ")"
showMasked (ProfileMasked f) = "(excluded from profile in " ++ f ++")"
showMasked (Shadowed t) = "(shadowed by " ++ showLocation t ++ ")"

data Selection  =  Accept   Variant
                |  Reject   Failure

data Failure  =  AllMasked Category Package [Variant]



-- x :: Graph -> DepString -> Graph
-- y :: Graph -> DepTerm -> Graph
-- z :: Graph -> DepAtom -> Graph

type DGraph = Graph
type Graph = Gr Action DepType
data DepType = Normal | Runtime | RPost

data Action  =  Build      Variant
             |  Available  Variant
             |  Block      Variant
             |  Fail       Failure

data DepState =  DepState
                   {
                      pconfig  ::  PortageConfig,
                      dlocuse  ::  [UseFlag],
                      graph    ::  Graph
                   }

-- | Graph generation monad.
type GG = State DepState

modifyGraph :: (Graph -> Graph) -> GG ()
modifyGraph f = modify (\s -> s { graph = f (graph s) })

buildGraphForDepString :: DepString -> GG ()
buildGraphForDepString = mapM_ buildGraphForDepTerm

buildGraphForDepTerm :: DepTerm -> GG ()
buildGraphForDepTerm dt =
    do  luse <- gets dlocuse
        case dt of
          Plain d                   ->  buildGraphForDepAtom d
          Use n f ds
            | n /= (f `elem` luse)  ->  buildGraphForDepString ds
            | otherwise             ->  return ()

buildGraphForDepAtom :: DepAtom -> GG ()
buildGraphForDepAtom da =
    do  pc <- gets pconfig
        let s          =  strategy pc
            t          =  itree pc
            (cat,pkg)  =  catpkgFromDepAtom da
        case sselect s cat pkg (findVersions t da) of
          Accept v@(Variant m e)  ->  
            let  available  =  isAvailable (location m)
                 stop       =  available && sstop s v -- if it's an installed ebuild, we can decide to stop here!
            in                     let  rdeps    =  rdepend  e
                                        deps     =  depend   e
                                        pdeps    =  pdepend  e
                                        luse     =  mergeUse (use (config pc)) (locuse m)
                                   in   -- set new local USE context
                                        withState (\s -> s { dlocuse = luse }) $
                                        do
                                            -- add deps to graph
                                            buildGraphForDepString deps
                                            -- add "build v" to graph
                                            -- add "remove v of same slot (if exists)" to graph
                                            -- add rdeps to graph
                                            buildGraphForDepString rdeps
                                            -- add pdeps to graph
                                            buildGraphForDepString pdeps

strategy :: PortageConfig -> Strategy
strategy = const updateStrategy

data Strategy =  Strategy
                   {
                      -- | Select the best variant for a given package.
                      sselect  ::  Category -> Package -> [Variant] -> Selection,
                      -- | Decide whether to stop on an already available variant.
                      sstop    ::  Variant -> Bool
                   }

updateStrategy :: Strategy
updateStrategy =  Strategy
                    {
                       sselect  =  select,
                       sstop    =  const True
                    }
  where
    select :: Category -> Package -> [Variant] -> Selection
    select cat pkg vs =
      case sortBy (\(Variant m1 _) (Variant m2 _) -> compare (version (pv m2)) (version (pv m1))) . filterMaskedVariants $ vs of
        (v:_)  ->  Accept v
        []     ->  Reject (AllMasked cat pkg vs)
