{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Generation of dependency graph.
-}

module Portage.Graph
  where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List (sortBy)
import Data.Graph.Inductive hiding (version, Graph())
import Control.Monad.Identity
import Control.Monad.State

import Portage.Tree
import Portage.Match
import Portage.Dependency
import Portage.Ebuild hiding (isAvailable)
import qualified Portage.Ebuild as E
import Portage.Package
import Portage.PortageConfig
import Portage.Config
import Portage.Use

findVersions :: Tree -> DepAtom -> [Variant]
findVersions = flip matchDepAtomTree

showVariant :: Config -> Variant -> String
showVariant cfg (Variant m e)  =  showPV (pv m) ++ showLocation (location m) 
                                  ++ " " ++ unwords (map showMasked (masked m))
                                  ++ "\n" ++ concatMap hardMask (masked m) ++ unwords (diffUse (mergeUse (use cfg) (locuse m)) (iuse e))

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
  deriving (Eq,Show)


-- x :: Graph -> DepString -> Graph
-- y :: Graph -> DepTerm -> Graph
-- z :: Graph -> DepAtom -> Graph

type DGraph = Graph
type Graph = Gr Action DepType
data DepType = Normal | Runtime | Post | Meta

data Action  =  Unavailable  Variant
             |  Available    Variant
             |  Unblocked    Variant
             |  Build        Variant
             |  Unblocking   Variant
             |  Fail         Failure
             |  Top

-- | More efficient comparison for actions.
instance Eq Action where
  (Unavailable  v1)  ==  (Unavailable  v2)  =  pv (meta v1) == pv (meta v2)
  (Available    v1)  ==  (Available    v2)  =  pv (meta v1) == pv (meta v2)
  (Unblocked    v1)  ==  (Unblocked    v2)  =  pv (meta v1) == pv (meta v2)
  (Build        v1)  ==  (Build        v2)  =  pv (meta v1) == pv (meta v2)
  (Unblocking   v1)  ==  (Unblocking   v2)  =  pv (meta v1) == pv (meta v2)
  Top                ==  Top                =  True
  _                  ==  _                  =  False

isAvailable :: Variant -> Map PV NodeInfo -> Bool
isAvailable v m =  case M.lookup (pv (meta v)) m of
                     Nothing  ->  False
                     Just i   ->  active i

data DepState =  DepState
                   {
                      pconfig  ::  PortageConfig,
                      dlocuse  ::  [UseFlag],
                      graph    ::  Graph,
                      labels   ::  Map PV NodeInfo,
                      counter  ::  Int
                   }

data NodeInfo =  NodeInfo
                   {  node     ::  Node,
                      active   ::  Bool
                   }

unavailable  =  0
available    =  1
unblocked    =  2
build        =  3
unblocking   =  4

offset       =  5

at = (+)

-- | Graph generation monad.
type GG = State DepState

-- | Modify the graph within the monad.
modifyGraph :: (Graph -> Graph) -> GG ()
modifyGraph f = modify (\s -> s { graph = f (graph s) })

-- | Modify the counter within the monad.
stepCounter :: GG Int
stepCounter = do  c <- gets counter
                  modify (\s -> s { counter = 5 + counter s })
                  return c

-- | Create a new node within the monad.
newNode :: GG Node
newNode =
    do  g <- gets graph
        return (head $ newNodes 1 g)

-- | Insert a set of new nodes into the graph, if it doesn't exist yet.
insNewNode :: Variant -> GG Node
insNewNode v =
    do  ls <- gets labels
        let pv' = pv (meta v)
        case M.lookup pv' ls of
          Nothing ->  do  n <- stepCounter
                          modifyGraph (insEdges [  (s,t,Meta)
                                                |  (s,t) <- 
                                                   [(unavailable  `at` n, available   `at` n),
                                                    (available    `at` n, unblocked   `at` n),
                                                    (unblocked    `at` n, build       `at` n),
                                                    (build        `at` n, unblocked   `at` n)] ] .
                                       insNodes [  (unavailable  `at` n, Unavailable  v),
                                                   (available    `at` n, Available    v),
                                                   (unblocked    `at` n, Unblocked    v),
                                                   (build        `at` n, Build        v),
                                                   (unblocking   `at` n, Unblocking   v)])
                          return n
          Just (NodeInfo n _) -> return n

activate :: Variant -> GG ()
activate v = modify (\s -> s { labels = M.update (\i -> Just $ i { active = True }) (pv (meta v)) (labels s) })

-- | Builds a graph for the whole dependency string,
--   by processing each term separately.
buildGraphForDepString :: DepString -> GG [Node]
buildGraphForDepString = fmap concat . mapM buildGraphForDepTerm

-- | Builds a graph for the whole dependency string,
--   and adds edges as specified by the given arguments.
buildGraphForDepStringWith :: DepType -> Int -> Int -> Node -> DepString -> GG ()
buildGraphForDepStringWith dt s t m ds =
    do  ns <- buildGraphForDepString ds
        modifyGraph (insEdges [(s `at` m, t `at` n, dt) | n <- ns])

-- | Builds a graph for a single term.
--   If it is an atom, just delegate.
--   If it is a use-qualified thing, check the USE flag and delegate.
--   If it is an or-dependency, ...
buildGraphForDepTerm :: DepTerm -> GG [Node]
buildGraphForDepTerm dt =
    do  luse <- gets dlocuse
        case dt of
          Plain d                   ->  fmap (:[]) (buildGraphForDepAtom d)
          Use n f ds
            | n /= (f `elem` luse)  ->  buildGraphForDepString ds
            | otherwise             ->  return []

buildGraphForDepAtom :: DepAtom -> GG Node
buildGraphForDepAtom da =
    do  pc  <-  gets pconfig
        g   <-  gets graph
        ls  <-  gets labels
        let s          =  strategy pc
            t          =  itree pc
            (cat,pkg)  =  catpkgFromDepAtom da
        case sselect s cat pkg (findVersions t da) of
          Accept v@(Variant m e)  ->  
            let  avail      =  E.isAvailable (location m)  -- installed or provided?
                 already    =  isAvailable v ls
                 stop       =  already || avail && sstop s v 
                                 -- if it's an installed ebuild, we can decide to stop here!
            in                     let  rdeps    =  rdepend  e
                                        deps     =  depend   e
                                        pdeps    =  pdepend  e
                                        luse     =  mergeUse (use (config pc)) (locuse m)
                                   in   -- set new local USE context
                                        withState (\s -> s { dlocuse = luse }) $
                                        do
                                            -- insert nodes for v, and activate
                                            n <- insNewNode v
                                            activate v
                                            -- add deps to graph
                                            buildGraphForDepStringWith Normal build available n deps
                                            -- add rdeps to graph
                                            buildGraphForDepStringWith Runtime available available n rdeps
                                            -- add pdeps to graph
                                            buildGraphForDepStringWith Runtime available build n pdeps
                                            return n

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
