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
import Portage.Ebuild (Variant(..), Ebuild(iuse), EbuildMeta(..), TreeLocation(..), Mask(..))
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
  deriving (Eq,Show)

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

instance Show Action where
  show (Unavailable  v) = "NA " ++ showPV (pv (meta v))
  show (Available    v) = "A  " ++ showPV (pv (meta v))
  show (Unblocked    v) = "U< " ++ showPV (pv (meta v))
  show (Build        v) = "B  " ++ showPV (pv (meta v))
  show (Unblocking   v) = "U> " ++ showPV (pv (meta v))
  show Top              = "/"

isAvailable :: Variant -> Map PV NodeInfo -> Bool
isAvailable v m =  case M.lookup (pv (meta v)) m of
                     Nothing  ->  False
                     Just i   ->  active i

data DepState =  DepState
                   {
                      pconfig   ::  PortageConfig,
                      dlocuse   ::  [UseFlag],
                      graph     ::  Graph,
                      labels    ::  Map PV NodeInfo,
                      counter   ::  Int,
                      dcontext  ::  NodeContext
                   }

data NodeInfo =  NodeInfo
                   {  
                      node     ::  Node,
                      active   ::  Bool
                   }

data NodeContext =  NodeContext
                      {
                         base         ::  Node,
                         deptype      ::  DepType,
                         source       ::  Int,
                         target       ::  Int,
                         blocktarget  ::  Int
                      }

type Progress = String

depend :: Node -> NodeContext
depend b   =  NodeContext
                {
                   base         =  b,
                   deptype      =  Normal,
                   source       =  build,
                   target       =  available,
                   blocktarget  =  unblocked
                }

rdepend :: Node -> NodeContext
rdepend b  =  NodeContext
                {
                   base         =  b,
                   deptype      =  Runtime,
                   source       =  available,
                   target       =  available,
                   blocktarget  =  unavailable
                }

pdepend :: Node -> NodeContext
pdepend b  =  NodeContext
                {
                   base         =  b,
                   deptype      =  Post,
                   source       =  available,
                   target       =  build,
                   blocktarget  =  unavailable
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
insNewNode :: Variant -> Bool -> GG Node
insNewNode v a =
    do  ls <- gets labels
        let pv' = pv (meta v)
        case M.lookup pv' ls of
          Nothing ->  do  n <- stepCounter
                          registerNode pv' n
                          modifyGraph (insEdges [  (s,t,Meta)
                                                |  (s,t) <- 
                                                   (unavailable  `at` n, available   `at` n) :
                                                   if a then
                                                   [  (available    `at` n, build       `at` n)]
                                                   else
                                                   [  (available    `at` n, unblocked   `at` n),
                                                      (unblocked    `at` n, build       `at` n),
                                                      (build        `at` n, unblocked   `at` n)] ] .
                                       insNodes (  [  (unavailable  `at` n, Unavailable  v),
                                                      (available    `at` n, Available    v),
                                                      (unblocked    `at` n, Unblocked    v)] ++
                                                   (  if a then
                                                      [  (build        `at` n, Available    v)]
                                                      else
                                                      [  (build        `at` n, Build        v)]) ++
                                                   [  (unblocking   `at` n, Unblocking   v)]))
                          return n
          Just (NodeInfo n _) -> return n

activate :: Variant -> GG ()
activate v = modify (\s -> s { labels = M.update (\i -> Just $ i { active = True }) (pv (meta v)) (labels s) })

registerNode :: PV -> Node -> GG ()
registerNode pv n = modify (\s -> s { labels = M.insert pv (NodeInfo n False) (labels s) })

-- | Builds a graph for the whole dependency string,
--   by processing each term separately.
buildGraphForDepString :: DepString -> GG [Progress]
buildGraphForDepString ds = fmap concat (mapM buildGraphForDepTerm ds)

-- | Builds a graph for a single term.
--   If it is an atom, just delegate.
--   If it is a use-qualified thing, check the USE flag and delegate.
--   If it is an or-dependency, ...
buildGraphForDepTerm :: DepTerm -> GG [Progress]
buildGraphForDepTerm dt =
    do  luse <- gets dlocuse
        case dt of
          Plain d                   ->  buildGraphForDepAtom d
          Use n f ds
            | n /= (f `elem` luse)  ->  buildGraphForDepString ds
            | otherwise             ->  return []
          Or []                     ->  return []  -- preliminary!
          Or (dt:_)                 ->  buildGraphForDepTerm dt

buildGraphForDepAtom :: DepAtom -> GG [Progress]
buildGraphForDepAtom da =
    do  pc  <-  gets pconfig
        g   <-  gets graph
        ls  <-  gets labels
        let s          =  strategy pc
            t          =  itree pc
            (cat,pkg)  =  catpkgFromDepAtom da
        case sselect s cat pkg (findVersions t da) of
          Reject f -> return []  -- fail (show f)
          Accept v@(Variant m e)  ->  
            let  avail      =  E.isAvailable (location m)  -- installed or provided?
                 already    =  isAvailable v ls
                 stop       =  avail && sstop s v 
                                 -- if it's an installed ebuild, we can decide to stop here!
            in   if already
                   then  return []
                   else            let  rdeps    =  E.rdepend  e
                                        deps     =  E.depend   e
                                        pdeps    =  E.pdepend  e
                                        luse     =  mergeUse (use (config pc)) (locuse m)
                                   in   -- set new local USE context
                                        withState (\s -> s { dlocuse = luse }) $
                                        do
                                            -- insert nodes for v, and activate
                                            n <- insNewNode v stop
                                            activate v
                                            -- insert edge according to current context
                                            ctx <- gets dcontext
                                            modifyGraph (insEdge (  source ctx `at` base ctx, 
                                                                    target ctx `at` n,
                                                                    deptype ctx))
                                            if stop
                                              then return [show n ++ ": " ++ showPV (pv (meta v))]
                                              else do
                                                       -- add deps to graph
                                                       p1 <- withState (\s -> s { dcontext = depend n }) $
                                                               buildGraphForDepString deps
                                                       -- add rdeps to graph
                                                       p2 <- withState (\s -> s { dcontext = rdepend n }) $
                                                               buildGraphForDepString rdeps
                                                       -- add pdeps to graph
                                                       p3 <- withState (\s -> s { dcontext = pdepend n }) $
                                                               buildGraphForDepString pdeps
                                                       return ([show n ++ ": " ++ showPV (pv (meta v)) ++ "PDEPEND: " ++ show pdeps] ++ p1 ++ p2 ++ p3)

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
      case sortBy (\(Variant m1 _) (Variant m2 _) -> compare (version (pv m2)) (version (pv m1))) . E.filterMaskedVariants $ vs of
        (v:_)  ->  Accept v
        []     ->  Reject (AllMasked cat pkg vs)
