{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Graph generation monad.
-}

module Portage.GraphGeneration 
  (module Control.Monad.State, module Portage.GraphGeneration)
  where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Graph.Inductive hiding (version, Graph(), NodeMap())
import Control.Monad.State

import Portage.PortageConfig
import Portage.Config
import Portage.Use
import Portage.Dependency hiding (getDepAtom)
import Portage.Package
import Portage.Ebuild (Variant(..), Ebuild(iuse), EbuildMeta(..), EbuildOrigin(..), TreeLocation(..), Mask(..), Link(..), pvs)
import qualified Portage.Ebuild as E

type DGraph = Graph
type Graph = Gr [Action] DepType
data DepType = Depend        Bool DepAtom
             | RDepend       Bool DepAtom
             | PDepend       Bool DepAtom
             | Meta                        -- ^ meta-logic (e.g. build before available)
  deriving (Eq,Show)

data Action  =  Available    Variant
             |  Built        Variant
             |  Removed      Variant  -- ^ mainly for the future, reverse deps
             |  Top
             |  Bot

getVariant :: Action -> Maybe Variant
getVariant (Available v)  =  Just v
getVariant (Built v)      =  Just v
getVariant (Removed v)    =  Just v
getVariant _              =  Nothing

getDepAtom :: DepType -> Maybe DepAtom
getDepAtom (Depend _ a)   =  Just a
getDepAtom (RDepend _ a)  =  Just a
getDepAtom (PDepend _ a)  =  Just a
getDepAtom _              =  Nothing

-- | More efficient comparison for actions.
instance Eq Action where
  (Available    v1)  ==  (Available    v2)  =  pv (meta v1) == pv (meta v2)
  (Built        v1)  ==  (Built        v2)  =  pv (meta v1) == pv (meta v2)
  (Removed      v1)  ==  (Removed      v2)  =  pv (meta v1) == pv (meta v2)
  Top                ==  Top                =  True
  Bot                ==  Bot                =  True
  _                  ==  _                  =  False

instance Show Action where
  show (Available    v) = "A  " ++ showPV (pv (meta v))
  show (Built        v) = "B  " ++ showPV (pv (meta v))
  show (Removed      v) = "D  " ++ showPV (pv (meta v))
  show Top              = "/"
  show Bot              = "_"

showAction :: Config -> Action -> String
showAction c (Available    v) = "A  " ++ E.showVariant c v
showAction c (Built        v) = "B  " ++ E.showVariant c v
showAction c (Removed      v) = "D  " ++ E.showVariant c v
showAction c Top              = "/"
showAction c Bot              = "_"

data DepState =  DepState
                   {
                      pconfig   ::  PortageConfig,
                      dlocuse   ::  [UseFlag],
                      graph     ::  Graph,
                      labels    ::  Map PV NodeMap,
                      active    ::  Map PS PV,
                      counter   ::  Int,
                      callback  ::  Callback
                   }

type Callback = DepAtom -> NodeMap -> GG [Progress]
type NodeMap = (Int,Int,Int)

available, built, removed :: NodeMap -> Int
available  (b,a,r) = a
built      (b,a,r) = b
removed    (b,a,r) = r

top = 0
bot = 1

data Progress =  LookAtEbuild  PV EbuildOrigin
              |  AddEdge       Node Node DepType
              |  Message       String
  deriving (Eq,Show)

-- | Graph generation monad.
type GG = State DepState


isActive :: Variant -> Map PS PV -> Bool
isActive v m =  case M.lookup (extractPS . pvs $ v) m of
                  Nothing   ->  False
                  Just pv'  ->  (pv . meta $ v) == pv'

activate :: Variant -> GG ()
activate v = 
    do  a   <-  gets active
        ls  <-  gets labels
        let ps' = extractPS . pvs $ v
        let pv' = pv . meta $ v
        case M.lookup (extractPS . pvs $ v) a of
          Nothing   ->  do
                            let pv' = pv . meta $ v
                            modify (\s -> s { active = M.insert ps' pv' a })
                            modifyGraph ( insEdge (top, available (ls M.! pv'), Meta) )
          Just pv''
            | pv'' == pv'  ->  return ()
            | otherwise    ->  fail $ "Depgraph slot conflict: " ++ showPVS (pvs v) ++ " vs. " ++ showPV pv''


depend :: NodeMap -> DepAtom -> NodeMap -> GG [Progress]
depend source da target
    | blocking da =
        do
            modifyGraph (insEdges [  (built target,built source,Depend True da) ])
            return [AddEdge (built target) (built source) (Depend True da)]
    | otherwise =
        do
            modifyGraph (insEdges [  (built source,available target,Depend False da),
                                     (removed target,built source,Depend True da) ])
            return [AddEdge (built source) (available target) (Depend False da)]

rdepend :: NodeMap -> DepAtom -> NodeMap -> GG [Progress]
rdepend source da target
    | blocking da =
        do
           modifyGraph (insEdges [  (built target,removed source,RDepend True da) ])
           return [AddEdge (built target) (removed source) (RDepend True da)]
    | otherwise =
        do
           modifyGraph (insEdges [  (available source,available target,RDepend False da),
                                    (removed target,removed source,RDepend True da) ])
           return [AddEdge (available source) (available target) (RDepend False da)]

-- | Like 'rdepend' really, except for the 'DepType'. Could be unified.
pdepend :: NodeMap -> DepAtom -> NodeMap -> GG [Progress]
pdepend source da target
    | blocking da =
        do
           modifyGraph (insEdges [  (built target,removed source,PDepend True da) ])
           return [AddEdge (built target) (removed source) (PDepend True da)]
    | otherwise =
        do
           modifyGraph (insEdges [  (available source,available target,PDepend False da),
                                    (removed target,removed source,PDepend True da) ])
           return [AddEdge (available source) (available target) (PDepend False da)]

-- | Modify the graph within the monad.
modifyGraph :: (Graph -> Graph) -> GG ()
modifyGraph f = modify (\s -> s { graph = f (graph s) })

-- | Modify the counter within the monad.
stepCounter :: Int -> GG Int
stepCounter n = do  c <- gets counter
                    modify (\s -> s { counter = n + counter s })
                    return c

-- | Create a new node within the monad.
newNode :: GG Node
newNode =
    do  g <- gets graph
        return (head $ newNodes 1 g)

-- | Insert a set of new nodes into the graph, if it doesn't exist yet.
insNewNode :: Variant -> Bool -> GG NodeMap
insNewNode v a =
    do  ls <- gets labels
        case M.lookup (pv . meta $ v) ls of
          Nothing | a && (E.isAvailable . location $ meta v)  ->  insAvailableHistory v
                  | otherwise                                 ->  
                      case location . meta $ v of
                        PortageTree _ (Linked v') -> insUpgradeHistory v' v
                        _                         -> insInstallHistory v
          Just nm -> return nm

insAvailableHistory :: Variant -> GG NodeMap
insAvailableHistory v =
    do  n <- stepCounter 2
        let  a    =  n
             r    =  n + 1
             ps'  =  extractPS (pvs v)
             pv'  =  pv . meta $ v
             nm   =  (a,a,r)
        registerNode pv' nm
        modifyGraph (  insEdges [ (r,a,Meta),
                                  (bot,a,Meta),
                                  (r,top,Meta) ] .
                       insNodes [ (a,[Available v]),
                                  (r,[Removed v]) ])
        return nm

-- | An upgrade history can also be a downgrade history, or even a recompilation
--   history. The only common element is that one variant of one package is
--   replaced by another variant of the same package.
insUpgradeHistory :: Variant -> Variant -> GG NodeMap
insUpgradeHistory v v' =
    do  n <- stepCounter 4
        let  a    =  n
             b'   =  n + 1
             r    =  b'
             a'   =  n + 2
             r'   =  n + 3
             nm   =  (a,a,r)
             nm'  =  (b',a',r')
        registerNode (pv . meta $ v) nm
        registerNode (pv . meta $ v') nm'
        modifyGraph (  insEdges [ (r',a',Meta),
                                  (a',b',Meta),
                                  (r,a,Meta),
                                  (bot,a,Meta),
                                  (r',top,Meta) ] .
                       insNodes [ (b',[Built v',Removed v]),
                                  (a',[Available v']),
                                  (r',[Removed v']),
                                  (a,[Available v]) ])
        return nm'

insInstallHistory :: Variant -> GG NodeMap
insInstallHistory v =
    do  n <- stepCounter 3
        let  b    =  n
             a    =  n + 1
             r    =  n + 2
             pv'  =  pv . meta $ v
             nm   =  (b,a,r)
        registerNode pv' nm
        modifyGraph (  insEdges [ (r,a,Meta),
                                  (a,b,Meta),
                                  (b,bot,Meta),
                                  (r,top,Meta) ] .
                       insNodes [ (b,[Built v]),
                                  (a,[Available v]),
                                  (r,[Removed v]) ])
        return nm

registerNode :: PV -> NodeMap -> GG ()
registerNode pv nm = modify (\s -> s { labels = M.insert pv nm (labels s) })


