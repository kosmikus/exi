{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Graph generation monad.
-}

module Portage.GraphGeneration 
  (module Portage.GraphGeneration)
  where

import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Graph.Inductive hiding (version, Graph(), NodeMap())
import Control.Monad (when)

import Portage.Match
import Portage.PortageConfig
import Portage.Config
import Portage.Use
import Portage.Dependency hiding (getDepAtom)
import Portage.Package
import Portage.Ebuild (Variant(..), Ebuild(iuse), EbuildMeta(..), EbuildOrigin(..), TreeLocation(..), Mask(..), Link(..), pvs)
import qualified Portage.Ebuild as E
import Portage.Strategy

type DGraph = Graph
type Graph = Gr [Action] DepType
data DepType = Depend        Bool DepAtom
             | RDepend       Bool DepAtom
             | PDepend       Bool DepAtom
             | Meta                        -- ^ meta-logic
  deriving (Eq,Show)

data Action  =  Available    Variant       -- ^ used for PDEPENDs
             |  Built        Variant
             |  Top

getVariant :: Action -> Maybe Variant
getVariant (Available v)  =  Just v
getVariant (Built v)      =  Just v
getVariant _              =  Nothing

getDepAtom :: DepType -> Maybe DepAtom
getDepAtom (Depend _ a)   =  Just a
getDepAtom (RDepend _ a)  =  Just a
getDepAtom (PDepend _ a)  =  Just a
getDepAtom _              =  Nothing

-- | More efficient comparison for actions.
instance Eq Action where
  (Available    v1)  ==  (Available    v2)  =  v1 == v2
  (Built        v1)  ==  (Built        v2)  =  v1 == v2
  Top                ==  Top                =  True
  _                  ==  _                  =  False

instance Show Action where
  show (Built        v) = "B  " ++ showPV (pv (meta v))
  show Top              = "/"

showAction :: Config -> Action -> String
showAction c (Built        v) = "B  " ++ E.showVariant c v
showAction c Top              = "/"

data DepState =  DepState
                   {
                      pconfig   ::  PortageConfig,
                      dlocuse   ::  [UseFlag],
                      graph     ::  Graph,
                      labels    ::  Map Variant NodeMap,
                      precs     ::  PrecMap,
                      active    ::  ActiveMap,
                      counter   ::  !Int,
                      callback  ::  Callback
                   }

type ActiveMap  =  Map P (Map Slot (Either Variant [Blocker]))
type PrecMap    =  IntMap IntSet

data Callback =  CbDepend   { nodemap :: NodeMap }
              |  CbRDepend  { nodemap :: NodeMap }
              |  CbPDepend  { nodemap :: NodeMap }

data NodeMap = NodeMap
                 {
                    available  ::  !Int,
                    built      ::  !Int
                 }

top :: Int
top = 0

data Progress =  LookAtEbuild  PV EbuildOrigin
              |  Backtrack     (Maybe DepState) Failure
              |  AddEdge       Node Node DepType
              |  Message       String

-- | Graph generation monad.
newtype GG a = GG { runGG :: DepState -> [Either Progress (a,DepState)] }

returnGG :: a -> GG a
returnGG x = GG (\s -> [Right (x,s)])

bindGG :: GG a -> (a -> GG b) -> GG b
bindGG (GG a) f = 
  GG (\s -> concatMap  (\x -> case x of
                                Right (t,s')  ->  runGG (f t) s'
                                Left p        ->  [Left p])
                       (a s))

fmapGG :: (a -> b) -> GG a -> GG b
fmapGG f (GG a) =
    GG (\s -> map  (\x -> case x of { Right (y,s') -> Right (f y,s'); Left x -> Left x })
                   (a s))

instance Monad GG where
  return = returnGG
  (>>=) = bindGG

instance Functor GG where
  fmap = fmapGG

get :: GG DepState
get = GG (\s -> [Right (s,s)])

put :: DepState -> GG ()
put s = GG (\_ -> [Right ((),s)])

gets :: (DepState -> a) -> GG a
gets f = GG (\s -> [Right (f s,s)])

modify :: (DepState -> DepState) -> GG ()
modify f = GG (\s -> [Right ((),f s)])

progress :: Progress -> GG ()
progress p = GG (\s -> [Left p,Right ((),s)])

choice :: [a] -> GG a
choice cs = GG (\s -> [Right (c,s) | c <- cs])

backtrack :: GG a
backtrack = GG (\_ -> [])

lookupPS :: PS -> Map P (Map Slot a) -> Maybe a
lookupPS (PS cat pkg slot) m = M.lookup (P cat pkg) m >>= M.lookup slot

insertPS :: PS -> a -> Map P (Map Slot a) -> Map P (Map Slot a)
insertPS (PS cat pkg slot) v m = 
   M.insertWith  (\ m _ -> M.insert slot v m) 
                 (P cat pkg) (M.singleton slot v) m

getActives :: P -> Map P (Map Slot a) -> [a]
getActives p m = M.elems (M.findWithDefault M.empty p m)

isActive :: Variant -> ActiveMap -> Bool
isActive v m =  case lookupPS (extractPS . pvs $ v) m of
                  Nothing          ->  False
                  Just (Left v')   ->  v == v'

-- | Modify the graph within the monad.
modifyGraph :: (Graph -> Graph) -> GG ()
modifyGraph f = modify (\s -> s { graph = f (graph s) })

-- | Modify the precs within the monad.
modifyPrecs :: (PrecMap -> PrecMap) -> GG ()
modifyPrecs f = modify (\s -> s { precs = f (precs s) })

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

-- | Insert a set of new nodes into the graph, and connects it.
--   Returns if the node has existed before, and the node map.
insVariant :: Variant -> (NodeMap -> GG ()) -> GG (Bool,NodeMap)
insVariant v cb =
    do  ls <- gets labels
        case M.lookup v ls of
          Nothing  ->  do  let  ps'  =  extractPS . pvs $ v
                           a <- gets active
                           let  insVariant' bs =
                                    do  modify (\s -> s { active = insertPS ps' (Left v) a })
                                        nm <- insHistory v  -- really adds the nodes
                                        cb nm               -- ties in the nodes with the rest of the graph
                                        resolveBlockers v bs  -- checks the previously accumulated blockers for this package
                                        return nm
                           nm <- case lookupPS ps' a of
                                   Nothing          ->  insVariant' []
                                   Just (Left v')   ->  do  s   <-  gets (strategy . pconfig)
                                                            ds  <-  get
                                                            let  f  =  SlotConflict v v'
                                                                 b  |  sbacktrack s f  =  Nothing
                                                                    |  otherwise       =  Just ds
                                                            progress (Backtrack b f)
                                                            backtrack
                                   Just (Right bs)  ->  insVariant' bs
                           return (False,nm)
          Just nm  ->  return (True,nm)

resolveBlockers :: Variant -> [Blocker] -> GG ()
resolveBlockers v = mapM_ (\b ->  if matchDepAtomVariant (unblock $ bdepatom b) v
                                    then  do  ds  <-  get
                                              let  s  =  strategy . pconfig $ ds
                                                   f  =  Block b v
                                                   x  |  sbacktrack s f  =  Nothing
                                                      |  otherwise       =  Just ds
                                              progress (Backtrack x f)
                                              backtrack
                                    else  return ())

insHistory :: Variant -> GG NodeMap
insHistory v  =
    do  n <- stepCounter nr
        let  a   =  n
             b   =  n + nr - 1
             nm  =  NodeMap a b
        registerNode v nm
        return nm
    where  hasPDepend       =  not . null . E.pdepend . E.ebuild $ v
           nr | hasPDepend  =  2
              | otherwise   =  1

-- | insert new node(s) and update labels
registerNode :: Variant -> NodeMap -> GG ()
registerNode v nm@(NodeMap a b) = 
    do  modify (\s -> s { labels = M.insert v nm (labels s) })
        if a /= b
          then  modifyGraph ( insNodes [(a,[Available v]),(b,[Built v])] )
          else  modifyGraph ( insNodes [(a,[Available v,Built v])] )
        when (a /= b) (registerEdge a b Meta >> return ())  -- cannot fail

-- | insert a new edge if it does not create a cycle
registerEdge :: Int -> Int -> DepType -> GG (Maybe Path)
registerEdge s t d =
    do  ps <- gets precs
        let sPrecs = IM.findWithDefault IS.empty s ps
        if IS.member t sPrecs
          then  do  g <- gets graph
                    return (Just (sp t s (emap (const 1.0) g))) -- returns cycle
          else  do  modifyGraph (insEdges [(s,t,d)])
                    modifyPrecs (\p -> IM.update (\tPrecs -> Just (IS.union tPrecs sPrecs)) t p)
                    return Nothing -- indicates success

runGGWith :: DepState -> GG a -> ([Progress],DepState)
runGGWith s cmp = proc (runGG cmp s)
  where  proc []                =  ([],error "no solution found")
         proc (Right ~(_,s):_)  =  ([],s)
         proc (Left p:xs)       =  (\ ~(x,y) -> (p:x,y)) (proc xs)


doCallback :: Callback -> DepAtom -> NodeMap -> GG ()
doCallback (CbDepend   nm) = depend nm
doCallback (CbRDepend  nm) = rdepend nm
doCallback (CbPDepend  nm) = pdepend nm

depend :: NodeMap -> DepAtom -> NodeMap -> GG ()
depend source da target
{-
    | blocking da =
        do
            let bt = built target
                bs = built source
                d  = Depend True da
            modifyGraph (insEdges [ (bt,bs,d) ])
            progress (AddEdge bt bs d)
-}
    | otherwise =
        do
            let bs  =  built source
                at  =  available target
                d   =  Depend False da
            registerEdge bs at d
            progress (AddEdge bs at d)


rdepend, pdepend :: NodeMap -> DepAtom -> NodeMap -> GG ()
rdepend = rpdepend RDepend
pdepend = rpdepend PDepend

rpdepend :: (Bool -> DepAtom -> DepType) -> NodeMap -> DepAtom -> NodeMap -> GG ()
rpdepend rpd source da target
{-
    | blocking da =
        do
           let bt = built target
               rs = removed source
               d  = rpd True da
           modifyGraph (insEdges [  (bt,rs,d) ])
           progress (AddEdge bt rs d)
-}
    | otherwise =
        do
           let as = available source
               at = available target
               d  = rpd False da
           registerEdge as at d
           progress (AddEdge as at d)
