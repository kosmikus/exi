{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Graph generation monad.
-}

module Portage.GraphGeneration 
  (module Portage.GraphGeneration)
  where

import Data.Maybe (fromJust)
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
import Portage.Utilities

type DGraph = Graph
type Graph = Gr [Action] DepType

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
  show (Available    v) = "A  " ++ showPV (pv (meta v))
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
                      -- precs     ::  PrecMap,
                      active    ::  ActiveMap,
                      counter   ::  !Int,
                      callback  ::  Callback,
                      strategy  ::  Strategy
                   }

type ActiveMap  =  Map P (Map Slot (Either Variant [Blocker]))
type PrecMap    =  IntMap IntSet

data Callback =  CbDepend   { nodemap :: NodeMap }
              |  CbRDepend  { nodemap :: NodeMap }
              |  CbPDepend  { nodemap :: NodeMap }
              |  CbBlock    { nodemap :: NodeMap, cblocker :: Blocker }

data NodeMap = NodeMap
                 {
                    available  ::  !Int,
                    built      ::  !Int
                 }
  deriving (Show,Eq)

showNodeMap (NodeMap a b) = "(" ++ show a ++ "," ++ show b ++ ")"

top :: Int
top = 0

data Progress =  LookAtEbuild  PV EbuildOrigin
              |  Backtrack     (Maybe DepState) Failure
              |  AddEdge       Node Node DepType
              |  AddNode       NodeMap Variant
              |  Message       String
              |  Done

-- | Graph generation monad.
newtype GG a = GG { runGG :: DepState -> (GGLabel,[GGStep a]) }

data GGStep a  =  Step      Progress
               |  Return    (a,DepState)
               |  Continue  (GGLabel -> Bool) (DepState -> DepState)

type GGLabel = Maybe PS

returnGG :: a -> GG a
returnGG x = GG (\s -> (Nothing,[Return (x,s)]))

bindGG :: GG a -> (a -> GG b) -> GG b
bindGG (GG a) f = 
  GG (\s ->  case a s of
               (l,xs)  ->  (l,  foldr (\x r ->
                                  case x of
                                    Step p         ->  Step p : r
                                    Return (t,s')  ->  snd (runGG (f t) s') ++ r
                                    Continue p st
                                      | p l        ->  mapGGStep st r
                                      | otherwise  ->  [Continue p st])
                                  [] xs))

mapGGStep :: (DepState -> DepState) -> [GGStep a] -> [GGStep a]
mapGGStep st = map (\x ->  case x of
                             Return (t,s)  ->  Return (t,st s)
                             _             ->  x)

fmapGG :: (a -> b) -> GG a -> GG b
fmapGG f (GG a) =
  GG (\s ->  case a s of
               (l,xs)  ->  (l,  map (\x ->
                                  case x of
                                    Step p          ->  Step p
                                    Return (y,s')   ->  Return (f y,s')
                                    Continue p st   ->  Continue p st)
                                  xs))

instance Monad GG where
  return = returnGG
  (>>=) = bindGG

instance Functor GG where
  fmap = fmapGG

get :: GG DepState
get = GG (\s -> (Nothing,[Return (s,s)]))

put :: DepState -> GG ()
put s = GG (\_ -> (Nothing,[Return ((),s)]))

gets :: (DepState -> a) -> GG a
gets f = GG (\s -> (Nothing,[Return (f s,s)]))

modify :: (DepState -> DepState) -> GG ()
modify f = GG (\s -> (Nothing,[Return ((),f s)]))

progress :: Progress -> GG ()
progress p = GG (\s -> (Nothing,[Step p,Return ((),s)]))

lchoice :: GGLabel -> [a] -> GG a
lchoice l cs = GG (\s -> (l,[Return (c,s) | c <- cs]))

lchoiceM :: GGLabel -> [GG a] -> GG a
lchoiceM l cs = GG (\s -> (l,[r | GG f <- cs, r <- snd (f s)]))

choice :: [a] -> GG a
choice = lchoice Nothing

choiceM :: [GG a] -> GG a
choiceM = lchoiceM Nothing

firstM :: Monad m => [m Bool] -> m () -> m ()
firstM  =  flip (foldr (\x y -> x >>= \r -> if r then return () else y)) 

backtrack :: GG a
backtrack = GG (\_ -> (Nothing,[]))

continue :: (GGLabel -> Bool) -> (DepState -> DepState) -> GG a
continue p st = GG (\_ -> (Nothing,[Continue p st]))

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

{-
-- | Modify the precs within the monad.
modifyPrecs :: (PrecMap -> PrecMap) -> GG ()
modifyPrecs f = modify (\s -> s { precs = f (precs s) })
-}

-- | Modify the counter within the monad.
stepCounter :: Int -> GG Int
stepCounter n = do  c <- gets counter
                    modify (\s -> s { counter = n + counter s })
                    return c

-- | Create a new node within the monad.
newNode :: GG Node
newNode =
    do  g <- gets graph
        return (head' "newNode" $ newNodes 1 g)

insHistory :: Variant -> GG NodeMap
insHistory v  =
    do  n <- stepCounter nr
        let  a   =  n
             b   =  n + nr - 1
             nm  =  NodeMap a b
        registerNode v nm av
        return nm
    where  hasPDepend       =  not . null . E.pdepend . E.ebuild $ v
           av               =  (E.isAvailable . location . meta) v
           nr | hasPDepend && not av 
                            =  2
              | otherwise   =  1

-- | Insert new node(s) and update labels.
registerNode :: Variant -> NodeMap -> Bool -> GG ()
registerNode v nm@(NodeMap a b) av =
    do  modify (\s -> s { labels = M.insert v nm (labels s) })
        progress (AddNode nm v)
        if a /= b
          then  modifyGraph ( insNodes [(a,[Available v]),(b,[Built v])] )
          else  modifyGraph ( insNode (a,Available v : if av then [] else [Built v]) )
        when (a /= b) (registerEdge a b Meta >> return ())  -- cannot fail

-- | Insert a new edge if it does not create a cycle. Returns the cycle
--   that would have been created if insertion fails, and Nothing on success.
registerEdge :: Int -> Int -> DepType -> GG (Maybe Path)
registerEdge s t d =
    do  -- ps <- gets precs
        g <- gets graph
        let sPrecs = ancestors s g -- IM.findWithDefault IS.empty s ps
        if elem t sPrecs
          then  do  g <- gets graph
                    progress (Message "adding edge would cause cycle")
                    return (Just (sp t s (emap (const 1.0) g))) -- returns cycle
          else  do  modifyGraph (insEdge (s,t,d))
                    progress (Message $ "precs of " ++ show s ++ ": " ++ show sPrecs)
                    progress (AddEdge s t d)
                    -- modifyPrecs (\p -> IM.insertWith IS.union t (IS.insert s sPrecs) p)
                    return Nothing -- indicates success

ancestors :: Node -> Graph -> [Node]
ancestors v g = preorderF (rdff [v] g)

runGGWith :: DepState -> GG a -> ([Progress],DepState)
runGGWith s cmp = proc (snd (runGG cmp s))
  where  proc []                 =  ([],error "no solution found")
         proc (Return ~(_,s):_)  =  ([],s)
         proc (Step p:xs)        =  (\ ~(x,y) -> (p:x,y)) (proc xs)
         proc (Continue _ _:_)   =  ([],error "no solution found")


{-
-- | Recompute the predecessors for a certain node.
recomputePrecs :: Int -> GG ()
recomputePrecs n =
    do  g    <-  gets graph
        let  ps = pre g n
        modifyPrecs (\prs -> IM.insert n (IS.unions (map (\k -> IM.findWithDefault IS.empty k prs) ps)) prs)
-}

-- | Remove an edge from the graph while maintaining the map of predecessors.
removeEdge :: Int -> Int -> GG ()
removeEdge s t =
    do  modifyGraph (delEdge (s,t))
        -- recomputePrecs t
        progress (Message $ "removed edge: " ++ show s ++ " " ++ show t)

-- | Find the label of an edge.
labEdge :: (DynGraph gr) => gr a b -> Node -> Node -> b
labEdge g s t = head' "labEdge" [ l | (_,t',l) <- out g s, t == t' ]

-- | Generate a debug trace of a path in the graph.
pathTrace :: DepState -> [Node] -> Maybe DepType -> [(Variant, Maybe DepType)]
pathTrace s p dt = 
    let  g   =  graph s
         es  =  map Just (zipWith (labEdge g) p (tail p)) ++ [dt]
    in   [  (r,x) |
            (a,x) <- zip p es, case x of { Just Meta -> False; _ -> True },
            Just r <- [  getVariant . head' "pathTrace" . 
                         fromJust' "pathTrace" . lab g $ a ] ]

-- | Generate a debug trace of a cycle in the graph.
cycleTrace :: DepState -> [Node] -> DepType -> [(Variant,Maybe DepType)]
cycleTrace s p dt = pathTrace s p (Just dt)

