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
import Data.Graph.Inductive hiding (version, Graph(), NodeMap())

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
                      labels    ::  Map Variant NodeMap,
                      active    ::  Map P (Map Slot Variant),
                      counter   ::  !Int,
                      callback  ::  Callback
                   }

data Callback =  CbDepend   { nodemap :: NodeMap }
              |  CbRDepend  { nodemap :: NodeMap }
              |  CbPDepend  { nodemap :: NodeMap }

data NodeMap = NodeMap
                 {
                    available  ::  !Int,
                    built      ::  !Int,
                    removed    ::  !Int
                 }

top = 0
bot = 1

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

isActive :: Variant -> Map P (Map Slot Variant) -> Bool
isActive v m =  case lookupPS (extractPS . pvs $ v) m of
                  Nothing   ->  False
                  Just v'   ->  v == v'

-- | Activates a variant. Returns whether the package does already exist.
activate :: Variant -> GG Bool
activate v = 
    do  let ps' = extractPS . pvs $ v
        a <- gets active
        case lookupPS ps' a of
          Nothing          ->  do
                                   ls  <-  gets labels
                                   modify (\s -> s { active = insertPS ps' v a })
                                   modifyGraph ( insEdge (top, available (ls M.! v), Meta) )
                                   return False
          Just v'
            | v == v'      ->  return True
            | otherwise    ->  do
                                   s   <-  gets (strategy . pconfig)
                                   ds  <-  get
                                   let  f  =  SlotConflict v v'
                                        b  |  sbacktrack s f  =  Nothing
                                           |  otherwise       =  Just ds
                                   progress (Backtrack b f)
                                   backtrack

doCallback :: Callback -> DepAtom -> NodeMap -> GG ()
doCallback (CbDepend   nm) = depend nm
doCallback (CbRDepend  nm) = rdepend nm
doCallback (CbPDepend  nm) = pdepend nm

depend :: NodeMap -> DepAtom -> NodeMap -> GG ()
depend source da target
    | blocking da =
        do
            let bt = built target
                bs = built source
                d  = Depend True da
            modifyGraph (insEdges [ (bt,bs,d) ])
            progress (AddEdge bt bs d)
    | otherwise =
        do
            let bs = built source
                at = available target
                d1 = Depend False da
                rt = removed target
                bt = built source
                d2 = Depend True da
            modifyGraph (insEdges [  (bs,at,d1),
                                     (rt,bs,d2) ])
            progress (AddEdge bs at d1)
            progress (AddEdge rt bs d2)

rdepend, pdepend :: NodeMap -> DepAtom -> NodeMap -> GG ()
rdepend = rpdepend RDepend
pdepend = rpdepend PDepend

rpdepend :: (Bool -> DepAtom -> DepType) -> NodeMap -> DepAtom -> NodeMap -> GG ()
rpdepend rpd source da target
    | blocking da =
        do
           let bt = built target
               rs = removed source
               d  = rpd True da
           modifyGraph (insEdges [  (bt,rs,d) ])
           progress (AddEdge bt rs d)
    | otherwise =
        do
           let as = available source
               at = available target
               d1 = rpd False da
               rt = removed target
               rs = removed source
               d2 = rpd True da
           modifyGraph (insEdges [  (as,at,d1),
                                    (rt,rs,d2) ])
           progress (AddEdge as at d1)
           progress (AddEdge rt rs d2)

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
        case M.lookup v ls of
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
             nm   =  NodeMap a a r
        registerNode v nm
        modifyGraph (  insEdges [ (r,a,Meta),
                                  (bot,a,Meta)
                                  {- (r,top,Meta) -} ] .
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
             nm   =  NodeMap a a r
             nm'  =  NodeMap a' b' r'
        registerNode v   nm
        registerNode v'  nm'
        modifyGraph (  insEdges [ (r',a',Meta),
                                  (a',b',Meta),
                                  (r,a,Meta),
                                  (bot,a,Meta)
                                  {- (r',top,Meta) -} ] .
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
             nm   =  NodeMap a b r
        registerNode v nm
        modifyGraph (  insEdges [ (r,a,Meta),
                                  (a,b,Meta),
                                  (b,bot,Meta)
                                  {- (r,top,Meta) -} ] .
                       insNodes [ (b,[Built v]),
                                  (a,[Available v]),
                                  (r,[Removed v]) ])
        return nm

registerNode :: Variant -> NodeMap -> GG ()
registerNode v nm = modify (\s -> s { labels = M.insert v nm (labels s) })


runGGWith :: DepState -> GG a -> ([Progress],DepState)
runGGWith s cmp = proc (runGG cmp s)
  where  proc []                =  ([],error "no solution found")
         proc (Right ~(_,s):_)  =  ([],s)
         proc (Left p:xs)       =  (\ ~(x,y) -> (p:x,y)) (proc xs)
