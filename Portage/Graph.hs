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
import Data.List
import Data.Graph.Inductive hiding (version, Graph())
import Control.Monad.Identity
import Control.Monad.State

import Portage.Tree
import Portage.Match
import Portage.Dependency
import Portage.Ebuild (Variant(..), Ebuild(iuse), EbuildMeta(..), EbuildOrigin(..), TreeLocation(..), Mask(..))
import qualified Portage.Ebuild as E
import Portage.Package
import Portage.PortageConfig
import Portage.Config
import Portage.Use
import Portage.Strategy

findVersions :: Tree -> DepAtom -> [Variant]
findVersions = flip matchDepAtomTree

showVariant :: Config -> Variant -> String
showVariant cfg (Variant m e)  =  showPV (pv m) ++ showSlot (E.slot e) ++ showLocation cfg (location m) 
                                  ++ " " ++ unwords (map showMasked (masked m))
                                  ++ " " ++ concatMap hardMask (masked m) ++ unwords (diffUse (mergeUse (use cfg) (locuse m)) (iuse e))

showSlot :: String -> String
showSlot ['0'] = ""
showSlot s = "{" ++ s ++ "}"

showLocation :: Config -> TreeLocation -> String
showLocation c Installed        =  " (installed)"
showLocation c (Provided f)     =  " (provided in " ++ f ++ ")"
showLocation c (PortageTree t)  =  if portDir c == t then "" else " [" ++ t ++ "]"

showTreeLocation :: TreeLocation -> String
showTreeLocation Installed        =  "installed packages"
showTreeLocation (Provided f)     =  "provided packages from " ++ f
showTreeLocation (PortageTree t)  =  t

hardMask :: Mask -> String
hardMask (HardMasked f r) = unlines r
hardMask _                = ""

showMasked :: Mask -> String
showMasked (KeywordMasked xs) = "(masked by keyword: " ++ show xs ++ ")"
showMasked (HardMasked f r) = "(hardmasked in " ++ f ++ ")"
showMasked (ProfileMasked f) = "(excluded from profile in " ++ f ++")"
showMasked (Shadowed t) = "(shadowed by " ++ showTreeLocation t ++ ")"


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

showAction :: Config -> Action -> String
showAction c (Unavailable  v) = "NA " ++ showVariant c v
showAction c (Available    v) = "A  " ++ showVariant c v
showAction c (Unblocked    v) = "U< " ++ showVariant c v
showAction c (Build        v) = "B  " ++ showVariant c v
showAction c (Unblocking   v) = "U> " ++ showVariant c v
showAction c Top              = "/"

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

data Progress =  LookAtEbuild  PV EbuildOrigin
              |  Message       String
  deriving (Eq,Show)

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

-- | Builds a graph for an uninterpreted dependency string,
--   i.e., a dependency string containing USE flags.
buildGraphForUDepString :: DepString -> GG [Progress]
buildGraphForUDepString ds =
    do
        luse  <-  gets dlocuse
        buildGraphForDepString (interpretDepString luse ds)

-- | Builds a graph for the whole dependency string,
--   by processing each term separately.
--   Precondition: dependency string must be interpreted,
--   without USE flags.
buildGraphForDepString :: DepString -> GG [Progress]
buildGraphForDepString ds = fmap concat (mapM buildGraphForDepTerm ds)

-- | Builds a graph for a single term.
--   Precondition: dependency string must be interpreted,
--   without USE flags.
--   If it is an atom, just delegate.
--   If it is a use-qualified thing, check the USE flag and delegate.
--   If it is an or-dependency, ...
buildGraphForDepTerm :: DepTerm -> GG [Progress]
buildGraphForDepTerm dt =
    do  
        pc    <-  gets pconfig
        case resolveVirtuals pc dt of
          Plain d                   ->  buildGraphForDepAtom d
          And ds                    ->  buildGraphForDepString ds
          Or ds                     ->  buildGraphForOr ds

resolveVirtuals :: PortageConfig -> DepTerm -> DepTerm
resolveVirtuals pc (Plain d)  =  maybe (Plain d) id (virtuals pc d)
resolveVirtuals _  dt         =  dt

buildGraphForOr :: DepString -> GG [Progress]
buildGraphForOr []         =  return []  -- strange case, empty OR, but ok
buildGraphForOr ds@(dt:_)  =
    do
        pc    <-  gets pconfig
        case findInstalled pc ds of
          Nothing   ->  do  p <- buildGraphForDepTerm dt  -- default
                            return $ [Message $ "|| (" ++ show ds ++ ")) resolved to default"] ++ p
          Just dt'  ->  do  
                            p <- buildGraphForDepTerm dt'  -- installed
                            return $ [Message $ "|| (" ++ show ds ++ ")) resolved to available: " ++ show dt'] ++ p
  where
    findInstalled :: PortageConfig -> DepString -> Maybe DepTerm
    findInstalled pc = find (isInstalledTerm . resolveVirtuals pc)
      where
        isInstalledTerm :: DepTerm -> Bool
        isInstalledTerm (Or ds)    =  any isInstalledTerm ds
        isInstalledTerm (And ds)   =  all isInstalledTerm ds
        isInstalledTerm (Plain d)  =  isInstalled pc d

-- Critical case for OR-dependencies:
-- || ( ( a b ) c )
-- genone says in #gentoo-portage:
-- if nothing is installed it selects a+b, 
-- if only a and/or b is installed it selects a+b, 
-- if c and one of a or b is installed it selects c

isInstalled :: PortageConfig -> DepAtom -> Bool
isInstalled pc da =
    let  p  =  pFromDepAtom da
         t  =  itree pc
    in   case selectInstalled p (findVersions t da) of
           Accept v  ->  True
           _         ->  False

buildGraphForDepAtom :: DepAtom -> GG [Progress]
buildGraphForDepAtom da =
    do  pc  <-  gets pconfig
        g   <-  gets graph
        ls  <-  gets labels
        let  s  =  strategy pc
             t  =  itree pc
             p  =  pFromDepAtom da
        case sselect s p (findVersions t da) of
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
                                              then return [LookAtEbuild (pv (meta v)) (origin (meta v))]
                                              else do
                                                       -- add deps to graph
                                                       p1 <- withState (\s -> s { dcontext = depend n }) $
                                                               buildGraphForUDepString deps
                                                       -- add rdeps to graph
                                                       p2 <- withState (\s -> s { dcontext = rdepend n }) $
                                                               buildGraphForUDepString rdeps
                                                       -- add pdeps to graph
                                                       p3 <- withState (\s -> s { dcontext = pdepend n }) $
                                                               buildGraphForUDepString pdeps
                                                       return ([LookAtEbuild (pv (meta v)) (origin (meta v))] ++ p1 ++ p2 ++ p3)

