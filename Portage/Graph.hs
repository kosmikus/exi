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
import Data.Graph.Inductive hiding (version, Graph(),NodeMap())
import Control.Monad.Identity
import Control.Monad.State

import Portage.Tree
import Portage.Match
import Portage.Dependency
import Portage.Ebuild (Variant(..), Ebuild(iuse), EbuildMeta(..), EbuildOrigin(..), TreeLocation(..), Mask(..), pvs)
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
showAction c (Available    v) = "A  " ++ showVariant c v
showAction c (Built        v) = "B  " ++ showVariant c v
showAction c (Removed      v) = "D  " ++ showVariant c v
showAction c Top              = "/"
showAction c Bot              = "_"

isAvailable :: Variant -> Map PS NodeInfo -> Bool
isAvailable v m =  case M.lookup (extractPS . pvs $ v) m of
                     Nothing  ->  False
                     Just _   ->  True

data DepState =  DepState
                   {
                      pconfig   ::  PortageConfig,
                      dlocuse   ::  [UseFlag],
                      graph     ::  Graph,
                      labels    ::  Map PS NodeInfo,
                      counter   ::  Int,
                      callback  ::  DepAtom -> NodeMap -> GG ()
                   }

data NodeInfo =  NodeInfo
                   {  
                      nodes    ::  NodeMap,
                      npv      ::  PV
                   }

type NodeMap = (Int,Int,Int)

available, built, removed :: NodeMap -> Int
available  (a,b,r) = a
built      (a,b,r) = b
removed    (a,b,r) = r

top = 0
bot = 1


depend :: NodeMap -> DepAtom -> NodeMap -> GG ()
depend source da target
    | blocking da =
        modifyGraph (insEdges [  (built target,built source,Depend True da) ])
    | otherwise =
        modifyGraph (insEdges [  (built source,available target,Depend False da),
                                 (removed target,built source,Depend True da) ])

rdepend :: NodeMap -> DepAtom -> NodeMap -> GG ()
rdepend source da target
    | blocking da =
        modifyGraph (insEdges [  (built target,removed source,RDepend True da) ])
    | otherwise =
        modifyGraph (insEdges [  (available source,available target,RDepend False da),
                                 (removed target,removed source,RDepend True da) ])

pdepend :: NodeMap -> DepAtom -> NodeMap -> GG ()
pdepend = rdepend
                                  

data Progress =  LookAtEbuild  PV EbuildOrigin
              |  Message       String
  deriving (Eq,Show)

-- | Graph generation monad.
type GG = State DepState

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
        let ps' = extractPS (pvs v)
        case M.lookup ps' ls of
          Nothing | a && (E.isAvailable . location $ meta v)  ->  insAvailableHistory v
                  | otherwise                                 ->  insInstallHistory v
          Just (NodeInfo nm pv')
                  | pv' == pv (meta v)  ->  return nm
                  | otherwise           ->  fail "depgraph creation conflict" -- TODO, not fail here

insAvailableHistory :: Variant -> GG NodeMap
insAvailableHistory v =
    do  n <- stepCounter 2
        let  a    =  n
             r    =  n + 1
             ps'  =  extractPS (pvs v)
             pv'  =  pv . meta $ v
             nm   =  (a,a,r)
        registerNode ps' nm pv'
        modifyGraph (  insEdges [ (r,a,Meta),
                                  (bot,a,Meta),
                                  (top,a,Meta),  -- temporary
                                  (r,top,Meta) ] .
                       insNodes [ (a,Available v),
                                  (r,Removed v) ])
        return nm

insInstallHistory :: Variant -> GG NodeMap
insInstallHistory v =
    do  n <- stepCounter 3
        let  b    =  n
             a    =  n + 1
             r    =  n + 2
             ps'  =  extractPS (pvs v)
             pv'  =  pv . meta $ v
             nm   =  (b,a,r)
        registerNode ps' nm pv'
        modifyGraph (  insEdges [ (r,a,Meta),
                                  (a,b,Meta),
                                  (b,bot,Meta),
                                  (top,a,Meta),  -- temporary
                                  (r,top,Meta) ] .
                       insNodes [ (b,Built v),
                                  (a,Available v),
                                  (r,Removed v) ])
        return nm

registerNode :: PS -> NodeMap -> PV -> GG ()
registerNode ps nm pv = modify (\s -> s { labels = M.insert ps (NodeInfo nm pv) (labels s) })

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
                                            -- insert edges according to current state
                                            cb <- gets callback
                                            cb da n
                                            if stop
                                              then return [LookAtEbuild (pv (meta v)) (origin (meta v))]
                                              else do
                                                       -- add deps to graph
                                                       p1 <- withState (\s -> s { callback = depend n }) $ buildGraphForUDepString deps
                                                       -- add rdeps to graph
                                                       p2 <- withState (\s -> s { callback = rdepend n }) $ buildGraphForUDepString rdeps
                                                       -- add pdeps to graph
                                                       p3 <- withState (\s -> s { callback = pdepend n }) $ buildGraphForUDepString pdeps
                                                       return ([LookAtEbuild (pv (meta v)) (origin (meta v))] ++ p1 ++ p2 ++ p3)

