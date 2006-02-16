{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Generation of dependency graph.
-}

module Portage.Graph
  (module Portage.GraphGeneration, module Portage.Graph)
  where

import Data.List
import Data.Maybe (fromJust, isJust, listToMaybe, maybeToList)
import Data.Graph.Inductive hiding (version, Graph(), NodeMap())
import Data.Map (Map)
import qualified Data.Map as M

import Portage.PortageConfig
import Portage.Config
import Portage.Dependency hiding (getDepAtom)
import Portage.Package
import Portage.Tree
import Portage.Match
import Portage.Use
import Portage.Ebuild (Variant(..), Ebuild(iuse), EbuildMeta(..), EbuildOrigin(..), TreeLocation(..), Mask(..), Link(..), pvs)
import Portage.Version
import Portage.Strategy
import Portage.Ebuild (Variant(..), Ebuild(iuse), EbuildMeta(..), EbuildOrigin(..), TreeLocation(..), Mask(..), Link(..), pvs)
import qualified Portage.Ebuild as E
import Portage.GraphGeneration

findVersions :: Tree -> DepAtom -> [Variant]
findVersions = flip matchDepAtomTree



-- x :: Graph -> DepString -> Graph
-- y :: Graph -> DepTerm -> Graph
-- z :: Graph -> DepAtom -> Graph


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

withLocUse :: [UseFlag] -> GG a -> GG a
withLocUse luse' g =
    do
        luse <- gets dlocuse
        modify (\s -> s { dlocuse = luse' })
        r <- g
        modify (\s -> s { dlocuse = luse })
        return r

withCallback :: Callback -> GG a -> GG a
withCallback cb' g =
    do
        cb <- gets callback
        modify (\s -> s { callback = cb' })
        r <- g
        modify (\s -> s { callback = cb })
        return r

buildGraphForDepAtom :: DepAtom -> GG [Progress]
buildGraphForDepAtom da
    | blocking da =
        do  -- for blocking dependencies, we have to consider all matching versions
            pc  <-  gets pconfig
            let  s  =  strategy pc
                 t  =  itree pc
            cb  <-  gets callback
            p   <-  mapM
                      (\v -> do
                                 n <- insNewNode v (sstop s v)
                                 p' <- cb da n
                                 return (LookAtEbuild (pv (meta v)) (origin (meta v)) : p'))
                      (findVersions t (unblock da))
            return ((Message $ "blocker " ++ show da) : concat p)
    | otherwise =
        do  pc  <-  gets pconfig
            g   <-  gets graph
            ls  <-  gets labels
            a   <-  gets active
            let  s  =  strategy pc
                 t  =  itree pc
                 p  =  pFromDepAtom da
            case sselect s p (findVersions t da) of
              Reject f -> return []  -- fail (show f)
              Accept v@(Variant m e)  ->  
                let  avail      =  E.isAvailable (location m)  -- installed or provided?
                     already    =  isActive v a
                     stop       =  avail && sstop s v 
                                     -- if it's an installed ebuild, we can decide to stop here!
                in                 let  rdeps    =  E.rdepend  e
                                        deps     =  E.depend   e
                                        pdeps    =  E.pdepend  e
                                        luse     =  mergeUse (use (config pc)) (locuse m)
                                   in   -- set new local USE context
                                        withLocUse luse $
                                        do
                                            -- insert nodes for v, and activate
                                            n <- insNewNode v stop
                                            activate v
                                            -- insert edges according to current state
                                            cb <- gets callback
                                            p0 <- cb da n
                                            if already || stop
                                              then return ([LookAtEbuild (pv (meta v)) (origin (meta v))] ++ p0)
                                              else do
                                                       -- add deps to graph
                                                       p1 <- withCallback (depend n) $ buildGraphForUDepString deps
                                                       -- add rdeps to graph
                                                       p2 <- withCallback (rdepend n) $ buildGraphForUDepString rdeps
                                                       -- add pdeps to graph
                                                       p3 <- withCallback (pdepend n) $ buildGraphForUDepString pdeps
                                                       return ([LookAtEbuild (pv (meta v)) (origin (meta v))] ++ p0 ++ p1 ++ p2 ++ p3)


isAvailable :: Action -> Maybe Variant
isAvailable (Available v)  =  Just v
isAvailable _              =  Nothing

isAvailableNode :: Graph -> Int -> Maybe Variant
isAvailableNode g = msum . map isAvailable . fromJust . lab g

getVariantsNode :: Graph -> Int -> [Variant]
getVariantsNode g = concatMap (maybeToList . getVariant) . fromJust . lab g

isUpgradeNode :: Graph -> Node -> Maybe (Node,Variant,Node,Variant)
isUpgradeNode g n =  listToMaybe $
                     [ (n,v',a,v) |  let va' = isAvailableNode g n, isJust va', let (Just v') = va',
                                     (_,t,Meta) <- out g n, (_,a,Meta) <- out g t,
                                     let va = isAvailableNode g a, isJust va, let (Just v) = va ]

-- Types of cycles:
-- * Bootstrap cycle. A cycle which wants to recursively update itself, but a
--   version is already installed (happens if an upgrade strategy always selects
--   the most recent version). If we have an Upgrade-history, and incoming dependencies
--   on the newer version from within the cycle, we can redirect these dependencies
--   to the old version.
-- * PDEPEND cycle. All cycles that contain PDEPEND edges. For an Available-type node
--   with an outgoing PDEPEND, we redirect incoming DEPENDs and RDEPENDs from within 
--   the cycle to the Built state.
-- * Self-blockers. We just drop them, but with a headache.
-- This is currently very fragile w.r.t. ordering of the resolutions.
resolveCycle :: [Node] -> GG (Bool,[Progress])
resolveCycle cnodes = 
    do  g <- gets graph
        let cedges = zipWith findEdge  (map (out g) cnodes)
                                       (tail cnodes ++ [head cnodes])
        sumR $ 
                   map resolvePDependCycle (filter isPDependEdge cedges)
               ++  map resolveSelfBlockCycle (filter (\x -> isBlockingDependEdge x || isBlockingRDependEdge x) cedges)
               ++ (case detectBootstrapCycle g cnodes of
                    Just (a',v',a,v)  ->  [resolveBootstrapCycle a' v' a v]
                    Nothing           ->  [])
  where
    detectBootstrapCycle :: Graph -> [Node] -> Maybe (Node,Variant,Node,Variant)
    detectBootstrapCycle g ns = msum (map (isUpgradeNode g) ns)

    resolveBootstrapCycle :: Node -> Variant -> Node -> Variant -> GG (Bool,[Progress])
    resolveBootstrapCycle a' v' a v =
        do
            g <- gets graph
            let incoming  =  filter  (\e@(s',_,d') ->  s' `elem` cnodes &&
                                                       (isJust . getDepAtom $ d') &&
                                                       matchDepAtomVariant (fromJust . getDepAtom $ d') v &&
                                                       (isDependEdge e || isRDependEdge e))
                                     (inn g a')
            if null incoming
              then failR []
              else do  modifyGraph (  insEdges (map (\(s',_,d') -> (s',a,d')) incoming) .
                                      delEdges (map (\(s',t',_) -> (s',t')) incoming) )
                       succeedR [Message $ "Resolved bootstrap cycle at "  ++ (showPV . pv . meta $ v')
                                                                           ++ " (redirected " ++ show incoming ++ ")"]

    resolveSelfBlockCycle :: LEdge DepType -> GG (Bool,[Progress])
    resolveSelfBlockCycle (s,t,d) =
        do
            g <- gets graph
            case (getVariantsNode g s,getVariantsNode g t) of
              (vs,vs')     ->  let  vsi = intersect (map (extractPS . pvs) vs) (map (extractPS . pvs) vs')
                               in   if not . null $ vsi
                                      then  do  modifyGraph (delEdge (s,t))
                                                succeedR [Message $ "Resolved self-block cycle for " ++ (showPS $ head vsi)
                                                                                                    ++ " (redirected " ++ show (s,t,d) ++ ")"]
                                      else  failR []
 
    resolvePDependCycle :: LEdge DepType -> GG (Bool,[Progress])
    resolvePDependCycle (s,t,d) =
        do
            ls <- gets labels
            g <- gets graph
            case isAvailableNode g s of
              Nothing  ->  failR []
              Just v   ->  do  let incoming = 
                                     filter  (\e@(s',_,_) ->  s' `elem` cnodes &&
                                                              (isDependEdge e || isRDependEdge e))
                                             (inn g s)
                               if null incoming
                                 then failR []
                                 else do  let pv'  =  pv . meta $ v
                                          let nm   =  ls M.! pv'
                                          modifyGraph (  insEdges (map (\(s',_,d') -> (s',built nm,d')) incoming) .
                                                         delEdges (map (\(s',t',_) -> (s',t')) incoming) )
                                          succeedR [Message $ "Resolved PDEPEND cycle at " ++ showPV pv' ++ " (redirected " ++ show incoming ++ ")" ]

(||.) :: GG (Bool,[a]) -> GG (Bool,[a]) -> GG (Bool,[a])
f ||. g = do  (b,r) <- f
              if b  then return (b,r)
                    else do  (c,r') <- g
                             return (c, r ++ r')

(&&.) :: GG (Bool,[a]) -> GG (Bool,[a]) -> GG (Bool,[a])
f &&. g = do  (b1,r1) <- f
              (b2,r2) <- g
              return (b1 || b2, r1 ++ r2)

succeedR :: [a] -> GG (Bool,[a])
succeedR ps = return (True,ps)

failR :: [a] -> GG (Bool,[a])
failR ps = return (False,ps)

sumR :: [GG (Bool,[a])] -> GG (Bool,[a])
sumR = foldr (||.) (failR [])

allR :: [GG (Bool,[a])] -> GG (Bool,[a])
allR = foldr (&&.) (failR [])

isPDependEdge :: LEdge DepType -> Bool
isPDependEdge  (_,_,PDepend False _)  =  True
isPDependEdge  _                      =  False

isDependEdge :: LEdge DepType -> Bool
isDependEdge   (_,_,Depend False _)   =  True
isDependEdge   _                      =  False

isRDependEdge :: LEdge DepType -> Bool
isRDependEdge  (_,_,RDepend False _)  =  True
isRDependEdge  _                      =  False

isBlockingDependEdge :: LEdge DepType -> Bool
isBlockingDependEdge   (_,_,Depend True da)   =  blocking da
isBlockingDependEdge   _                      =  False

isBlockingRDependEdge :: LEdge DepType -> Bool
isBlockingRDependEdge  (_,_,RDepend True da)  =  blocking da
isBlockingRDependEdge  _                      =  False

findEdge :: [LEdge b] -> Node -> LEdge b
findEdge es n = fromJust $ find (\(_,t,_) -> n == t) es

