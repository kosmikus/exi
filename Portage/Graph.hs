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
import Control.Monad

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
buildGraphForUDepString :: DepString -> GG ()
buildGraphForUDepString ds =
    do
        luse  <-  gets dlocuse
        buildGraphForDepString (interpretDepString luse ds)

-- | Builds a graph for the whole dependency string,
--   by processing each term separately.
--   Precondition: dependency string must be interpreted,
--   without USE flags.
buildGraphForDepString :: DepString -> GG ()
buildGraphForDepString ds = mapM_ buildGraphForDepTerm ds

-- | Builds a graph for a single term.
--   Precondition: dependency string must be interpreted,
--   without USE flags.
--   If it is an atom, just delegate.
--   If it is a use-qualified thing, check the USE flag and delegate.
--   If it is an or-dependency, ...
buildGraphForDepTerm :: DepTerm -> GG ()
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

buildGraphForOr :: DepString -> GG ()
buildGraphForOr []         =  return ()  -- strange case, empty OR, but ok
buildGraphForOr ds@(dt:_)  =
    do
        pc    <-  gets pconfig
        case findInstalled pc ds of
          Nothing   ->  do  buildGraphForDepTerm dt  -- default
                            progress (Message $ "|| (" ++ show ds ++ ")) resolved to default")
          Just dt'  ->  do  
                            buildGraphForDepTerm dt'  -- installed
                            progress (Message $ "|| (" ++ show ds ++ ")) resolved to available: " ++ show dt')
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
    let  t  =  itree pc
    in   case selectInstalled da (findVersions t da) of
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

eliminateSlots :: [Variant] -> [Variant] -> [Variant]
eliminateSlots rs xs = filter (\x -> not ((E.slot . ebuild) x `elem` map (E.slot . ebuild) rs)) xs

buildGraphForDepAtom :: DepAtom -> GG ()
buildGraphForDepAtom da
    | blocking da =
        do  -- for blocking dependencies, we have to consider all matching versions
            pc  <-  gets pconfig
            g   <-  gets graph
            cb  <-  gets callback
            let  nm  =  nodemap cb
                 s   =  strategy pc
                 t   =  itree pc
                 w   =  head (getVariantsNode g (available nm))
                 b   =  Blocker w da (case cb of CbRDepend _ -> True; _ -> False)
                 ps  =  extractPS (pvs w)
            ls  <-  gets labels
            -- for each variant, there are the following possibilities:
            -- 0. we ignore self-blockers (but with a headache)
            -- 1. if v is in the graph, resolve the blocker b for v
            -- 2. if a variant v' of the same slot as v is in the graph
            --    already, we can pass, because the blocker will be resolved
            --    for v'
            -- 3. if v is available, we have to choose another variant v'
            -- 4. if v is unavailable, we record b as a blocker for the slot
            --    of v
            progress (Message $ "blocker " ++ show da)
            mapM_ (\v -> do
                             progress (LookAtEbuild (pv (meta v)) (origin (meta v)))
                             let ps' = extractPS (pvs v)
                             when (ps /= ps') $ do -- 0.
                               case M.lookup v ls of
                                 Just _   ->  resolveBlockers v [b] -- 1.
                                 Nothing  ->  do  a <- gets active
                                                  let  continue bs = 
                                                         if E.isAvailable . location . meta $ v
                                                           then  let  reject = do  ds  <-  get
                                                                                   s   <-  gets (strategy . pconfig)
                                                                                   let  f  =  Block b v
                                                                                        x  |  sbacktrack s f  =  Nothing
                                                                                           |  otherwise       =  Just ds
                                                                                   progress (Backtrack x f)
                                                                                   backtrack
                                                                 in   withCallback (CbBlock nm b) $ chooseVariant [reject] (const reject) -- 3.
                                                           else  modify (\s -> s { active = insertPS ps' (Right (b:bs)) a }) -- 4.
                                                  case lookupPS ps' a of
                                                    Just (Left _)    ->  return () -- 2.
                                                    Just (Right bs)  ->  continue bs
                                                    Nothing          ->  continue []
                  )
                  (findVersions t (unblock da))
    | otherwise =  let  reject f =  do  ds <- get
                                        s  <- gets (strategy . pconfig)
                                        let  b  |  sbacktrack s f  =  Nothing
                                                |  otherwise       =  Just ds
                                        progress (Backtrack b f)
                                        backtrack
                   in   chooseVariant [] reject
  where
    -- failChoice: what to do if all acceptable ebuilds fail (nothing normally, but error for blockers)
    -- failReject: what to do if all ebuilds are masked (complain about blocker instead?)
    chooseVariant failChoice failReject =
        do  pc  <-  gets pconfig
            -- g   <-  gets graph
            -- ls  <-  gets labels
            a   <-  gets active
            let  s     =  strategy pc
                 t     =  itree pc
                 p     =  pFromDepAtom da
                 acts  =  concatMap (either (:[]) (const [])) (getActives p a)
                 vs    =  acts ++ {- eliminateSlots acts -} (findVersions t da)
            case sselect s da vs of
              Reject f  ->  failReject f
              Accept vs ->
                do   v@(Variant m e) <- choiceM (map return vs ++ failChoice)
                     progress (Message $ "CHOOSING: " ++ E.showVariant' (config pc) v ++ " (out of " ++ show (length vs) ++ ")")
                     let  avail  =  E.isAvailable (location m)  -- installed or provided?
                          stop   =  avail && sstop s v  -- if it's an installed ebuild, we can decide to stop here!
                          luse   =  mergeUse (use (config pc)) (locuse m)
                     -- set new local USE context
                     withLocUse luse $ do
                       progress (LookAtEbuild (pv (meta v)) (origin (meta v)))
                       -- insert nodes for v, and activate
                       cb <- gets callback
                       (already,n) <- insVariant v (doCallback cb da)
                       if already || stop
                         then return ()
                         else do  let  rdeps    =  E.rdepend  e
                                       deps     =  E.depend   e
                                       pdeps    =  E.pdepend  e
                                  -- add deps to graph
                                  withCallback (CbDepend n) $ buildGraphForUDepString deps
                                  -- add rdeps to graph
                                  withCallback (CbRDepend n) $ buildGraphForUDepString rdeps
                                  -- add pdeps to graph
                                  withCallback (CbPDepend n) $ buildGraphForUDepString pdeps


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
resolveCycle :: [Node] -> GG Bool
resolveCycle cnodes = 
    do  progress (Message "trying to resolve a cycle")
        g <- gets graph
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

    resolveBootstrapCycle :: Node -> Variant -> Node -> Variant -> GG Bool
    resolveBootstrapCycle a' v' a v =
        do
            g <- gets graph
            let incoming  =  filter  (\e@(s',_,d') ->  s' `elem` cnodes &&
                                                       (isJust . getDepAtom $ d') &&
                                                       matchDepAtomVariant (fromJust . getDepAtom $ d') v &&
                                                       (isDependEdge e || isRDependEdge e))
                                     (inn g a')
            if null incoming
              then return False
              else do  s <- get
                       mapM_ (\ (s',t',_) -> removeEdge s' t') incoming
                       cs <- mapM (\ (s',_,d') -> registerEdge s' a d') incoming
                       if all isJust cs
                         then  do  progress (Message $ "Resolved bootstrap cycle at "  ++ (showPV . pv . meta $ v')
                                                                                       ++ " (redirected " ++ show incoming ++ ")")
                                   return True
                         else  do  put s  -- undo unsuccessful redirections
                                   return False

    resolveSelfBlockCycle :: LEdge DepType -> GG Bool
    resolveSelfBlockCycle (s,t,d) =
        do
            g <- gets graph
            case (getVariantsNode g s,getVariantsNode g t) of
              (vs,vs')     ->  let  vsi = intersect (map (extractPS . pvs) vs) (map (extractPS . pvs) vs')
                               in   if not . null $ vsi
                                      then  do  removeEdge s t
                                                progress (Message $ "Resolved self-block cycle for " ++ (showPS $ head vsi)
                                                                                                    ++ " (redirected " ++ show (s,t,d) ++ ")")
                                                return True
                                      else  return False
 
    resolvePDependCycle :: LEdge DepType -> GG Bool
    resolvePDependCycle (s,t,d) =
        do
            g   <-  gets graph
            case isAvailableNode g s of
              Nothing  ->  return False
              Just v   ->  do  let incoming = 
                                     filter  (\e@(s',_,_) ->  s' `elem` cnodes &&
                                                              (isDependEdge e || isRDependEdge e))
                                             (inn g s)
                               if null incoming
                                 then return False
                                 else do  let pv'  =  pv . meta $ v
                                          ls  <-  gets labels
                                          let nm   =  ls M.! v
                                          s <- get
                                          mapM_ (\ (s',t',_) -> removeEdge s' t') incoming
                                          cs <- mapM (\ (s',_,d') -> registerEdge s' (built nm) d') incoming
                                          if all isJust cs
                                            then  do  progress (Message $ "Resolved PDEPEND cycle at " ++ showPV pv' ++ " (redirected " ++ show incoming ++ ")" )
                                                      return True
                                            else  do  put s  -- undo unsuccessful redirections
                                                      return False

sumR :: [GG Bool] -> GG Bool
sumR = foldr (liftM2 (||)) (return False)

allR :: [GG Bool] -> GG Bool
allR = foldr (liftM2 (&&)) (return True)

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

-- | Insert a set of new nodes into the graph, and connects it.
--   Returns if the node has existed before, and the node map.
insVariant :: Variant -> (NodeMap -> Variant -> GG ()) -> GG (Bool,NodeMap)
insVariant v cb =
    do  ls <- gets labels
        case M.lookup v ls of
          Nothing  ->  do  let  ps'  =  extractPS . pvs $ v
                           a <- gets active
                           let  insVariant' bs =
                                    do  modify (\s -> s { active = insertPS ps' (Left v) a })
                                        nm <- insHistory v    -- really adds the nodes
                                        cb nm v               -- ties in the nodes with the rest of the graph
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
resolveBlockers v bs =
    do  ls <- gets labels
        let nm = ls M.! v
        mapM_ (\b ->  let  da         =  (unblock . bdepatom) b
                           lvBlocked  =  maybe False (matchDepAtomVariant da) (E.getLinked v)
                           vBlocked   =  matchDepAtomVariant da v
                           bnm        =  ls M.! bvariant b
                      in   case (vBlocked,lvBlocked,bruntime b) of
                             (False,False,_)     ->  return ()
                             (True,False,False)  ->  registerEdgeAndResolveCycle (built bnm) (built nm)
                                                       (Depend False (bdepatom b))
                             (False,True,rd)     ->  registerEdgeAndResolveCycle (built nm) (built bnm)
                                                       ((if rd then RDepend else Depend) True (bdepatom b))
                             _                   ->  do  ds  <-  get
                                                         let  s  =  strategy . pconfig $ ds
                                                              f  =  Block b v
                                                              x  |  sbacktrack s f  =  Nothing
                                                                 |  otherwise       =  Just ds
                                                         progress (Backtrack x f)
                                                         backtrack) bs

registerEdgeAndResolveCycle :: Int -> Int -> DepType -> GG ()
registerEdgeAndResolveCycle s t d =
    do  r <- registerEdge s t d
        case r of
          Nothing  ->  return ()
          Just c   ->  do  ok <- resolveCycle c
                           if ok
                             then  registerEdgeAndResolveCycle s t d
                             else  return () -- fail

doCallback :: Callback -> DepAtom -> NodeMap -> Variant -> GG ()
doCallback (CbDepend   nm)    =  depend nm
doCallback (CbRDepend  nm)    =  rdepend nm
doCallback (CbPDepend  nm)    =  pdepend nm
doCallback (CbBlock    nm b)  =  blocker b

blocker :: Blocker -> DepAtom -> NodeMap -> Variant -> GG ()
blocker b _ target v =
        do
            let at  =  available target
                d   =  Meta
            registerEdgeAndResolveCycle top at d
            resolveBlockers v [b]

depend :: NodeMap -> DepAtom -> NodeMap -> Variant -> GG ()
depend source da target _ =
        do
            let bs  =  built source
                at  =  available target
                d   =  Depend False da
            registerEdgeAndResolveCycle bs at d

rdepend, pdepend :: NodeMap -> DepAtom -> NodeMap -> Variant -> GG ()
rdepend = rpdepend RDepend
pdepend = rpdepend PDepend

rpdepend :: (Bool -> DepAtom -> DepType) -> NodeMap -> DepAtom -> NodeMap -> Variant -> GG ()
rpdepend rpd source da target _ =
        do
           let as = available source
               at = available target
               d  = rpd False da
           registerEdgeAndResolveCycle as at d
