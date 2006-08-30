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
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe, maybeToList)
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
import Portage.Virtual
import Portage.Ebuild (Variant(..), Ebuild(iuse), EbuildMeta(..), EbuildOrigin(..), TreeLocation(..), Mask(..), Link(..), pvs)
import qualified Portage.Ebuild as E
import Portage.GraphGeneration

findVersions :: Tree -> DepAtom -> [Variant]
findVersions = flip matchDepAtomTree

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

-- | Assumes that the depstring is a list of alternatives. This
--   function simplifies such a list by eliminating masked alternatives,
--   and then calls |buildGraphForOr'| on the simplified list.
buildGraphForOr :: DepString -> GG ()
buildGraphForOr ds  =
    do
        pc <- gets pconfig
        case filterUnmasked pc ds of
          []   ->  buildGraphForOr' ds  -- should fail here already
          ds'  ->  do  
                       progress (Message $ "|| (" ++ show ds ++ ") simplified to || (" ++ show ds' ++ ")")
                       buildGraphForOr' ds'
  where
    filterUnmasked :: PortageConfig -> DepString -> DepString
    filterUnmasked pc = filter (isUnmaskedTerm . resolveVirtuals pc)
      where
        isUnmaskedTerm :: DepTerm -> Bool
        isUnmaskedTerm (Or ds)     =  any isUnmaskedTerm ds
        isUnmaskedTerm (And ds)    =  all isUnmaskedTerm ds
        isUnmaskedTerm (Plain d)   =  isUnmasked pc d

-- | Helper function for |buildGraphForOr|. If any of the alternatives
--   is available, it is chosen. Otherwise, we select the default (i.e.,
--   the first element).
buildGraphForOr' :: DepString -> GG ()
buildGraphForOr' []         =  return ()  -- strange case, empty OR, but ok
buildGraphForOr' ds@(dt:_)  =
    do
        pc    <-  gets pconfig
        case findInstalled pc ds of
          Nothing   ->  do  progress (Message $ "|| (" ++ show ds ++ ")) resolved to default")
                            buildGraphForDepTerm dt   -- default
          Just dt'  ->  do  progress (Message $ "|| (" ++ show ds ++ ")) resolved to available: " ++ show dt')
                            buildGraphForDepTerm dt'  -- installed
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
-- if only a and\/or b is installed it selects a+b, 
-- if c and one of a or b is installed it selects c

-- | Checks whether a given depatom is available.
isInstalled :: PortageConfig -> DepAtom -> Bool
isInstalled pc da =
    let  t  =  itree pc
    in   not . null . filter (E.isAvailable . location . meta) $
         (t !? (pFromDepAtom da))

-- | Checks whether a given depatom has an unmasked match.
isUnmasked :: PortageConfig -> DepAtom -> Bool
isUnmasked pc da =
    let  t  =  itree pc
    in   not . null . E.filterMaskedVariants $
         findVersions t da

-- | Temporarily changes the set of local use flags.
withLocUse :: [UseFlag] -> GG a -> GG a
withLocUse luse' g =
    do
        luse <- gets dlocuse
        modify (\s -> s { dlocuse = luse' })
        r <- g
        modify (\s -> s { dlocuse = luse })
        return r

-- | Temporarily changes the callback.
withCallback :: Callback -> GG a -> GG a
withCallback cb' g =
    do
        cb <- gets callback
        modify (\s -> s { callback = cb' })
        r <- g
        modify (\s -> s { callback = cb })
        return r

-- | Temporarily changes the strategy.
withStrategy :: Strategy -> GG a -> GG a
withStrategy st' g =
    do
        st <- gets strategy
        modify (\s -> s { strategy = st' })
        r <- g
        modify (\s -> s { strategy = st })
        return r

-- | Handles depgraph generation for a single depatom. This is where
--   a lot of subtle magic happens currently.
buildGraphForDepAtom :: DepAtom -> GG ()
buildGraphForDepAtom da
    | blocking da =
        do  -- for blocking dependencies, we have to consider all matching versions
            pc  <-  gets pconfig
            g   <-  gets graph
            cb  <-  gets callback
            s   <-  gets strategy
            let  nm  =  nodemap cb
                 t   =  itree pc
                 w   =  head (getVariantsNode g (available nm))
                 b   =  Blocker w da (case cb of CbRDepend _ -> Just True; _ -> Just False)
                 p   =  extractP (pv . meta $ w)
                 ps  =  extractPS (pvs w)
            ls  <-  gets labels
            -- for each variant, there are the following possibilities:
            -- 0. we ignore self-blockers (but with a headache)
            -- 1. if v is in the graph, resolve the blocker b for v
            -- 2. if a variant v' of the same slot as v is in the graph
            --    already, we can pass, because the blocker will be resolved
            --    for v'
            -- 3. if v is available, we have to choose another variant v'
            -- 4. if v is unavailable, we rely on the recorded blocker
            progress (Message $ "blocker " ++ show da)
            -- remember the blocker
            modify (\s -> s { saved = M.insertWith (++) (pFromDepAtom da) [b] (saved s) })
            mapM_ (\v -> do
                             let pv' = pv . meta $ v
                             let p' = extractP pv'
                             progress (LookAtEbuild pv' (origin (meta v)))
                             let ps' = extractPS (pvs v)
                             progress (Message $ "comparing " ++ show p ++ "(" ++ show (pvs w) ++ ") vs. " ++ show p') -- was ps, ps'
                             when (p /= p') $ do -- 0.  WAS: ps /= ps'
                               case M.lookup v ls of
                                 Just _   ->  resolveBlockers v [b] -- 1.
                                 Nothing  ->  do  if E.isAvailable . location . meta $ v
                                                           then  let  reject = do  ds  <-  get
                                                                                   s   <-  gets strategy
                                                                                   let  f  =  Block b v
                                                                                        x  |  sbacktrack s f  =  Nothing
                                                                                           |  otherwise       =  Just ds
                                                                                   progress (Backtrack x f)
                                                                                   backtrack
                                                                 in   choiceM [withCallback (CbBlock nm b) $ chooseVariant [reject] (const reject) -- 3.
                                                                              ,reject] -- never backtrack beyond this point
                                                           else  return () -- 4.
                  )
                  (findVersions t (unblock da))
            progress (Message $ "done with blocker " ++ show da ++ " (for now)")
    | otherwise =  let  reject f  =  do  ds <- get
                                         s  <- gets strategy
                                         let  b  |  sbacktrack s f  =  Nothing
                                                 |  otherwise       =  Just ds
                                         progress (Backtrack b f)
                                         backtrack
                        p         =  pFromDepAtom da
                        choice    =  [  progress (Message "CONTINUING") >>
                                        continue  (== Just p) 
                                                  (\s -> s { saved =  M.insertWith (++) p
                                                                        [Blocker (error "my brain just exploded") {- TODO -} da Nothing] (saved s) })
                                     ]
                   in   chooseVariant choice reject
  where
    -- failChoice: what to do if all acceptable ebuilds fail (nothing normally, but error for blockers)
    -- failReject: what to do if all ebuilds are masked (complain about blocker instead?)
    chooseVariant failChoice failReject =
        do  pc  <-  gets pconfig
            s   <-  gets strategy
            let  t     =  itree pc
                 p     =  pFromDepAtom da
                 -- it might be a good idea to speed up the process of finding
                 -- the correct variants by preferring currently active variants
                 guse  =  use (config pc)
                 vs    =  findVersions t da
            case sselect s guse da vs of
              Reject f      ->  failReject f
              Accept vs ns  ->
                do   v@(Variant m e) <- lchoiceM (Just p) (map return vs ++ failChoice)
                     progress (Message $ "CHOOSING: " ++ E.showVariant' (config pc) v ++ " (out of " ++ show (length vs) ++ ")")
                     let  avail  =  E.isAvailable (location m)  -- installed or provided?
                          luse   =  mergeUse guse (locuse m)
                          stop   =  avail && sstop s v  -- if it's an installed ebuild, we can decide to stop here!
                     -- set new local USE context
                     withLocUse luse $ withStrategy ns $ do
                       progress (LookAtEbuild (pv (meta v)) (origin (meta v)))
                       -- insert nodes for v, and activate
                       cb <- gets callback
                       (already,n) <- insVariant v (doCallback cb da)
                       if already || stop
                         then progress (Message $ "stopping at " ++ showPV (pv (meta v)))
                         else do  let  rdeps    =  E.rdepend  e
                                       deps     =  E.depend   e
                                       pdeps    =  E.pdepend  e
                                  -- add deps to graph
                                  withCallback (CbDepend n) $ buildGraphForUDepString deps
                                  -- add rdeps to graph
                                  withCallback (CbRDepend n) $ buildGraphForUDepString rdeps
                                  -- add pdeps to graph
                                  withCallback (CbPDepend n) $ buildGraphForUDepString pdeps
                                  progress (Message $ "done with dependencies for " ++ showPV (pv (meta v)))


-- | Checks whether a certain node in the graph corresponds to an upgrade.
--   This is the case if there's an older version of the same slot that
--   is available on the system (such packages are already linked in the
--   ebuild data structure). If yes, we return the node, the new variant,
--   and the old variant in a tuple.
hasUpgradeHistory :: Graph -> Node -> Maybe (Node,Variant,Variant)
hasUpgradeHistory g n = 
   listToMaybe $ [ (n,v',v) |  let va' = isAvailableNode g n, isJust va',
                               let (Just v') = va',
                               let lv = E.getLinked v', isJust lv,
                               let (Just v) = lv ]

-- | Checks whether a certain node in the graph corresponds to an already
--   installed variant. If yes, we return the node and twice that variant
--   in a tuple.
hasAvailableHistory :: Graph -> Node -> Maybe (Node,Variant,Variant)
hasAvailableHistory g n =
   listToMaybe $ [ (n,v,v)  |  let va = isAvailableNode g n, isJust va,
                               let (Just v) = va,
                               E.isAvailable . location . meta $ v ]

-- Types of cycles:
-- o Bootstrap cycle. A cycle which wants to recursively update itself, but a
--   version is already installed (happens if an upgrade strategy always selects
--   the most recent version). If we have an Upgrade-history, and incoming dependencies
--   on the newer version from within the cycle, we can redirect these dependencies
--   to the old version. Also, if the package is available and not upgraded, dependencies
--   on the package can be dropped to resolve cycles.
-- o PDEPEND cycle. All cycles that contain PDEPEND edges. For an Available-type node
--   with an outgoing PDEPEND, we redirect incoming DEPENDs and RDEPENDs from within 
--   the cycle to the Built state.
-- This is currently very fragile w.r.t. ordering of the resolutions.
resolveCycle :: [Node] -> SavedDep -> GG ()
resolveCycle cnodes sd = 
    do  g <- gets graph
        let  cedges  =  zipWith findEdge (map (out g) cnodes) (tail cnodes)
                        ++ [(s0,t0,sd)]
        progress (Message (show cnodes ++ "\n" ++ show cedges))
        firstM  (  map (resolvePDependCycle cedges) (filter isPDependEdge cedges) ++
                   (map  (\ (a',v',v) -> resolveBootstrapCycle cedges a' v' v)
                         (detectBootstrapCycle g cnodes)))
                (do  ds  <-  get
                     s   <-  gets strategy
                     let  f                     =  Cycle (cycleTrace ds cnodes sd)
                          b  |  sbacktrack s f  =  Nothing
                             |  otherwise       =  Just ds
                     progress (Backtrack b f)
                     backtrack)
  where
    s0  =  last cnodes
    t0  =  head cnodes

    detectBootstrapCycle :: Graph -> [Node] -> [(Node,Variant,Variant)]
    detectBootstrapCycle g ns =
       concatMap (maybeToList . hasUpgradeHistory g) ns
       ++ concatMap (maybeToList . hasAvailableHistory g) ns

    resolveBootstrapCycle :: [LEdge SavedDep] -> Node -> Variant -> Variant -> GG Bool
    resolveBootstrapCycle cedges a' v' v =
        do
            progress (Message "trying to resolve bootstrap cycle")
            g <- gets graph
            let incoming  =  filter  (\e@(_,t',d') ->  t' == a' &&
                                                       (isJust . getDepAtom $ d') &&
                                                       matchDepAtomVariant (fromJust . getDepAtom $ d') v &&
                                                       (isDependEdge e || isRDependEdge e || isPDependEdge e))
                                     cedges
            if null incoming
              then do  progress (Message "none incoming")
                       return False
              else do  let (s',t',d'):_ = incoming
                       removeEdge s' t'
                       -- register original edge, if not selected for removal
                       c0 <- if s' /= s0 then registerEdge s0 t0 sd else return Nothing
                       -- check if t' is still reachable from the top;
                       -- (this is a bit subtle: if it is not, it'd be possible
                       -- to `lose' parts of the graph by the edge removal)
                       g' <- gets graph
                       -- inserting a node from top helps, but destroys the tree
                       -- structure
                       let reach = top `elem` ancestors t' g'
                       c1 <- if not reach then registerEdge top t' Meta else return Nothing
                       if isNothing c0 && isNothing c1
                         then  do  progress (Message $ "Resolved bootstrap cycle at "  ++ (showPV . pv . meta $ v')
                                                                                       ++ " (dropped " ++ show incoming ++ ")")
                                   return True
                         else  do  progress (Message $ "unsuccessful attempt to resolve bootstrap cycle")
                                   return False

    resolvePDependCycle :: [LEdge SavedDep] -> LEdge SavedDep -> GG Bool
    resolvePDependCycle cedges (s,t,d) =
        do
            progress (Message "trying to resolve PDEPEND cycle")
            g   <-  gets graph
            let (Just v) = isAvailableNode g s
            let incoming = filter  (\e@(_,t',_) ->  t' == s &&
                                                    (isDependEdge e || isRDependEdge e))
                                   cedges
            if null incoming
              then do  progress (Message "none incoming")
                       return False
              else do  let (s',t',d'):_ = incoming
                       let pv'  =  pv . meta $ v
                       ls  <-  gets labels
                       let nm   =  ls M.! v
                       removeEdge s' t'
                       -- register original edge, if not selected for removal
                       c0 <- if s' /= s0 then registerEdge s0 t0 sd else return Nothing
                       cs <- registerEdge s' (built nm) d'
                       if all isNothing [c0,cs]
                         then  do  progress (Message $ "Resolved PDEPEND cycle at " ++ showPV pv' ++ " (redirected " ++ show incoming ++ ")" )
                                   return True
                         else  do  progress (Message $ "unsuccessful attempt to resolve PDEPEND cycle")
                                   return False

sumR :: [GG Bool] -> GG Bool
sumR = foldr (liftM2 (||)) (return False)

allR :: [GG Bool] -> GG Bool
allR = foldr (liftM2 (&&)) (return True)

-- | Insert a set of new nodes into the graph, and connects it.
--   Returns if the node has existed before, and the node map.
insVariant :: Variant -> (NodeMap -> Variant -> GG ()) -> GG (Bool,NodeMap)
insVariant v cb =
    do  ls <- gets labels
        case M.lookup v ls of
          Nothing  ->  do  let  ps'  =  extractPS . pvs $ v
                                p    =  extractP . pv . meta $ v
                           a <- gets active
                           b <- gets saved
                           let  insVariant' bs =
                                    do  modify (\s -> s { active = insertPS ps' v a })
                                        nm <- insHistory v    -- really adds the nodes
                                        cb nm v               -- ties in the nodes with the rest of the graph
                                        resolveBlockers v bs  -- checks the previously accumulated blockers for this package
                                        return nm
                           nm <- case lookupPS ps' a of
                                   Just v'          ->  do  s   <-  gets strategy
                                                            ds  <-  get
                                                            let  f  =  SlotConflict v v'
                                                                 b  |  sbacktrack s f  =  Nothing
                                                                    |  otherwise       =  Just ds
                                                            progress (Message "Slot conflict")
                                                            continue (== Just p) id -- testing
                                   Nothing          ->  case M.lookup p b of
                                                          Nothing  ->  insVariant' []
                                                          Just bs  ->  insVariant' bs
                           return (False,nm)
          Just nm  ->  do  cb nm v
                           return (True,nm)

resolveBlockers :: Variant -> [Blocker] -> GG ()
resolveBlockers v bs =
    do  ls <- gets labels
        let nm = ls M.! v
        mapM_ (\b ->  let  da         =  (unblock . bdepatom) b
                           -- linked variant exists and blocked?
                           lvBlocked  =  maybe False (matchDepAtomVariant da) (E.getLinked v)
                           -- variant itself blocked?
                           vBlocked   =  matchDepAtomVariant da v
                           bnm        =  ls M.! bvariant b
                           -- bruntime returns Nothing on a saved dependency
                           --                  Just False on a build-time blocker
                           --                  Just True on a run-time blocker
                      in   case (vBlocked,lvBlocked,bruntime b) of
                             (False,False,Just _)     ->  return ()  -- nothing is blocked
                             (True,_,Nothing)         ->  return ()  -- saved dependency fulfilled
                             (True,False,Just False)  ->  registerEdgeAndResolveCycle (built nm) (built bnm)
                                                            (SavedDep Depend (bvariant b) True (bdepatom b))
                             (False,True,Just rd)     ->  registerEdgeAndResolveCycle (built bnm) (built nm)
                                                            (SavedDep (if rd then RDepend else Depend) (bvariant b) False (bdepatom b))
                             _                        ->  do  ds  <-  get
                                                              let  s  =  strategy ds
                                                                   f  =  Block b v
                                                                   x  |  sbacktrack s f  =  Nothing
                                                                      |  otherwise       =  Just ds
                                                              progress (Backtrack x f)
                                                              backtrack) bs

registerEdgeAndResolveCycle :: Int -> Int -> SavedDep -> GG ()
registerEdgeAndResolveCycle s t d =
    do  progress (Message $ "trying to register edge " ++ show s ++ " " ++ show t ++ " " ++ show d)
        r <- registerEdge s t d
        case r of
          Nothing  ->  return ()
          Just c   ->  resolveCycle c d

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
depend source da target v =
        do
            let bs  =  built source
                at  =  available target
                d   =  SavedDep Depend v False da
            registerEdgeAndResolveCycle bs at d

rdepend, pdepend :: NodeMap -> DepAtom -> NodeMap -> Variant -> GG ()
rdepend = rpdepend RDepend
pdepend = rpdepend PDepend

rpdepend :: DepType -> NodeMap -> DepAtom -> NodeMap -> Variant -> GG ()
rpdepend dt source da target v =
        do
           let as = available source
               at = available target
               d  = SavedDep dt v False da
           registerEdgeAndResolveCycle as at d
