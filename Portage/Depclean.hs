{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Performing depclean calculation to determine packages that
    can safely be removed. Also contains code for the unmerge command
    at the moment.
-}

module Portage.Depclean
  where

import Control.Monad
import Data.Graph.Inductive
import Data.List
import Data.Maybe
import Data.Tree
import Data.IORef
import qualified Data.Map as M
import System.IO

import Portage.Config
import Portage.PortageConfig
import Portage.Tree
import Portage.Ebuild
import Portage.World
import Portage.Dependency
import Portage.Match
import Portage.Package
import Portage.Virtual
import Portage.AnsiColor
import Portage.Utilities
import qualified Portage.Merge as Merge

data UnmergeState =  UnmergeState
                     {
                       upretend    ::  Bool,
                       utree       ::  Bool,
                       uoneshot    ::  Bool,
                       uverbose    ::  Bool,
                       uask        ::  Bool,
                       udebug      ::  Bool,
                       ucomplete   ::  Bool
                     }

-- Assumes that dependencies of installed packages are already interpreted,
-- look in |Portage.Ebuild| in function |getInstalledVariantFromDisk|.

-- Why we need a graph:
--
-- First, think of cyclic dependency groups that aren't referenced from world
-- or system. All dependencies of all installed packages do currently count,
-- that's bad.
--
-- Second, we'd need a fixpoint computation otherwise.
--
-- How to proceed? We populate a graph with all installed packages. We enter
-- dependencies as arcs (as before) between the nodes. We create a world meta-node
-- and add system and world depencies. Finally, we check what isn't reachable
-- from the world node.

depcleanGr :: Bool -> UnmergeState -> PortageConfig -> IO [Variant]
depcleanGr rdepclean s pc =
    do  let all = concat . concat . map snd . M.toList . M.map (map snd . M.toList) . ebuilds $ i  -- flattened list of all installed variants
        let gnodes   =  Nothing : map Just all  -- all nodes plus world node
        when (udebug s) $ putStrLn $ "building graph with " ++ show (length gnodes) ++ " nodes"
        let gedges   =  nub $
                        handleDeps Nothing (world pc ++ system pc) ++
                        concatMap
                          (\v -> handleDeps  (Just v)
                                             (  let  e = ebuild v
                                                in   rdepend e ++ pdepend e ++
                                                     if rdepclean then [] else depend e))
                          all
        when (udebug s) $ putStrLn $ "there are " ++ show (length gedges) ++ " edges"
        let (g,nm)  =  mkMapGraph gnodes gedges
        when (udebug s) $ putStrLn $ nodes g `seq` "done"
        -- print g
        let flagged' = reachable (fst $ mkNode_ nm Nothing) (g :: Gr (Maybe Variant) ())
        when (udebug s) $ putStrLn $ "flagged " ++ show (length flagged') ++ " nodes"
        let flagged = map (fromJust) (filter isJust (map (fromJust . lab g) flagged'))
        let unmergelist = all \\ flagged
        putStr (concatMap (showUnmergeLine pc True {- TODO -} 0 False) (map Just unmergelist))
        return unmergelist
  where
    i = inst pc
    handleDeps :: Maybe Variant -> DepString -> [(Maybe Variant, Maybe Variant, ())]
    handleDeps s ds =
      map (\t -> (s,Just t,())) $
      concatMap (\d -> matchDepAtomTree d i) $
      filter (not . blocking) $
      depStringAtoms $
      -- We keep the virtual itself during resolution of virtuals, because there
      -- are new-style virtual packages which are still PROVIDEd. assuming worst
      -- case, the PROVIDEs could have been installed later than the new-style virtual,
      -- and of course we don't know if the new-style virtual doesn't provide any
      -- functionality itself, so it shouldn't be removed.
      concatMap (\v -> let pv = Plain v in [pv,resolveVirtuals pc pv]) $
      depStringAtoms ds

-- | "Main" function for the "unmerge" command. This is very similar
--   to "doMerge" from "Portage.Merge".
doUnmerge :: IORef PortageConfig -> UnmergeState -> [String] -> IO ()
doUnmerge rpc ms ds =
    readIORef rpc >>= \pc -> do
    pc <-  return $
           if udebug ms then pc { config = (config pc) { debug = True } } else pc
    msM <- sanityCheck (config pc) ms
    case msM of
      (Just ms') | ucomplete ms' -> Merge.complete pc ds
                 | otherwise ->
                     do  d <- return $ getDepString' (expand pc) (unwords ds)
                         v <- revdep pc ms' d
                         case v of
                           (Just vs)  ->  do  countMessage "unmerge" (length vs)
                                              when (not (upretend ms')) (unmerge pc ms' vs d)
                           _          ->  return ()
      _ -> return ()  -- aborted in sanityCheck

-- | Performs a sanity check on the options specified for the "unmerge" command.
--   Checks whether certain options are incompatible or imply each other.
sanityCheck :: Config -> UnmergeState -> IO (Maybe UnmergeState)
sanityCheck config ms = do
    onTerminal <- hIsTerminalDevice stdin
    let check ms
            | upretend ms && uask ms = do
                putStrLn ">>> --pretend disables --ask... removing --ask from options."
                check (ms { uask = False })
            | not onTerminal && uask ms = do
                putStrLn $ inColor config Red True Default
                    ">>> You are not on a terminal, yet you use --ask. Aborting."
                return Nothing
            | otherwise = return (Just ms)
    check ms


revdep :: PortageConfig -> UnmergeState -> DepString -> IO (Maybe [Variant])
revdep pc s ds =
    do  let vs  = map Just $ concatMap (flip matchDepAtomTree i) $ depStringAtoms $ ds
        let all = concat . concat . map snd . M.toList . M.map (map snd . M.toList) . ebuilds $ i  -- flattened list of all installed variants
        let gnodes   =  Nothing : map Just all  -- all nodes plus world node
        let gedges   =  nub $
                        handleDeps Nothing ({- world pc ++ -} system pc) ++
                        concatMap
                          (\v -> handleDeps  (Just v)
                                             (  let  e = ebuild v
                                                in   rdepend e ++ pdepend e {- ++ depend e -}))
                          all
        let (g,nm)  =  mkMapGraph gnodes gedges
        let unmergeforest  =  unfoldForest (\ (Node x xs) -> (fromJust (lab g x), xs))
                              $ rdff (map (fst . mkNode_ nm) vs) (g :: Gr (Maybe Variant) ())
        let unmergelist    =  postorderF $ unmergeforest
        -- Normal output. (Only if --pretend??)
        putStr $ if (utree s)  then  showForest (showUnmergeLine pc (uverbose s)) 0 unmergeforest
                               else  concatMap (showUnmergeLine pc (uverbose s) 0 False) unmergelist
        -- Check for occurrence of "system" target.
        if any isNothing unmergelist
          then  do  putStrLn "\nSystem depends on unmerged packages, cannot unmerge safely."
                    return Nothing
          else  return (Just (map fromJust unmergelist))
  where
    i = inst pc
    handleDeps :: Maybe Variant -> DepString -> [(Maybe Variant, Maybe Variant, ())]
    handleDeps s ds =
      map (\t -> (s,Just t,())) $
      concatMap (\d -> matchDepAtomTree d i) $
      filter (not . blocking) $
      depStringAtoms $
      -- We keep the virtual itself during resolution of virtuals, because there
      -- are new-style virtual packages which are still PROVIDEd. assuming worst
      -- case, the PROVIDEs could have been installed later than the new-style virtual,
      -- and of course we don't know if the new-style virtual doesn't provide any
      -- functionality itself, so it shouldn't be removed.
      concatMap (\v -> let pv = Plain v in [pv,resolveVirtuals pc pv]) $
      depStringAtoms ds

-- | "Main" function for the "depclean" and "rdepclean" commands.
doDepclean :: Bool -> IORef PortageConfig -> UnmergeState -> IO ()
doDepclean rdepclean rpc ms =
    readIORef rpc >>= \pc -> do
    msM <- sanityCheck (config pc) ms
    case msM of
      (Just ms') ->  do  vs  <-  depcleanGr rdepclean ms' pc
                         countMessage "unmerge" (length vs)
                         when (not (upretend ms')) (unmerge pc ms' vs [])
      _ -> return ()  -- aborted in sanityCheck
{-
    vs  <-  depcleanGr rdepclean pc
-}

-- | Is similar to |showMergeLine| from |Portage.Merge| and to |showStatus|
--   from |Portage.Ebuild|. Refactoring necessary ...

showUnmergeLine :: PortageConfig -> Bool -> Int -> Bool -> Maybe Variant -> String
showUnmergeLine pc _ n _ Nothing  =
    inColor (config pc) Red True Default "X " ++
    replicate (1 + 2*n) ' ' ++ inColor (config pc) Red True Default "system" ++ "\n"
showUnmergeLine pc verbose n _ (Just v) =
    inColor (config pc) Red True Default "X " ++
    replicate (1 + 2*n) ' ' ++
    showVariant pc verbose v ++ "\n"

unmerge :: PortageConfig -> UnmergeState -> [Variant] -> DepString -> IO ()
unmerge pc s unmergelist d' =
  Merge.action  pc "unmerge" (uask s) 
                (warnDelay  (\n -> putStr $ inColor (config pc) Red True Default (show n ++ " "))
                            (cleanDelay (config pc)))
                unmergelist
                (\ ((m,n),v) -> Merge.runEbuild pc False (not (uoneshot s)) d' m n v)