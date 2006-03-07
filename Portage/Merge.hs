{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Merge functionality (including --pretend).
-}

module Portage.Merge
  where

import Control.Monad (when, foldM)
import Data.Graph.Inductive hiding (Graph())
import Data.Tree
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Maybe (fromJust, maybeToList)
import Data.IORef
import Data.List (nub)
import System.IO
import System.Environment
import System.Exit

import Portage.Dependency hiding (getDepAtom)
import Portage.Ebuild
import Portage.Package
import Portage.Strategy
import Portage.Graph
import Portage.GraphUtils
import Portage.PortageConfig
import Portage.Config
import Portage.Constants
import Portage.Utilities
import Portage.AnsiColor
import Portage.Shell
import Portage.Use

data MergeState =  MergeState
                     {
                       mpretend    ::  Bool,
                       mupdate     ::  Bool,
                       mdeep       ::  Bool,
                       munmask     ::  Bool,
                       mtree       ::  Bool,
                       mbacktrack  ::  Bool,
                       mverbose    ::  Bool
                     }

pretend :: PortageConfig -> MergeState -> String -> IO Graph
pretend pc s d = 
    do  pc <- return $ pc { strategy = makeStrategy (mupdate s) (munmask s) (mdeep s) }
        let initialState =  DepState
                              {
                                 pconfig   =  pc,
                                 dlocuse   =  [],
                                 graph     =  insNodes [(top,[Top])] empty,
                                 -- precs     =  IM.empty,
                                 labels    =  M.empty,
                                 active    =  M.empty,
                                 counter   =  top + 1,
                                 callback  =  CbRDepend (NodeMap top top)
                              }
        let d' | d == "system"  =  system pc
               | d == "world"   =  world pc ++ system pc
               | otherwise      =  getDepString' (expand pc) d
        let fs = runGGWith initialState $ 
                           do  buildGraphForUDepString d'
                               gr <- gets graph
                               progress Done
        putStr $ "Calculating dependencies: "
        when (mverbose s) $ putStrLn ""
        (if (mverbose s) then id else withoutBuffering) $ do
          putStrLn .  
            (if mverbose s then id else spin 10) . concat .
            foldr (showProgress (mverbose s) (config pc)) [] $ fst fs
          putStrLn $ "\n"
        let gr = graph $ snd $ fs
        let mergeforest  =  dffWith lab' [0] $ gr
        let mergelist    =  concat $ postorderF $ dffWith lab' [0] $ gr
        -- Verbose (debug) output.
        when (mverbose s) $ do
          putStr $ if (mtree s)  then  showForest (showAllLines (config pc)) 0 mergeforest
                                 else  unlines $ map show $ mergelist
          putStrLn $ "\nShort version: "
        -- Normal output. (Only if --pretend??)
        putStr $ if (mtree s)  then  showForest (showMergeLines (config pc)) 0 mergeforest
                               else  concatMap (showMergeLine (config pc) 0) mergelist
        let cycles = cyclesFrom gr [top]
        -- If cycles remain, print them.
        when (not (null cycles)) $
            do
                putStrLn "\nThe graph has cycles:" 
                putStr $ unlines $ map (unlines . map (showNode (mverbose s) gr)) cycles
        -- If --unmask, print a list of necessary changes to package.keywords and package.unmask.
        when (munmask s) $
            do
                let vs     =  concatMap (maybeToList . getVariant) .
                              concatMap snd . labNodes $ gr
                    kmask  =  nub $ filter (\v -> any (isKMasked) (masked . meta $ v)) vs
                    hmask  =  nub $ filter (\v -> any (isHMasked) (masked . meta $ v)) vs
                    isKMasked (KeywordMasked _)  =  True
                    isKMasked _                  =  False
                    isHMasked (HardMasked _ _)   =  True
                    isHMasked _                  =  False
                when (not . null $ kmask) $ 
                    do
                        putStrLn $ "\nChanges to " ++ localKeywordsFile ++ ":"
                        putStr . unlines . map (\v -> "=" ++ showPV (pv . meta $ v) ++ " ~" ++ (arch . config $ pc)) $ kmask
                when (not . null $ hmask) $
                    do
                        putStrLn $ "\nChanges to " ++ (localConfigDir ./. packageUnMask) ++ ":"
                        putStr . unlines . map (\v -> "=" ++ showPV (pv . meta $ v)) $ hmask
        -- Perform merging.
        when (null cycles && not (mpretend s)) $
             do
                 exit <- foldM  (\exit a ->  case exit of
                                               ExitSuccess  ->  processMergeLine (config pc) a
                                               _            ->  return exit)
                                ExitSuccess mergelist
                 case exit of
                   ExitSuccess  ->  return ()
                   _            ->  putStrLn "Quitting due to errors."
        return gr

runEbuild :: Config -> Variant -> IO ExitCode
runEbuild cfg v =
    case location m of
      PortageTree pt _ ->
        do
            let file    =  pt ./. (showEbuildPV . pv) m
            let uses    =  diffUse (mergeUse (use cfg) (locuse m)) (iuse e)
            let addEnv  =  [("USE", unwords uses)]
            let cmd     =  ebuildBin ++ " " ++ quote file ++ " merge clean"
            env <- getEnvironment
            putStrLn (inColor cfg Green True Default (">>> " ++ cmd))
            exit <- systemInEnv cmd (addEnv ++ [ e | e@(v,_) <- env, v /= "USE" ])
            case exit of
              ExitSuccess  ->  -- cleaning should look at AUTOCLEAN, and possibly call emerge unmerge directly
                               systemInEnv (emergeBin ++ " --clean") []
              _            ->  return exit
      _ -> return ExitSuccess  -- or should it be an error?
  where
    m  =  meta v
    e  =  ebuild v

showMergeLines :: Config -> Int -> Bool -> [Action] -> String
showMergeLines c n child a =  
    case a of
      Built v : _   ->  showStatus c v ++ replicate (1 + 2*n) ' ' ++ showVariant c v ++ "\n"
      [Top]         ->  ""
      [a] | child   ->  "_ " ++ replicate (1 + 2*n) ' ' ++ (showPV . pv . meta . fromJust . getVariant $ a) ++ "\n"
      _ : a'        ->  showMergeLines c n child a'
      []            ->  ""

showMergeLine :: Config -> Int -> Action -> String
showMergeLine c n a =  case a of
                         Built v  ->  showStatus c v ++ replicate (1 + 2*n) ' ' ++ showVariant c v ++ "\n"
                         _        ->  ""

processMergeLine :: Config -> Action -> IO ExitCode
processMergeLine c a =  case a of
                          Built v  ->  runEbuild c v
                          _        ->  return ExitSuccess

showAllLines :: Config -> Int -> Bool -> [Action] -> String
showAllLines c n child a =
    case a of
      [Top]          ->  ""
      [Available v]  ->  "_ " ++ replicate (1 + 2*n) ' ' ++ (showPV . pv . meta $ v) ++ "\n"
      Built v : _    ->  showStatus c v ++ replicate (1 + 2*n) ' ' ++ showVariant c v ++ "\n"
      _ : a'         ->  showAllLines c n child a'
      []             ->  ""

-- | Temporarily disables buffering on stdout. Should probably depend on
--   whether the output is a terminal of a file.
withoutBuffering :: IO a -> IO a
withoutBuffering x =
    do
        b <- hGetBuffering stdout
        hSetBuffering stdout NoBuffering
        r <- x
        hSetBuffering stdout b
        return r

doMerge :: IORef PortageConfig -> MergeState -> [String] -> IO ()
doMerge r s ds = readIORef r >>= \pc -> pretend pc s (unwords ds) >> return ()

showNode :: Bool -> DGraph -> Int -> String
showNode v gr n = (show . fromJust . lab gr $ n) ++ number
  where number = if v then " [" ++ show n ++ "]" else ""

showProgress :: Bool -> Config -> Progress -> [String] -> [String]
showProgress True   =  showProgressLong
showProgress False  =  showProgressShort

showProgressLong c (LookAtEbuild pv o)     r   =  (showPV pv ++ " " ++ showOriginLong o ++ "\n") : r
showProgressLong c (AddEdge n1 n2 d)       r   =  ("added edge " ++ show n1 ++ " " ++ show n2 ++ " " ++ show d ++ "\n") : r
showProgressLong c (AddNode nm v)          r   =  ("added nodes " ++ showNodeMap nm ++ " " ++ showVariant c v ++ "\n") : r
showProgressLong c (Message s)             r   =  (s ++ "\n") : r
showProgressLong c (Backtrack Nothing f)   r   =  showFailure c f : r
showProgressLong c (Backtrack (Just s) f)  r   =  ("\n" ++ showFailure c f ++ printStackTrace s) : []
showProgressLong c Done                    r   =  "\nGraph complete." : []

showProgressShort c (LookAtEbuild pv o)     r  =  showOriginShort o : r
showProgressShort c (AddEdge _ _ _)         r  =  r
showProgressShort c (AddNode _ _)           r  =  r
showProgressShort c (Message s)             r  =  r
showProgressShort c (Backtrack Nothing f)   r  =  "B" : r
showProgressShort c (Backtrack (Just s) f)  r  =  ("B" ++ "\n\n" ++ showFailure c f ++ printStackTrace s) : []
showProgressShort c Done                    r  =  "\nDone." : []

showFailure c (AllMasked da vs) =
    inColor c Red True Default ("All variants that could satisfy " ++ show da ++ " are masked.\n" ++ "Candidates:\n") ++ 
    unlines (map (showVariantMasked c) vs)
showFailure c (NoneInstalled da vs) =
    inColor c Red True Default ("None of the variants that could satisfy " ++ show da ++ " are installed.\n") ++
    "Candidates:\n" ++
    unlines (map (showVariantMasked c) vs)
showFailure c (Block (Blocker v1 da _) v2) =
    inColor c Red True Default ("The package\n" ++ showVariant c v1 ++ "\nis blocked (" ++ show da ++ ") by the package\n" ++
    showVariant c v2 ++ "\n")
showFailure c (Cycle p) =
    unlines (  inColor c Red True Default ("Could not resolve cyclic dependencies in graph:") : 
               map (showStackLine c) p)
showFailure c (SlotConflict v1 v2) =
    inColor c Red True Default ("Dependencies require two incompatible variants simultaneously.\n") ++ 
    showVariantMasked c v1 ++ "\n" ++
    showVariantMasked c v2 ++ "\n"
showFailure c (Other s) = inColor c Red True Default s ++ "\n"

printStackTrace :: DepState -> String
printStackTrace s =
    let  c = config . pconfig $ s
    in   unlines (inColor c Red True Default "Stack:" : map (showStackLine c) (stackTrace s))

showStackLine :: Config -> (Variant,Maybe DepType) -> String
showStackLine c (v,d)  =  showVariant' c v ++ showDepend d
  where
    showDepend Nothing                     =  ""
    showDepend (Just Meta)                 =  ""
    showDepend (Just (Depend False da))    =  " depends on " ++ show da
    showDepend (Just (RDepend False da))   =  " runtime-depends on " ++ show da
    showDepend (Just (PDepend False da))   =  " post-depends on " ++ show da
    showDepend _                           =  " (illegal dependency)"

stackTrace :: DepState -> [(Variant,Maybe DepType)]
stackTrace s =
    let  g   =  graph s
         n   =  built (nodemap (callback s))
         p   =  sp top n (emap (const 1.0) g)
    in   pathTrace s p Nothing

showOriginLong FromCache         =  "(from cache)"
showOriginLong CacheRegen        =  "(regenerated cache entry)"
showOriginLong EclassDummy       =  "(made eclass dummy)"
showOriginLong FromInstalledDB   =  "(available)"
showOriginLong IsProvided        =  "(provided)"

showOriginShort FromCache        =  "."
showOriginShort CacheRegen       =  "C"
showOriginShort EclassDummy      =  "E"
showOriginShort FromInstalledDB  =  "i"
showOriginShort IsProvided       =  "p"

showForest :: (Int -> Bool -> a -> String) -> Int -> Forest a -> String
showForest pe d []               =  ""
showForest pe d (Node n f : ts)  =  pe d (not (null f)) n ++ showForest pe (d+1) f ++ showForest pe d ts
