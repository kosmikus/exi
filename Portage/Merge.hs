{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Merge functionality (including --pretend).
-}

module Portage.Merge
  where

import Control.Monad (when)
import Data.Graph.Inductive hiding (Graph())
import Data.Tree
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Char (toLower)
import Data.Maybe (fromJust, maybeToList)
import Data.IORef
import Data.List (nub)
import System.IO
import System.Exit
import System.Environment

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
import Portage.Tree
import Portage.World

data MergeState =  MergeState
                     {
                       mpretend    ::  Bool,
                       mupdate     ::  Bool,
                       mdeep       ::  Bool,
                       munmask     ::  Bool,
                       mtree       ::  Bool,
                       moneshot    ::  Bool,
                       mbacktrack  ::  Bool,
                       mverbose    ::  Bool,
                       mask        ::  Bool,
                       mcomplete   ::  Bool
                     }

-- | Expand the user-specified goals into a dependency string.
--   Handles the case where the user specified "system" or "world",
--   or in the future possibly more package sets.
expandGoals :: PortageConfig -> MergeState -> String -> IO DepString
expandGoals pc s d =
    do  let d' | d == "system"  =  system pc
               | d == "world"   =  world pc ++ system pc
               | otherwise      =  getDepString' (expand pc) d
        when (mverbose s) $ putStrLn $ "goal: " ++ show d'
        return d'

-- | Generate a dependency graph for a user-specified dependency string.
--   Produces output according to settings. Returns the graph, the flattened
--   graph, plus a boolean indicating whether merging could proceed (i.e., False if
--   the graph is cyclic).
depgraph :: PortageConfig -> MergeState -> DepString -> IO (Graph,[Action],Bool)
depgraph pc s d' = 
    do  let initialState =  DepState
                              {
                                 pconfig   =  pc,
                                 dlocuse   =  [],
                                 graph     =  insNodes [(top,[Top])] empty,
                                 -- precs     =  IM.empty,
                                 labels    =  M.empty,
                                 active    =  M.empty,
                                 saved     =  M.empty,
                                 counter   =  top + 1,
                                 callback  =  CbRDepend (NodeMap top top),
                                 strategy  =  makeStrategy (mupdate s) (munmask s) (mdeep s)
                              }
        let fs = runGGWith initialState $ 
                           do  buildGraphForUDepString d'
                               gr <- gets graph
                               progress Done
        graphCalcProgressTalk (mverbose s) (config pc) (fst fs)
        let gr = graph $ snd $ fs
        let mergeforest  =  dffWith lab' [0] $ gr
        -- note that using postorder in the following is essential for correctness!
        let mergelist    =  concat . postorderF $ mergeforest
        -- Verbose (debug) output.
        when (mverbose s) $ do
          putStr $ if (mtree s)  then  showForest (showAllLines (config pc)) 0 mergeforest
                                 else  unlines $ map show $ mergelist
          putStrLn $ "\nShort version: "
        -- Normal output. (Only if --pretend??)
        putStr $ if (mtree s)  then  showForest (showMergeLines (config pc)) (-1) mergeforest
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
        return (gr,mergelist,null cycles)


merge pc s mergelist d' = do
        let mergelist' = filter isBuilt mergelist
            count = length mergelist'
        putStrLn $ show count ++ case count of
                                     1 -> " package to merge."
                                     _ -> " packages to merge."
        let zeroCheck cont | null mergelist' =
                putStrLn "Nothing to do, exiting."
                           | otherwise = cont
        zeroCheck $ do
        -- If --ask is specified, ask the user if he/she approves the merging.
        userApproves <-
            if mask s
              then do ans <- askUserYesNo (config pc) "Do you want me to merge these packages? "
                      when (not ans) $
                          putStrLn "Quitting."
                      return ans
              else return True
        -- Perform merging if approved.
        when userApproves $
            do  whileSuccess (map (processMergeLine pc s d') mergelist')
                  >|| (putStrLn "Quitting due to errors." >> succeed)
                return ()

-- | Merges a single variant. The "DepString" is only passed along to recognize
--   user-specified targets and possibly add them to the world file after a
--   successful merge.
runEbuild :: PortageConfig -> MergeState -> DepString -> Variant -> IO ExitCode
runEbuild pc s d' v =
    case location m of
      PortageTree pt _ ->
        do
            let file    =  pt ./. (showEbuildPV . pv) m
            let uses    =  diffUse (mergeUse (use cfg) (locuse m)) (iuse e)
            let addEnv  =  [("USE", unwords uses)]
            let ecmd   f op  =  ebuildBin ++ " " ++ quote f ++ " " ++ op
            let ecmd'  f op  =  ecmd f op ++
                                -- ignore error 127 due to a bug in ebuild
                                "; exittemp=$?; if [[ $exittemp -eq 127 ]]; then echo \"The above error is safe to ignore.\"; (exit 0); else (exit $exittemp); fi" 
            env <- getEnvironment
            putStrLn (inColor cfg Green True Default (">>> merging " ++ showPV (pv m)))
            whileSuccess $
              [  systemInEnv cmd (addEnv ++ [ e | e@(v,_) <- env, v /= "USE" ]) |
                 cmd <- [  ecmd file op |
                           -- the final clean should only happen if noclean is unset
                           op <- ["clean", "merge", "clean"] ] ] ++
              -- cleaning should look at AUTOCLEAN
              (  case l of -- do we have an older version of the same package?
                   Just v'  ->  let  m'     =  meta v'
                                     file'  =  dbDir ./. showPV (pv m') ./.
                                               showEbuildPV' (pv m')
                                in   if    (pv m') /= (pv m) -- really important!
                                     then  [  do  putStrLn (inColor cfg Green True Default ("<<< unmerging " ++ showPV (pv m')))
                                                  systemInEnv (ecmd' file' "unmerge") [],
                                              systemInEnv envUpdateBin []]
                                     else  []
                   Nothing  ->  []) ++
              (  if    not (moneshot s) && p `elem` dps
                 then  [do  r <- addToWorldFile pc p
                            when r $ putStrLn (inColor cfg Green True Default (">>> added " ++ showP p ++ " to world file"))
                            succeed]
                 else  []) 
      _ -> succeed  -- or should it be an error?
  where
    cfg  =  config pc
    m    =  meta v
    e    =  ebuild v
    p    =  extractP . pv $ m
    dps  =  map pFromDepAtom (depStringAtoms d')
    l    =  getLinked v

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

processMergeLine :: PortageConfig -> MergeState -> DepString -> Action -> IO ExitCode
processMergeLine pc s d' a =  case a of
                                Built v  ->  runEbuild pc s d' v
                                _        ->  succeed

showAllLines :: Config -> Int -> Bool -> [Action] -> String
showAllLines c n child a =
    case a of
      [Top]          ->  ""
      [Available v]  ->  "_ " ++ replicate (1 + 2*n) ' ' ++ (showPV . pv . meta $ v) ++ "\n"
      Built v : _    ->  showStatus c v ++ replicate (1 + 2*n) ' ' ++ showVariant c v ++ "\n"
      _ : a'         ->  showAllLines c n child a'
      []             ->  ""

-- | Prints the progress of a graph calculation.
-- In particular, it removes the spinner if we're not running on a terminal.
graphCalcProgressTalk :: Bool   -- ^ Verbose
              -> Config
              -> [Progress]
              -> IO ()
graphCalcProgressTalk verbose conf ps = do
    putStr "Calculating dependencies: "
    when verbose $ putStr "\n"
    onTerminal <- hIsTerminalDevice stdout
    let pr | verbose        = putStrLn . concat
           | not onTerminal = putStrLn . last
           | otherwise      = withoutBuffering . putStrLn . spin 10 . concat
    pr . foldr (showProgress verbose conf) [] $ ps
    putStrLn "\n"  

-- | Temporarily disables buffering on stdout.
withoutBuffering :: IO a -> IO a
withoutBuffering x =
    do
        b <- hGetBuffering stdout
        hSetBuffering stdout NoBuffering
        r <- x
        hSetBuffering stdout b
        return r

-- | "Main" function for the "merge" command.
doMerge :: IORef PortageConfig -> MergeState -> [String] -> IO ()
doMerge rpc ms ds =
    readIORef rpc >>= \pc -> do
    msM <- sanityCheck (config pc) ms
    case msM of
      (Just ms') | mcomplete ms' ->
                      do complete pc ds
                 | otherwise ->
                      do  d <- expandGoals pc ms' (unwords ds)
                          (_,pkgs,ok) <- depgraph pc ms' d
                          when (ok && not (mpretend ms')) (merge pc ms' pkgs d)
      Nothing     ->  return ()  -- aborted in sanityCheck

-- | Performs a sanity check on the options specified for the "merge" command.
--   Checks whether certain options are incompatible or imply each other.
sanityCheck :: Config -> MergeState -> IO (Maybe MergeState)
sanityCheck config ms = do
    onTerminal <- hIsTerminalDevice stdin
    let check ms
            | mpretend ms && mask ms = do
                putStrLn ">>> --pretend disables --ask... removing --ask from options."
                check (ms { mask = False })
            | not onTerminal && mask ms = do
                putStrLn $ inColor config Red True Default
                    ">>> You are not on a terminal, yet you use --ask. Aborting."
                return Nothing
            | otherwise = return (Just ms)
    check ms

-- | Function that asks the user a yes\/no question, used for --ask.
askUserYesNo :: Config -> String -> IO Bool
askUserYesNo config prompt =
    withoutBuffering $ do
    putStr $ inColor config Default True Default prompt
    let stubbornAsk = do
            putStr yesNo
            ans <- getLine
            case (map toLower ans) of
              []    -> return True
              "y"   -> return True
              "yes" -> return True
              "n"   -> return False
              "no"  -> return False
              _     -> do putStr $ "Sorry, response '" ++ ans ++ "' not understood. "
                          stubbornAsk
    stubbornAsk
    where
    yesNo = "[" ++
            inColor config Green True Default "Yes" ++
            "/" ++
            inColor config Red   True Default "no"  ++
            "]"

complete :: PortageConfig -> [String] -> IO ()
complete pc [] = complete pc [""]
complete pc [x] = do
    let ptree = tree pc
        (gle, category, packageM) =
            let (gle, rest) = span (`elem` "><=") x
                (cat, rest') = span (/='/') rest
                pkg = case rest' of '/':p -> Just p ; _ -> Nothing
            in (gle, cat, pkg) 
    case packageM of
        Nothing -> do -- no package given, complete categories
            cats <- categories (config pc)
            mapM_ putStrLn [ gle ++ c ++ "/" | c <- cats ]
        Just pkg -> do
            let packagesMap = M.findWithDefault M.empty category (ebuilds . tree $ pc)
                packages = M.toList packagesMap
            mapM_ putStrLn $ do
                (name, variants) <- packages
                v <- case () of
                        _ | null gle -> return ""
                          | otherwise -> map (("-"++) . show . verPV . pv . meta) variants
                return (gle ++ category ++ '/':name ++ v)
complete pc xs = complete pc [last xs] -- this case should never be needed
                                       -- since the bash completion function
                                       -- only sends one word to complete

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
showForest pe d (Node n f : ts)  =  showForest pe (d+1) f ++ pe d (not (null f)) n ++ showForest pe d ts
