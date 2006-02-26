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
import Data.Maybe (fromJust, maybeToList)
import Data.IORef
import System.IO

import Portage.Dependency hiding (getDepAtom)
import Portage.Ebuild
import Portage.Package
import Portage.Strategy
import Portage.Graph
import Portage.GraphUtils
import Portage.PortageConfig
import Portage.Config

data MergeState =  MergeState
                     {
                       mupdate   ::  Bool,
                       munmask   ::  Bool,
                       mtree     ::  Bool,
                       mverbose  ::  Bool
                     }

pretend :: PortageConfig -> MergeState -> String -> IO Graph
pretend pc s d = 
    do  x <- return $ pc { strategy = makeStrategy (mupdate s) (munmask s) }
        let initialState =  DepState
                              {
                                 pconfig   =  x,
                                 dlocuse   =  [],
                                 graph     =  insNodes [(top,[Top])] empty,
                                 -- precs     =  IM.empty,
                                 labels    =  M.empty,
                                 active    =  M.empty,
                                 counter   =  top + 1,
                                 callback  =  CbRDepend (NodeMap top top)
                              }
        let d' | d == "system"  =  system x
               | d == "world"   =  world x ++ system x
               | otherwise      =  getDepString' (expand x) d
        let fs = runGGWith initialState $ 
                           do  buildGraphForUDepString d'
                               gr <- gets graph
                               progress (Message "Graph complete.")
        putStr $ "Calculating dependencies: "
        when (mverbose s) $ putStrLn ""
        (if (not . mverbose $ s) then withoutBuffering else id) $ do
          sequence_ $ foldr (showProgress (mverbose s) (config pc)) [] (fst fs)
          putStrLn $ "\n"
        let gr = graph $ snd $ fs
        let mergeforest  =  dffWith lab' [0] $ gr
        let mergelist    =  concat $ postorderF $ dffWith lab' [0] $ gr
        when (mverbose s) $ do
          putStr $ if (mtree s)  then  showForest (showAllLines (config x)) 0 mergeforest
                                 else  unlines $ map show $ mergelist
          putStrLn $ "\nShort version: "
        putStr $ if (mtree s)  then  showForest (showMergeLines (config x)) 0 mergeforest
                               else  concatMap (showMergeLine (config x) 0) mergelist
        let cycles = cyclesFrom gr [top]
        when (not (null cycles)) $
            do
                putStrLn "\nThe graph has cycles:" 
                putStr $ unlines $ map (unlines . map (showNode (mverbose s) gr)) cycles
        return gr

showMergeLines :: Config -> Int -> Bool -> [Action] -> String
showMergeLines c n child a =  
    case a of
      Built v : _   ->  showStatus v ++ replicate (1 + 2*n) ' ' ++ showVariant c v ++ "\n"
      [Top]         ->  ""
      [a] | child   ->  "_ " ++ replicate (1 + 2*n) ' ' ++ (showPV . pv . meta . fromJust . getVariant $ a) ++ "\n"
      _ : a'        ->  showMergeLines c n child a'
      []            ->  ""

showMergeLine :: Config -> Int -> Action -> String
showMergeLine c n a =  case a of
                         Built v  ->  showStatus v ++ replicate (1 + 2*n) ' ' ++ showVariant c v ++ "\n"
                         _        ->  ""

showAllLines :: Config -> Int -> Bool -> [Action] -> String
showAllLines c n child a =
    case a of
      [Top]          ->  ""
      [Available v]  ->  "_ " ++ replicate (1 + 2*n) ' ' ++ (showPV . pv . meta $ v) ++ "\n"
      Built v : _    ->  showStatus v ++ replicate (1 + 2*n) ' ' ++ showVariant c v ++ "\n"
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

showProgress :: Bool -> Config -> Progress -> [IO ()] -> [IO ()]
showProgress True   =  showProgressLong
showProgress False  =  showProgressShort

showProgressLong c (LookAtEbuild pv o)     r   =  putStrLn (showPV pv ++ " " ++ showOriginLong o) : r
showProgressLong c (AddEdge n1 n2 d)       r   =  putStrLn ("added edge " ++ show n1 ++ " " ++ show n2 ++ " " ++ show d) : r
showProgressLong c (AddNode nm v)          r   =  putStrLn ("added nodes " ++ showNodeMap nm ++ " " ++ showVariant c v) : r
showProgressLong c (Message s)             r   =  putStrLn s : r
showProgressLong c (Backtrack Nothing f)   r   =  putStr (showFailure c f) : r
showProgressLong c (Backtrack (Just s) f)  r   =  (putStr (showFailure c f) >> printStackTrace s) : []

showProgressShort c (LookAtEbuild pv o)     r  =  putStr (showOriginShort o) : r
showProgressShort c (AddEdge _ _ _)         r  =  r
showProgressShort c (AddNode _ _)           r  =  r
showProgressShort c (Message s)             r  =  r
showProgressShort c (Backtrack Nothing f)   r  =  putStr "B" : r
showProgressShort c (Backtrack (Just s) f)  r  =  (putStrLn "B" >> putStr (showFailure c f) >> printStackTrace s) : []

showFailure c (AllMasked da vs) =
    "All variants that could satisfy " ++ show da ++ " are masked.\n" ++
    "Candidates:\n" ++ 
    unlines (map (showVariant c) vs)
showFailure c (NoneInstalled da vs) =
    "None of the variants that could satisfy " ++ show da ++ " are installed.\n" ++
    "Candidates:\n" ++
    unlines (map (showVariant c) vs)
showFailure c (Block (Blocker v1 da _) v2) =
    "The package\n" ++ showVariant c v1 ++ "\nis blocked (" ++ show da ++ ") by the package\n" ++
    showVariant c v2 ++ "\n"
showFailure c (SlotConflict v1 v2) =
    "Dependencies require two incompatible variants simultaneously.\n" ++ 
    showVariant c v1 ++ "\n" ++
    showVariant c v2 ++ "\n"
showFailure c (Other s) = s ++ "\n"

printStackTrace :: DepState -> IO ()
printStackTrace s =
    do
        putStrLn "Stack:"
        putStr $ unlines (map (showStackLine (config . pconfig $ s)) (stackTrace s))

  where
    showStackLine c (v,d)  =  showVariant' c v ++ showDepend d
    showDepend Nothing     =  ""
    showDepend (Just da)   =  " depends on " ++ show (fromJust . getDepAtom $ da) ++ ":"

stackTrace :: DepState -> [(Variant,Maybe DepType)]
stackTrace s = 
    let  g   =  graph s
         n   =  built (nodemap (callback s))
         p   =  sp top n (emap (const 1.0) g)
         es  =  map Just (zipWith (labEdge g) p (tail p)) ++ [Nothing]
    in   [  (r,x) |
            (a,x) <- zip p es, case x of { Just Meta -> False; _ -> True },
            Just r <- [getVariant . head . fromJust . lab g $ a] ]

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
