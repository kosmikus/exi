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
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.IORef
import System.IO

import Portage.Dependency
import Portage.Ebuild hiding (rdepend)
import Portage.Package
import Portage.Strategy
import Portage.Graph
import Portage.GraphUtils
import Portage.PortageConfig

data MergeState =  MergeState
                     {
                       mupdate   ::  Bool,
                       mverbose  ::  Bool
                     }

pretend :: PortageConfig -> MergeState -> String -> IO Graph
pretend pc s d = 
    do  x <- return $ if mupdate s then pc { strategy = updateStrategy } else pc
        let initialState =  DepState
                              {
                                 pconfig   =  x,
                                 dlocuse   =  [],
                                 graph     =  insNodes [(top,[Top]),(bot,[Bot])] empty,
                                 labels    =  M.empty,
                                 active    =  M.empty,
                                 counter   =  max top bot + 1,
                                 callback  =  rdepend (NodeMap top top top)
                              }
        let d' | d == "system"  =  system x
               | d == "world"   =  world x
               | otherwise      =  getDepString' (expand x) d
        let fs = runGGWith initialState $ 
                           do  buildGraphForUDepString d'
                               gr <- gets graph
                               progress (Message "Graph complete, checking for cycles.")
                               let cycles = cyclesFrom gr [top]
                               if null cycles
                                       then  return ()
                                       else  do  progress (Message "Removing cycles.")
                                                 let loop cs = do b <- allR (map resolveCycle cs)
                                                                  gr <- gets graph
                                                                  let ncs = cyclesFrom gr [top]
                                                                  if b && not (null ncs)
                                                                    then loop ncs 
                                                                    else return ()
                                                 loop cycles
        putStr $ "Calculating dependencies: "
        when (mverbose s) $ putStrLn ""
        (if (not . mverbose $ s) then withoutBuffering else id) $ do
          sequence_ $ foldr (showProgress (mverbose s)) [] (fst fs)
          putStrLn $ "\n"
        let gr = graph $ snd $ fs
        let mergelist = concat $ postorderF $ dffWith lab' [0] $ gr
        when (mverbose s) $ do
          putStr $ unlines $ map show $ mergelist
          putStrLn $ "\nShort version: "
        putStr $ unlines $ 
          concatMap (\ a ->  case a of
                               Built v  ->  [showStatus v ++ " " ++ showVariant (config x) v]
                               _        ->  []) $ mergelist
        let cycles = cyclesFrom gr [top]
        when (not (null cycles)) $
            do
                putStrLn "\nThe graph has cycles:" 
                putStr $ unlines $ map (unlines . map (showNode (mverbose s) gr)) cycles
        return gr

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

showProgress :: Bool -> Progress -> [IO ()] -> [IO ()]
showProgress True   =  showProgressLong
showProgress False  =  showProgressShort

showProgressLong (LookAtEbuild pv o)  r   =  putStrLn (showPV pv ++ " " ++ showOriginLong o) : r
showProgressLong (AddEdge n1 n2 d)    r   =  putStrLn ("added edge " ++ show n1 ++ " " ++ show n2 ++ " " ++ show d) : r
showProgressLong (Message s)          r   =  putStrLn s : r
showProgressLong (Backtrack True _)   r   =  putStrLn "Backtracking" : r
showProgressLong (Backtrack False _)  r   =  putStrLn "Backtracking" : []

showProgressShort (LookAtEbuild pv o)  r  = putStr (showOriginShort o) : r
showProgressShort (AddEdge _ _ _)      r  = r
showProgressShort (Message s)          r  = r
showProgressShort (Backtrack True _)   r  = putStr "B" : r
showProgressShort (Backtrack False _)  r  = putStr "B" : []

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

