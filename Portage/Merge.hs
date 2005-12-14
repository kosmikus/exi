{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Merge functionality (including --pretend).
-}

module Portage.Merge
  where

import Control.Monad (when)
import Control.Monad.State
import Data.Graph.Inductive hiding (Graph())
import qualified Data.Map as M
import Data.Maybe (fromJust)

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

pretend :: MergeState -> String -> IO Graph
pretend s d = 
    do  x <- fmap (if mupdate s then (\x -> x { strategy = updateStrategy }) else id ) portageConfig
        let initialState =  DepState
                              {
                                 pconfig   =  x,
                                 dlocuse   =  [],
                                 graph     =  insNodes [(top,[Top]),(bot,[Bot])] empty,
                                 labels    =  M.empty,
                                 active    =  M.empty,
                                 counter   =  max top bot + 1,
                                 callback  =  rdepend (top,top,top)
                              }
        let fs = runState (do  p1 <- buildGraphForUDepString (getDepString d)
                               gr <- gets graph
                               let cycles = cyclesFrom gr [top]
                               p2 <- if True -- (null cycles)
                                       then  return []
                                       else  do  let p21 = [Message "Graph complete, removing cycles."]
                                                 let loop cs = do (b,ps1) <- allR (map resolveCycle cs)
                                                                  gr <- gets graph
                                                                  let ncs = cyclesFrom gr [top]
                                                                  if b && not (null ncs)
                                                                    then do  ps2 <- loop ncs 
                                                                             return $ ps1 ++ ps2
                                                                    else return ps1
                                                 p22 <- loop cycles
                                                 return $ p21 ++ p22
                               return $ p1 ++ p2
                          ) initialState 
        putStr $ "Calculating dependencies: "
        when (mverbose s) $ putStrLn ""
        putStr $ concatMap (showProgress (mverbose s)) $ fst fs
        putStrLn $ "\n"
        let gr = graph $ snd $ fs
        let mergelist = concat $ postorderF $ dffWith lab' [0] $ gr
        putStr $ unlines $ map show $ mergelist
        putStrLn $ "\nShort version: "
        putStr $ unlines  $  map (showAction (config x)) 
                          $  filter (\ a -> case a of Built _ -> True; _ -> False) $ mergelist
        let cycles = cyclesFrom gr [top]
        when (not (null cycles)) $
            do
                putStrLn "\nThe graph has cycles:" 
                putStr $ unlines $ map (unlines . map (showNode (mverbose s) gr)) cycles
        return gr

doMerge :: MergeState -> [String] -> IO ()
doMerge s ds = pretend s (unwords ds) >> return ()

showNode :: Bool -> DGraph -> Int -> String
showNode v gr n = (show . fromJust . lab gr $ n) ++ number
  where number = if v then " [" ++ show n ++ "]" else ""

showProgress :: Bool -> Progress -> String
showProgress True = showProgressLong
showProgress False = showProgressShort

showProgressLong (LookAtEbuild pv o) = showPV pv ++ " " ++ showOriginLong o ++ "\n"
showProgressLong (AddEdge n1 n2 d)   = "added edge " ++ show n1 ++ " " ++ show n2 ++ " " ++ show d ++ "\n"
showProgressLong (Message s)         = s ++ "\n"

showProgressShort (LookAtEbuild pv o) = showOriginShort o
showProgressShort (AddEdge _ _ _)     = ""
showProgressShort (Message s)         = ""

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

