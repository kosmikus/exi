module Main where

import System.Environment
import Data.Graph.Inductive hiding (context)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Control.Monad.State

import Portage.Graph
import Portage.Dependency
import Portage.PortageConfig
import Portage.Strategy
import Portage.Ebuild hiding (rdepend)
import Portage.Package

main = do  [a] <- getArgs
           main' a
           

main' d = 
    do  x <- portageConfig
        putStr $ unlines $ map (showVariant (config x)) $ findVersions (itree x) (getDepAtom d)


pretend' b v d = 
    do  x <- fmap (if b then (\x -> x { strategy = updateStrategy }) else id ) portageConfig
        let initialState =  DepState
                              {
                                 pconfig   =  x,
                                 dlocuse   =  [],
                                 graph     =  insNode (0,Top) empty,
                                 labels    =  M.empty,
                                 counter   =  1,
                                 dcontext  =  (rdepend 0) { source = 0 }
                              }
        let fs = runState (buildGraphForUDepString (getDepString d)) initialState 
        putStr $ "Calculating dependencies: "
        when v $ putStrLn ""
        putStr $ (if v then unlines . map showProgressLong else concatMap showProgressShort) $ fst fs
        putStrLn $ "\nGraph complete: "
        let gr = graph $ snd $ fs
        let mergelist = postorderF $ dffWith lab' [0] $ gr
        putStr $ unlines $ map show $ mergelist
        putStrLn $ "\nShort version: "
        putStr $ unlines  $  map (showAction (config x)) 
                          $  filter (\ a -> case a of Build _ -> True; _ -> False) $ mergelist
        -- preliminary; should be only on the graph reachable from Top:
        let sccs = scc $ gr
        let cycles = filter (\x -> length x > 1) sccs
        when (not (null cycles)) $
            do
                putStrLn "The graph has cycles:" 
                putStr $ unlines $ map (unlines . map (show . fromJust . lab gr)) cycles
        return gr

showProgressLong (LookAtEbuild pv o) = showPV pv ++ " " ++ showOriginLong o
showProgressLong (Message s)         = s

showProgressShort (LookAtEbuild pv o) = showOriginShort o
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

p   = pretend' False False
up  = pretend' True  False
pv  = pretend' False True
upv = pretend' True  True

