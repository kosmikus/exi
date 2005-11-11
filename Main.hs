module Main where

import System.Environment
import Data.Graph.Inductive hiding (context)
import qualified Data.Map as M
import Control.Monad.State

import Portage.Graph
import Portage.Dependency
import Portage.PortageConfig
import Portage.Strategy

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
        putStr $ (if v then unlines else map (const '.')) $ fst fs
        putStrLn $ "\nGraph complete: "
        let mergelist = postorderF $ dffWith lab' [0] $ graph $ snd $ fs
        putStr $ unlines $ map show $ mergelist
        putStrLn $ "\nShort version: "
        putStr $ unlines  $  map (showAction (config x)) 
                          $  filter (\ a -> case a of Build _ -> True; _ -> False) $ mergelist

p   = pretend' False False
up  = pretend' True  False
pv  = pretend' False True
upv = pretend' True  True
