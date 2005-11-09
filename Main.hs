module Main where

import System.Environment
import Data.Graph.Inductive hiding (context)
import qualified Data.Map as M
import Control.Monad.State

import Portage.Graph
import Portage.Dependency
import Portage.PortageConfig

main = do  x <- portageConfig
           [a] <- getArgs
           main' x a
           

main' x d = putStr $ unlines $ map (showVariant (config x)) $ findVersions (itree x) (getDepAtom d)



test d = do  x <- portageConfig
             let initialState =  DepState
                                   {
                                      pconfig   =  x,
                                      dlocuse   =  [],
                                      graph     =  insNode (0,Top) empty,
                                      labels    =  M.empty,
                                      counter   =  1,
                                      dcontext  =  (rdepend 0) { source = 0 }
                                   }
             let fs = runState (buildGraphForDepString (getDepString d)) initialState 
             print (fst fs)
             putStr $ unlines $ map show $ postorderF $ dffWith lab' [0] $ graph $ snd $ fs
             
