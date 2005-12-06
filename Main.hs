module Main where

import System.Environment
import Data.Graph.Inductive hiding (context)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Control.Monad.State

import Portage.Graph
import Portage.GraphUtils
import Portage.Dependency
import Portage.PortageConfig
import Portage.Strategy
import Portage.Ebuild hiding (rdepend)
import Portage.Package
import Portage.Tree
import Portage.Config

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
                                 graph     =  insNodes [(top,Top),(bot,Bot)] empty,
                                 labels    =  M.empty,
                                 active    =  M.empty,
                                 counter   =  max top bot + 1,
                                 callback  =  rdepend (top,top,top)
                              }
        let fs = runState (buildGraphForUDepString (getDepString d)) initialState 
        putStr $ "Calculating dependencies: "
        when v $ putStrLn ""
        putStr $ concatMap (showProgress v) $ fst fs
        putStrLn $ "\nGraph complete: "
        let gr = graph $ snd $ fs
        let mergelist = postorderF $ dffWith lab' [0] $ gr
        putStr $ unlines $ map show $ mergelist
        putStrLn $ "\nShort version: "
        putStr $ unlines  $  map (showAction (config x)) 
                          $  filter (\ a -> case a of Built _ -> True; _ -> False) $ mergelist
        let cycles = cyclesFrom gr [top]
        when (not (null cycles)) $
            do
                putStrLn "\nThe graph has cycles:" 
                putStr $ unlines $ map (unlines . map (showNode v gr)) cycles
        return gr

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

ep   = pretend' False False
eup  = pretend' True  False
epv  = pretend' False True
eupv = pretend' True  True


-- expand function, too slow:
expand :: Package -> Tree -> [P]
expand p t =  M.foldWithKey 
              (\c m r ->  if p `elem` M.keys m 
                          then (P c p : r) else r) [] (ebuilds t)
