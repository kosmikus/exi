module Main where

import System.Environment
import qualified Data.Map as M

import Portage.Graph hiding (getDepAtom)
import Portage.Dependency
import Portage.PortageConfig
import Portage.Package
import Portage.Tree
import Portage.Merge
import Portage.Interface

main = do  args <- getArgs
           handleArgs args

interactive = handleArgs . words
           
main' d = 
    do  x <- portageConfig
        putStr $ unlines $ map (showVariant (config x)) $ findVersions (itree x) (getDepAtom d)


-- expand function, too slow:
{-
expand :: Package -> Tree -> [P]
expand p t =  M.foldWithKey 
              (\c m r ->  if p `elem` M.keys m 
                          then (P c p : r) else r) [] (ebuilds t)
-}
