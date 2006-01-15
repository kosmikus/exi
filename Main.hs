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
           
