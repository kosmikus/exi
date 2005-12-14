{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Graph algorithms.
-}

module Portage.GraphUtils
  (sccf, cyclesFrom)
  where

import Data.Graph.Inductive
import Data.Tree
import Prelude hiding (cycle)

sccf :: Graph gr => gr a b -> [Tree Node]
sccf g = rdff (topsort g) g

cyclesFrom :: Graph gr => gr a b -> [Node] -> [[Node]]
cyclesFrom g n = filter ((`elem` r) . head) all
  where  r    =  concatMap (flip reachable g) n
         all  =  map reverse $ cycles g $ sccf g

cycles :: Graph gr => gr a b -> [Tree Node] -> [[Node]]
cycles g = concatMap (cycle g)

cycle :: Graph gr => gr a b -> Tree Node -> [[Node]]
cycle g (Node v ts) = take 1 (map (v:) (cycle' ts))
  where  ps = suc g v
         cycle' :: [Tree Node] -> [[Node]]
         cycle' (Node v' ts':ts'') | v' `elem` ps  =  [[v']]
                                   | otherwise     =  map (v':) (cycle' ts') ++ cycle' ts''
         cycle' []                                 =  []
