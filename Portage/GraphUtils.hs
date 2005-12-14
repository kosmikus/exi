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
         all  =  map reverse $ filter (\x -> length x > 1) . cycles $ sccf g

cycles :: [Tree a] -> [[a]]
cycles = map cycle

cycle :: Tree a -> [a]
cycle (Node v ts) = v : let r = reverse (cycles ts) in case r of [] -> []; (x:_) -> x

