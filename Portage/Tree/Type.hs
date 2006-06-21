{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    The data structure that contains a portage tree.
-}

module Portage.Tree.Type
  where

import qualified Data.Map as M
import Data.Map (Map)

import Portage.Ebuild.Type
import Portage.Eclass
import Portage.Package

data Tree =  Tree
               {
                  eclasses  ::  Eclasses,
                  ebuilds   ::  Ebuilds
               }

type Eclasses  =  Map Eclass EclassMeta
type Ebuilds   =  Map Category (Map Package [Variant])

