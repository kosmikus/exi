{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Data type for portage configuration.
-}

module Portage.PortageConfig.Type
  where

import Portage.Config
import Portage.Tree
import Portage.Package
import Portage.Dependency

data PortageConfig =  PortageConfig
                        {
                           config    ::  Config,
                           tree      ::  Tree,
                           inst      ::  Tree,
                           itree     ::  Tree,
                           virtuals  ::  DepAtom -> Maybe DepTerm,
                           expand    ::  Package -> [Category],
                           system    ::  DepString,                 -- ^ system target
                           world     ::  DepString                  -- ^ world target
                        }
