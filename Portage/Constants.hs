{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Real built-in constants.
-}

module Portage.Constants
  where

import Portage.Utilities

profileDir     =  "/etc/make.profile"
globalConfig   =  "/etc/make.globals"
profileConfig  =  profileDir ./. "make.defaults"
userConfig     =  "/etc/make.conf"

profileParent  =  "parent"