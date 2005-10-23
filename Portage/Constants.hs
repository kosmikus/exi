{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Real built-in constants.
-}

module Portage.Constants
  where

import Portage.Utilities

bashBin          =  "/bin/bash"

profileDir       =  "/etc/make.profile"
globalConfig     =  "/etc/make.globals"
profileConfig    =  profileDir ./. "make.defaults"
userConfig       =  "/etc/make.conf"


profileParent    =  "parent"

profilesDir  pt  =  pt ./. "profiles"
eclassDir    pt  =  pt ./. "eclass"
licenseDir   pt  =  pt ./. "licenses"
metadataDir  pt  =  pt ./. "metadata"

cacheDir     pt  =  "/var/cache/edb/dep" ./. pt

ebuildsh         =  "/usr/sbin/ebuild.sh"

categoriesFile  pt  =  profilesDir pt ./. "categories"

