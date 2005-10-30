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
profileConfig    =  "make.defaults"
userConfig       =  "/etc/make.conf"

packageMask      =  "package.mask"
packageUnMask    =  "package.unmask"


profileParent    =  "parent"

profilesDir  pt      =  pt ./. "profiles"
eclassDir    pt      =  pt ./. "eclass"
licenseDir   pt      =  pt ./. "licenses"
metadataDir  pt      =  pt ./. "metadata"
metadataCacheDir pt  =  metadataDir pt ./. "cache"

cacheDir     pt  =  "/var/cache/edb/dep" ./. pt
dbDir            =  "/var/db/pkg"

ebuildsh         =  "/usr/sbin/ebuild.sh"

categoriesFile  pt  =  profilesDir pt ./. "categories"

localConfigDir   =  "/etc/portage"
localProfileDir  =  localConfigDir ./. "profile"

useDefaults      =  "use.defaults"

