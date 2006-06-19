{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Real built-in constants.
-}

module Portage.Constants
  where

import Portage.Utilities

myself           =  "exi"

bashBin          =  "/bin/bash"

profileDir       =  "/etc/make.profile"
globalConfig     =  "/etc/make.globals"
profileConfig    =  "make.defaults"
userConfig       =  "/etc/make.conf"

packageMask      =  "package.mask"
packageUnMask    =  "package.unmask"
packageProvided  =  "package.provided"
virtuals         =  "virtuals"
packages         =  "packages"

profileParent    =  "parent"

profilesDir  pt      =  pt ./. "profiles"
eclassDir    pt      =  pt ./. "eclass"
licenseDir   pt      =  pt ./. "licenses"
metadataDir  pt      =  pt ./. "metadata"
metadataCacheDir pt  =  metadataDir pt ./. "cache"

files            =  "files"

cacheDir     pt  =  "/var/cache/edb/dep" ./. pt
ownCacheDir  pt  =  "/var/cache/hdb/dep" ./. pt  -- for non-portage compliant own files
dbDir            =  "/var/db/pkg"

emergeBin        =  "/usr/bin/emerge"
ebuildBin        =  "/usr/lib/portage/bin/ebuild"
ebuildsh         =  "/usr/lib/portage/bin/ebuild.sh"
envUpdateBin     =  "/usr/sbin/env-update"

categoriesFile  pt  =  profilesDir pt ./. "categories"

localConfigDir   =  "/etc/portage"
localProfileDir  =  localConfigDir ./. "profile"

useDefaults      =  "use.defaults"

localUseFlagsFile    =  localConfigDir ./. "package.use"
localKeywordsFile    =  localConfigDir ./. "package.keywords"

worldFile        =  "/var/lib/portage/world"

-- original portage code
portageBasePath  =  "/usr/lib/portage"
portageBinPath   =  portageBasePath ./. "bin"
portagePymPath   =  portageBasePath ./. "pym"

bashrcFile       =  localConfigDir ./. "bashrc"
