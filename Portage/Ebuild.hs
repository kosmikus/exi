{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Ebuilds and cache entries.
-}

module Portage.Ebuild
  where

import System.IO.Unsafe
import System.Directory
import Data.Map
import Control.Monad

import Portage.Dependency
import Portage.Use
import Portage.Keyword
import Portage.Version
import Portage.Package
import Portage.Eclass
import Portage.Constants
import Portage.Utilities
import Portage.Shell

-- The ebuild cache format (as created by calling "ebuild depend") is as follows:
-- DEPEND
-- RDEPEND
-- SLOT
-- SRC_URI
-- RESTRICT
-- HOMEPAGE
-- LICENSE
-- DESCRIPTION
-- KEYWORDS
-- INHERITED
-- IUSE
-- CDEPEND
-- PDEPEND
-- PROVIDE
-- In addition, we store the tree in which the ebuild is located.

data Ebuild = Ebuild  {
                         depend       ::  DepString,
                         rdepend      ::  DepString,
                         slot         ::  String,
                         src_uri      ::  String,
                         restrict     ::  [String],
                         homepage     ::  String,
                         license      ::  String,
                         description  ::  String,
                         keywords     ::  [Keyword],
                         inherited    ::  String,
                         iuse         ::  [UseFlag],
                         cdepend      ::  DepString,
                         pdepend      ::  DepString,
                         provide      ::  DepAtom 
                      }
  deriving (Show,Eq)

-- | The "EbuildMeta" type contains additional information about an ebuild
--   that is not directly stored within the ebuild file / cache entry.
data EbuildMeta =  EbuildMeta
                      {
                         version      ::  Version,
                         location     ::  TreeLocation,
                         masked       ::  [Mask]   -- empty means the ebuild is visible
                      }
  deriving (Show,Eq)

data TreeLocation  =  Installed
                   |  PortageTree  FilePath
  deriving (Show,Eq)

data Mask          =  KeywordMasked
                   |  HardMasked     [String]      -- reason from package.mask
                   |  Shadowed       TreeLocation  -- by which tree?
  deriving (Show,Eq)

-- | A variant is everything that makes a specific instance of an ebuild.
--   It's supposed to be more than this datatype currently encodes.

data Variant = Variant EbuildMeta Ebuild
  deriving (Show,Eq)

getEbuild :: String -> Ebuild
getEbuild e  |  length l <= 14  =  error "getEbuild: corrupted ebuild (too short)"
             |  otherwise       =  Ebuild  
                                           (getDepString dep)
                                           (getDepString rdep)
                                           slt
                                           src
                                           (words restr)
                                           home
                                           lic
                                           des
                                           (splitKeywords key)
                                           inh
                                           (splitUse use)
                                           (getDepString cdep)
                                           (getDepString pdep)
                                           (getDepAtom prov)
  where  l = lines e
         (dep:rdep:slt:src:restr:home:lic:des:key:inh:use:cdep:pdep:prov:_) = l

-- | Reads an ebuild from disk. Reads the cache entry if that is sufficient,
--   otherwise refreshes the cache.
--
--   The cache entry is sufficiently new if:
--     * the mtime of the cache is newer than the mtime of the original ebuild,
--       AND
--     * the eclass mtimes in the cache match the final eclass mtimes of the
--       current tree configuration

getEbuildFromDisk :: FilePath -> PV -> Map Eclass EclassMeta -> IO Ebuild
getEbuildFromDisk pt pv@(PV cat pkg ver) ecs =
    do
        let originalFile  =  pt ./. showEbuildPV pv
        let cacheFile     =  cacheDir pt ./. showPV pv
        cacheExists  <-  unsafeInterleaveIO $ doesFileExist cacheFile
        cacheNewer   <-  unsafeInterleaveIO $
                         do  cacheMTime     <-  getMTime cacheFile
                             originalMTime  <-  getMTime originalFile
                             return (cacheMTime > originalMTime)
        eclassOK     <-  return $ True  -- unimplemented
        when (not (cacheExists && cacheNewer && eclassOK)) refreshCache
        fmap getEbuild (strictReadFile cacheFile)
  where
    refreshCache = return ()
