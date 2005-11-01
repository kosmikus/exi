{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Ebuilds and cache entries.
-}

module Portage.Ebuild
  where

import System.IO.Unsafe
import System.Directory
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad

import Portage.Config
import Portage.Dependency
import Portage.Use
import Portage.Keyword
import Portage.Version
import Portage.Package
import Portage.Eclass
import Portage.Constants
import Portage.Utilities
import Portage.Shell

-- The ebuild cache format (as created by calling @ebuild depend@) is as follows:
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
                         inherited    ::  [Eclass],
                         iuse         ::  [UseFlag],
                         cdepend      ::  DepString,
                         pdepend      ::  DepString,
                         provide      ::  Maybe DepAtom 
                      }
  deriving (Show,Eq)

-- | The 'EbuildMeta' type contains additional information about an ebuild
--   that is not directly stored within the ebuild file or cache entry.
data EbuildMeta =  EbuildMeta
                      {
                         pv           ::  PV,
                         location     ::  TreeLocation,
                         masked       ::  [Mask]   -- ^ empty means the ebuild is visible
                      }
  deriving (Show,Eq)

data TreeLocation  =  Installed
                   |  PortageTree  FilePath
  deriving (Show,Eq)

data Mask          =  KeywordMasked
                   |  HardMasked     FilePath [String]      -- ^ filename and reason
                   |  ProfileMasked  FilePath               -- ^ in which file?
                   |  Shadowed       TreeLocation           -- ^ by which tree?
  deriving (Show,Eq)

-- | A variant is everything that makes a specific instance of an ebuild.
--   It's supposed to be more than this datatype currently encodes.
data Variant =  Variant
                  {
                     meta    ::  EbuildMeta,
                     ebuild  ::  Ebuild
                  }
  deriving (Show,Eq)

filterMaskedVariants :: [Variant] -> [Variant]
filterMaskedVariants = filter (\(Variant m _) -> null (masked m))

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
                                           (splitEclasses inh)
                                           (splitUse use)
                                           (getDepString cdep)
                                           (getDepString pdep)
                                           (getMaybeDepAtom prov)
  where  l = lines e
         (dep:rdep:slt:src:restr:home:lic:des:key:inh:use:cdep:pdep:prov:_) = l

-- | Reads the ebuild of an installed package from disk.
--   This is very different than for uninstalled ebuilds, because installed
--   packages are stored differently.
getInstalledEbuildFromDisk :: Config -> PV -> IO Ebuild
getInstalledEbuildFromDisk cfg pv@(PV cat pkg ver) =
    do
        let dir           =  dbDir ./. showPV pv
        dep            <-  strictReadFile $ dir ./. "DEPEND"
        rdep           <-  strictReadFile $ dir ./. "RDEPEND"
        slt            <-  strictReadFile $ dir ./. "SLOT"
        src            <-  return "" -- for some reason not stored
        restr          <-  strictReadFile $ dir ./. "RESTRICT"
        home           <-  return "" -- for some reason not stored
        lic            <-  return "" -- for some reason not stored
        des            <-  return "" -- for some reason not stored
        key            <-  return (arch cfg) -- for some reason not stored
        inh            <-  strictReadFile $ dir ./. "INHERITED"
        use            <-  strictReadFile $ dir ./. "IUSE"
        cdep           <-  strictReadFile $ dir ./. "CDEPEND"
        pdep           <-  strictReadFile $ dir ./. "PDEPEND"
        prov           <-  strictReadFile $ dir ./. "PROVIDE"
        return $  Ebuild
                          (getDepString dep)
                          (getDepString rdep)
                          (stripNewlines slt)
                          src
                          (words restr)
                          home
                          lic
                          des
                          (splitKeywords key)
                          (splitEclasses inh)
                          (splitUse use)
                          (getDepString cdep)
                          (getDepString pdep)
                          (getMaybeDepAtom prov)

-- | Reads an ebuild from disk. Reads the cache entry if that is sufficient,
--   otherwise refreshes the cache.
--
--   The cache entry is sufficiently new if:
--
--     * the mtime of the cache is newer than the mtime of the original ebuild,
--       AND
--
--     * the eclass mtimes in the cache match the final eclass mtimes of the
--       current tree configuration
getEbuildFromDisk ::  Config -> FilePath -> 
                      PV -> Map Eclass EclassMeta -> IO Ebuild
getEbuildFromDisk cfg pt pv@(PV cat pkg ver) ecs =
    do
        let originalFile  =  pt ./. showEbuildPV pv
        let cacheFile     =  cacheDir pt ./. showPV pv
        let metadataFile  =  metadataCacheDir pt ./. showPV pv
        let eclassesFile  =  cacheDir pt ./. (showPV pv ++ ".eclasses")
        cacheExists    <-  unsafeInterleaveIO $ doesFileExist cacheFile
        cacheNewer     <-  unsafeInterleaveIO $
                           do  
                               cacheMTime     <-  getMTime cacheFile
                               originalMTime  <-  getMTime originalFile
                               return (cacheMTime >= originalMTime)
        cacheOriginal  <-  unsafeInterleaveIO $
                           do
                               metaExists     <-  doesFileExist metadataFile
                               if metaExists
                                 then do  cacheMTime  <-  getMTime cacheFile
                                          metaMTime   <-  getMTime metadataFile
                                          return (cacheMTime == metaMTime)
                                 else return False
        eclassesExist  <-  unsafeInterleaveIO $ doesFileExist eclassesFile
        let env            =  [("ECLASSDIR",eclassDir (portDir cfg)),
                               ("PORTDIR_OVERLAY",unwords (overlays cfg)), 
                                          -- see below
                               ("EBUILD",originalFile),
                               ("dbkey",cacheFile)]
        let eclassesDummy  =  -- If no eclasses mtime file is present, we assume
                              -- the current tree
                              do
                                  putStrLn ("making eclass dummy for " ++ show pv)
                                  makePortageFile eclassesFile
                                  c <- fmap getEbuild (strictReadFile cacheFile)
                                  let eclasses  =  inherited c
                                  let efiles    =  
                                        map  (\x -> eclassDir pt ./. (x ++ ".eclass"))
                                             eclasses
                                  mtimes <- mapM getMTime efiles
                                  writeEclassesFile  eclassesFile
                                                     (zip eclasses mtimes)
        when  (cacheExists && cacheNewer && cacheOriginal && not eclassesExist) 
              eclassesDummy
        -- eclasses could exist now, therefore we check again
        eclassesExist  <-  unsafeInterleaveIO $ doesFileExist eclassesFile
        eclassesOK     <-  unsafeInterleaveIO $
                           do
                               eclasses <- readEclassesFile eclassesFile
                               return (all (\(e,m) -> mtime (ecs M.! e) == m) eclasses)
        -- read this only once you know the cache is ok
        cacheContents  <-  unsafeInterleaveIO $
                           fmap getEbuild (strictReadFile cacheFile)
        let refreshCache   =  do
                                  putStrLn ("cache refresh for " ++ show pv)
                                  makePortageFile cacheFile
                                  runCommandInEnv (ebuildsh ++ " depend") env
                                  -- the cache is refreshed, now update eclasses
                                  makePortageFile eclassesFile
                                  let eclasses  =  inherited cacheContents
                                  let mtimes    =
                                        map (\e -> mtime (ecs M.! e)) eclasses
                                  writeEclassesFile  eclassesFile
                                                     (zip eclasses mtimes)
        when  (not (cacheExists && cacheNewer && eclassesExist && eclassesOK))
              refreshCache
        return cacheContents

-- A note on current portage eclass resolution: In ebuild.sh, the eclass
-- is looked up in the current ECLASSDIR and the current overlays specified
-- in the PORTDIR_OVERLAY environment variable. Priority is given to the
-- last match. No cache is used for this resolution.

-- A note on eclass optimisation: I thought that it is possible
-- to assume some sane default for the eclass mtimes if we haven't generated our
-- own file, in one special case: if there's a metadata directory in the tree,
-- and the cache mtime matches the metadata mtime, then we know that we have the
-- "original" ebuild from the tree. Accordingly, all the eclasses used should be
-- the ones in this tree. This allows us not to regenerate the cache in a very
-- frequent situation (the main portage tree).
