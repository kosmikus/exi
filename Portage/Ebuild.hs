{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Ebuilds and cache entries.
-}

module Portage.Ebuild
  where

import System.Environment
import System.IO.Unsafe
import System.Directory
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (isPrefixOf)
import Control.Monad
import Control.Exception
import Prelude hiding (catch)

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
import Portage.Profile

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
                         masked       ::  [Mask],           -- ^ empty means the ebuild is visible
                         locuse       ::  [UseFlag],        -- ^ local USE flags
                         lockey       ::  [Keyword],        -- ^ local ACCEPT_KEYWORDS
                         origin       ::  EbuildOrigin      -- ^ where did we get this data from?
                      }
  deriving (Show,Eq)

pvs :: Variant -> PVS
pvs v = addSlot (pv . meta $ v) (slot . ebuild $ v)

data EbuildOrigin = FromCache | CacheRegen | EclassDummy | FromInstalledDB | IsProvided
  deriving (Show,Eq)

data TreeLocation  =  Installed
                   |  Provided       FilePath               -- ^ in which file?
                   |  PortageTree    FilePath Link
  deriving (Show,Eq)

isAvailable :: TreeLocation -> Bool
isAvailable Installed     =  True
isAvailable (Provided _)  =  True
isAvailable _             =  False

-- | The 'Link' is used to link an uninstalled variant to an installed variant
--   of the same slot. We can thus say whether selecting this variant would be
--   an up- or a downgrade, and we can compare use flags.
data Link          =  NoLink
                   |  Linked  Variant     -- ^ installed variant
  deriving (Show,Eq)

data Mask          =  KeywordMasked  [UseFlag]              -- ^ reasoning
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

getEbuild :: FilePath -> String -> Ebuild
getEbuild f e  |  length l <= 14  =  error $ "getEbuild: corrupted ebuild cache " ++ f ++ " (too short by " ++ show (14 - length l) ++ " lines)"
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
getInstalledVariantFromDisk :: Config -> PV -> IO Variant
getInstalledVariantFromDisk cfg pv@(PV cat pkg ver) =
    do
        let readCacheFile f  =  unsafeInterleaveIO $ strictReadFileIfExists f
        let dir              =  dbDir ./. showPV pv
        dep            <-  readCacheFile $ dir ./. "DEPEND"
        rdep           <-  readCacheFile $ dir ./. "RDEPEND"
        slt            <-  readCacheFile $ dir ./. "SLOT"
        src            <-  return "" -- for some reason not stored
        restr          <-  readCacheFile $ dir ./. "RESTRICT"
        home           <-  return "" -- for some reason not stored
        lic            <-  return "" -- for some reason not stored
        des            <-  return "" -- for some reason not stored
        key            <-  return (arch cfg) -- for some reason not stored
        inh            <-  readCacheFile $ dir ./. "INHERITED"
        iuse           <-  fmap splitUse $ readCacheFile $ dir ./. "IUSE"
        cdep           <-  readCacheFile $ dir ./. "CDEPEND"
        pdep           <-  readCacheFile $ dir ./. "PDEPEND"
        prov           <-  readCacheFile $ dir ./. "PROVIDE"
        -- this one is to compute the local USE flag modifications
        use            <-  fmap splitUse $ readCacheFile $ dir ./. "USE"
        let m  =  EbuildMeta
                    pv
                    Installed
                    []
                    (diffUse use iuse)
                    []
                    FromInstalledDB
        let e  =  Ebuild
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
                    iuse
                    (getDepString cdep)
                    (getDepString pdep)
                    (getMaybeDepAtom prov)
        return (Variant m e)

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
                      PV -> Map Eclass EclassMeta -> IO (Ebuild,EbuildOrigin)
getEbuildFromDisk cfg pt pv@(PV cat pkg ver) ecs =
    do
        let originalFile  =  pt ./. showEbuildPV pv
        let cacheFile     =  cacheDir pt ./. showPV pv
        let metadataFile  =  metadataCacheDir pt ./. showPV pv
        let eclassesFile  =  ownCacheDir pt ./. (showPV pv ++ ".eclasses")
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
        let eclassesDummy  =  -- If no eclasses mtime file is present, we assume
                              -- the current tree
                              do
                                  putStrLn ("making eclass dummy for " ++ showPV pv)
                                  makePortageFile eclassesFile
                                  c <- fmap (getEbuild cacheFile) (strictReadFile cacheFile)
                                  let eclasses  =  inherited c
                                  let efiles    =  
                                        map  (\x -> eclassDir pt ./. (x ++ ".eclass"))
                                             eclasses
                                  mtimes <- mapM getMTime efiles
                                  writeEclassesFile  eclassesFile
                                                     (zip eclasses mtimes)
        oed <- if (cacheExists && cacheNewer && cacheOriginal && not eclassesExist) 
                 then eclassesDummy >> return True
                 else return False
        -- eclasses could exist now, therefore we check again
        eclassesExist  <-  unsafeInterleaveIO $ doesFileExist eclassesFile
        eclassesOK     <-  unsafeInterleaveIO $
                           do
                               eclasses <- readEclassesFile eclassesFile
                               return (all (\(e,m) -> mtime (ecs M.! e) == m) eclasses)
        -- read this only once you know the cache is ok
        cacheContents  <-  unsafeInterleaveIO $
                           fmap (getEbuild cacheFile) (strictReadFile cacheFile)
        let refreshCache   =  do
                                  putStrLn ("cache refresh for " ++ showPV pv)
                                  makePortageFile cacheFile
                                  makeCacheEntry cfg pt pv
                                  -- the cache is refreshed, now update eclasses
                                  makePortageFile eclassesFile
                                  let eclasses  =  inherited cacheContents
                                  let mtimes    =
                                        map (\e -> mtime (ecs M.! e)) eclasses
                                  writeEclassesFile  eclassesFile
                                                     (zip eclasses mtimes)
        orc <- if (not (cacheExists && cacheNewer && eclassesExist && eclassesOK))
                 then refreshCache >> return True
                 else return False
        let origin | orc        =  CacheRegen
                   | oed        =  EclassDummy
                   | otherwise  =  FromCache
        return (cacheContents,origin)

-- | This code is following the code in @doebuild@ from @portage.py@.
makeCacheEntry :: Config -> FilePath -> PV -> IO ()
makeCacheEntry cfg pt pv@(PV cat pkg ver) =
    do
        let originalFile  =  pt ./. showEbuildPV pv
        let cacheFile     =  cacheDir pt ./. showPV pv
        let metadataFile  =  metadataCacheDir pt ./. showPV pv

        -- ROOT
        envroot  <-  catch (getEnv "ROOT") (const $ return "/")
        envroot  <-  if null envroot || not ("/" `isPrefixOf` envroot)
                       then return ('/' : envroot)
                       else return envroot
        -- current directory
        wd       <-  getCurrentDirectory

        -- ebuild directory
        let ebuildDir     =  dirname originalFile
        let filesDir      =  ebuildDir ./. files

        -- profile directories
        profileDirs <- getProfileDirs

        -- version without revision
        let ver           =  showVersion (stripRev (verPV pv))
        -- only the revision
        let rev           =  getRev (verPV pv)

        -- processing the path
        path     <-  catch (getEnv "PATH") (const $ return "")
        path     <-  return $
                     if not (portageBinPath `elem` split ':' path)
                       then  portageBinPath ++ ":" ++ path
                       else  path

        -- build prefix
        let buildPrefix   =  tmpDir cfg ./. "portage"
        let home          =  buildPrefix ./. "homedir"
        let pkgTmpDir     =  tmpDir cfg ./. "portage-pkg"
        let buildDir      =  buildPrefix ./. showPV pv
        
        let env =
              [  ("ROOT",            envroot),
                 ("STARTDIR",        wd),
                 ("EBUILD",          originalFile),
                 ("O",               ebuildDir),
                 ("FILESDIR",        filesDir),
                 ("PF",              showPV pv),
                 ("ECLASSDIR",       eclassDir (portDir cfg)),
                 -- TODO: do we need SANDBOX_LOG?
                 ("PROFILE_PATHS",   unlines profileDirs),
                 ("P",               (pkg ++ "-" ++ ver)),
                 ("PN",              pkg),
                 ("PV",              ver),
                 ("PR",              showRevPR rev),
                 ("PVR",             showVersion (verPV pv)),
                 ("SLOT",            ""),
                 ("PATH",            path),
                 ("BUILD_PREFIX",    buildPrefix),
                 ("HOME",            home),
                 ("PKG_TMPDIR",      pkgTmpDir),
                 ("BUILDDIR",        buildDir),
                 ("PORTAGE_BASHRC",  bashrcFile),
                 -- TODO: KV and KVERS missing ...
                 ("dbkey",           cacheFile)
              ]

        runCommandInEnv (ebuildsh ++ " depend") env
        return ()
                


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
