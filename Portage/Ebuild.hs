{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Ebuilds and cache entries.
-}

module Portage.Ebuild
  (module Portage.Ebuild, module Portage.Ebuild.Type)
  where

import System.Environment
import System.IO.Unsafe
import System.IO
import System.Directory
import System.Exit
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (isPrefixOf, intersperse, (\\))
import Control.Monad
import Control.Exception
import Prelude hiding (catch)

import Portage.Ebuild.Type
import Portage.PortageConfig.Type
import Portage.Config
import Portage.Dependency
import Portage.Use
import Portage.NewUse
import Portage.Keyword
import Portage.Version
import Portage.Package
import Portage.Eclass hiding (location)
import qualified Portage.Eclass as E
import Portage.Constants
import Portage.Utilities
import Portage.Shell
import Portage.Profile
import Portage.Cache
import Portage.AnsiColor


pvs :: Variant -> PVS
pvs v = addSlot (pv . meta $ v) (slot . ebuild $ v)


showLocation :: Config -> TreeLocation -> String
showLocation c Installed          =  " (installed)"
showLocation c (Provided f)       =  " (provided in " ++ f ++ ")"
showLocation c (PortageTree t l)  =  inColor c Blue True Default (showLink l) ++
                                     if portDir c == t then "" else " [" ++ t ++ "]"

showTreeLocation :: TreeLocation -> String
showTreeLocation Installed          =  "installed packages"
showTreeLocation (Provided f)       =  "provided packages from " ++ f
showTreeLocation (PortageTree t _)  =  t

isAvailable :: TreeLocation -> Bool
isAvailable Installed     =  True
isAvailable (Provided _)  =  True
isAvailable _             =  False


showLink :: Link -> String
showLink (Linked Nothing Nothing)   =  ""
showLink (Linked (Just v) _)        =  " [" ++ showVersion (verPV . pv . meta $ v) ++ "]"
showLink (Linked Nothing (Just v))  =  " [" ++ showVersion (verPV . pv . meta $ v) ++ "]"

getLinked :: Variant -> Maybe Variant
getLinked v = case (location . meta) v of
                PortageTree _ (Linked (Just l) _)  ->  Just l
                _                                  ->  Nothing


hardMask :: Mask -> String
hardMask (HardMasked f r) = "\n" ++ unlines r
hardMask _                = ""

showMasked :: Mask -> String
showMasked (KeywordMasked xs) = "(masked by keyword: " ++ show xs ++ ")"
showMasked (HardMasked f r) = "(hardmasked in " ++ f ++ ")"
showMasked (ProfileMasked f) = "(excluded from profile in " ++ f ++")"
showMasked (Shadowed t) = "(shadowed by " ++ showTreeLocation t ++ ")"

showVariant :: PortageConfig -> Bool -> Variant -> String
showVariant pc verbose v@(Variant m e)  =
    showVariant' (config pc) v
    ++ concatMap (\x -> ' ' : showMasked x) (masked m)
    ++ " " ++ useflags
  where useflags  =  let  c = showUseFlags pc verbose v (getLinked v)
                     in   if null c then "" else c

showUseFlags :: PortageConfig -> Bool -> Variant -> Maybe Variant -> String
showUseFlags pc verbose v@(Variant m e) Nothing                    =
  showUseFlags pc True v (Just v)
showUseFlags pc verbose v@(Variant m e) (Just v'@(Variant m' e'))  =
  let  cfg       =  config pc
       d         =  extUse (mergeUse (use cfg) (locuse m))   (iuse e)
                           (mergeUse (use cfg) (locuse m'))  (iuse e')
       masked    =  usemask pc
       d' :: [ExtUseFlag]
       d'        =  filter (\ (f,b) ->  not (f `elem` masked) &&
                                        (verbose || b /= Just False)) d
       expanded' :: [String]
       expanded' =  map fst (useExpand cfg)
       grouped, sorted :: [(String, [ExtUseFlag])]
       grouped   =  filter  (\ (u,_) ->  not (u `elem` useExpandHidden cfg))
                            (unexpandExtUses expanded' d')
       sorted    =  sortByList grouped fst ("USE" : expanded')
  in   unwords $ map (\ (n,f) -> n ++ "=\"" ++ (unwords . map (showExtUseFlag cfg)) f ++ "\"") sorted

-- | Like "showVariant", but additionally print the reason for hard-masking.
--   Takes up more than one line, therefore not suitable in a context where
--   space is critical.
showVariantMasked :: PortageConfig -> Bool -> Variant -> String
showVariantMasked pc verbose v@(Variant m e) =
  showVariant pc verbose v ++ concatMap hardMask (masked m)

showVariant' :: Config -> Variant -> String
showVariant' cfg (Variant m e)  =  inColor cfg Green False Default (showPV (pv m)) ++
                                   showSlot (slot e) ++ showLocation cfg (location m) 

showStatus :: Config -> Variant -> String
showStatus cfg (Variant m e)  = f
    where  f  =  case location m of
                   Installed                    ->  inColor cfg Yellow True Default "R "
                   Provided _                   ->  inColor cfg Black True Default "P "
                   PortageTree t (Linked Nothing Nothing)
                                                ->  inColor cfg Green True Default "N "
                   PortageTree t (Linked Nothing (Just o))
                                                ->  inColor cfg Green True Default ("NS")
                   PortageTree t (Linked (Just v) o)   ->
                     let  lver   =  verPV . pv . meta $ v
                          ver    =  verPV . pv $ m
                          s      =  case o of
                                      Nothing  ->  " "
                                      Just _   ->  case slot e of
                                                     ['0']  ->  " "
                                                     _      ->  "S"
                     in   case () of
                            _ | lver > ver      ->  inColor cfg Blue True Default ("D" ++ s)
                              | lver < ver      ->  inColor cfg Cyan True Default ("U" ++ s)
                              | otherwise       ->  inColor cfg Yellow True Default ("R" ++ s)

filterMaskedVariants :: [Variant] -> [Variant]
filterMaskedVariants = filter (\ (Variant m _) -> null (masked m))

filterOtherArchVariants :: [Variant] -> [Variant]
filterOtherArchVariants = filter (\ (Variant m _) -> all ok (masked m))
  where  ok (KeywordMasked xs)  =  any (isPrefixOf "~") xs
         ok (HardMasked f r)    =  True
         ok (ProfileMasked f)   =  False
         ok NotInProfile        =  False
         ok (Shadowed t)        =  False

filterAvailableVariants :: [Variant] -> [Variant]
filterAvailableVariants = filter (\ (Variant m _) -> (not . isAvailable . location) m)

putEbuildFlatHash :: FilePath -> Ebuild -> [(Eclass,FilePath,MTime)] -> IO ()
putEbuildFlatHash f ebuild eclasses  =
    do
        let  em  =  M.fromList $
                    [ ("_eclasses_", concat . intersperse "\t" . concatMap (\ (x,y,z) -> [x,y,show z]) $ eclasses),
                      ("DEPEND",show $ depend ebuild),
                      ("RDEPEND",show $ rdepend ebuild),
                      ("SLOT",slot ebuild),
                      ("SRC_URI",src_uri ebuild),
                      ("RESTRICT",unwords $ restrict ebuild),
                      ("HOMEPAGE",homepage ebuild),
                      ("LICENSE",license ebuild),
                      ("DESCRIPTION",description ebuild),
                      ("KEYWORDS",unwords $ keywords ebuild),
                      ("IUSE",unwords $ iuse ebuild),
                      ("CDEPEND",show $ cdepend ebuild),
                      ("PDEPEND",show $ pdepend ebuild),
                      ("PROVIDE",show $ provide ebuild),
                      ("EAPI",eapi ebuild) ]
        h <- openFile f WriteMode
        hPutStr h (unlines . writeStringMap $ em)
        hClose h

getEbuild :: CacheFormat -> FilePath -> [String] -> Ebuild
getEbuild FlatList f e  =  getEbuildFlatList f e
getEbuild FlatHash f e  =  snd (getEbuildFlatHash f e)

getEbuildFlatHash :: FilePath -> [String] -> ([(Eclass,FilePath,MTime)],Ebuild)
getEbuildFlatHash f e  =  let  em      =  readStringMap e
                               f !. k  =  case M.lookup k f of
                                            Nothing  ->  ""
                                            Just x   ->  x
                               dep     =  em !. "DEPEND"
                               rdep    =  em !. "RDEPEND"
                               slt     =  em !. "SLOT"
                               src     =  em !. "SRC_URI"
                               restr   =  em !. "RESTRICT"
                               home    =  em !. "HOMEPAGE"
                               lic     =  em !. "LICENSE"
                               des     =  em !. "DESCRIPTION"
                               key     =  em !. "KEYWORDS"
                               ecl     =  map (\[x,y,z] -> (x,y,read z)) $ groupnr 3 $ split '\t' $ em !. "_eclasses_"
                               inh     =  map (\(x,y,z) -> x) ecl
                               use     =  em !. "IUSE"
                               pdep    =  em !. "PDEPEND"
                               prov    =  em !. "PROVIDE"
                               eapi    =  em !. "EAPI"
                               ebuild  =  Ebuild
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
                                            (getDepString "")
                                            (getDepString pdep)
                                            (getDepString prov)
                                            (if null eapi then "0" else eapi)
                          in   (ecl,ebuild)

getEbuildFlatList :: FilePath -> [String] -> Ebuild
getEbuildFlatList f e
  |  length e <= 15  =  error $ "getEbuild: corrupted ebuild cache " ++ f ++ " (too short by " ++ show (15 - length e) ++ " lines)"
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
                          (getDepString prov)
                          (if null eapi then "0" else eapi)
  where  (dep:rdep:slt:src:restr:home:lic:des:key:inh:use:cdep:pdep:prov:eapi:_) = e

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
                    (interpretDepString use . getDepString $ dep)
                    (interpretDepString use . getDepString $ rdep)
                         -- interpret dependencies of installed packages with the USE flags at
                         -- time of installation; this is inadequate if we ever want to reinstall
                         -- installed ebuilds from the information in the installed-packages-database
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
                    (interpretDepString use . getDepString $ prov)
                         -- interpet provided packages with the USE flags at time of installation
                    "0"  -- does EAPI make sense for installed ebuilds?
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
        origCache      <-  unsafeInterleaveIO $ fmap lines (strictReadFile cacheFile)
        let cacheFormat    =  if cacheExists then detectStringFormat origCache else FlatHash
        let eclassesDummy  =  -- Only used for FlatList cache format:
                              -- If no eclasses mtime file is present, we assume
                              -- the current tree
                              do
                                  when (debug cfg) $ putStrLn ("making eclass dummy for " ++ showPV pv)
                                  makePortageFile eclassesFile
                                  let c         =  getEbuild cacheFormat cacheFile origCache
                                  let eclasses  =  inherited c
                                  let efiles    =  
                                        map  (\x -> eclassDir pt ./. (x ++ ".eclass"))
                                             eclasses
                                  mtimes <- mapM getMTime efiles
                                  writeEclassesFile  eclassesFile
                                                     (zip3 eclasses (repeat $ eclassDir pt) mtimes)
        oed <- if (cacheExists && cacheNewer && cacheOriginal && not eclassesExist && not (cacheFormat == FlatHash))
                 then eclassesDummy >> return True
                 else return False
        -- eclasses could exist now, therefore we check again
        eclassesExist  <-  unsafeInterleaveIO $ case cacheFormat of
                                                  FlatList  ->  doesFileExist eclassesFile
                                                  FlatHash  ->  return cacheExists
        ~(eclasses,cacheContents) <- unsafeInterleaveIO $ getEbuildAndEclasses cacheFormat eclassesFile cacheFile origCache
        let eclassesOK     =  all (\(e,l,m) ->  E.location (ecs M.! e) == l
                                                && mtime (ecs M.! e) == m) eclasses
        let refreshCache   =  do
                                  when (debug cfg) $ putStrLn ("cache refresh for " ++ showPV pv)
                                  makePortageFile cacheFile
                                  makeCacheEntry cfg pt pv
                                  ebuild <- fmap (getEbuildFlatList cacheFile) (fmap lines (strictReadFile cacheFile))
                                  let eclasses   =  inherited ebuild
                                  let eclasses'  =
                                        map  (\e -> (e, E.location (ecs M.! e), mtime (ecs M.! e)))
                                             eclasses
                                  if cacheFormat == FlatHash
                                    then  do  -- update cache format
                                              putEbuildFlatHash cacheFile ebuild eclasses'
                                    else  do  -- the cache is refreshed, now update eclasses
                                              makePortageFile eclassesFile
                                              writeEclassesFile  eclassesFile eclasses'
                                  return ebuild
        (orc,cacheContents) <- if (not (cacheExists && cacheNewer && eclassesExist && eclassesOK))
                               then  do  when (debug cfg) $ do
                                           putStr $ " cacheExists:   " ++ show cacheExists
                                           when cacheExists $ putStr $ " cacheNewer:    " ++ show cacheNewer
                                           putStr $ " eclassesExist: " ++ show eclassesExist
                                           when eclassesExist $ putStr $ " eclassesOK:    " ++ show eclassesOK
                                           putStr "\n"
                                           when (eclassesExist && not eclassesOK) $ do
                                             putStrLn "Eclasses reference:"
                                             print (M.filterWithKey (\e _ -> e `elem` (map (\ (x,_,_) -> x) eclasses)) ecs)
                                             putStrLn "Eclasses here:"
                                             print eclasses
                                         refreshCache >>= \ebuild -> return (True,ebuild)
                               else  return (False,cacheContents)
        let origin | orc        =  CacheRegen
                   | oed        =  EclassDummy
                   | otherwise  =  FromCache
        return (cacheContents,origin)

getEbuildAndEclasses :: CacheFormat -> FilePath -> FilePath -> [String] -> IO ([(Eclass,FilePath,MTime)],Ebuild)
getEbuildAndEclasses FlatList eclassesFile cacheFile cache =
    do
        ecc  <-  unsafeInterleaveIO $ readEclassesFile eclassesFile
        let ebc  =  getEbuildFlatList cacheFile cache
        return (ecc,ebc)
getEbuildAndEclasses FlatHash _ cacheFile cache =
    return $ getEbuildFlatHash cacheFile cache

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
              [  ("ROOT",              envroot),
                 ("STARTDIR",          wd),
                 ("EBUILD",            originalFile),
                 ("O",                 ebuildDir),
                 ("CATEGORY",          cat),
                 ("FILESDIR",          filesDir),
                 ("PF",                showPV pv),
                 ("ECLASSDIR",         eclassDir (portDir cfg)),
                 ("PORTDIR_OVERLAY",   unwords (overlays cfg)),
                 -- TODO: do we need SANDBOX_LOG?
                 ("PROFILE_PATHS",     unlines profileDirs),
                 ("P",                 (pkg ++ "-" ++ ver)),
                 ("PN",                pkg),
                 ("PV",                ver),
                 ("PR",                showRevPR rev),
                 ("PVR",               showVersion (verPV pv)),
                 ("SLOT",              ""),
                 ("PATH",              path),
                 ("BUILD_PREFIX",      buildPrefix),
                 ("HOME",              home),
                 ("PKG_TMPDIR",        pkgTmpDir),
                 ("PORTAGE_BUILDDIR",  buildDir),
                 ("PORTAGE_BASHRC",    bashrcFile),
                 -- TODO: KV and KVERS missing ...
                 -- TODO: PORTAGE_DEBUG missing ...
                 ("dbkey",             cacheFile)
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
