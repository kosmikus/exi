{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    The data structure that contains a portage tree.
-}

module Portage.Tree
  where

import System.IO
import System.IO.Unsafe
import System.Directory
import qualified Data.Map as M
import Data.Map (Map)
import Data.List

import Portage.Package
import Portage.Ebuild
import Portage.Eclass
import Portage.Version
import Portage.Config
import Portage.Utilities
import Portage.Constants
import Portage.Shell

data Tree =  Tree
               {
                  eclasses  ::  Map Eclass EclassMeta,
                  ebuilds   ::  Map Category (Map Package [Variant])
               }

-- | Create a tree from an overlay.
createTree ::  FilePath ->                -- ^ the portage tree
               [Category] ->              -- ^ the list of categories
               Map Eclass EclassMeta ->   -- ^ final eclass map
               IO Tree
createTree pt cats ecs =  
    do
        eclasses' <- getEclasses
        ebuilds' <- fmap M.fromList (mapM categoryEntries cats)
        return (Tree eclasses' ebuilds')
  where
    getEclasses :: IO (Map Eclass EclassMeta)
    getEclasses             =  do
                                   eclasses <- getDirectoryContents (eclassDir pt)
                                   fmap M.fromList (mapM eclassEntries eclasses)

    eclassEntries :: Eclass -> IO (Eclass, EclassMeta)
    eclassEntries eclass    =  do
                                   mtime <-  unsafeInterleaveIO $ 
                                             getMTime (eclassDir pt ./. eclass)
                                   return (eclass, EclassMeta mtime)

    categoryEntries :: Category -> IO (Category, Map Package [Variant])
    categoryEntries cat     =  do  
                                   ps <- unsafeInterleaveIO $ categoryMap cat
                                   return (cat, ps)

    categoryMap :: Category -> IO (Map Package [Variant])
    categoryMap cat         =  do
                                   pkgs <- getSubdirectories (pt ./. cat)
                                   fmap M.fromList (mapM (packageEntries cat) pkgs)

    packageEntries :: Category -> Package -> IO (Package, [Variant])
    packageEntries cat pkg  =  do
                                   es <- unsafeInterleaveIO $ packageMap cat pkg
                                   return (pkg, es)

    packageMap :: Category -> Package -> IO [Variant]
    packageMap cat pkg      =  do
                                   ebuilds <- fmap  (  map (\x -> take (length x - 7) x) .
                                                       filter (".ebuild" `isSuffixOf`))
                                                    (getDirectoryContents (pt ./. cat ./. pkg))
                                   mapM (ebuildEntries cat pkg) ebuilds

    ebuildEntries :: Category -> Package -> String -> IO Variant
    ebuildEntries cat pkg ebuild
                            =  do
                                   let version          =  drop (length pkg + 1) ebuild
                                   let ver              =  getVersion version
                                   let pv               =  PV cat pkg ver
                                   let meta             =  EbuildMeta
                                                             {
                                                               version   =  ver,
                                                               location  =  PortageTree pt,
                                                               masked    =  []
                                                             }
                                   c <- getEbuildFromDisk pt pv ecs
                                   return (Variant meta c)

testTree = do
               cfg <- portageConfig
               cats <- categories cfg
               fixIO (\r -> createTree "/usr/portage" cats (eclasses r))


cacheEntry ::  FilePath -> PV -> FilePath
cacheEntry pt pv = cacheDir pt ./. showPV pv

-- | Returns the list of categories (from disk).
categories :: Config -> IO [Category]
categories c =  do  r <- findOverlayFile c categoriesFile lines (++)
                    case r of
                      Nothing  ->  error "categories: file not found, corrupted portage tree?"
                      Just x   ->  return x

-- | Finds and parses a file in a list of overlays.
findOverlayFile ::  Config ->                  -- ^ portage configuration
                    (FilePath -> FilePath) ->  -- ^ the filename (modulo portage tree)
                    (String -> a) ->           -- ^ the parser
                    (a -> a -> a) ->           -- ^ how to merge
                    IO (Maybe a)
findOverlayFile c f p mrg =
  let  files = map f (trees c)
       testFile n = do  ex <- doesFileExist n
                        if ex  then  strictReadFile n >>= return . (:[]) . p
                               else  return []
  in   do  found <- mapM testFile files >>= return . concat
           return $  case found of
                       []  ->  Nothing
                       xs  ->  Just (foldl1 mrg xs)

