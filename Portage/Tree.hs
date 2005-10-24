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
import Portage.Eclass (Eclass, EclassMeta(EclassMeta))
import qualified Portage.Eclass as EC
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
createTree ::  Config ->                  -- ^ portage configuration
               FilePath ->                -- ^ the portage tree
               [Category] ->              -- ^ the list of categories
               Map Eclass EclassMeta ->   -- ^ final eclass map
               IO Tree
createTree cfg pt cats ecs =  
    do
        eclasses' <- getEclasses
        ebuilds' <- fmap M.fromList (mapM categoryEntries cats)
        return (Tree eclasses' ebuilds')
  where
    getEclasses :: IO (Map Eclass EclassMeta)
    getEclasses             =  do
                                   eclasses <- fmap  (  map (\x -> take (length x - 7) x) .
                                                        filter (".eclass" `isSuffixOf`))
                                                     (ifDirectoryExists getDirectoryContents (eclassDir pt))
                                   fmap M.fromList (mapM eclassEntries eclasses)

    eclassEntries :: Eclass -> IO (Eclass, EclassMeta)
    eclassEntries eclass    =  do
                                   mtime <-  unsafeInterleaveIO $ 
                                             getMTime (eclassDir pt ./. (eclass ++ ".eclass"))
                                   return (eclass, EclassMeta pt mtime)

    categoryEntries :: Category -> IO (Category, Map Package [Variant])
    categoryEntries cat     =  do  
                                   ps <- unsafeInterleaveIO $ categoryMap cat
                                   return (cat, ps)

    categoryMap :: Category -> IO (Map Package [Variant])
    categoryMap cat         =  do
                                   pkgs <- ifDirectoryExists getSubdirectories (pt ./. cat)
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
                                   c <- unsafeInterleaveIO $ getEbuildFromDisk cfg pt pv ecs
                                   return (Variant meta c)

-- | Combines two trees such that the second one is the overlay and has priority.
overlayTree :: Tree -> Tree -> Tree
overlayTree (Tree ec1 eb1) (Tree ec2 eb2) = Tree  (overlayEclasses  ec1  ec2)
                                                  (overlayEbuilds   eb1  eb2)
  where
    overlayEclasses :: Map Eclass EclassMeta -> Map Eclass EclassMeta -> Map Eclass EclassMeta
    overlayEclasses  =  M.unionWith (curry snd)

    overlayEbuilds ::  Map Category (Map Package [Variant]) ->
                       Map Category (Map Package [Variant]) ->
                       Map Category (Map Package [Variant])
    overlayEbuilds   =  M.unionWith (M.unionWith shadowVariants)

    shadowVariants :: [Variant] -> [Variant] -> [Variant]
    shadowVariants vs1 vs2 = vs2 ++ foldr shadowVariant vs1 vs2

    shadowVariant :: Variant -> [Variant] -> [Variant]
    shadowVariant (Variant (EbuildMeta { version = v, location = l }) _) vs = 
        [  if v == w then Variant (m { masked = (Shadowed l) : masked m }) x else o | 
           o@(Variant (m@(EbuildMeta { version = w })) x) <- vs ]

testTree = do
               cfg <- portageConfig
               cats <- categories cfg
               fixIO (\r ->  do
                                 pt  <-  createTree cfg (portDir cfg) cats (eclasses r)
                                 po  <-  mapM (\t -> createTree cfg t cats (eclasses r)) (overlays cfg)
                                 return $ foldl overlayTree pt po)

cacheEntry ::  FilePath -> PV -> FilePath
cacheEntry pt pv = cacheDir pt ./. showPV pv

-- | Returns the list of categories (from disk).
categories :: Config -> IO [Category]
categories c =  do  r <- findOverlayFile c categoriesFile lines (++)
                    case r of
                      Nothing  ->  error "categories: file not found, corrupted portage tree?"
                      Just x   ->  return x

-- | Performs a traversal on a tree.
traverseTree :: (Category -> Package -> Variant -> Variant) -> Tree -> Tree
traverseTree f (Tree c e) =
  Tree c (M.mapWithKey (\cat -> M.mapWithKey (\pkg -> map (\var -> f cat pkg var))) e)

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

