{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    The data structure that contains a portage tree.
-}

module Portage.Tree
  where

import System.Directory
import qualified Data.Map as M
import Data.Map (Map)

import Portage.Package
import Portage.Ebuild
import Portage.Version
import Portage.Config
import Portage.Utilities
import Portage.Constants

data Tree = Map Category (Map Package (Map Version [Ebuild]))

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

