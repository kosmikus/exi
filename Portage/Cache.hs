{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Portage cache.
-}

module Portage.Cache
  where

import Prelude hiding (catch)
import Data.Maybe (listToMaybe)
import Data.List (isPrefixOf)
import Control.Exception
import System.Directory
import System.IO.Unsafe

import Portage.Utilities

data CacheFormat = FlatHash | FlatList
  deriving (Eq,Show)

detectCacheFormat :: FilePath -> IO CacheFormat
detectCacheFormat c = findCacheFile c >>= detectFileFormat

findCacheFile :: FilePath -> IO (Maybe FilePath)
findCacheFile c =
  catch
    (  do
           subdirs  <-  fmap (filter (not . isPrefixOf ".")) (getDirectoryContents c)
           files    <-  mapM  (\x -> (fmap (map (\f -> c ./. x ./. f)) 
                                           (unsafeInterleaveIO $
                                              catch (getDirectoryContents (c ./. x)) (const $ return []))))
                              subdirs
           return . listToMaybe . filter (not . isPrefixOf "." . basename) . concat $ files)
    (const $ return Nothing)

detectFileFormat :: Maybe FilePath -> IO CacheFormat
detectFileFormat Nothing   =  return FlatHash  -- assume new format
detectFileFormat (Just f)  =
  catch
    (  do
           s <- readFile f
           return (if length (lines s) == 22 then FlatList else FlatHash))
    (const $ return FlatHash)
    
