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
import Portage.Config.Type

-- |CacheFormat| is now declared in |Portage.Config.Type|.

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
      (do
          s <- fmap lines . strictReadFile $ f
          return (detectStringFormat s))
      (const $ return FlatHash)

detectStringFormat :: [String] -> CacheFormat
detectStringFormat s
  | length s == 22  =  scan s
  | otherwise       =  FlatHash
  where  -- we test the RESTRICT line for an occurrence of an '='
         scan (_:_:_:_:x:_)  =  if elem '=' x then FlatHash else FlatList
         scan _              =  FlatHash
                           
