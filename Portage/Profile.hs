{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Information located in the profile.
-}

module Portage.Profile
  where

import Prelude hiding (catch)
import System.Posix
import System.Directory
import System.IO.Unsafe
import Control.Monad
import Control.Exception

import Portage.Constants
import Portage.Utilities

-- | Reads a file in the profile directories, cascading-profiles-style.
--   It takes the name of the file (without directory), and a parser for
--   the file. The resulting list contains the result for each of the 
--   found files, the file with highest priority last.
readProfileFile :: FilePath -> (FilePath -> IO a) -> IO [a]
readProfileFile f parser =
  unsafeInterleaveIO $
  do
      pdirs <- getProfileDirs
      fmap reverse $ foldM (\acc d -> readProfileIn d acc f parser) [] pdirs

readProfileIn :: FilePath -> [a] -> FilePath -> (FilePath -> IO a) -> IO [a]
readProfileIn pdir acc f parser =
  do  -- is the file here?
      let f_local = pdir ./. f
      f_exists    <-  doesFileExist f_local
      f_contents  <-  if f_exists  then  fmap (:[]) (parser f_local)
                                   else  return []
      return $ f_contents ++ acc

getProfileDirs :: IO [FilePath]
getProfileDirs =  unsafeInterleaveIO $ getProfileDirsFrom profileDir [localProfileDir]

getProfileDirsFrom :: FilePath -> [FilePath] -> IO [FilePath]
getProfileDirsFrom pdir acc =
  do  pdir' <- canonicalizePath pdir
      let new_acc = pdir' : acc
      -- is there a parent?
      let p_local = pdir' ./. profileParent
      p_exists <- doesFileExist p_local
      if p_exists  then  do  p_dir <- strictReadFile p_local >>= return . stripNewlines . stripComments
                             getProfileDirsFrom (pdir' ./. p_dir) new_acc
                   else  return new_acc
