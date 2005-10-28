{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Information located in the profile.
-}

module Portage.Profile
  where

import System.Directory
import System.IO.Unsafe

import Portage.Constants
import Portage.Utilities

readProfileFile :: FilePath -> (FilePath -> IO a) -> IO [a]
readProfileFile f parser =  unsafeInterleaveIO $ 
                            do  -- read local profile (with priority)
                                acc <- readProfileIn localProfileDir [] f parser
                                -- read selected profile
                                readProfileFileFrom profileDir acc f parser

readProfileFileFrom :: FilePath -> [a] -> FilePath -> (FilePath -> IO a) -> IO [a]
readProfileFileFrom pdir acc f parser =
  do  new_acc     <-  readProfileIn pdir acc f parser
      -- is there a parent?
      let p_local = pdir ./. profileParent
      p_exists    <-  doesFileExist p_local
      if p_exists  then  do  p_dir <- strictReadFile p_local >>= return . stripNewlines . stripComments
                             readProfileFileFrom (pdir ./. p_dir) new_acc f parser
                   else  return new_acc

readProfileIn :: FilePath -> [a] -> FilePath -> (FilePath -> IO a) -> IO [a]
readProfileIn pdir acc f parser =
  do  -- is the file here?
      let f_local = pdir ./. f
      f_exists    <-  doesFileExist f_local
      f_contents  <-  if f_exists  then  fmap (:[]) (parser f_local)
                                   else  return []
      return $ f_contents ++ acc
