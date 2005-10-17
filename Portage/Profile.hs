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

readProfileFile :: FilePath -> (String -> a) -> IO [a]
readProfileFile f parser = unsafeInterleaveIO $ readProfileFileFrom profileDir [] f parser

readProfileFileFrom :: FilePath -> [a] -> FilePath -> (String -> a) -> IO [a]
readProfileFileFrom pdir acc f parser =
  do  -- is the file here?
      let f_local = pdir ./. f
      f_exists    <-  doesFileExist f_local
      f_contents  <-  if f_exists  then  strictReadFile f_local >>= return . (:[]) . parser
                                   else  return []
      let new_acc = f_contents ++ acc
      -- is there a parent?
      let p_local = pdir ./. profileParent
      p_exists    <-  doesFileExist p_local
      if p_exists  then  do  p_dir <- strictReadFile p_local >>= return . stripNewlines . stripComments
                             readProfileFileFrom (pdir ./. p_dir) new_acc f parser
                   else  return new_acc