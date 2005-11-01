{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Eclasses.
-}

module Portage.Eclass
  where

import System.IO
import System.Posix

import Portage.Shell
import Portage.Utilities

type Eclass  =  String

-- | The 'EclassMeta' type contains information about the identity of
--   an eclass. We use modification time and tree location.
data EclassMeta =  EclassMeta
                     {
                        location  ::  FilePath, -- use 'TreeLocation' instead?
                        mtime     ::  MTime
                     }
  deriving (Show,Eq)

-- We do not save shadowed eclasses, because unlike ebuilds, eclasses
-- are not directly visible to the user (although this might change in
-- the future). Having only one eclass per name in the finite map
-- simplifies the situation a bit.

-- | Splits a string of eclasses.
splitEclasses :: String -> [Eclass]
splitEclasses = words

-- | Reads a file associating eclasses with mtimes.
readEclassesFile :: FilePath -> IO [(Eclass,MTime)]
readEclassesFile f =  do
                          c <- strictReadFile f
                          return $
                            map  ((\(x,y) -> (tail y,read x)) . break (==' '))
                                 (filter (not . null) (lines c))

-- | Writes a file associating eclasses with mtimes.
writeEclassesFile :: FilePath -> [(Eclass,MTime)] -> IO ()
writeEclassesFile f c =  do
                             let out  =  map  (\(e,m) -> show m ++ " " ++ e) c
                             h <- openFile f WriteMode
                             hPutStr h (unlines out)
                             hClose h
