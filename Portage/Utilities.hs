{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Common utilities module.
-}

module Portage.Utilities
  where

import System.Directory
import Data.List

-- | Variant of |words| that accepts a custom separator.
split :: Char -> String -> [String]
split c s = case dropWhile (==c) s of
              ""  ->  []
              s'  ->  w : split c s''
                        where (w, s'') = break (==c) s'


-- | Split a list at the last occurrence of an element.
splitAtLast :: (Eq a) => a -> [a] -> ([a],[a])
splitAtLast s xs  =   splitAt (maximum (elemIndices s xs)) xs

-- | Reads a file completely into memory.
strictReadFile :: FilePath -> IO String
strictReadFile f  =   do  f <- readFile f
                          f `stringSeq` return f

-- | Reads a file completely into memory. Returns the
--   empty string if the file does not exist.
strictReadFileIfExists :: FilePath -> IO String
strictReadFileIfExists f  =   do  x <- doesFileExist f
                                  if x then strictReadFile f else return []

-- | Completely evaluates a string.
stringSeq :: String -> b -> b
stringSeq []      c  =  c
stringSeq (x:xs)  c  =  stringSeq xs c

-- | Concatenate two paths.
(./.) :: FilePath -> FilePath -> FilePath
path ./. file  =  path ++ "/" ++ file

-- | Strip empty lines and comments from a string.
stripComments :: String -> String
stripComments = unlines . filter (not . null) . map (fst . break (=='#')) . lines

-- | Strip newline characters.
stripNewlines :: String -> String
stripNewlines = filter (/='\n')

-- | The function 'splitPath' is a reimplementation of the Python
--   function @os.path.split@.
splitPath :: FilePath 
          -> (FilePath,  -- the part before the final slash; may be empty
              FilePath)  -- the part after the final slash; may be empty
splitPath p
    = let
          slashes = elemIndices '/' p 
          index   = if null slashes then 0 else last slashes + 1
          (hd,tl) = splitAt index p
          fhd | null hd || hd `isPrefixOf` repeat '/'
                  = hd
              | otherwise
                  = reverse . dropWhile (=='/') . reverse $ hd
      in
          (fhd,tl)

-- | The function 'dirname' is a reimplementation of the Python function
--   @os.path.dirname and@ returns the directory component of a pathname.
dirname :: FilePath -> FilePath
dirname = fst . splitPath

-- | The function 'basename' is a reimplementation of the Python function
--   @os.path.basename@ and returns the non-directory component of a pathname.
basename :: FilePath -> FilePath
basename = snd . splitPath
