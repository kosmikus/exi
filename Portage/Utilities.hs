{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Common utilities module.
-}

module Portage.Utilities
  where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import System.Directory
import System.IO
import System.Exit
import Data.List
import Data.Maybe (fromJust)
import Data.Map (Map)
import Control.Concurrent (threadDelay)
import qualified Data.Map as Map
import Data.Tree
import Data.Char

import Portage.AnsiColor
import Portage.Config.Type

-- | Implements a "spinner".
spin :: Int -> String -> String
spin w = spin' w
  where spin' _ []           =  []
        spin' 0 xs           =  replicate w '\b' ++ spin' w xs
        spin' c xs@('\n':_)  =  xs
        spin' c (x:xs)       =  x : ' ' : '\b' : spin' (c-1) xs

-- | A delay for warning purposes. Takes the number of
--   seconds as an argument, as well as a display function.
warnDelay :: (Int -> IO ()) -> Int -> IO ()
warnDelay f 0 = return ()
warnDelay f n = f n >> threadDelay 1000000 >> warnDelay f (n-1)

-- | Aligns a table of strings.
align :: [[String]] -> String
align ts =  let  maxlengths = map (maximum . map length) (transpose ts)
            in   unlines . map (concat . zipWith formatIn maxlengths) $ ts
  where  formatIn :: Int -> String -> String
         formatIn n s = s ++ replicate (n - length s) ' '

-- | Status message about a number of packages that are eligible for
--   a certain action.
countMessage :: String -> Int -> IO ()
countMessage action n =  putStrLn $  "\n" ++ show n ++
                                     (  case n of
                                          1  ->  " package "
                                          _  ->  " packages "  ) ++
                                     "to " ++ action ++ "."

-- | Variant of |words| that accepts a custom separator.
split :: Char -> String -> [String]
split c s = case dropWhile (==c) s of
              ""  ->  []
              s'  ->  w : split c s''
                        where (w, s'') = break (==c) s'

-- | Group a number of elements.
groupnr :: Int -> [a] -> [[a]]
groupnr n = map (take n) . takeWhile (not . null) . iterate (drop n)

-- | Split a list at the last occurrence of an element.
splitAtLast :: (Eq a) => a -> [a] -> ([a],[a])
splitAtLast s xs  =   splitAt (maximum (elemIndices s xs)) xs

-- | Sort a list according to another list.
sortByList :: Ord b => [a] -> (a -> b) -> [b] -> [a]
sortByList xs p rs =  let  m = Map.fromList (zip rs [1..])
                      in   sortBy (\x y -> compare (m Map.! p x) (m Map.! p y)) xs

-- | Group by first element.
groupByFst :: Ord a => [(a,b)] -> [(a,[b])]
groupByFst  =  Map.toList . Map.fromListWith (++) . reverse . map (\ (x,y) -> (x,[y]))

-- | Reads a file completely into memory.
strictReadFile :: FilePath -> IO String
strictReadFile f  =   do  f <- readFile f
                          f `stringSeq` return f

-- | Reads a file completely into memory. Returns the
--   empty string if the file does not exist.
strictReadFileIfExists :: FilePath -> IO String
strictReadFileIfExists f  =   do  x <- doesFileExist f
                                  if x then strictReadFile f else return []

-- | Normalize the end of a file to a newline character.
normalizeEOF :: FilePath -> IO ()
normalizeEOF f  =  do  h  <-  openFile f ReadWriteMode
                       c  <-  catch  (do  hSeek h SeekFromEnd (-1)
                                          hGetChar h)
                                     (const $ return '\n')
                       case c of
                         '\n'  ->  return ()
                         _     ->  hPutChar h '\n'
                       hClose h

-- | Performs the list of actions sequentially, as long as they
--   return an exit code indicating success.
whileSuccess :: [IO ExitCode] -> IO ExitCode
whileSuccess = foldM ((>&&) . return) ExitSuccess

-- | Combines two IO actions. The second is performed if and
--   only if the first returns an exit code indicating success.
--   Like && in bash.
(>&&) :: IO ExitCode -> IO ExitCode -> IO ExitCode
a >&& b = do  exit <- a
              case exit of
                ExitSuccess  ->  b
                _            ->  return exit

-- | Combines two IO actions. The second is performed if and
--   only if the first returns an exit code indicating failure.
--   Like || in bash.
(>||) :: IO ExitCode -> IO ExitCode -> IO ExitCode
a >|| b = do  exit <- a
              case exit of
                ExitSuccess  ->  return ExitSuccess
                _            ->  b

-- | Abbreviation for "return ExitSuccess"
succeed :: IO ExitCode
succeed = return ExitSuccess

-- | Completely evaluates a string.
stringSeq :: String -> b -> b
stringSeq []      c  =  c
stringSeq (x:xs)  c  =  stringSeq xs c

-- | Concatenate two paths.
(./.) :: FilePath -> FilePath -> FilePath
path ./. file  =  path ++ "/" ++ file

-- | Checks if one list is contained in another.
contains :: Eq a => [a] -> [a] -> Bool
contains x y = any (x `isPrefixOf`) (tails y)

-- | Strip empty lines and comments from a string.
stripComments :: String -> String
stripComments = unlines . filter (not . null) . map (fst . break (=='#')) . lines

-- | Strip newline characters.
stripNewlines :: String -> String
stripNewlines = filter (/='\n')

-- | Reads a string into a map from strings to strings.
readStringMap :: [String] -> Map String String
readStringMap = Map.fromList . map ((\ (x,y) -> (x,tail y)) . break (=='='))

-- | Writes a map from strings to strings into a collapsed string.
writeStringMap :: Map String String -> [String]
writeStringMap = sortBy underscoreFirst . map (\ (x,y) -> x ++ "=" ++ y) . Map.toList
  where  underscoreFirst ('_':_)  ('_':_)  =  EQ
         underscoreFirst ('_':_)  _        =  LT
         underscoreFirst _        ('_':_)  =  GT
         underscoreFirst _        _        =  EQ

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

-- | Variant of 'head' that takes a location.
head' :: String -> [a] -> a
head' err []     =  error ("(" ++ err ++ ") empty list")
head' err (x:_)  =  x

-- | Variant of 'fromJust' that takes a location.
fromJust' :: String -> Maybe a -> a
fromJust' err Nothing   =  error ("(" ++ err ++ ") fromJust applied to Nothing")
fromJust' err (Just x)  =  x

-- | Prints a forest.
showForest :: (Int -> Bool -> a -> String) -> Int -> Forest a -> String
showForest pe d []               =  ""
showForest pe d (Node n f : ts)  =  showForest pe (d+1) f ++ pe d (not (null f)) n ++ showForest pe d ts

-- | Function that asks the user a yes\/no question, used for --ask.
askUserYesNo :: Config -> String -> IO Bool
askUserYesNo config prompt =
    withoutBuffering $ do
    putStr $ inColor config Default True Default prompt
    let stubbornAsk = do
            putStr yesNo
            ans <- getLine
            case (map toLower ans) of
              []    -> return False -- ks, I've changed this 03.09.2006, deviating from portage
              "y"   -> return True
              "yes" -> return True
              "n"   -> return False
              "no"  -> return False
              _     -> do putStr $ "Sorry, response '" ++ ans ++ "' not understood. "
                          stubbornAsk
    stubbornAsk
    where
    yesNo = "[" ++
            inColor config Green True Default "yes" ++
            "/" ++
            inColor config Red   True Default "No"  ++
            "]"

-- | Temporarily disables buffering on stdout.
withoutBuffering :: IO a -> IO a
withoutBuffering x =
    do
        b <- hGetBuffering stdout
        hSetBuffering stdout NoBuffering
        r <- x
        hSetBuffering stdout b
        return r

