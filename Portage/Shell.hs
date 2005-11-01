{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Shell utilities.
-}

module Portage.Shell
  where

import Data.Bits
import System.Posix
import System.Process
import System.Exit
import System.IO
import System.Directory
import Control.Monad

import Portage.Constants
import Portage.Utilities

type MTime   =  EpochTime

-- | Run a command in a specific environment and return the output and errors.
runCommandInEnv  ::  String                -- ^ the command
                 ->  [(String,String)]     -- ^ the environment
                 ->  IO (ExitCode,String,String)
runCommandInEnv cmd env = 
                 do
                     let env' = if null env then Nothing else Just env
                     (cin,cout,cerr,pid) <- runInteractiveProcess "/bin/bash" ["-c", cmd] Nothing env'
                     hClose cin
                     out <- hGetContents cout
                     err <- hGetContents cerr
                     stringSeq out (hClose cout)
                     stringSeq err (hClose cerr)
                     exit <- waitForProcess pid
                     return (exit,out,err)

-- | Run a command and return the output and errors.
runCommand  ::  String               -- ^ the command
            ->  IO (ExitCode,String,String)
runCommand cmd = runCommandInEnv cmd []

-- | Quotes a string such that it will survive the shell.
quote :: String -> String
quote x =  "\"" ++ concatMap quotesingle x ++ "\""
  where quotesingle '"'  = "\\\""
        quotesingle '`'  = "\\`"
        quotesingle '\\' = "\\\\"
        quotesingle '$'  = "\\$"
        quotesingle x    = [x]

-- | Set the group-write bit for a file.
makeGroupWritable :: FilePath -> IO ()
makeGroupWritable f = do
                          mode <- fmap fileMode (getFileStatus f)
                          setFileMode f (mode .|. groupReadMode
                                              .|. groupWriteMode)

-- | Determine the file modification time (mtime).
getMTime :: FilePath -> IO MTime
getMTime f = fmap modificationTime (getFileStatus f)

-- | Create (touch) a group-writable file (along with directories).
makePortageFile :: FilePath -> IO ()
makePortageFile f = do
                        ex <- doesFileExist f
                        when (not ex) $
                          do
                              createDirectoryIfMissing True (dirname f)
                              h <- openFile f WriteMode
                              hClose h
                              makeGroupWritable f

-- | Returns a list of subdirectories.
getSubdirectories :: FilePath -> IO [FilePath]
getSubdirectories f = 
    do
        fs <- getDirectoryContents f
        filterM  (doesDirectoryExist . (f ./.)) 
                 (filter  (\x -> case x of  ('.':_)  ->  False
                                            _        ->  True)
                          fs)

ifDirectoryExists :: (FilePath -> IO [FilePath]) -> FilePath -> IO [FilePath]
ifDirectoryExists l f =
    do
        ex <- doesDirectoryExist f
        if ex  then  l f
               else  return []
