{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
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

import Portage.Constants
import Portage.Utilities

type MTime   =  EpochTime

-- | Run a command and return the output and errors.
runCommand :: String -> IO (ExitCode,String,String)
runCommand cmd = do
                     (cin,cout,cerr,pid) <- runInteractiveProcess "/bin/bash" ["-c", cmd] Nothing Nothing
                     hClose cin
                     out <- hGetContents cout
                     err <- hGetContents cerr
                     stringSeq out (hClose cout)
                     stringSeq err (hClose cerr)
                     exit <- waitForProcess pid
                     return (exit,out,err)

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

-- | Create a group-writable file (along with directories).
makePortageFile :: FilePath -> IO ()
makePortageFile f = do
                        createDirectoryIfMissing True (dirname f)
                        h <- openFile f WriteMode
                        hClose h
