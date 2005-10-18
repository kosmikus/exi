{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Shell utilities.
-}

module Portage.Shell
  where

import System.Process
import System.Exit
import System.IO

import Portage.Constants
import Portage.Utilities

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
