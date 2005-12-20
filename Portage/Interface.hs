{-# OPTIONS -fglasgow-exts #-}

{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    User interface.
-}

module Portage.Interface
  where

import Data.IORef
import System.Console.GetOpt

import Portage.Merge
import Portage.PortageConfig

data Command a =  Command
                    {
                      command      ::  [String],
                      description  ::  String,
                      state        ::  a,
                      options      ::  [OptDescr (a -> a)],
                      handler      ::  IORef PortageConfig -> a -> [String] -> IO ()
                    }

data Command' = forall a. Command' (Command a)

commands :: [Command']
commands =  [Command' mergeCmd]

mergeCmd :: Command MergeState
mergeCmd =  Command
              {
                command = ["merge"],
                description = "merge one or more variants",
                state = MergeState { mupdate = False, mverbose = False },
                options = mergeOpts,
                handler = doMerge
              }

mergeOpts :: [OptDescr (MergeState -> MergeState)]
mergeOpts = [Option "u" ["update"] (NoArg (\s -> s { mupdate = True })) "update variants",
             Option "p" ["pretend"] (NoArg id) "calculate dependencies only",
             Option "v" ["verbose"] (NoArg (\s -> s { mverbose = True })) "be verbose"]

handleArgs :: [String] -> IO ()
handleArgs []      =  printGlobalHelp
handleArgs [a]
  | isHelp a       =  printGlobalHelp
handleArgs (x:xs)  =  do
                          r <- portageConfig >>= newIORef
                          case findCommand x of
                            Nothing -> handleCommand  r  (Command' mergeCmd)  (x:xs)
                            Just c  -> handleCommand  r  c                    xs

isHelp :: String -> Bool
isHelp "-?"      =  True
isHelp "-h"      =  True
isHelp "--help"  =  True
isHelp _         =  False

findCommand :: String -> Maybe Command'
findCommand x = lookup x [ (n,c') | c'@(Command' c) <- commands, n <- command c ] 

printGlobalHelp = putStrLn "<global help>"

handleCommand :: IORef PortageConfig -> Command' -> [String] -> IO ()
handleCommand r (Command' c) args =  
    let (fs,n,es)  =  getOpt Permute (options c) args
    in  case es of
          []  ->  handler c r (foldl (flip ($)) (state c) fs) n
          _   ->  putStrLn (unlines es) >> putStrLn (usageInfo "" . options $ c)
