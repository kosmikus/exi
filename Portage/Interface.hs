{-# OPTIONS -fglasgow-exts #-}

{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  limited, requires existential quantification

    User interface.
-}

module Portage.Interface
  where

import Data.List
import Data.IORef
import System.Console.GetOpt

import Portage.Merge
import Portage.Info
import Portage.PortageConfig
import Portage.ProgramVersion
import Portage.Utilities
import Portage.Constants

data Command a =  Command
                    {
                      command      ::  [String],
                      description  ::  String,
                      usage        ::  String -> String,
                      state        ::  a,
                      options      ::  [OptDescr (a -> a)],
                      handler      ::  IORef PortageConfig -> a -> [String] -> IO ()
                    }

data Command' = forall a. Command' (Command a)

commands :: [Command']
commands =  [Command' mergeCmd, Command' nullCmd, Command' showInstCmd]

mergeCmd :: Command MergeState
mergeCmd =  Command
              {
                command = ["merge"],
                description = "Merge one or more variants.",
                usage = \c -> myself ++ " " ++ c ++ " [OPTIONS] dependency atoms ...",
                state = MergeState {  mupdate = False,
                                      munmask = False,
                                      mtree = False,
                                      mverbose = False },
                options = mergeOpts,
                handler = doMerge
              }

mergeOpts :: [OptDescr (MergeState -> MergeState)]
mergeOpts = [Option "u" ["update"] (NoArg (\s -> s { mupdate = True })) "update variants",
             Option "p" ["pretend"] (NoArg id) "calculate dependencies only",
             Option "M" ["unmask"] (NoArg (\s -> s { munmask = True {- , mpretend = True -} })) "unmask if necessary",
             Option "t" ["tree"] (NoArg (\s -> s { mtree = True })) "display packages to merge in tree form",
             Option "v" ["verbose"] (NoArg (\s -> s { mverbose = True })) "be verbose"]

-- | The 'nullCmd' is only for debugging purposes. It does nothing (except initialization)
--   and therefore should be really fast.

nullCmd :: Command ()
nullCmd =  Command
             {
                command = ["null"],
                usage = \c -> myself ++ " " ++ c,
                description = "Do nothing (for debugging only).",
                state = (),
                options = [],
                handler = \_ _ _ -> return ()
             }

-- | Shows the installed versions of one or more atoms.

showInstCmd :: Command ()
showInstCmd =  Command
                 {
                    command = ["showinst"],
                    usage = \c -> myself ++ " " ++ c ++ " dependency atoms",
                    description = "Show installed versions of one or more packages.",
                    state = (),
                    options = [],
                    handler = \cr _ atoms -> doShowInst cr atoms
                 }

handleArgs :: [String] -> IO ()
handleArgs []      =  printGlobalHelp
handleArgs [a]
  | isHelp a       =  printGlobalHelp
handleArgs (x:xs)  =  do
                          r <- portageConfig >>= newIORef
                          case findCommand x of
                            Nothing -> handleCommand  r  x  (Command' mergeCmd)  (x:xs)
                            Just c  -> handleCommand  r  x  c                    xs

isHelp :: String -> Bool
isHelp "-?"      =  True
isHelp "-h"      =  True
isHelp "--help"  =  True
isHelp _         =  False

findCommand :: String -> Maybe Command'
findCommand x = lookup x [ (n,c') | c'@(Command' c) <- commands, n <- command c ] 

printGlobalHelp = putStrLn $ header ++ "\n\n" ++ printCommands commands

header =
  "exi version " ++ programVersion ++ "\n(c) 2005-2006 Andres Loeh <exi@andres-loeh.de>"

handleCommand :: IORef PortageConfig -> String -> Command' -> [String] -> IO ()
handleCommand r cname (Command' c) args =  
    let (fs,n,es)  =  getOpt Permute (options c) args
    in  case es of
          []  ->  handler c r (foldl (flip ($)) (state c) fs) n
          _   ->  do  putStrLn (unlines es)
                      putStrLn (usageInfo (usage c cname) . options $ c)

printCommands :: [Command'] -> String
printCommands = align . map printCommand
  where
    printCommand (Command' cmd) =
        [(concat . intersperse ", ") (command cmd), "  ", description cmd]
