{-# OPTIONS -fglasgow-exts #-}

{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  limited, requires existential quantification

    User interface.
-}

module Portage.Interface
  where

import Control.Monad (when)
import Data.List
import Data.IORef
import System.Console.GetOpt

import Portage.Merge
import Portage.Depclean
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
                      options      ::  Bool -> [OptDescr (a -> a)],
                      handler      ::  IORef PortageConfig -> a -> [String] -> IO ()
                    }

data Command' = forall a. Command' (Command a)

commands :: Bool -> [Command']
commands showPrivate = 
    [ Command' mergeCmd
    , Command' unmergeCmd
    , Command' depcleanCmd
    , Command' rdepcleanCmd
    , Command' showInstCmd
    ] ++ concat
    [ -- these aren't listed as available commands to the user
      -- since they are for internal use only
        [ Command' nullCmd     -- dummy command
        , Command' commandsCmd -- list commands, used by the bash completion
        ]
    | showPrivate ]

mergeCmd :: Command MergeState
mergeCmd =  Command
              {
                command = ["merge"],
                description = "Merge one or more variants.",
                usage = \c -> myself ++ " " ++ c ++ " [OPTIONS] <dependency atoms>",
                state = MergeState {  mpretend = False,
                                      mupdate = False,
                                      mdeep = False,
                                      mnewuse = False,
                                      munmask = False,
                                      mtree = False,
                                      moneshot = False,
                                      mnodeps = False,
                                      mempty = False,
                                      mbacktrack = False,
                                      mverbose = False,
                                      mask = False,
                                      mdebug = False,
                                      mcomplete = False
                                   },
                options = mergeOpts,
                handler = \rpc ms ds -> do
                            when (mcomplete ms) $
                                mergeListOpts rpc ms ds
                            doMerge rpc ms ds
              }

mergeListOpts :: IORef PortageConfig -> MergeState -> [String] -> IO ()
mergeListOpts rpc ms ds =
    mapM_ putStrLn $
        [ "--" ++ o | Option _ os _ _ <- mergeOpts False, o <- os ]

mergeOpts :: Bool -> [OptDescr (MergeState -> MergeState)]
mergeOpts showPrivate =
    [Option "u" ["update"] (NoArg (\s -> s { mupdate = True })) "update variants",
     Option "D" ["deep"] (NoArg (\s -> s { mdeep = True, mupdate = True })) "deep update",
     Option "N" ["newuse"] (NoArg (\s -> s { mnewuse = True, mupdate = True })) "recompile on USE flag change",
     Option "p" ["pretend"] (NoArg (\s -> s { mpretend = True })) "calculate dependencies only",
     Option "M" ["unmask"] (NoArg (\s -> s { munmask = True, mpretend = True })) "unmask if necessary",
     Option "t" ["tree"] (NoArg (\s -> s { mtree = True })) "display packages to merge in tree form",
     Option "1" ["oneshot"] (NoArg (\s -> s { moneshot = True })) "do not modify world file",
     Option "O" ["nodeps"] (NoArg (\s -> s { mnodeps = True })) "do not merge dependencies",
     Option "e" ["emptytree"] (NoArg (\s -> s { mempty = True })) "assume that nothing is installed",
     Option "B" ["backtrack"] (NoArg (\s -> s { mbacktrack = True })) "backtrack to find more solutions",
     Option "v" ["verbose"] (NoArg (\s -> s { mverbose = True })) "be verbose",
     Option "a" ["ask"] (NoArg (\s -> s { mask = True })) "ask before merging",
     Option "d" ["debug"] (NoArg (\s -> s { mdebug = True })) "print debugging info"
    ] ++
    [Option ""  ["list-options"] (NoArg (\s -> s { mcomplete = True })) "list available options"
    | showPrivate
    ]

-- | The 'nullCmd' is only for debugging purposes. It does nothing (except initialization)
--   and therefore should be really fast.

nullCmd :: Command ()
nullCmd =  Command
             {
                command = ["null"],
                usage = \c -> myself ++ " " ++ c,
                description = "Do nothing (for debugging only).",
                state = (),
                options = const [],
                handler = \_ _ _ -> return ()
             }

-- | Shows the installed versions of one or more atoms.

showInstCmd :: Command ()
showInstCmd =  Command
                 {
                    command = ["showinst"],
                    usage = \c -> myself ++ " " ++ c ++ " <dependency atoms>",
                    description = "Show installed versions of one or more packages.",
                    state = (),
                    options = const [],
                    handler = \cr _ atoms -> doShowInst cr atoms
                 }

-- | Removes all packages that aren't either in the world file, system targets,
--   or required by installed packages.

xdepcleanCmd :: Bool -> Command UnmergeState
xdepcleanCmd rdepclean =
               Command
                 {
                    command = [if rdepclean then "rdepclean" else "depclean"],
                    usage = \c -> myself ++ " " ++ c ++ " [OPTIONS]",
                    description = "Remove variants that are no longer required by "
                               ++ (if rdepclean then "" else "buildtime and ") ++
                               "runtime dependencies.",
                    state = defaultUnmergeState,
                    options = unmergeOpts,
                    handler = \rpc ms ds -> do
                               if (ucomplete ms)
                                 then  unmergeListOpts rpc ms ds
                                 else  doDepclean rdepclean rpc ms
                 }

depcleanCmd = xdepcleanCmd False
rdepcleanCmd = xdepcleanCmd True

-- | Default state for unmerge commands.
defaultUnmergeState :: UnmergeState
defaultUnmergeState =
  UnmergeState {  upretend = False,
                  utree = False,
                  uoneshot = False,
                  unodeps = False,
                  uverbose = False,
                  uask = False,
                  udebug = False,
                  ucomplete = False  }
                            

-- | Removes a package and everything that (runtime-)depends on it.

unmergeCmd :: Command UnmergeState
unmergeCmd =   Command
                 {
                    command = ["unmerge"],
                    usage = \c -> myself ++ " " ++ c ++ " [OPTIONS] <dependency atoms>",
                    description = "Remove one or more variants.",
                    state = defaultUnmergeState,
                    options = unmergeOpts,
                    handler = \rpc ms ds -> do
                               when (ucomplete ms) $
                                   unmergeListOpts rpc ms ds
                               doUnmerge rpc ms ds
                 }

unmergeListOpts :: IORef PortageConfig -> UnmergeState -> [String] -> IO ()
unmergeListOpts rpc ms ds =
    mapM_ putStrLn $
        [ "--" ++ o | Option _ os _ _ <- unmergeOpts False, o <- os ]

unmergeOpts :: Bool -> [OptDescr (UnmergeState -> UnmergeState)]
unmergeOpts showPrivate =
    [Option "p" ["pretend"] (NoArg (\s -> s { upretend = True })) "calculate dependencies only",
     Option "t" ["tree"] (NoArg (\s -> s { utree = True })) "display packages to merge in tree form",
     Option "1" ["oneshot"] (NoArg (\s -> s { uoneshot = True })) "do not modify world file",
     Option "O" ["nodeps"] (NoArg (\s -> s { unodeps = True })) "do not merge dependencies",
     Option "v" ["verbose"] (NoArg (\s -> s { uverbose = True })) "be verbose",
     Option "a" ["ask"] (NoArg (\s -> s { uask = True })) "ask before merging",
     Option "d" ["debug"] (NoArg (\s -> s { udebug = True })) "print debugging info"
    ] ++
    [Option ""  ["list-options"] (NoArg (\s -> s { ucomplete = True })) "list available options"
    | showPrivate
    ]

handleArgs :: [String] -> IO ()
handleArgs []      =  printGlobalHelp
handleArgs [a]
  | isHelp a       =  printGlobalHelp
handleArgs (x:xs)  =  do
                          r <- portageConfig >>= newIORef
                          case findCommand x of
                            Nothing -> handleCommand  r  "merge"  (Command' mergeCmd)  (x:xs)
                            Just c  -> handleCommand  r  x        c                    xs

isHelp :: String -> Bool
isHelp "-?"      =  True
isHelp "-h"      =  True
isHelp "--help"  =  True
isHelp _         =  False

findCommand :: String -> Maybe Command'
findCommand x = lookup x [ (n,c') | c'@(Command' c) <- commands True, n <- command c ] 

printGlobalHelp = putStrLn $ header ++ "\n\n" ++ printCommands (commands False)

commandsCmd :: Command ()
commandsCmd = Command
                {
                    command = ["commands"],
                    usage = \c -> myself ++ " " ++ c,
                    description = "Show available commands.",
                    state = (),
                    options = const [],
                    handler = \_ _ _ -> listCommands
                }

listCommands :: IO ()
listCommands =
    mapM_ putStrLn $
        [ n | (Command' c) <- commands False, n <- command c]

header =
  "exi version " ++ programVersion ++ "\n(c) 2005-2006 Andres Loeh <exi@andres-loeh.de>"

handleCommand :: IORef PortageConfig -> String -> Command' -> [String] -> IO ()
handleCommand r cname (Command' c) args =  
    let (fs,n,es)  =  getOpt Permute (options c True) args
    in  case es of
          []  ->  handler c r (foldl (flip ($)) (state c) fs) n
          _   ->  do  putStrLn (unlines es)
                      putStrLn (usageInfo (usage c cname) . options c $ False)

printCommands :: [Command'] -> String
printCommands = align . map printCommand
  where
    printCommand (Command' cmd) =
        [(concat . intersperse ", ") (command cmd), "  ", description cmd]
