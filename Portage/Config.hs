{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Configuration files.
-}

module Portage.Config
  where

import System.Environment
import Data.List
import Data.Map (Map(..))
import qualified Data.Map as M
import Data.Set (Set(..))
import qualified Data.Set as S

import Portage.Constants
import Portage.Keyword
import Portage.Use
import Portage.Shell
import Portage.Profile

type EnvMap = Map String String  -- ^ untyped environment map

data Config = Config  {
                         arch              ::  Keyword,
                         acceptedKeywords  ::  [Keyword],
                         use               ::  [UseFlag],
                         portDir           ::  FilePath,
  --                     distDir           ::  FilePath,
  --                     pkgDir            ::  FilePath,
  --                     logDir            ::  FilePath,
                         overlays          ::  [FilePath],
                         features          ::  [String],
                         useExpand         ::  [(String,String)]
                      }
  deriving (Eq,Show)

-- | Returns main portage tree plus overlays.
trees :: Config -> [FilePath]
trees c = portDir c : overlays c

getConfig :: EnvMap -> Config
getConfig c  =  Config
                  arch
                  (splitKeywords key)     -- unprocessed for now
                  (splitUse use)          -- unprocessed for now
                  pd
  --              distd
  --              pkgd
  --              logd
                  (nub (words overlays))  -- space as separator seems strange to me, but apparently that's the current rule
                  (nub (words features))
                  (zip expand expandvars)
  where  vars        =  map (\k -> M.findWithDefault "" k c) configEnvVars
         (arch:key:use:pd:{- distd:pkgd:logd: -}overlays:features:exp:_) = vars
         expand      =  words exp
         expandvars  =  map (\k -> M.findWithDefault "" k c) expand

mergeEnvMap :: EnvMap -> EnvMap -> EnvMap
mergeEnvMap m1 m2 =  M.unionWithKey
                       (\k -> if S.member k incrementals'
                              then (\x y -> x ++ " " ++ y)
                              else (<<<))
                       m1 m2
  where  x <<< y  |  null y     =  x
                  |  otherwise  =  y

incrementals'  =  S.fromList incrementals
incrementals   =  ["USE","USE_EXPAND","PORTDIR_OVERLAY","FEATURES"]

configEnvVars = ["ARCH","ACCEPT_KEYWORDS","USE","PORTDIR","PORTDIR_OVERLAY","FEATURES","USE_EXPAND"]

getEnvironmentConfig :: IO EnvMap
getEnvironmentConfig =  fmap M.fromList getEnvironment

-- | Reads a configuration file with the help of the shell. We unset 
--   all we can explicity, because otherwise clutter from the current
--   environment might accumulate (this is particularly important for
--   the incremental variables).
getConfigFile :: FilePath -> IO EnvMap
getConfigFile f =  do  (_,r,s) <- runCommand $  "unset $(set | sed 's/^\\([^=]*\\)=.*$/\\1/') 2>/dev/null;" ++
                                                "source " ++ f ++ "; set"
                       return (parseEnvMap r)

parseEnvMap :: String -> EnvMap
parseEnvMap r =  M.fromList $
                   [  (v,stripQuotes c) | 
                      l <- lines r,
                      (v,'=':c) <- return $ break (=='=') l ]
  where  stripQuotes ('\'':r@(_:_))  =  init r  -- makes use of knowledge how bash outputs the vars
         stripQuotes x              =  x


getGlobalConfig    ::  IO EnvMap
getProfileConfigs  ::  IO [EnvMap]
getUserConfig      ::  IO EnvMap

getGlobalConfig    =  getConfigFile globalConfig
getProfileConfigs  =  readProfileFile profileConfig getConfigFile
getUserConfig      =  getConfigFile userConfig

