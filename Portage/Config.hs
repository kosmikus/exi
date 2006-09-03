{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Basic portage configuration (via configuration files).
-}

module Portage.Config
  (module Portage.Config, module Portage.Config.Type)
  where

import System.IO.Unsafe
import System.Environment
import Data.List
import Data.Map (Map(..))
import qualified Data.Map as M
import Data.Set (Set(..))
import qualified Data.Set as S

import Portage.Config.Type
import Portage.Constants
import Portage.Keyword
import Portage.Use
import Portage.Shell
import Portage.Profile
import Portage.Cache

-- | Returns main portage tree plus overlays.
trees :: Config -> [FilePath]
trees c = portDir c : overlays c

getConfig :: EnvMap -> IO Config
getConfig c  =  
    do
        cf <- unsafeInterleaveIO $ detectCacheFormat . cacheDir $ pd
        return $
          Config  arch
                  (splitKeywords key)     -- unprocessed for now
                  (splitUse use)          -- unprocessed for now
                  useorder
                  pd
                  tmpd
  --              distd
  --              pkgd
  --              logd
                  cf
                  (nub (words overlays))  -- space as separator seems strange to me, but apparently that's the current rule
                  (nub (words configprotecteds))  -- dito
                  (nub (words features))
                  (zip expand expandvars)
                  (nub (words exphidden))
                  False  -- debug
                  True   -- color
  where  vars        =  map (\k -> M.findWithDefault "" k c) configEnvVars
         (arch:key:use:useorder:pd:tmpd:{- distd:pkgd:logd: -}overlays:configprotecteds:features:exp:exphidden:_) = vars
         expand      =  words exp
         expandvars  =  map (\k -> M.findWithDefault "" k c) expand

mergeEnvMap :: EnvMap -> EnvMap -> EnvMap
mergeEnvMap m1 m2 =  M.unionWithKey
                       (\k -> if S.member k incrementals
                              then (\x y -> x ++ " " ++ y)
                              else const id) -- prefer the second, note that an empty setting still overwrites!
                       m1 m2

incrementals   =  S.fromList  [  "USE",
                                 "USE_EXPAND",
                                 "USE_EXPAND_HIDDEN",
                                 "CONFIG_PROTECT",
                                 "FEATURES",
                                 "ACCEPT_KEYWORDS"  ]

configEnvVars  =  [  "ARCH",
                     "ACCEPT_KEYWORDS",
                     "USE",
                     "USE_ORDER",
                     "PORTDIR",
                     "PORTAGE_TMPDIR",
                     "PORTDIR_OVERLAY",
                     "CONFIG_PROTECT",
                     "FEATURES",
                     "USE_EXPAND",
                     "USE_EXPAND_HIDDEN"  ]

getEnvironmentConfig :: IO EnvMap
getEnvironmentConfig =  unsafeInterleaveIO $ fmap M.fromList getEnvironment

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

getGlobalConfig    =  unsafeInterleaveIO $ getConfigFile globalConfig
getProfileConfigs  =  unsafeInterleaveIO $ readProfileFile profileConfig getConfigFile
getUserConfig      =  unsafeInterleaveIO $ getConfigFile userConfig

