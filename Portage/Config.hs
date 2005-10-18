{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Configuration files.
-}

module Portage.Config
  where

import System.Environment
import Data.List

import Portage.Constants
import Portage.Keyword
import Portage.Use
import Portage.Shell


data Config = Config  {
                         acceptedKeywords  ::  [Keyword],
                         use               ::  [UseFlag],
                         portDir           ::  FilePath,
  --                     distDir           ::  FilePath,
  --                     pkgDir            ::  FilePath,
  --                     logDir            ::  FilePath,
                         overlays          ::  [FilePath],
                         features          ::  [String]
                      }
  deriving (Eq,Show)

getConfig :: String -> Config
getConfig c  |  length l < 5   =  error "getConfig: corrupted portage configuration (too short)"
             |  otherwise      =  Config  (splitKeywords key)
                                          (splitUse use)
                                          pd
  --                                      distd
  --                                      pkgd
  --                                      logd
                                          (words overlays)  -- seems strange to me, but apparently that's the current rule
                                          (words features)
  where  l = lines c
         (key:use:pd:{- distd:pkgd:logd: -}overlays:features:_) = l

mergeConfig :: Config -> Config -> Config
mergeConfig c1 c2 = Config  {
                               acceptedKeywords  =  mergeKeywords (acceptedKeywords c1) (acceptedKeywords c2),
                               use               =  mergeUse (use c1) (use c2),
                               portDir           =  portDir c1 <<< portDir c2,
                               overlays          =  nub (overlays c1 ++ overlays c2),
                               features          =  nub (features c1 ++ features c2)
                            }
  where  x <<< y  |  null y     =  x
                  |  otherwise  =  y

configEnvVars = ["ACCEPT_KEYWORDS","USE","PORTDIR","PORTDIR_OVERLAY","FEATURES"]

getEnvironmentConfig :: IO Config
getEnvironmentConfig =  do  env <- getEnvironment
                            let vars = lookupList env configEnvVars
                            return $ getConfig (unlines vars)
  where  lookupList :: [(String,String)] -> [String] -> [String]
         lookupList xs = map (maybe [] id . flip lookup xs)

getConfigFile :: FilePath -> IO Config
getConfigFile f =  do  (_,r,s) <- runCommand $  "source " ++ f ++ "; VARS=" ++ show (unwords configEnvVars) ++ "; " ++
                                                "for i in ${VARS}; do echo ${!i}; done"
                       return (getConfig r)

getGlobalConfig   ::  IO Config
getProfileConfig  ::  IO Config
getUserConfig     ::  IO Config

getGlobalConfig   =  getConfigFile globalConfig
getProfileConfig  =  getConfigFile profileConfig
getUserConfig     =  getConfigFile userConfig

-- | Portage configuration is read in the following order, in increasing priority:
--   global < profile < user < environment < (package specific)
portageConfig :: IO Config
portageConfig =  do
                     global   <-  getGlobalConfig
                     profile  <-  getProfileConfig
                     user     <-  getUserConfig
                     env      <-  getEnvironmentConfig
                     return (foldl1 mergeConfig [global,profile,user,env])