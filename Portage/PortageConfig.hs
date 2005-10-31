{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Create the portage configuration.
-}

module Portage.PortageConfig
  where

import System.IO

import Portage.Config
import Portage.Tree
import Portage.Use
import Portage.UseDefaults
import Portage.Mask

data PortageConfig =  PortageConfig
                        {
                           config  ::  Config,
                           tree    ::  Tree,
                           inst    ::  Tree,
                           itree   ::  Tree
                        }

-- | Portage configuration is read in the following order, in increasing priority:
--   global < profile < user < environment < (package specific)
portageConfig :: IO PortageConfig
portageConfig = 
    do
        -- read basic configuration data
        global    <-  getGlobalConfig
        profiles  <-  getProfileConfigs
        user      <-  getUserConfig
        env       <-  getEnvironmentConfig
        let merged  =  foldl1 mergeConfig (global : profiles ++ [user,env])
        -- read installed tree, because that's required to determine virtuals
        -- USE data
        inst      <-  createInstalledTree merged
        ud        <-  computeUseDefaults inst
        -- the following is the "final" basic configuration
        let cfg     =  merged { use = arch merged : mergeUse (use merged) ud }
        -- now read the portage tree(s)
        cats      <-  categories cfg
        tree      <-  fixIO (\r ->  do
                                        pt  <-  createTree cfg (portDir cfg) cats (eclasses r)
                                        po  <-  mapM (\t -> createTree cfg t cats (eclasses r)) (overlays cfg)
                                        return $ foldl overlayTree pt po)
        -- hardmasking (not on the installed tree!)
        gmask     <-  globalMask cfg
        pmask     <-  profileMask
        umask     <-  userMask
        let itree   =  overlayInstalledTree tree inst
        return (PortageConfig cfg tree inst itree)
