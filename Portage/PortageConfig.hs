{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Create the portage configuration.
-}

module Portage.PortageConfig
  where

import Portage.Config
import Portage.Tree
import Portage.Use
import Portage.UseDefaults

-- | Portage configuration is read in the following order, in increasing priority:
--   global < profile < user < environment < (package specific)
portageConfig :: IO Config
portageConfig =  do
                     global    <-  getGlobalConfig
                     profiles  <-  getProfileConfigs
                     user      <-  getUserConfig
                     env       <-  getEnvironmentConfig
                     let merged = foldl1 mergeConfig (global : profiles ++ [user,env])
                     inst      <-  createInstalledTree merged
                     ud        <-  computeUseDefaults inst
                     return (merged { use = arch merged : mergeUse (use merged) ud })
