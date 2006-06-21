{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Create the portage configuration.
-}

module Portage.PortageConfig
  (module Portage.PortageConfig, module Portage.PortageConfig.Type)
  where

import qualified Data.Map as M
import System.IO
import System.IO.Unsafe

import Portage.PortageConfig.Type
import Portage.Config
import Portage.Tree
import Portage.Keyword
import Portage.Use
import Portage.UseDefaults
import Portage.UseMask
import Portage.Mask
import Portage.Package
import Portage.PackageKeywords
import Portage.PackageUse
import Portage.PackageProvided
import Portage.Dependency
import Portage.Virtual
import Portage.World
import Portage.Utilities

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
        merged    <-  return $ foldl1 mergeEnvMap (global : profiles ++ [user,env])
        cfg       <-  getConfig merged
        envcfg    <-  getConfig env

        -- read installed tree, because that's required to determine USE data
        inst      <-  createInstalledTree cfg
        uprov     <-  profileProvided
        -- virtuals
        pvirt     <-  profileVirtuals
        virtuals  <-  return $ computeVirtuals pvirt inst

        inst      <-  return $ foldl (flip addProvided) inst uprov
        -- we don't fully support USE_ORDER, but only check if auto is in USE_ORDER
        ud        <-  if "auto" `contains` useOrder cfg
                        then  computeUseDefaults inst virtuals
                        else  return []
        usemask   <-  profileUseMask
        -- normalize keywords and USE flags; environment config overrides use.defaults
        cfg       <-  return $ cfg  {  
                                       acceptedKeywords = mergeKeywords [] (acceptedKeywords cfg),
                                       use = arch cfg : mergeUse [] (use cfg ++ ud ++ expandUse (useExpand cfg) ++ use envcfg ++ usemask)
                                    }
                          -- the outer (++) for expandUse is to produce the same order as original portage

        -- now read the portage tree(s)
        cats      <-  categories cfg
        tree      <-  fixIO (\r ->  do
                                        pt  <-  createTree cfg (portDir cfg) cats (eclasses r)
                                        po  <-  unsafeInterleaveIO $ mapM (\t -> createTree cfg t cats (eclasses r)) (overlays cfg)
                                        return $ foldl overlayTree pt po)

        -- hardmasking (not on the installed tree!)
        ppkgs     <-  profilePackages
        gmask     <-  globalMask cfg
        pmask     <-  profileMask
        umask     <-  userMask
        uunmask   <-  userUnMask
        tree      <-  return $ foldl (flip performProfilePackage)  tree ppkgs
        tree      <-  return $ foldl (flip performMask)            tree (concat [gmask, pmask, umask])
        tree      <-  return $ foldl (flip performUnMask)          tree uunmask

        -- keyword distribution (also not on the installed tree)
        -- Portage allows the environment to override package-specific keywords.
        -- We therefore add the environment keywords at the end of each package-specific
        -- entry.
        ukey      <-  userKeywords cfg
        ukey      <-  return $ map (mapKeywordsForPackage (++ acceptedKeywords envcfg)) ukey -- environment override
        tree      <-  return $ foldl (flip performKeywords)  tree ukey
        -- USE flag distribution (again, not on the installed tree)
        -- Same thing as for keywords with the environment-specific USE flags.
        uuse      <-  userUseFlags
        uuse      <-  return $ map (mapUseForPackage (++ use envcfg ++ usemask)) uuse -- environment override
        tree      <-  return $ foldl (flip performUseFlags)  tree uuse
        -- keyword masking
        tree      <-  return $ traverseTree (keywordMask cfg) tree

        -- system and world target
        let system    =  [ Plain (pdepatom p) | p <- ppkgs, psystem p ]
        world     <-  fmap (map Plain) worldTarget

        -- preparing the results
        let itree     =  overlayInstalledTree tree inst
        let exp       =  let m = categoryExpand itree in \x -> maybe [] id (M.lookup x m)

        return (PortageConfig cfg tree inst itree usemask virtuals exp system world)

