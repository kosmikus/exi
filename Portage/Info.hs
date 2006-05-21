{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  limited, requires existential quantification

    Information about the system.
-}

module Portage.Info
  where

import Data.List (intersperse)
import Data.IORef

import Portage.Dependency
import Portage.PortageConfig
import Portage.Graph hiding (getDepAtom)
import Portage.Ebuild
import Portage.Package

doShowInst :: IORef PortageConfig -> [String] -> IO ()
doShowInst r ds =  readIORef r >>= \pc -> 
                   putStr $
                     unlines . map (showVersions . (\x -> (x,findVersions (inst pc) x)) . getDepAtom' (expand pc)) $ ds

showVersions :: (DepAtom, [Variant]) -> String
showVersions (da,vs) = show da ++ ": " ++  (  concat . intersperse ", " .
                                              map (show . verPV . pv . meta) $ vs)
