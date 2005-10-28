{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Compute the default USE flags for a given installation.
-}

module Portage.UseDefaults
  where

import Portage.Profile
import Portage.Tree
import Portage.Dependency
import Portage.Use
import Portage.Utilities
import Portage.Constants
import Portage.Match

-- The format of the use.defaults file (from "man portage"):
-- Line-based, with shell-style comments.
-- Each line contains a USE flag followed by a list of depend atoms.
-- The semantics is that the USE flag should be active if all of the
-- depend atoms are fulfilled in the installed tree.
-- If you want an "or"-like semantics, use the same USE flag multiple
-- times on different lines.

-- | Parse a use.defaults file.
getUseDefaults :: String -> [(UseFlag,[DepAtom])]
getUseDefaults = map getUseDefault . lines . stripComments

-- | Parse a single line of the use.defaults file.
getUseDefault :: String -> (UseFlag,[DepAtom])
getUseDefault l =  let  (u:ds) = words l
                   in   (u,map getDepAtom ds)


computeUseDefaults ::  Tree ->         -- ^ the tree of installed packages
                       IO [UseFlag]    -- ^ resulting USE flags
computeUseDefaults t = 
    do
        ls <- fmap concat (readProfileFile  useDefaults
                                            (\x -> fmap getUseDefaults (strictReadFile x)))
        return [ u | l@(u,_) <- ls, checkUse l ]
  where
    checkUse :: (UseFlag,[DepAtom]) -> Bool
    checkUse (u,ds) = all (`isInTree` t) ds
