{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Matching of dependency atoms.
-}

module Portage.Match
  where

import Portage.Dependency
import Portage.Version
import Portage.Tree
import Portage.Ebuild
import Portage.Package

-- | Matches a dependency atom against a tree.
matchDepAtomTree :: DepAtom -> Tree -> [Variant]
matchDepAtomTree d@(DepAtom _ _ _ cat pkg _) t =
    let  candidates = t !? (cat,pkg)
    in   filter (matchDepAtomVariant d) candidates

-- | Variant of |matchDepAtomTree| that only checks if
--   any unmasked variant is present.
isInTree :: DepAtom -> Tree -> Bool
isInTree d t = not . null . filterMaskedVariants $ matchDepAtomTree d t

matchDepAtomVariant :: DepAtom -> Variant -> Bool
matchDepAtomVariant d (Variant m _) = matchDepAtomVersion d (version . pv $ m)

matchDepAtomVersion :: DepAtom -> Version -> Bool
matchDepAtomVersion (DepAtom True a b c d e) v
    =  not $ matchDepAtomVersion (DepAtom False a b c d e) v
matchDepAtomVersion (DepAtom False True b c d e) v
    =  matchDepAtomVersion  (DepAtom False False b c d ((liftDepVer stripRev) e))
                            (stripRev v) 
matchDepAtomVersion (DepAtom False False DNONE c d e) v
    =  True  -- no modifier, no version
matchDepAtomVersion (DepAtom False False m c d (DepVer w False)) v
    =  modm m v w
  where  modm DLT   =  (<)
         modm DLEQ  =  (<=)
         modm DEQ   =  (==)
         modm DGEQ  =  (>=)
         modm DGT   =  (>)
matchDepAtomVersion (DepAtom False False m c d (DepVer w True)) v
    | m `elem` [DLT,DGT]
        =  matchDepAtomVersion (DepAtom False False m c d (DepVer w False))   v
    | m `elem` [DLEQ,DGEQ]
        =  matchDepAtomVersion (DepAtom False False m c d (DepVer w False))   v  ||
           matchDepAtomVersion (DepAtom False False DEQ c d (DepVer w True))  v
    | otherwise
        =  w `versionPrefixOf` v

