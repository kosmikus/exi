{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
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
matchDepAtomTree d@(DepAtom _ _ _ cat pkg _ _) t =
    let  candidates = t !? (P cat pkg)
    in   filter (matchDepAtomVariant d) candidates

-- | Modifies a tree on all matches of a dependency atom.
modifyTreeForDepAtom :: DepAtom -> (Variant -> Variant) -> Tree -> Tree
modifyTreeForDepAtom d@(DepAtom _ _ _ cat pkg _ _) f =
    modifyTree  cat pkg
                (\v -> if matchDepAtomVariant d v then f v else v)

-- | Modifies a tree on all non-matches of a dependency atom.
modifyTreeForNegDepAtom :: DepAtom -> (Variant -> Variant) -> Tree -> Tree
modifyTreeForNegDepAtom d@(DepAtom _ _ _ cat pkg _ _) f =
    modifyTree  cat pkg
                (\v -> if matchDepAtomVariant d v then v else f v)

-- | Variant of 'matchDepAtomTree' that only checks if
--   any unmasked variant is present.
isInTree :: Tree -> DepAtom -> Bool
isInTree t d = not . null . filterMaskedVariants $ matchDepAtomTree d t

-- | Same as 'isInTree', but for a dependency term.
isInTreeTerm :: Tree -> DepTerm -> Bool
isInTreeTerm t (Or ds)    =  any (isInTreeTerm t) ds
isInTreeTerm t (And ds)   =  all (isInTreeTerm t) ds
isInTreeTerm t (Plain d)  =  isInTree t d

matchDepAtomVariant :: DepAtom -> Variant -> Bool
matchDepAtomVariant d (Variant m e) = matchDepAtomVersionSlot d (verPV . pv $ m) (slot $ e)

matchDepAtomVersionSlot :: DepAtom -> Version -> Slot -> Bool
matchDepAtomVersionSlot (DepAtom True a b c d e f) v s
    =  not $ matchDepAtomVersionSlot (DepAtom False a b c d e f) v s
matchDepAtomVersionSlot (DepAtom False True b c d e f) v s
    =  matchDepAtomVersionSlot  (DepAtom False False b c d ((liftDepVer stripRev) e) f)
                                (stripRev v) s
matchDepAtomVersionSlot (DepAtom False False DNONE c d NoVer NoSlot) v s
    =  True  -- no modifier, no version, no slot
matchDepAtomVersionSlot (DepAtom False False DNONE c d NoVer (DepSlot e)) v s
    =  e == s  -- slot match
matchDepAtomVersionSlot (DepAtom False False DNONE c d e f) v s
    =  matchDepAtomVersionSlot (DepAtom False False DEQ c d e f) v s
             -- no modifier with version defaults to =
matchDepAtomVersionSlot (DepAtom False False m c d (DepVer w False) e) v s
    =  modm m v w && matchDepAtomVersionSlot (DepAtom False False DNONE c d NoVer e) v s -- second is slot match
  where  modm DLT   =  (<)
         modm DLEQ  =  (<=)
         modm DEQ   =  (==)
         modm DGEQ  =  (>=)
         modm DGT   =  (>)
matchDepAtomVersionSlot (DepAtom False False m c d (DepVer w True) e) v s
    | m `elem` [DLT,DGT]
        =  matchDepAtomVersionSlot (DepAtom False False m c d (DepVer w False) e)   v s
    | m `elem` [DLEQ,DGEQ]
        =  matchDepAtomVersionSlot (DepAtom False False m c d (DepVer w False) e)   v s  ||
           matchDepAtomVersionSlot (DepAtom False False DEQ c d (DepVer w True) e)  v s
    | otherwise
        =  w `versionPrefixOf` v && matchDepAtomVersionSlot (DepAtom False False DNONE c d NoVer e) v s -- second is slot match

