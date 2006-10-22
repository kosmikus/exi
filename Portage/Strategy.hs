{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Strategies for dependency graph generation.
-}

module Portage.Strategy
  where

import Data.List (sortBy)
import Data.Monoid
import Data.Graph.Inductive

import Portage.Dependency
import Portage.Package
import Portage.Ebuild
import Portage.Use
import Portage.NewUse
import Portage.Config

data Selection  =  Accept   [Variant] Strategy
                |  Reject   Failure

data Strategy =  Strategy
                   {
                      -- | Select the best variant for a given package.
                      sselect     ::  [UseFlag] -> DepAtom -> [Variant] -> Selection,
                      -- | Decide whether to stop on an already available variant.
                      sstop       ::  Variant -> Bool,
                      -- | Decide whether to backtrack on a certain type of failure.
                      sbacktrack  ::  Failure -> Bool
                   }

-- | Stores a dependency.
data SavedDep  =  SavedDep  DepType
                            Variant                      -- origin (target if reversed?) of the dependency
                            Bool                         -- reversed direction / blockers?
                            DepAtom
               |  Meta                                   -- meta-logic
  deriving (Eq,Show)

showSavedDep :: Config -> SavedDep -> String
showSavedDep c (SavedDep dt v b a) = show dt ++ " " ++ showVariant' c v ++ " " ++ show b ++ " " ++ show a

data DepType   =  Depend
               |  RDepend
               |  PDepend
  deriving (Eq,Show)

data Failure  =  AllMasked DepAtom [Variant]
              |  NoneInstalled DepAtom [Variant]
              |  SlotConflict Variant Variant
              |  Block Blocker Variant
              |  Cycle [(Variant,Maybe SavedDep)]
              |  Other String
  deriving (Eq,Show)

data Blocker =  Blocker
                   {
                      bvariant  ::  Variant,         -- who's blocking?
                      bdepatom  ::  DepAtom,         -- what's blocked?
                      bruntime  ::  Maybe Bool       -- Nothing: it's a saved dependency rather than a blocker
                                                     -- Just: it's a blocker, boolean indicated RDEPEND?
                   }
  deriving (Eq,Show)

strategy' :: ([UseFlag] -> [Variant] -> [Variant]) -> Strategy -> (Variant -> Bool) -> Strategy
strategy' filter ns stop =  
    Strategy
      {
         sselect     =  select filter ns,
         sstop       =  stop,
         sbacktrack  =  standardBacktrack
      }

select :: ([UseFlag] -> [Variant] -> [Variant]) -> Strategy -> [UseFlag] -> DepAtom -> [Variant] -> Selection
select filter ns use da vs =
  case filter use vs of
    []     ->  Reject (AllMasked da vs)
    v      ->  Accept v ns

type VariantOrdering = [UseFlag] -> Variant -> Variant -> Ordering

updateOrder :: VariantOrdering
updateOrder _ v1 v2 =
    compare  (verPV (pv (meta v2)))
             (verPV (pv (meta v1)))

availableOrder :: VariantOrdering
availableOrder _ v1 v2  =
    compare  (isAvailable (location (meta v2)))
             (isAvailable (location (meta v1)))

unmaskOrder :: VariantOrdering
unmaskOrder _ (Variant { meta = m1 }) (Variant { meta = m2 }) =
    compare  (foldr max 0 . map maskRank . masked $ m1)
             (foldr max 0 . map maskRank . masked $ m2)
  where
      maskRank (KeywordMasked _)  =  1
      maskRank (HardMasked _ _)   =  2
      maskRank _                  =  3

newuseOrder :: VariantOrdering
newuseOrder use v1 v2 =
    compare (useRank v1) (useRank v2)
  where
    useRank v
      | isAvailable (location (meta v))  =   1
      | otherwise                        =
           case getLinked v of
             Nothing                     ->  0
             Just v'
               |  newUse (mergeUse use (locuse (meta v)))  (iuse (ebuild v))
                         (mergeUse use (locuse (meta v'))) (iuse (ebuild v'))
                                         ->  0
               |  otherwise              ->  1

-- What does newuse mean? If multiple variants *of the same version*
-- are available, then (and only then) the one which is not available
-- wins if it has new use flags.

(>>>) :: VariantOrdering -> VariantOrdering -> VariantOrdering
(p >>> q) x y z = p x y z `mappend` q x y z

standardBacktrack :: Failure -> Bool
standardBacktrack (AllMasked _ _)      =  False
standardBacktrack (NoneInstalled _ _)  =  False
standardBacktrack (Block _ _)          =  False
standardBacktrack (Cycle _)            =  False
standardBacktrack (SlotConflict _ _)   =  False  -- testing
standardBacktrack (Other _)            =  False

deepStop :: Variant -> Bool
deepStop v =
    case location . meta $ v of
      Provided _  ->  True
      _           ->  False

makeStrategy :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Strategy
makeStrategy update unmask deep newuse empty nodeps =
    let  def  =  updateOrder  -- sic!
         new  =  if newuse  then (>>> newuseOrder) else id -- note that newuse implies update!
         upd  =  if update  then id else (availableOrder >>>)
         unm  =  if unmask  then (unmaskOrder >>>) else id
         fl'  =  (if empty then filterAvailableVariants else id) .
                 if unmask  then filterOtherArchVariants
                            else filterMaskedVariants
         srt  =  \use -> sortBy ((unm . upd . new) def use)
         flt  =  \use -> srt use . fl'
         fl0  =  if update then flt else \use -> srt use . filterAvailableVariants . fl'
         -- the line above is to replace command-line targets in non-update mode
         stp  =  if nodeps
                 then const True
                 else if deep
                 then deepStop
                 else (isAvailable . location . meta)
         ns   =  s { sselect = select flt ns }
         s    =  strategy' fl0 ns stp
    in   s

defaultStrategy :: Strategy
defaultStrategy = makeStrategy False False False False False False
