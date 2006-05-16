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

data Selection  =  Accept   [Variant] Strategy
                |  Reject   Failure

data Strategy =  Strategy
                   {
                      -- | Select the best variant for a given package.
                      sselect     ::  DepAtom -> [Variant] -> Selection,
                      -- | Decide whether to stop on an already available variant.
                      sstop       ::  Variant -> Bool,
                      -- | Decide whether to backtrack on a certain type of failure.
                      sbacktrack  ::  Failure -> Bool
                   }

data DepType = Depend        Bool DepAtom
             | RDepend       Bool DepAtom
             | PDepend       Bool DepAtom
             | Meta                        -- ^ meta-logic
  deriving (Eq,Show)

data Failure  =  AllMasked DepAtom [Variant]
              |  NoneInstalled DepAtom [Variant]
              |  SlotConflict Variant Variant
              |  Block Blocker Variant
              |  Cycle [(Variant,Maybe DepType)]
              |  Other String
  deriving (Eq,Show)

data Blocker =  Blocker
                   {
                      bvariant  ::  Variant,   -- who's blocking?
                      bdepatom  ::  DepAtom,   -- what's blocked?
                      bruntime  ::  Bool       -- RDEPEND?
                   }
  deriving (Eq,Show)

strategy' :: ([Variant] -> [Variant]) -> Strategy -> (Variant -> Bool) -> Strategy
strategy' filter ns stop =  
    Strategy
      {
         sselect     =  select filter ns,
         sstop       =  stop,
         sbacktrack  =  standardBacktrack
      }

select :: ([Variant] -> [Variant]) -> Strategy -> DepAtom -> [Variant] -> Selection
select filter ns da vs =
  case filter vs of
    []     ->  Reject (AllMasked da vs)
    v      ->  Accept v ns

updateOrder :: Variant -> Variant -> Ordering
updateOrder v1 v2 = compare (verPV (pv (meta v2))) (verPV (pv (meta v1)))

defaultOrder :: Variant -> Variant -> Ordering
defaultOrder (Variant { meta = m1 }) (Variant { meta = m2 }) =
    compare  (isAvailable (location m2), verPV (pv m2))
             (isAvailable (location m1), verPV (pv m1))

unmaskOrder :: (Variant -> Variant -> Ordering) -> Variant -> Variant -> Ordering
unmaskOrder o = \ v w -> maskOrder v w `mappend` o v w
  where maskOrder (Variant { meta = m1 }) (Variant { meta = m2 }) =
          compare  (foldr max 0 . map maskRank . masked $ m1)
                   (foldr max 0 . map maskRank . masked $ m2)
        maskRank (KeywordMasked _)  =  1
        maskRank (HardMasked _ _)   =  2
        maskRank _                  =  3

standardBacktrack :: Failure -> Bool
standardBacktrack (AllMasked _ _)      =  False
standardBacktrack (NoneInstalled _ _)  =  False
standardBacktrack (Block _ _)          =  False
standardBacktrack (Cycle _)            =  False
standardBacktrack (SlotConflict _ _)   =  False -- testing
standardBacktrack (Other _)            =  True

deepStop :: Variant -> Bool
deepStop v =  case location . meta $ v of
                Provided _  ->  True
                _           ->  False

makeStrategy :: Bool -> Bool -> Bool -> Strategy
makeStrategy update unmask deep =
    let  upd  =  if update  then updateOrder else defaultOrder
         unm  =  if unmask  then unmaskOrder else id
         fl'  =  if unmask  then filterOtherArchVariants
                            else filterMaskedVariants
         flt  =  sortBy (unm upd) . fl'
         fl0  =  if update  then flt else sortBy (unm defaultOrder) . filterAvailableVariants . fl'
         stp  =  if deep    then deepStop else const True
         ns   =  s { sselect = select flt ns }
         s    =  strategy' fl0 ns stp
    in   s

defaultStrategy :: Strategy
defaultStrategy = makeStrategy False False False
