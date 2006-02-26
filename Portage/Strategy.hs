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

import Portage.Dependency
import Portage.Package
import Portage.Ebuild

data Selection  =  Accept   [Variant]
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

data Failure  =  AllMasked DepAtom [Variant]
              |  NoneInstalled DepAtom [Variant]
              |  SlotConflict Variant Variant
              |  Block Blocker Variant
              |  Other String
  deriving (Eq,Show)

data Blocker =  Blocker
                   {
                      bvariant  ::  Variant,   -- who's blocking?
                      bdepatom  ::  DepAtom,   -- what's blocked?
                      bruntime  ::  Bool       -- RDEPEND?
                   }
  deriving (Eq,Show)

strategy' :: ([Variant] -> [Variant]) -> Strategy
strategy' filter =  
    Strategy
      {
         sselect     =  select filter,
         sstop       =  const True,
         sbacktrack  =  standardBacktrack
      }

select :: ([Variant] -> [Variant]) -> DepAtom -> [Variant] -> Selection
select filter da vs =
  case filter vs of
    []     ->  Reject (AllMasked da vs)
    v      ->  Accept v

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

selectInstalled :: DepAtom -> [Variant] -> Selection
selectInstalled da vs =
  case sortBy updateOrder . filter (isAvailable . location . meta) . filterMaskedVariants $ vs of
    []     ->  Reject (NoneInstalled da vs)
    v      ->  Accept v

standardBacktrack :: Failure -> Bool
standardBacktrack (AllMasked _ _)      =  False
standardBacktrack (NoneInstalled _ _)  =  False
standardBacktrack (Block _ _)          =  False
standardBacktrack (SlotConflict _ _)   =  True
standardBacktrack (Other _)            =  True

makeStrategy :: Bool -> Bool -> Strategy
makeStrategy update unmask =
    let  upd  =  if update  then updateOrder else defaultOrder
         unm  =  if unmask  then unmaskOrder else id
         flt  =  if unmask  then filterOtherArchVariants
                            else filterMaskedVariants
    in  strategy' (sortBy (unm upd) . flt)

defaultStrategy :: Strategy
defaultStrategy = makeStrategy False False
