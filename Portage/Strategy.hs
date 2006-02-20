{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Strategies for dependency graph generation.
-}

module Portage.Strategy
  where

import Data.List (sortBy)

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
              |  Other String
  deriving (Eq,Show)

makeStrategy :: (Variant -> Variant -> Ordering) -> Strategy
makeStrategy order =  
    Strategy
      {
         sselect     =  select order,
         sstop       =  const True,
         sbacktrack  =  standardBacktrack
      }

select :: (Variant -> Variant -> Ordering) -> DepAtom -> [Variant] -> Selection
select order da vs =
  case sortBy order . filterMaskedVariants $ vs of
    []     ->  Reject (AllMasked da vs)
    v      ->  Accept v

updateOrder :: Variant -> Variant -> Ordering
updateOrder v1 v2 = compare (verPV (pv (meta v2))) (verPV (pv (meta v1)))

defaultOrder :: Variant -> Variant -> Ordering
defaultOrder (Variant { meta = m1 }) (Variant { meta = m2 }) =
    compare  (isAvailable (location m2), verPV (pv m2))
             (isAvailable (location m1), verPV (pv m1))

selectInstalled :: DepAtom -> [Variant] -> Selection
selectInstalled da vs =
  case sortBy updateOrder . filter (isAvailable . location . meta) . filterMaskedVariants $ vs of
    []     ->  Reject (NoneInstalled da vs)
    v      ->  Accept v

standardBacktrack :: Failure -> Bool
standardBacktrack (AllMasked _ _)      =  False
standardBacktrack (NoneInstalled _ _)  =  False
standardBacktrack (SlotConflict _ _)   =  True
standardBacktrack (Other _)            =  True

updateStrategy, defaultStrategy :: Strategy
updateStrategy   =  makeStrategy updateOrder
defaultStrategy  =  makeStrategy defaultOrder

