{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Strategies for dependency graph generation.
-}

module Portage.Strategy
  where

import Data.List (sortBy)

import Portage.Package
import Portage.Ebuild

data Selection  =  Accept   Variant
                |  Reject   Failure

data Failure  =  AllMasked P [Variant]
              |  NoneInstalled P [Variant]
  deriving (Eq,Show)

data Strategy =  Strategy
                   {
                      -- | Select the best variant for a given package.
                      sselect  ::  P -> [Variant] -> Selection,
                      -- | Decide whether to stop on an already available variant.
                      sstop    ::  Variant -> Bool
                   }

makeStrategy :: (Variant -> Variant -> Ordering) -> Strategy
makeStrategy order =  
    Strategy
      {
         sselect  =  select order,
         sstop    =  const True
      }

select :: (Variant -> Variant -> Ordering) -> P -> [Variant] -> Selection
select order p vs =
  case sortBy order . filterMaskedVariants $ vs of
    (v:_)  ->  Accept v
    []     ->  Reject (AllMasked p vs)

updateOrder :: Variant -> Variant -> Ordering
updateOrder v1 v2 = compare (verPV (pv (meta v2))) (verPV (pv (meta v1)))

defaultOrder :: Variant -> Variant -> Ordering
defaultOrder (Variant { meta = m1 }) (Variant { meta = m2 }) =
    compare  (isAvailable (location m2), verPV (pv m2))
             (isAvailable (location m1), verPV (pv m1))

selectInstalled :: P -> [Variant] -> Selection
selectInstalled p vs =
  case sortBy updateOrder . filter (isAvailable . location . meta) . filterMaskedVariants $ vs of
    (v:_)  ->  Accept v
    []     ->  Reject (NoneInstalled p vs)

updateStrategy, defaultStrategy :: Strategy
updateStrategy   =  makeStrategy updateOrder
defaultStrategy  =  makeStrategy defaultOrder

