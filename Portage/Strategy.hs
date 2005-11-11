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

data Failure  =  AllMasked Category Package [Variant]
              |  NoneInstalled Category Package [Variant]
  deriving (Eq,Show)

data Strategy =  Strategy
                   {
                      -- | Select the best variant for a given package.
                      sselect  ::  Category -> Package -> [Variant] -> Selection,
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

select :: (Variant -> Variant -> Ordering) -> Category -> Package -> [Variant] -> Selection
select order cat pkg vs =
  case sortBy order . filterMaskedVariants $ vs of
    (v:_)  ->  Accept v
    []     ->  Reject (AllMasked cat pkg vs)

updateOrder :: Variant -> Variant -> Ordering
updateOrder v1 v2 = compare (version (pv (meta v2))) (version (pv (meta v1)))

defaultOrder :: Variant -> Variant -> Ordering
defaultOrder (Variant { meta = m1 }) (Variant { meta = m2 }) =
    compare  (isAvailable (location m2), version (pv m2))
             (isAvailable (location m1), version (pv m1))

selectInstalled :: Category -> Package -> [Variant] -> Selection
selectInstalled cat pkg vs =
  case sortBy updateOrder . filter (isAvailable . location . meta) . filterMaskedVariants $ vs of
    (v:_)  ->  Accept v
    []     ->  Reject (NoneInstalled cat pkg vs)

updateStrategy, defaultStrategy :: Strategy
updateStrategy   =  makeStrategy updateOrder
defaultStrategy  =  makeStrategy defaultOrder

