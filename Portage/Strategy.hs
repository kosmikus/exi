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
  deriving (Eq,Show)

data Strategy =  Strategy
                   {
                      -- | Select the best variant for a given package.
                      sselect  ::  Category -> Package -> [Variant] -> Selection,
                      -- | Decide whether to stop on an already available variant.
                      sstop    ::  Variant -> Bool
                   }

updateStrategy :: Strategy
updateStrategy =  Strategy
                    {
                       sselect  =  select,
                       sstop    =  const True
                    }
  where
    select :: Category -> Package -> [Variant] -> Selection
    select cat pkg vs =
      case sortBy updateOrder . filterMaskedVariants $ vs of
        (v:_)  ->  Accept v
        []     ->  Reject (AllMasked cat pkg vs)

updateOrder :: Variant -> Variant -> Ordering
updateOrder v1 v2 = compare (version (pv (meta v1))) (version (pv (meta v2)))


defaultStrategy :: Strategy
defaultStrategy =  Strategy
                     {
                        sselect  =  select,
                        sstop    =  const True
                     }
  where
    select :: Category -> Package -> [Variant] -> Selection
    select cat pkg vs =
      case sortBy defaultOrder . filterMaskedVariants $ vs of
        (v:_)  ->  Accept v
        []     ->  Reject (AllMasked cat pkg vs)

defaultOrder :: Variant -> Variant -> Ordering
defaultOrder (Variant { meta = m1 }) (Variant { meta = m2 }) =
    compare  (isAvailable (location m1), version (pv m1))
             (isAvailable (location m2), version (pv m2))

