{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Support for changed USE flags during package upgrades.
-}

module Portage.NewUse where

import qualified Data.Set as S

import Portage.Use
import Portage.Config
import Portage.AnsiColor

type ExtUseFlag = (UseFlag,Maybe Bool)

-- | Computes a difference of USE flag differences.
diffExtUse :: [UseFlag] -> [UseFlag] -> [ExtUseFlag]
diffExtUse xs ys =  let  all  =  S.fromList (ys ++ map turn ys)
                         sy   =  S.fromList ys
                    in   map  (\x ->  (x,  if S.member x all
                                             then Just (not (S.member x sy))  -- in old set
                                             else Nothing))                   -- not in old set
                            xs
  where  turn ('-':xs)  =  xs
         turn xs        =  '-':xs


-- | Shows a use flag with additional information, in color.
showExtUseFlag :: Config -> ExtUseFlag -> String
showExtUseFlag cfg (x,Nothing)    =  inColor cfg Yellow True Default x ++ "%"
showExtUseFlag cfg (x,Just True)  =  inColor cfg Green True Default x ++ "*"
showExtUseFlag cfg (x@('-':_),_)  =  inColor cfg Blue True Default x
showExtUseFlag cfg (x,_)          =  inColor cfg Red True Default x
