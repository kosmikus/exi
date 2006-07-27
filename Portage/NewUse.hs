{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Support for changed USE flags during package upgrades.
-}

module Portage.NewUse where

import Data.Char
import Data.List
import qualified Data.Set as S

import Portage.Use
import Portage.Config
import Portage.AnsiColor
import Portage.Utilities

-- | Type for USE flags with difference information (compared to a previous
--   version). "Nothing" means that the USE flag is new, "Just True" means
--   it has changed, and "Just False" means that it is unchanged.
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

-- | Computes USE flag difference information.
--   Takes four USE lists: use flags for new ebuild,
--                         IUSE for new ebuild,
--                         use flags for old ebuild,
--                         IUSE for old ebuild.
extUse :: [UseFlag] -> [UseFlag] -> [UseFlag] -> [UseFlag] -> [ExtUseFlag]
extUse nu ni ou oi =  diffExtUse (diffUse nu ni) (diffUse ou oi)

-- | Reports if there are USE flag differences between two ebuilds.
newUse :: [UseFlag] -> [UseFlag] -> [UseFlag] -> [UseFlag] -> Bool
newUse nu ni ou oi =  (not . null . filter (\ (_,b) -> b == Just True)) (extUse nu ni ou oi)

-- | Shows a use flag with additional information, in color.
showExtUseFlag :: Config -> ExtUseFlag -> String
showExtUseFlag cfg (x,Nothing)    =  inColor cfg Yellow True Default x ++ "%"
showExtUseFlag cfg (x,Just True)  =  inColor cfg Green True Default x ++ "*"
showExtUseFlag cfg (x@('-':_),_)  =  inColor cfg Blue True Default x
showExtUseFlag cfg (x,_)          =  inColor cfg Red True Default x

-- | Perform the opposite of USE flag expansion for a list of extended USE flags.
unexpandExtUses :: [String] -> [ExtUseFlag] -> [(String,[ExtUseFlag])]
unexpandExtUses expanded' xs =
  let  expanded  =  map (\x -> (x, map toLower x ++ "_")) expanded'
       grouped   =  groupByFst $
                    map  (\ (x,b) -> let  neg  =  if ("-" `isPrefixOf` x) then "-" else ""
                                          px   =  drop (length neg) x
                                     in  case find (\y -> snd y `isPrefixOf` px) expanded of
                                           Nothing  ->  ("USE",(x,b))
                                           Just y   ->  (fst y,((neg ++ drop (length (snd y)) px),b)))
                         xs
  in   grouped

