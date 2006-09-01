{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Basic portage configuration (via configuration files).
-}

module Portage.Config.Type
  where

import Data.Map
import Portage.Keyword
import Portage.Use

type EnvMap = Map String String  -- ^ untyped environment map

data Config = Config  {
                         arch              ::  Keyword,
                         acceptedKeywords  ::  [Keyword],
                         use               ::  [UseFlag],
                         useOrder          ::  String,
                         portDir           ::  FilePath,
                         tmpDir            ::  FilePath,
  --                     distDir           ::  FilePath,
  --                     pkgDir            ::  FilePath,
  --                     logDir            ::  FilePath,
                         cacheFormat       ::  CacheFormat,
                         overlays          ::  [FilePath],
                         configProtecteds  ::  [FilePath],
                         features          ::  [String],
                         useExpand         ::  [(String,String)],
                         debug             ::  Bool,
                         color             ::  Bool
                      }
  deriving (Eq,Show)

data CacheFormat = FlatHash | FlatList
  deriving (Eq,Show)


