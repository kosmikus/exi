{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Data type for ebuilds.
-}

module Portage.Ebuild.Type
  where

import Data.Monoid

import Portage.Dependency
import Portage.Keyword
import Portage.Use
import Portage.Eclass hiding (location)
import Portage.Package

-- The ebuild cache format (as created by calling @ebuild depend@) is as follows:
-- DEPEND
-- RDEPEND
-- SLOT
-- SRC_URI
-- RESTRICT
-- HOMEPAGE
-- LICENSE
-- DESCRIPTION
-- KEYWORDS
-- INHERITED
-- IUSE
-- CDEPEND
-- PDEPEND
-- PROVIDE
-- In addition, we store the tree in which the ebuild is located.

data Ebuild = Ebuild  {
                         depend       ::  DepString,
                         rdepend      ::  DepString,
                         slot         ::  String,
                         src_uri      ::  String,
                         restrict     ::  [String],
                         homepage     ::  String,
                         license      ::  String,
                         description  ::  String,
                         keywords     ::  [Keyword],
                         inherited    ::  [Eclass],
                         iuse         ::  [UseFlag],
                         cdepend      ::  DepString,
                         pdepend      ::  DepString,
                         provide      ::  DepString,
                         eapi         ::  String
                      }
  deriving (Show,Eq)

-- | The 'EbuildMeta' type contains additional information about an ebuild
--   that is not directly stored within the ebuild file or cache entry.
data EbuildMeta =  EbuildMeta
                      {
                         pv           ::  PV,
                         location     ::  TreeLocation,
                         masked       ::  [Mask],           -- ^ empty means the ebuild is visible
                         locuse       ::  [UseFlag],        -- ^ local USE flags
                         lockey       ::  [Keyword],        -- ^ local ACCEPT_KEYWORDS
                         origin       ::  EbuildOrigin      -- ^ where did we get this data from?
                      }
  deriving (Show)

data EbuildOrigin = FromCache | CacheRegen | EclassDummy | FromInstalledDB | IsProvided
  deriving (Show,Eq)

data TreeLocation  =  Installed
                   |  Provided       FilePath               -- ^ in which file?
                   |  PortageTree    FilePath Link
  deriving (Show)

instance Eq EbuildMeta where
  EbuildMeta { pv = pv1, location = loc1 } == EbuildMeta { pv = pv2, location = loc2 } =
     pv1 == pv2 && loc1 == loc2

instance Ord EbuildMeta where
  compare (EbuildMeta { pv = pv1, location = loc1 }) (EbuildMeta { pv = pv2, location = loc2 }) =
     compare pv1 pv2 `mappend` compare loc1 loc2

instance Eq TreeLocation where
  Installed        ==  Installed        =  True
  Provided _       ==  Provided _       =  True
  PortageTree x _  ==  PortageTree y _  =  x == y 
  _                ==  _                =  False

instance Ord TreeLocation where
  compare Installed Installed                  =  EQ
  compare Installed _                          =  LT
  compare (Provided _) Installed               =  GT
  compare (Provided _) (Provided _)            =  EQ
  compare (Provided _) _                       =  LT
  compare (PortageTree x _) (PortageTree y _)  =  compare x y
  compare (PortageTree _ _) _                  =  GT

-- | The 'Link' is used to link an uninstalled variant to an installed variant
--   of the same slot and to an installed variant of another slot. 
--   We can thus say whether selecting this variant would be
--   an up- or a downgrade, and we can compare use flags.
data Link          =  Linked     (Maybe Variant) (Maybe Variant)   -- ^ installed variant, and installed variant of other slot
  deriving (Show,Eq)

-- The list added to KeywordMasked is supposed not to contain keywords
-- that do not affect the current arch.
data Mask          =  KeywordMasked  [Keyword]              -- ^ reasoning
                   |  HardMasked     FilePath [String]      -- ^ filename and reason
                   |  ProfileMasked  FilePath               -- ^ in which file?
                   |  NotInProfile                          -- ^ without further reason
                   |  PreviousChoice Variant
                   |  Shadowed       TreeLocation           -- ^ by which tree?
  deriving (Show,Eq)

-- | A variant is everything that makes a specific instance of an ebuild.
--   It's supposed to be more than this datatype currently encodes.
data Variant =  Variant
                  {
                     meta    ::  EbuildMeta,
                     ebuild  ::  Ebuild
                  }
  deriving (Show)

instance Eq Variant where
  Variant { meta = m1 } == Variant { meta = m2 } = m1 == m2

instance Ord Variant where
  compare (Variant { meta = m1 }) (Variant { meta = m2 }) = compare m1 m2  

