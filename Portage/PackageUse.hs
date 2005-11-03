{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Support for package-specific USE flag settings.
-}

module Portage.PackageUse where

import System.IO.Unsafe

import Portage.Use
import Portage.Utilities
import Portage.Constants
import Portage.Tree
import Portage.Match
import Portage.Dependency
import Portage.Ebuild

data UseForPackage  =  UseForPackage
                         {
                            uuse      ::  [UseFlag],
                            udepatom  ::  DepAtom
                         }
  deriving (Eq,Show)

-- TODO: unify with "Portage.PackageKeywords"
mapUseForPackage :: ([UseFlag] -> [UseFlag]) -> UseForPackage -> UseForPackage
mapUseForPackage f u = u { uuse = f (uuse u) }

-- | Performs a USE flag modification.

-- | Parse a @package.use@ file.
parseUse :: String -> [UseForPackage]
parseUse = map parseUseLine . lines . stripComments

parseUseLine :: String -> UseForPackage
parseUseLine x =
    case words x of
      []      ->  error $ "parseUseLine: internal error, empty line in package.use file"
      (d:us)  ->  UseForPackage us (getDepAtom d)

readUseFile :: FilePath -> IO [UseForPackage]
readUseFile f = fmap parseUse (strictReadFile f)

userUseFlags  ::  IO [UseForPackage]
userUseFlags  =   unsafeInterleaveIO $
                  readUseFile localUseFlagsFile

performUseFlags :: UseForPackage -> Tree -> Tree
performUseFlags (UseForPackage us d) =
    modifyTreeForDepAtom d (\v -> v { meta =  case meta v of
                                                me -> me { locuse = us } })

