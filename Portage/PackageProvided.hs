{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Profile-provided packages.
-}

module Portage.PackageProvided
  where

import System.IO.Unsafe
import qualified Data.Map as M

import Portage.Utilities
import Portage.Package
import Portage.Tree
import Portage.Profile
import Portage.Constants
import Portage.Ebuild

type Provided = (FilePath,PV)

-- | Parse a @package.provided@ file.
parseProvided :: FilePath -> String -> [Provided]
parseProvided f = zip (repeat f) . map getPV . lines . stripComments

readProvidedFile :: FilePath -> IO [Provided]
readProvidedFile f = fmap (parseProvided f) (strictReadFile f)

profileProvided :: IO [Provided]
profileProvided  =  unsafeInterleaveIO $
                    fmap concat (readProfileFile packageProvided readProvidedFile)


addProvided :: Provided -> Tree -> Tree
addProvided (f,pv@(PV cat pkg ver)) t =
    t { ebuilds = updateWithDefault  (Just . updateWithDefault (\vs -> Just $ providedVar : vs) pkg [])
                                     cat M.empty (ebuilds t) }
  where
    meta        = EbuildMeta
                    pv
                    (Provided f)
                    []
                    []
                    []
                    IsProvided
    ebuild      = Ebuild
                    []
                    []
                    "provided"  -- TODO: not nice
                    ""
                    []
                    ""
                    ""
                    ""
                    []
                    []
                    []
                    []
                    []
                    Nothing
    providedVar = Variant meta ebuild

