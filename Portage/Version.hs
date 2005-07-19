{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Version parser, according to Portage spec.
-}

module Portage.Version
  (
  Version(..),
  Suffix(..),
  showVersion,
  showSuffix,
  readVersion,
  parseVersion,
  getVersion,
  stripRev,
  versionPrefixOf
  ) where

import Control.Monad
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec

data Version  =  Version  [Int]         -- [1,42,3] ~= 1.42.3
                          (Maybe Char)  -- optional letter
                          Suffix
                          Int           -- revision, 0 means none
  deriving (Eq,Ord)

data Suffix   =  Alpha Int | Beta Int | Pre Int | RC Int | Normal | P Int
  deriving (Eq,Ord)

instance Show Version where
  show = showVersion

instance Show Suffix where
  show = showSuffix

showVersion :: Version -> String
showVersion (Version ver c suf rev) 
        = showver ++ showc ++ showSuffix suf ++ showRev rev
  where showver  =  concat . intersperse "." . map show $ ver
        showc    =  maybe "" (:[]) c

showSuffix :: Suffix -> String
showSuffix (Alpha n)  =  "_alpha" ++ showPos n
showSuffix (Beta n)   =  "_beta"  ++ showPos n
showSuffix (Pre n)    =  "_pre"   ++ showPos n
showSuffix (RC n)     =  "_rc"    ++ showPos n
showSuffix Normal     =  ""
showSuffix (P n)      =  "_p"     ++ showPos n

showPos :: Int -> String
showPos 0 = ""
showPos n = show n

showRev :: Int -> String
showRev 0 = ""
showRev n = "-r" ++ show n

-- | Function to call if you want to parse a version number.
getVersion :: String -> Version
getVersion ver = case parseVersion ver of
                   Left   _  -> 
                     error $ "getVersion: version parse error " ++ ver
                   Right  x  ->  x

parseVersion :: String -> Either ParseError Version
parseVersion = parse readVersion "<version number>"

readVersion :: CharParser st Version
readVersion = do ver  <-  readVer
                 c    <-  readC
                 suf  <-  readSuf
                 rev  <-  readRev
                 return (Version ver c suf rev)

readVer  ::  CharParser st [Int]
readNum  ::  CharParser st Int
readC    ::  CharParser st (Maybe Char)
readSuf  ::  CharParser st Suffix
readSufType :: CharParser st (Int -> Suffix)
readRev  ::  CharParser st Int

readVer  =   sepBy1 readNum (char '.')
readNum  =   liftM read (many1 digit)
readC    =   option Nothing (liftM Just letter)
readSuf  =   option Normal  (do  char '_'
                                 f  <-  readSufType
                                 n  <-  option 0 readNum
                                 return (f n)
                            )
readSufType = choice [liftM (const Alpha) (try $ string "alpha")
                     ,liftM (const Beta ) (try $ string "beta" )
                     ,liftM (const Pre  ) (try $ string "pre"  )
                     ,liftM (const RC   ) (try $ string "rc"   )
                     ,liftM (const P    ) (try $ string "p"    )
                     ]
readRev  =  option 0 (do  string "-r"
                          readNum
                     )

-- | Strip a revision number from a version.
stripRev :: Version -> Version
stripRev (Version a b c r) = Version a b c 0

-- | Check if one version is a prefix of the other (for comparisons with
--   starred dependencies).
versionPrefixOf :: Version -> Version -> Bool
versionPrefixOf v@(Version ver1 c1 suf1 rev1) w@(Version ver2 c2 suf2 rev2)
  | rev1 > 0        =  v == w
  | suf1 /= Normal  =  ver1 == ver2 && c1 == c2 && suf1 == suf2
  | isJust c1       =  ver1 == ver2 && c1 == c2
  | otherwise       =  ver1 == take (length ver1) ver2
