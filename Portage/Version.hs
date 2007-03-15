{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Version parser, according to Portage spec.
-}

module Portage.Version
  (
  Version(),
  Suffix(..),
  Version'(..),
  projectVersion,
  makeVersion,
  showVersion,
  showVersion',
  showSuffix,
  readVersion,
  parseVersion,
  getVersion,
  stripRev,
  getRev,
  showRevPR,
  versionPrefixOf
  ) where

import Control.Monad
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec

data Version   =  Version   [Int]         -- [1,42,3] ~= 1.42.3
                            (Maybe Char)  -- optional letter
                            [Suffix]
                            Int           -- revision, 0 means none
                            String        -- string representation, for leading zeros

data Version'  =  Version'  [Int]
                            (Maybe Char)
                            [Suffix]
                            Int
  deriving (Eq,Ord)

projectVersion :: Version -> Version'
projectVersion (Version ver c suf rev _) = Version' ver c suf rev

makeVersion :: Version' -> Version
makeVersion v@(Version' ver c suf rev) = Version ver c suf rev (showVersion' v)

data Suffix   =  Alpha Int | Beta Int | Pre Int | RC Int | P_ Int
  deriving (Eq,Ord)

instance Show Version where
  show = showVersion

instance Show Suffix where
  show = showSuffix

instance Eq Version where
  v1 == v2 = projectVersion v1 == projectVersion v2

instance Ord Version where
  compare v1 v2 = compare (projectVersion v1) (projectVersion v2)

showVersion :: Version -> String
showVersion (Version _ _ _ _ rep) = rep

showVersion' :: Version' -> String
showVersion' (Version' ver c suf rev) 
        = showver ++ showc ++ concatMap showSuffix suf ++ showRev rev
  where showver  =  concat . intersperse "." . map show $ ver
        showc    =  maybe "" (:[]) c

showSuffix :: Suffix -> String
showSuffix (Alpha n)  =  "_alpha" ++ showPos n
showSuffix (Beta n)   =  "_beta"  ++ showPos n
showSuffix (Pre n)    =  "_pre"   ++ showPos n
showSuffix (RC n)     =  "_rc"    ++ showPos n
showSuffix (P_ n)     =  "_p"     ++ showPos n

showPos :: Int -> String
showPos 0 = ""
showPos n = show n

showRev :: Int -> String
showRev 0 = ""
showRev n = "-r" ++ show n

showRevPR :: Int -> String
showRevPR n = "r" ++ show n

-- | Function to call if you want to parse a version number.
getVersion :: String -> Version
getVersion ver = case parseVersion ver of
                   Left   _  -> 
                     error $ "getVersion: version parse error '" ++ ver ++ "'"
                   Right  x  ->  x

parseVersion :: String -> Either ParseError Version
parseVersion = parse (readVersion >>= \x -> eof >> return x) "<version number>"

readVersion :: CharParser st Version
readVersion =  do  (ver,  verr)  <-  readVer
                   (c,    cr  )  <-  readC
                   (suf,  sufr)  <-  readSufs
                   (rev,  revr)  <-  readRev
                   let  rep = verr ++ cr ++ sufr ++ revr
                   return (Version ver c suf rev rep)

readVer      ::  CharParser st ([Int],          String)
readNum      ::  CharParser st (Int,            String)
readC        ::  CharParser st (Maybe Char,     String)
readSuf      ::  CharParser st (Suffix,         String)
readSufType  ::  CharParser st (Int -> Suffix,  String)
readSufs     ::  CharParser st ([Suffix],       String)
readRev      ::  CharParser st (Int,            String)

readVer      =  liftM ((\(x,y) -> (x, concat . intersperse "." $ y)) . unzip) (sepBy1 readNum (char '.'))
readNum      =  do  ds <- many1 digit
                    case read ds of
                      n -> return (n,ds)
readC        =  option (Nothing,  "")  (liftM (\x -> (Just x, [x])) letter)
readSuf      =  do  char '_'
                    (f,sr)  <-  readSufType
                    (n,nr)  <-  option (0, "") readNum
                    return (f n,"_" ++ sr ++ nr)

readSufType  =  choice [  
                          liftM (\x -> (Alpha,  x)) (try $ string "alpha"),
                          liftM (\x -> (Beta,   x)) (try $ string "beta" ),
                          liftM (\x -> (Pre,    x)) (try $ string "pre"  ),
                          liftM (\x -> (RC,     x)) (try $ string "rc"   ),
                          liftM (\x -> (P_,     x)) (try $ string "p"    )
                       ]

readSufs     =  fmap ( ( \ (x,y) -> (x, concat y) ) . unzip ) (many readSuf)

readRev      =  option (0,        "")  (  do  rr      <- string "-r"
                                              (n,nr)  <-  readNum
                                              return (n,rr ++ nr)
                                       )

-- | Strip a revision number from a version.
stripRev :: Version -> Version
stripRev (Version a b c r rep) = Version a b c 0 (fst . break (=='-') $ rep)

-- | Get a revision number from a version.
getRev :: Version -> Int
getRev (Version a b c r rep) = r

-- | Check if one version is a prefix of the other (for comparisons with
--   starred dependencies). Apparently portage treats this as a string
--   comparison.
versionPrefixOf :: Version -> Version -> Bool
versionPrefixOf (Version _ _ _ _ r1) (Version _ _ _ _ r2) = isPrefixOf r1 r2

-- This is the original definition we had:
{-
versionPrefixOf :: Version -> Version -> Bool
versionPrefixOf v@(Version ver1 c1 suf1 rev1 _) w@(Version ver2 c2 suf2 rev2 _)
  | rev1 > 0        =  v == w
  | suf1 /= Normal  =  ver1 == ver2 && c1 == c2 && suf1 == suf2
  | isJust c1       =  ver1 == ver2 && c1 == c2
  | otherwise       =  ver1 == take (length ver1) ver2
-}
