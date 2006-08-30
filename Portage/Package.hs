{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Parser for categories and packages.
-}

module Portage.Package 
  where

import Control.Monad
import Text.ParserCombinators.Parsec

import Portage.Version
import Portage.Utilities

type Category  =  String
type Package   =  String
type Slot      =  String

data P         =  P
                    {
                       catP     ::  Category,
                       pkgP     ::  Package
                    }
  deriving (Show,Eq,Ord)
data PV        =  PV 
                    {
                       catPV    ::  Category,
                       pkgPV    ::  Package,
                       verPV    ::  Version
                    }
  deriving (Show,Eq,Ord)
data PS        =  PS
                    {
                       catPS    ::  Category,
                       pkgPS    ::  Package,
                       slotPS   ::  Slot
                    }
  deriving (Show,Eq,Ord)
data PVS       =  PVS
                    {
                       catPVS   ::  Category,
                       pkgPVS   ::  Package,
                       verPVS   ::  Version,
                       slotPVS  ::  Slot
                    }
  deriving (Show,Eq,Ord)

addSlot    ::  PV -> Slot -> PVS
addSlot (PV c p v) s = PVS c p v s

extractPS  ::  PVS -> PS
extractPS (PVS c p v s) = PS c p s

extractP   ::  PV -> P
extractP (PV c p v) = P c p

showPV     ::  PV -> String
showP      ::  P -> String
showPS     ::  PS -> String
showPVS    ::  PVS -> String
getPV      ::  String -> PV
getP       ::  String -> P

showPV'        (PV cat pkg ver)     =  pkg ++ "-" ++ showVersion ver
showPV         pv@(PV cat pkg ver)  =  cat ./. showPV' pv
showEbuildPV'  pv                   =  showPV' pv ++ ".ebuild"
showEbuildPV   pv@(PV cat pkg ver)  =  cat ./. pkg ./. showEbuildPV' pv
showP          (P cat pkg)          =  cat ++ "/" ++ pkg
showPS         (PS cat pkg slot)    =
    cat ++ "/" ++ pkg ++ showSlot slot
showPVS        (PVS cat pkg ver slot) =
    cat ++ "/" ++ pkg ++ "-" ++ showVersion ver ++ showSlot slot

showSlot   ::  Slot -> String
showSlot ['0'] = ""
showSlot slot = ":" ++ slot

getPV xs      =  case parsePV xs of
                   Left   e  ->
                     error $ "getPV: cat/pkg-ver parse error '" ++ xs ++ "'\n" ++ show e
                   Right  x  ->  x

getP xs       =  case parseP xs of
                   Left   e  ->
                     error $ "getCatPkg: cat/pkg parse error '" ++ xs ++ "'\n" ++ show e
                   Right  x  ->  x

parsePV       =  parse (readPV >>= \x -> eof >> return x) "<cat/pkg-ver>"

readPV        =  do  cat         <-  readCat
                     char '/'
                     (pkg,mver)  <-  readPkgAndVer
                     case mver of
                       Nothing   ->  error "readPV: version expected"
                       Just ver  ->  return (PV cat pkg ver)

parseP        =  parse (readP >>= \x -> eof >> return x) "<cat/pkg>"

readP         =  do  cat         <-  readCat
                     char '/'
                     (pkg,mver)  <-  readPkgAndVer
                     case mver of
                       Nothing   ->  return (P cat pkg)
                       Just _    ->  error "readCatPkg: unexpected version"

readCat        ::  CharParser st Category
readPkgAndVer  ::  CharParser st (Package,Maybe Version)

readCat        =   many1 (letter <|> digit <|> oneOf "_-")
readPkgAndVer  =   do  pre    <-  many1 (letter <|> digit <|> oneOf "_+")
                       (p,v)  <-  option ("",Nothing)
                                            (do  char '-'
                                                 liftM (\v -> ("",Just v)) readVerOrFail
                                                   <|> liftM (\(p,v) -> ('-':p,v)) readPkgAndVer
                                            )
                       return (pre ++ p,v)

readVerOrFail  ::  CharParser st Version
readVerOrFail  =   try $
                   do  ver    <-  many1 (letter <|> digit <|> oneOf "_+.-")
                       case parseVersion ver of
                           Left   _  -> 
                             fail $ "version parse error"
                           Right  x  -> return x

