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
                       slotPS   ::  String
                    }
  deriving (Show,Eq,Ord)
data PVS       =  PVS
                    {
                       catPVS   ::  Category,
                       pkgPVS   ::  Package,
                       verPVS   ::  Version,
                       slotPVS  ::  String
                    }
  deriving (Show,Eq,Ord)

showPV     ::  PV -> String
showP      ::  P -> String
showPS     ::  PS -> String
showPVS    ::  PVS -> String
getPV      ::  String -> PV
getP       ::  String -> P

showPV        (PV cat pkg ver)  =  cat ++ "/" ++ pkg ++ "-" ++ showVersion ver
showEbuildPV  (PV cat pkg ver)  =  cat ./. pkg ./. pkg ++ "-" ++ showVersion ver ++
                                   ".ebuild"
showP         (P cat pkg)       =  cat ++ "/" ++ pkg
showPS        (PS cat pkg slot) =
    cat ++ "/" ++ pkg ++ showSlot slot
showPVS       (PVS cat pkg ver slot) =
    cat ++ "/" ++ pkg ++ "-" ++ showVersion ver ++ showSlot slot

showSlot   ::  String -> String
showSlot ['0'] = ""
showSlot slot = "{" ++ slot ++ "}"

getPV xs      =  case parsePV xs of
                   Left   e  ->
                     error $ "getPV: cat/pkg-ver parse error '" ++ xs ++ "'\n" ++ show e
                   Right  x  ->  x

getP xs       =  case parseP xs of
                   Left   e  ->
                     error $ "getCatPkg: cat/pkg parse error '" ++ xs ++ "'\n" ++ show e
                   Right  x  ->  x

parsePV       =  parse readPV "<cat/pkg-ver>"

readPV        =  do  cat         <-  readCat
                     char '/'
                     (pkg,mver)  <-  readPkgAndVer
                     case mver of
                       Nothing   ->  error "readPV: version expected"
                       Just ver  ->  return (PV cat pkg ver)

parseP        =  parse readP "<cat/pkg>"

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
                                                 liftM (\v -> ("",Just v)) readVersion 
                                                   <|> liftM (\(p,v) -> ('-':p,v)) readPkgAndVer
                                            )
                       return (pre ++ p,v)
