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
data PV        =  PV 
                    {
                       category  ::  Category,
                       package   ::  Package,
                       version   ::  Version
                    }
  deriving (Show,Eq)

showPV  ::  PV -> String
getPV   ::  String -> PV

showPV        (PV cat pkg ver)  =  cat ++ "/" ++ pkg ++ "-" ++ showVersion ver
showEbuildPV  (PV cat pkg ver)  =  cat ./. pkg ./. pkg ++ "-" ++ showVersion ver ++
                                   ".ebuild"

getPV xs  =  case parsePV xs of
               Left   _  ->
                 error $ "getPV: cat/pkg-ver parse error '" ++ xs ++ "'"
               Right  x  ->  x

parsePV   =  parse readPV "<cat/pkg-ver>"

readPV    =  do  cat         <-  readCat
                 char '/'
                 (pkg,mver)  <-  readPkgAndVer
                 case mver of
                   Nothing   ->  error "readPV: version expected"
                   Just ver  ->  return (PV cat pkg ver)

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
