{-| Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Handling dependency atoms and dependency strings.
-}

module Portage.Dependency
  where

import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Token

import Portage.Version
import Portage.Package
import Portage.Use

-- Grammar of dependencies:
--
-- depstring ::= atom
--            |  usevar? ( depstring ) [ : ( depstring ) ]
--            |  !usevar? ( depstring )
--            |  || ( { depstring } )
--
-- ORs have to be interpreted as well, therefore *we* will consider
-- an OR of atoms an atom

data DepAtom  =  DepAtom DepNeg DepRev DepMod Category Package DepVer
  deriving (Eq,Ord)

catpkgFromDepAtom :: DepAtom -> (Category, Package)
catpkgFromDepAtom (DepAtom _ _ _ cat pkg _) = (cat,pkg)

data DepVer   =  NoVer
              |  DepVer Version DepAst
  deriving (Show,Eq,Ord)

-- lift a version function to a DepVer

liftDepVer :: (Version -> Version) -> (DepVer -> DepVer)
liftDepVer f NoVer         =  NoVer
liftDepVer f (DepVer v a)  =  DepVer (f v) a

type DepNeg   =  Bool -- !
type DepRev   =  Bool -- ~
type DepAst   =  Bool -- *
data DepMod   =  DNONE | DLT | DLEQ | DEQ | DGEQ | DGT -- < <= = >= >
  deriving (Show,Eq,Ord)

type DepString  =  [DepTerm]
data DepTerm    =  Plain  DepAtom
                |  Or     DepString
                |  Use    Bool -- negated?
                          UseFlag
                          DepString
  deriving (Eq)

-- | Interprets a DepString according to given USE flags (non-negatives)
interpretDepString     ::  [UseFlag] -> DepString -> DepString
interpretDepString fs  =   concatMap (interpretDepTerm fs)

interpretDepTerm fs (Plain a)    =  [Plain a]
interpretDepTerm fs (Or s)       =  [Or (interpretDepString fs s)]
interpretDepTerm fs (Use b f s)
  | (f `elem` fs) /= b           =  interpretDepString fs s
  | otherwise                    =  []


instance Show DepTerm where
  show           =  showDepTerm
  showList xs p  =  showDepString xs ++ p

instance Show DepAtom where
  show           =  showDepAtom

showDepString = concat . intersperse " " . map showDepTerm

showDepTerm (Plain atom)              =  showDepAtom atom
showDepTerm (Or depstring)            =  "|| ( " ++ showDepString depstring ++ " )"
showDepTerm (Use neg flag depstring)  =  (if neg then "!" else "")
                                         ++ flag ++ "? ( " ++ showDepString depstring ++ " )"

showDepAtom (DepAtom neg rev mod cat pkg ver) = 
    (if neg then "!" else "") ++ (if rev then "~" else "") ++
    showMod mod ++ cat ++ "/" ++ pkg ++ if null sver then "" else "-" ++ sver
  where sver = showDepVer ver

showMod DNONE  =  ""
showMod DLT    =  "<"
showMod DLEQ   =  "<="
showMod DEQ    =  "="
showMod DGEQ   =  ">="
showMod DGT    =  ">"

showDepVer NoVer             =  ""
showDepVer (DepVer ver ast)  =  showVersion ver ++ if ast then "*" else ""


-- | Parse a dependency string.
getDepString  ::  [Char] -> DepString

-- | Parse a dependency atom.
getDepAtom    ::  [Char] -> DepAtom


getDepString da  =  case parseDepString da of
                      Left   _ ->
                        error $ "getDepString: depstring parse error " ++ da
                      Right  x -> x

parseDepString   =  parse readDepString "<depstring>"

getDepAtom da    =  case parseDepAtom da of
                      Left   _ ->
                        error $ "getDepAtom: depatom parse error " ++ da
                      Right  x -> x

getMaybeDepAtom da
                 =  case parseMaybeDepAtom da of
                      Left   _ ->
                        error $ "getMaybeDepAtom: depatom parse error '" ++ da ++ "'"
                      Right  x -> x

parseDepAtom       =  parse readDepAtom "<depatom>"
parseMaybeDepAtom  =  parse (option Nothing (liftM Just readDepAtom)) "<depatom>"

readDepAtom  =  do  neg         <-  optchar '!'
                    rev         <-  optchar '~'
                    mod         <-  readDepMod
                    cat         <-  readCat
                    char '/'
                    (pkg,mver)  <-  readPkgAndVer
                    dver        <-  case mver of
                                      Nothing   ->  case (rev,mod) of
                                                      (False,DNONE) -> return NoVer
                                                      _ -> unexpected "absence of version"
                                      Just ver  ->  do  ast <- optchar '*'
                                                        return (DepVer ver ast)
                    return (DepAtom neg rev mod cat pkg dver)

readDepMod   =  option DNONE $
                  choice $ map (\(x,s) -> liftM (const x) (try $ string s))
                            [(DLEQ,"<=")
                            ,(DLT ,"<" )
                            ,(DEQ ,"=" )
                            ,(DGEQ,">=")
                            ,(DGT ,">" )
                            ]

optchar c = option False (liftM (const True) (char c))


-- read a depstring

readDepString :: CharParser st DepString
readDepString = 
    do ds <- many $ choice [readOr,P.try (readUseDep),readAtom]
       return (concat ds)

-- read a depstring and handle initial whitespace

readCompleteDepString =
    do
        white
        d <- readDepString
        eof
        return d


-- read an "or" dependency

readOr =
    do
        chc
        d <- pars readDepString
        -- simplify or's with just one alternative
        case d of
          [s]  ->  return [s]
          _    ->  return [Or d]


-- read a dependency qualified with a use flag

readUseDep =
    do
        neg <- option False (liftM (const True) excl)
        use <- ident
        qmark
        thenf  <-  {- pars -} readDepString  -- removed pars due to non-compliant ebuilds
        elsef  <-  option [] $
                     do
                         col
                         ({- pars -} readDepString)
        if null elsef  then  return [Use neg use thenf]
                       else  return [Use neg use thenf, Use (not neg) use elsef]

-- an atom can also be a parenthesized depstring, which is flattened

readAtom = -- hack due to strange ebuilds
    do
        readOrigAtom <|> pars readDepString

readOrigAtom =
    do
        neg  <-  option "" (liftM (const "!") excl)
        dep  <-  ident
        case parse readDepAtom "" (neg ++ dep) of
          Left error  ->  fail (show error)
          Right x     ->  return [Plain x]

-- Parsec language definition for the dependency language

depstringLang =
    LanguageDef
        { commentStart     =  "",
          commentEnd       =  "",
          commentLine      =  "",
          nestedComments   =  False,
          identStart       =  noneOf " \t\n():?!",
          identLetter      =  noneOf " \t\n():?!",
          opStart          =  oneOf "!?():|",
          opLetter         =  oneOf "|",
          reservedNames    =  [],
          reservedOpNames  =  ["!","?","(",")",":","||"],
          caseSensitive    =  True }

depstring = makeTokenParser depstringLang
          
ident  = identifier depstring
white  = whiteSpace depstring
qmark  = reservedOp depstring "?"
col    = reservedOp depstring ":"
excl   = reservedOp depstring "!"
chc    = reservedOp depstring "||"
pars   = parens depstring

