{-| 
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
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

import Portage.Package
import Portage.Version
import Portage.Package
import Portage.Use

-- Grammar of dependencies (more liberal than portage's, I think):
--
-- > depterm   ::= atom
-- >            |  ( depstring )
-- >            |  || ( depstring )
-- >            |  usevar? depterm [ : depterm ]
-- >            |  !usevar? depterm
--
-- > depstring ::= depterm*

data DepAtom  =  DepAtom DepNeg DepRev DepMod Category Package DepVer DepSlot
  deriving (Eq,Ord)

blocking :: DepAtom -> Bool
blocking (DepAtom b _ _ _ _ _ _) = b

unblock :: DepAtom -> DepAtom
unblock (DepAtom b r m c p v s) = DepAtom False r m c p v s

block :: DepAtom -> DepAtom
block (DepAtom b r m c p v s) = DepAtom True r m c p v s

pFromDepAtom :: DepAtom -> P
pFromDepAtom (DepAtom _ _ _ cat pkg _ _) = P cat pkg

pFromDepTerm :: DepTerm -> P
pFromDepTerm (Plain d) = pFromDepAtom d
-- add additional cases?

data DepVer   =  NoVer
              |  DepVer Version DepAst
  deriving (Show,Eq,Ord)

data DepSlot  =  NoSlot
              |  DepSlot Slot
  deriving (Show,Eq,Ord)

-- | Lifts a function on versions to a function on 'DepVer's.
liftDepVer :: (Version -> Version) -> (DepVer -> DepVer)
liftDepVer f NoVer         =  NoVer
liftDepVer f (DepVer v a)  =  DepVer (f v) a

type DepNeg   =  Bool   -- ^ '!'
type DepRev   =  Bool   -- ^ '~'
type DepAst   =  Bool   -- ^ '*'
data DepMod   =  DNONE  -- ^ no modifier
              |  DLT    -- ^ '<'
              |  DLEQ   -- ^ '<='
              |  DEQ    -- ^ '='
              |  DGEQ   -- ^ '>='
              |  DGT    -- ^ '>'
  deriving (Show,Eq,Ord)

type DepString  =  [DepTerm]
data DepTerm    =  Plain  DepAtom
                |  Or     DepString  -- || ( ... ) grouping
                |  And    DepString  -- simple grouping
                |  Use    Bool       -- is the USE flag negated?
                          UseFlag
                          DepTerm
  deriving (Eq)

-- | Returns all the atoms contained in a dependency string.
depStringAtoms         ::  DepString -> [DepAtom]
depStringAtoms  =  concatMap depTermAtoms
  where  depTermAtoms (Plain da)    =  [da]
         depTermAtoms (Or ds)       =  depStringAtoms ds
         depTermAtoms (And ds)      =  depStringAtoms ds
         depTermAtoms (Use _ _ dt)  =  depTermAtoms dt

-- | Interprets a DepString according to given USE flags (non-negatives).
interpretDepString     ::  [UseFlag] -> DepString -> DepString
interpretDepString fs  =   concatMap (interpretDepTerm fs)

interpretDepTerm       ::  [UseFlag] -> DepTerm -> DepString
interpretDepTerm fs (Plain a)    =  [Plain a]
interpretDepTerm fs (Or s)       =  [Or (interpretDepString fs s)]
interpretDepTerm fs (And s)      =  [And (interpretDepString fs s)]
interpretDepTerm fs (Use b f s)
  | (f `elem` fs) /= b           =  interpretDepTerm fs s
  | otherwise                    =  []


instance Show DepTerm where
  show           =  showDepTerm
  showList xs p  =  showDepString xs ++ p

instance Show DepAtom where
  show           =  showDepAtom

showDepString = concat . intersperse " " . map showDepTerm

showDepTerm (Plain atom)              =  showDepAtom atom
showDepTerm (Or depstring)            =  "|| ( " ++ showDepString depstring ++ " )"
showDepTerm (And depstring)           =  "( " ++ showDepString depstring ++ " )"
showDepTerm (Use neg flag depterm)    =  (if neg then "!" else "")
                                         ++ flag ++ "? " ++ showDepTerm depterm

showDepAtom (DepAtom neg rev mod cat pkg ver slt) = 
    (if neg then "!" else "") ++ (if rev then "~" else "") ++
    showMod mod ++ cat ++ "/" ++ pkg  ++ (if null sver then "" else "-") ++ sver
                                      ++ (if null sslt then "" else ":") ++ sslt
  where  sver  =  showDepVer ver
         sslt  =  showDepSlot slt

showMod DNONE  =  ""
showMod DLT    =  "<"
showMod DLEQ   =  "<="
showMod DEQ    =  "="
showMod DGEQ   =  ">="
showMod DGT    =  ">"

showDepVer NoVer             =  ""
showDepVer (DepVer ver ast)  =  showVersion ver ++ if ast then "*" else ""

showDepSlot NoSlot           =  ""
showDepSlot (DepSlot slt)    =  slt


-- | Parse a dependency string. Takes an 'expand' function.
getDepString'  ::  (Package -> [Category]) -> [Char] -> DepString

-- | Parse a dependency string. No 'expand' function.
getDepString   ::  [Char] -> DepString

-- | Parse a dependency atom. Takes an 'expand' function.
getDepAtom'    ::  (Package -> [Category]) -> [Char] -> DepAtom

-- | Parse a dependency atom. No 'expand' function.
getDepAtom     ::  [Char] -> DepAtom


getDepString' expand da =
  case parseDepString expand da of
    Left   e  ->  error $ "getDepString: depstring parse error " ++ da ++ "\n" ++ show e
    Right  x  ->  x

getDepString = getDepString' (const [])

parseDepString expand = parse (readCompleteDepString expand) "<depstring>"

getDepAtom' expand da =
  case parseDepAtom expand da of
    Left   _  ->  error $ "getDepAtom: depatom parse error " ++ da
    Right  x  ->  x

getDepAtom = getDepAtom' (const [])

getDepAtoms' expand da =
  case parseDepAtoms expand da of
    Left   _  ->  error $ "getDepAtoms: depatom parse error '" ++ da ++ "'"
    Right  x  ->  x

getDepAtoms = getDepAtoms' (const [])

parseDepAtom   expand  =  parse  (readDepAtom expand) "<depatom>"
parseDepAtoms  expand  =  parse  (do  white
                                      many (do  d <- readDepAtom expand
                                                white
                                                return d))
                                 "<depatoms>"

readDepAtom expand
             =  do  neg         <-  optchar '!'
                    rev         <-  optchar '~'
                    mod         <-  readDepMod
                    mcat        <-  option Nothing $ try $ do  cat <- readCat
                                                               char '/'
                                                               return (Just cat)
                    (pkg,mver)  <-  readPkgAndVer
                    cat <-  case mcat of
                              Nothing -> case expand pkg of
                                           [cat]  ->  return cat
                                           []     ->  fail $ "unknown package: " ++ pkg
                                           cats   ->  fail $ "ambiguous name " ++ pkg ++ ", possible matches: " ++ unwords (map (\c -> c ++ "/" ++ pkg) cats)
                              Just cat -> return cat
                    dver        <-  case mver of
                                      Nothing   ->  case (rev,mod) of
                                                      (False,DNONE) -> return NoVer
                                                      _ -> unexpected "absence of version"
                                      Just ver  ->  do  ast <- try $ optchar '*'
                                                        return (DepVer ver ast)
                    dslt        <-  option NoSlot $  do  char ':'
                                                         readSlot >>= (return . DepSlot)
                    return (DepAtom neg rev mod cat pkg dver dslt)

readDepMod   =  option DNONE $
                  choice $ map (\(x,s) -> liftM (const x) (try $ string s))
                            [(DLEQ,"<=")
                            ,(DLT ,"<" )
                            ,(DEQ ,"=" )
                            ,(DGEQ,">=")
                            ,(DGT ,">" )
                            ]

optchar c = option False (liftM (const True) (char c))

readSlot  ::  CharParser st Slot
readSlot  =   many1 (letter <|> digit <|> oneOf "_.-+")

-- | Read a depstring.
readDepString :: (Package -> [Category]) -> CharParser st DepString
readDepString expand = 
    do
        ds <- many $ (readDepTerm expand)
        return ds

-- | Reads a depterm.
readDepTerm :: (Package -> [Category]) -> CharParser st DepTerm
readDepTerm expand =
  choice [P.try (readUseDep expand),readGroup expand,readAtom expand]

-- | Read a depstring and handle initial whitespace.
readCompleteDepString expand =
    do
        white
        d <- readDepString expand
        eof
        return d


-- | Read a group or an @||@-dependency.
readGroup expand =
    do
        c  <-  option And (fmap (const Or) chc)
        d  <-  pars (readDepString expand)
        return (c d)

-- | Read a dependency qualified with a use flag.
readUseDep expand =
    do
        neg <- option False (liftM (const True) excl)
        use <- ident
        qmark
        thenf  <-  readDepTerm expand
        elsef  <-  option Nothing $
                     do
                         col
                         fmap Just (readDepTerm expand)
        case elsef of
          Nothing     ->  return (Use neg use thenf)
          Just elsef  ->  return (And [Use neg use thenf, Use (not neg) use elsef])

-- an atom can also be a parenthesized depstring, which is flattened

readAtom expand =
    do
        neg  <-  option "" (liftM (const "!") excl)
        dep  <-  ident
        case parse (readDepAtom expand) "" (neg ++ dep) of
          Left error  ->  fail (show error)
          Right x     ->  return (Plain x)

-- | Parsec language definition for the dependency language
depstringLang =
    LanguageDef
        { commentStart     =  "",
          commentEnd       =  "",
          commentLine      =  "",
          nestedComments   =  False,
          identStart       =  noneOf " \t\n():?!",
          identLetter      =  noneOf " \t\n()?!",
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

