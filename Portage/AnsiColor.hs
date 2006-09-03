{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Simplistic ANSI color support.
-}

module Portage.AnsiColor
  where

import Portage.Config.Type
import Data.List

data Color  =  Black
            |  Red
            |  Green
            |  Yellow
            |  Blue
            |  Magenta
            |  Cyan
            |  White
            |  Default
  deriving Enum

esc []  =  ""
esc xs  =  "\ESC[" ++ (concat . intersperse ";" $ xs) ++ "m"

col fg bf bg = show (fromEnum fg + 30) : bf' [show (fromEnum bg + 40)]
  where bf' | bf         =  ("01" :)
            | otherwise  =  id

inColor cfg c bf bg txt  |  color cfg  =  esc (col c bf bg) ++ txt ++ esc ["00"]
                         |  otherwise  =  txt

