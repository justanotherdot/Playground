{-
copyright (c) 2015 Ryan James Spencer

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/. 


My stab at the adventofcode.com's day 7 puzzle 'Some Assembly Required'

This solution takes advantage of Haskell's built-in memoization (this is what the white paper
"Implementation of the Spineless G-Tag Machine" is talking about.)

Other Haskell solutions to this puzzle involve imperative-styled solutions (say with Data.HashMap.Strict)

-}

import Data.Bits
import Data.Char
import Data.Map.Lazy ((!))
import Foreign.C.Types
import System.Environment
import qualified Data.Map.Lazy as M

type Ident = String
type Circuit = M.Map String CUShort
data Arg = Value CUShort | Var Ident deriving (Show, Eq)
data Rule = Assign Ident Arg
          | And Ident Arg Arg
          | Or Ident Arg Arg
          | Lshift Ident Arg Arg
          | Rshift Ident Arg Arg
          | Not Ident Arg deriving (Show, Eq)


main = do (fn:x:_) <- getArgs
          contents <- readFile fn
          let rs = rules contents
              c  = makeCircuit rs
          print (c ! x)

arg :: String -> Arg
arg s = if isDigit (head s) then Value (read s :: CUShort)
                            else Var s

rule :: String -> Rule
rule s = let elems = words s
             name  = last elems
             in case length elems of
                    5 -> case elems !! 1 of
                              "AND"    -> And name (arg $ head elems) (arg $ elems !! 2)
                              "OR"     -> Or name (arg $ head elems) (arg $ elems !! 2)
                              "LSHIFT" -> Lshift name (arg $ head elems) (arg $ elems !! 2)
                              "RSHIFT" -> Rshift name (arg $ head elems) (arg $ elems !! 2)
                    4 -> Not name (arg $ elems !! 1)
                    3 -> Assign name (arg $ head elems)

rules :: String -> [Rule]
rules s = map rule $ lines s

makeCircuit :: [Rule] -> Circuit
makeCircuit rls = circuit
          where circuit :: M.Map Ident CUShort
                circuit = M.fromList (map connect rls)
                connect :: Rule -> (Ident, CUShort)
                connect (Assign id val) = (id, get val)
                connect (And id a1 a2) = (id, get a1 .&. get a2)
                connect (Or id a1 a2) = (id, get a1 .|. get a2)
                connect (Lshift id a val) = (id, shiftL (get a) (fromIntegral (get val)))
                connect (Rshift id a val) = (id, shiftR (get a) (fromIntegral (get val)))
                connect (Not id a) = (id, complement (get a))
                get :: Arg -> CUShort
                get (Var x) = circuit ! x
                get (Value n) = n
