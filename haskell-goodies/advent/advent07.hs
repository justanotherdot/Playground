{-

My stab at the adventofcode.com's day 7 puzzle 'Some Assembly Required'

Ryan James Spencer
16 Dec 2015

-}

import Data.Bits
import Data.Char
import Data.List
import Foreign.C.Types
import System.Environment
import qualified Data.Map.Lazy as Map

-- for debugging only, will remove later XXX
import Debug.Trace

type Op = String
type Ident = String
type Circuit = Map.Map String CUShort
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
          print (c Map.! x)

t = "123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i"
rs = rules t

arg :: String -> Arg
arg s = if isDigit (head s) then Value (read s :: CUShort)
                            else Var s


rule :: String -> Rule
rule s = let elems = words s
             name  = last elems
             in case length elems of 5 -> case elems !! 1 of "AND"    -> And name (arg $ head elems) (arg $ elems !! 2)
                                                             "OR"     -> Or name (arg $ head elems) (arg $ elems !! 2)
                                                             "LSHIFT" -> Lshift name (arg $ head elems) (arg $ elems !! 2)
                                                             "RSHIFT" -> Rshift name (arg $ head elems) (arg $ elems !! 2)
                                     4 -> Not name (arg $ elems !! 1)
                                     3 -> Assign name (arg $ head elems)

rules :: String -> [Rule]
rules s = map rule $ lines s


makeCircuit :: [Rule] -> Circuit
makeCircuit rls = circuit
          where
                circuit :: Map.Map Ident CUShort
                circuit = Map.fromList (map connect rls)
                connect :: Rule -> (Ident, CUShort)
                connect (Assign id val) = (id, get val)
                connect (And id a1 a2) = (id, get a1 .&. get a2)
                connect (Or id a1 a2) = (id, get a1 .|. get a2)
                connect (Lshift id a val) = (id, shiftL (get a) (fromIntegral (get val)))
                connect (Rshift id a val) = (id, shiftR (get a) (fromIntegral (get val)))
                connect (Not id a) = (id, complement (get a))
                get :: Arg -> CUShort
                get (Var x) = circuit Map.! x
                get (Value n) = n
