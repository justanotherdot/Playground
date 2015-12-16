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
import qualified Data.Map as Map

type Op = String
type Ident = String
type RuleMap = Map.Map Ident Rule
data Arg = Value CUShort | Var String deriving (Show, Eq)
data Rule = Rule Ident Op [Arg] deriving (Show, Eq)

-- for debugging only, will remove later XXX
t = "123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i"
m = rules t

main = do (fn:x:_) <- getArgs
          contents <- readFile fn
          print $ connect x (rules contents)


rule :: String -> Rule
rule s = let elems   = words s
             ident   = last elems
             nthel i = elems !! i
             in case length elems of
                    5 -> Rule ident (nthel 1) (args [nthel 0, nthel 2]) -- Binary ops: AND, etc.
                    4 -> Rule ident (nthel 0) (args [nthel 1])          -- "NOT"
                    3 -> Rule ident "ASSIGN" (args [nthel 0])
                    _ -> error "Unrecognized rule"
                    where args = let parseArg :: String -> Arg
                                     parseArg a = case dropWhile isDigit a of
                                                      "" -> Value (read a :: CUShort)
                                                      _  -> Var a
                                                      in map parseArg

rules :: String -> RuleMap
rules s = let ls = lines s
              ruleList = map rule ls
              ruleMap  = foldl (\x r@(Rule n _ _) -> Map.insert n r x) Map.empty
              in ruleMap ruleList


evaluate :: Maybe Rule -> RuleMap -> CUShort
evaluate r m = case r of
    Just (Rule _ "ASSIGN" (a:_)) -> parseArg a
    Just (Rule _ "AND" (a:b:_)) -> parseArg a .&. parseArg b
    Just (Rule _ "OR" (a:b:_)) -> parseArg a .|. parseArg b
    Just (Rule _ "LSHIFT" (a:b:_)) -> shift (parseArg a) (fromIntegral $ parseArg b)
    Just (Rule _ "RSHIFT" (a:b:_)) -> shift (parseArg a) (fromIntegral $ parseArg b)
    Just (Rule _ "NOT" (a:_)) -> complement $ parseArg a
    Just r -> error $ "Unrecognized rule under evaluation: " ++ show r
    where parseArg :: Arg -> CUShort
          parseArg a = case a of
                              Value n -> n
                              Var x   -> evaluate (Map.lookup x m) m

connect :: String -> RuleMap -> CUShort
connect k m = evaluate (Map.lookup k m) m
