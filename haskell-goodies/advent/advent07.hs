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
type RuleMap = Map.Map Ident Rule
data Rule = Rule Ident Op [Rule] | Value CUShort | Var String deriving (Show, Eq)


main = do (fn:x:_) <- getArgs
          contents <- readFile fn
          let rs = rules contents
          print $ evaluate x rs


rule :: String -> Rule
rule s = let elems   = words s
             ident   = last elems
             nthel i = elems !! i
             in case length elems of
                    5 -> Rule ident (nthel 1) (args [nthel 0, nthel 2]) -- Binary ops: AND, etc.
                    4 -> Rule ident (nthel 0) (args [nthel 1])          -- "NOT"
                    3 -> Rule ident "ASSIGN" (args [nthel 0])
                    _ -> error "Unrecognized rule"
                    where args = map arg
                                 where arg :: String -> Rule
                                       arg a = case dropWhile isDigit a of
                                                   "" -> Value (read a :: CUShort)
                                                   _  -> Var a

rules :: String -> RuleMap
rules s = let ls = lines s
              ruleList = map rule ls
              ruleMap  = foldl (\x r@(Rule n _ _) -> Map.insert n r x) Map.empty
              in ruleMap ruleList


evaluate :: Ident -> RuleMap -> CUShort
evaluate r m = case Map.lookup r m of
                   Just (Rule _ "ASSIGN" (a:_))   -> parse a
                   Just (Rule _ "AND" (a:b:_))    -> parse a .&. parse b
                   Just (Rule _ "OR" (a:b:_))     -> parse a .|. parse b
                   Just (Rule _ "LSHIFT" (a:b:_)) -> shift (parse a) (fromIntegral $ parse b)
                   Just (Rule _ "RSHIFT" (a:b:_)) -> shift (parse a) (fromIntegral $ parse b)
                   Just (Rule _ "NOT" (a:_))      -> complement $ parse a
                   Just r                         -> error $ "Unrecognized rule under evaluation: " ++ show r
                   where parse r = case r of Value n -> n
                                             Var x   -> evaluate x m
