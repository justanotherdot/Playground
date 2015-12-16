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
data Arg = Value CUShort | Var String deriving (Show, Eq)
data Rule = Rule Ident Op [Arg] deriving (Show, Eq)

-- for debugging only, will remove later XXX
t = "123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i"
m = rules t

main = do (fn:_) <- getArgs
          contents <- readFile fn
          let rs = rules contents
           in print $ evaluate (Map.lookup "a" rs) rs

parseArgs :: [String] -> [Arg]
parseArgs = map parseArg
          where parseArg :: String -> Arg
                parseArg a = case dropWhile isDigit a of
                                       "" -> Value (read a :: CUShort)
                                       _  -> Var a


rules :: String -> Map.Map Ident Rule
rules s = let ls = lines s
              ruleList = map rule ls
              ruleMap  = foldl (\x r@(Rule n _ _) -> Map.insert n r x) Map.empty
              in ruleMap ruleList
              where rule s = case length elems of
                                       5 -> Rule ident (nthel 1) (parseArgs [nthel 0, nthel 2])
                                       4 -> Rule ident (nthel 0) (parseArgs [nthel 1])
                                       3 -> Rule ident "ASSIGN" (parseArgs [nthel 0])
                                       _ -> error "Unrecognized rule"
                                       where elems = words s
                                             ident = last elems
                                             nthel i = elems !! i
           

evaluate :: Maybe Rule -> Map.Map Ident Rule -> CUShort
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
