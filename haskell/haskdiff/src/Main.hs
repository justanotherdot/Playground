{-

copyright (c) 2015 Ryan James Spencer, Sean Donald Spencer

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.


haskdiff : A diff utility written in Haskell

-}

type Loc = Int
type Change = (String, Loc)

main :: IO ()
main = do
    putStrLn "To make `I am a` look like `I am amtrack`..."
    putStrLn $ showChange $ diff "I am a" "I am amtrack"

showChange :: [Change] -> String
showChange [] = []
showChange ((s,l):rst)
    | null rst = s ++ " at " ++ show l
    | otherwise = s ++ " at " ++ show l ++ "\n"  ++ showChange rst

-- Return the longest common subsequence of two seqs
lcs :: Eq a => [a] -> [a] -> [a]
lcs a b
  | or [null a, null b]  = []
  | x == y               = x : lcs xs ys
  | otherwise            = longest (lcs a xs) (lcs xs b)
    where (x:xs) = a
          (y:ys) = b

longest :: (Eq a) => [a] -> [a] -> [a]
longest as bs = if length as > length bs then as else bs

diff :: (Eq a, Show a) => [a] -> [a] -> [Change]
diff a b = diff' a b (lcs a b)  0
    where diff' :: (Eq a, Show a) => [a] -> [a] -> [a] -> Int -> [Change]
          diff' [] [] [] _ = []
          diff' x [] [] n = zip (map (\c -> "- " ++ show c) x) [n..]
          diff' [] y [] n = zip (map (\c -> "+ " ++ show c) y) [n..]
          diff' x@(a:as) y@(b:bs) z@(s:ss) n
            -- "If an item is absent in the subsequence but present in the [sequence a] then it must have been deleted
            | a `notElem` z = ("- " ++ show a, n) : diff' as y z (n+1)
            -- "If an item is absent in the subsequence but present in the [sequence b] then it must have been inserted
            | b `notElem` z = ("+ " ++ show b, n) : diff' x bs z (n+1)
            | otherwise = diff' as bs ss (n+1)


-- We'll setup HUnit / HSpec later for unit testing
simpleTest0 = diff "abc" "abcd"
simpleTest1 = diff "I am a" "I am not amtrack"
simpleTest2 = diff "I am a ck" "I am not amtrack"
