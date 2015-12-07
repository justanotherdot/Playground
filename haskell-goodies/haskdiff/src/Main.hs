{-

haskdiff

A diff utility written in Haskell

Authors:
Ryan James Spencer
Sean Donald Spencer

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

lcs :: Eq a => [a] -> [a] -> [a]
lcs [] _ = []
lcs _ [] = []
lcs a@(ah:arst) b@(bh:brst)
    | ah == bh = ah : lcs arst brst
    | otherwise = longest (lcs a brst) (lcs arst b)
    where longest :: [a] -> [a] -> [a]
          longest xs ys = if length xs > length ys then xs else ys

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
