{-

sequence-differ

A diff utility written in Haskell

Authors:
Ryan James Spencer
Sean Donald Spencer

-}

main :: IO ()
main = undefined

data ChangeType = Replace | Insert | Delete
type Change = (ChangeType, Int)

lcs :: Eq a => [a] -> [a] -> [a]
lcs [] _ = []
lcs _ [] = []
lcs a@(ah:arst) b@(bh:brst)
    | ah == bh = ah : lcs arst brst
    | otherwise = longest (lcs a brst) (lcs arst b)
    where longest :: [a] -> [a] -> [a]
          longest xs ys = if length xs > length ys then xs else ys
