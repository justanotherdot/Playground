{-

sequence-differ

A very simple tool to compare and suggest changes between two lists

Authors:
Ryan James Spencer
Sean Donald Spencer

-}

main :: IO ()
main = undefined

data ChangeType = Replace | Insert | Delete
type Change = (ChangeType, Int)

-- Checks to see if two sequences have the same contents
-- If they do, we add that to our list
-- If not, we recursively perform our lcs on all of seq a and the rest of b
-- and all of seq b and the rest of a
-- we choose the longest of the two to return
lcs :: Eq a => [a] -> [a] -> [a]
lcs [] _ = []
lcs _ [] = []
lcs a@(ah:arst) b@(bh:brst)
    | ah == bh = ah : lcs arst brst
    | otherwise = longest (lcs a brst) (lcs arst b)
    where longest :: [a] -> [a] -> [a]
          longest xs ys = if length xs > length ys then xs else ys
