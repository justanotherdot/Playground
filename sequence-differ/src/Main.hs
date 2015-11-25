{-

sequence-differ

A diff utility written in Haskell

Authors:
Ryan James Spencer
Sean Donald Spencer

-}

main :: IO ()
main = undefined

-- In Haskell, a cardinal rule: we define what things are, not how to get them.
-- Therefore, this function gives us the subsequence from the longest common
-- lcsuence of two lcsuences
--
-- This type signature says 'give me two lives of equatable things, and I'll give back a list of the same thing
-- the `a` here just means a polymorphic type
-- Eq is a 'type class constraint' (which is why we have the fat arrow `=>`)
lcs :: Eq a => [a] -> [a] -> [a]
-- Pattern matching is top-to-bottom, so whenever one of our lists is empty, we'll return an empty list
lcs [] _ = []
lcs _ [] = []
lcs a@(ah:arst) b@(bh:brst)
    -- These pipe guys are 'guards'
    -- Their form is...
    -- | (some boolean test) = (result)
    | ah == bh = ah : lcs arst brst
    | otherwise = longest (lcs a brst) (lcs arst b)
    -- We have two ways of declaring local functions / variables; we can use 'let ... in ...' or 'where' after the function body
    where longest :: [a] -> [a] -> [a]
          longest xs ys = if length xs > length ys then xs else ys

-- It helps to write type signatures first before writing function bodies, _usually_.
-- This helps us understand the form of our function; since functions are datum,
-- this is wildly helpful for making things more composable

type Loc = Int
type Change = (String, Loc)

-- Wikipedia:
-- "From a longest common subsequence it is only a small step to get
-- diff-like output: if an item is absent in the subsequence but present in the
-- first original sequence, it must have been deleted (as indicated by the '–'
-- marks, below). If it is absent in the subsequence but present in the second
-- original sequence, it must have been inserted (as indicated by the '+'
-- marks)."
-- For our cases, 'first original sequence' shall be sequence a
-- second original sequence shall be b
-- and subsequence shall be s
diff :: Eq a => [a] -> [a] -> [Change]
-- We can pattern match to any type constructor.
-- Most commonly we pattern match to lists, in the form of (head:tail)
-- Just like (car:cdr) from lisp, where `:` is the cons operator
-- There is a convention to name things with single values for brevity
-- and to name lists heads as some single letter, say, x
-- and 'exes' (xs) as the tail
diff a b = diff' a b (lcs a b)  0
    where diff' :: Eq a => [a] -> [a] -> [a] -> Int -> [Change]
          -- These at symbols capture the whole pattern group for us., therefore, the list x can be referred to entirely by x, or individually b a or as
          diff' _ _ [] _ = []
          diff' x [] _ n = [("-", (m+n)) | m <- [0..((length x)-1)]] -- put a bunch of -'s the length of x
          diff' [] y _ n = [("+", (m+n)) | m <- [0..((length y)-1)]] -- put a bunch of +'s the length of y
          diff' x@(a:as) y@(b:bs) s n
            -- these backticks around elem mean its going to be used as an infix vs. prefix manner
            | a `notElem` s = ("-", n) : diff' as y s (n+1)
            | b `notElem` s = ("+", n) : diff' x bs s (n+1)
            | otherwise = diff' as bs s (n+1)


simpleTest0 = diff "abc" "abcd"
simpleTest1 = diff "I am a" "I am not a"
