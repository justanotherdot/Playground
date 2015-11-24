{-

sequence-differ

A very simple tool to compare and suggest changes between two lists

Authors:
Ryan James Spencer
Sean Donald Spencer

-}

-- Note: `undefined` has a polymorphic type and can be put anywhere

-- This just says main is of type Input/Output 'action'. ()
-- stands for 'unit' and usually is akin to 'void'
-- it is where we'll put all our potentially 'impure' code
main :: IO ()
main = undefined

-- Haskell is strongly typed, therefore, unlike dynamically typed
-- languages, we use types to express a large part of our program's
-- functionality. Since types can be statically checked, this means
-- we really want to create types as we can guarantee runtime behavior
-- by simply compiling our program. Type theorists like to call this
-- property as being 'an expressive type system', which means our types
-- can express a lot of properties, both to us as humans and also
-- to the program as guarantees.
--
-- We use the 'data' keyword to define a type.
-- The pipe symbol denotes 'or', e.g. data Color = Red | Green | Blue
data ChangeType = Replace | Insert | Delete
-- A change is a tuple with a type of change and an index location
data Change = (ChangeType, Int)

-- This says 'matcher takes a list of as and a list of bs and
-- gives us back a list of changes
matcher :: [a] -> [b] -> [Change]
matcher (x:xs) (y:ys) = undefined
