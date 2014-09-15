headInt :: [Int] -> Bool
headInt (x:_) = True
headInt _     = False

--Prelude> :l PatternMatchTest.hs
--[1 of 1] Compiling Main             ( PatternMatchTest.hs, interpreted )

--PatternMatchTest.hs:2:9:
--    Conflicting definitions for `x'
--    Bound at: PatternMatchTest.hs:2:9
--              PatternMatchTest.hs:2:12
--    In an equation for `headInt'
--Failed, modules loaded: none.

data Foo = Bar Int | Baz Int | Bippy

f :: Foo -> Bool
f (Bar _) = True
f (Baz _) = True
f Bippy = False
