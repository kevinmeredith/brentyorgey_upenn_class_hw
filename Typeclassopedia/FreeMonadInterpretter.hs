data Toy b next = 
     Output b next 
     | Bell next
     | Done
    deriving Show

data Fix f = Fix (f (Fix f))