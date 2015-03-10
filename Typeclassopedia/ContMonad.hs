import Control.Monad.Cont

ex1 :: Monad m => m Int
ex1 = do
   a <- return 1
   b <- return 10
   return $ a+b

ex3 = do
  a <- return 1
  b <- ContT (\fred -> "escape")
  return $ a+b


ex4 = do
  a <- return 1
  b <- ContT (\fred -> fred 10 ++ fred 20)
  return $ a+b

test5 = do
  a <- return 1
  b <- [10,20]
  return $ a+b

ex6 = do
  a <- return 1
  b <- ContT (\fred -> fred 10 ++ fred 20)
  return $ a+b


--i x = ContT (\fred -> x >>= fred)

--run m = runCont m return

--test9 = run $ do
--  a <- i [1,2]
--  b <- i [10,20]
--  return $ a+b