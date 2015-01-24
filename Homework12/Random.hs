import Control.Monad.Random
import Control.Monad.Identity

threeInts :: Rand StdGen (Int, Int, Int)
threeInts = 
	getRandom >>= \i1 -> 
	getRandom >>= \i2 ->
	getRandom >>= \i3 ->
	 return (i1,i2,i3)

f :: ((Int, Int), String) -> String
f ((x, y), z) = z

subtract1 :: Int -> Identity Int
subtract1 = return . (subtract 1)

-- count down until x = 0 in `Identity x`
countDown :: Int -> Identity Int
countDown n 
 | n <= 0    = return n
 | otherwise = subtract1 n >>= countDown 