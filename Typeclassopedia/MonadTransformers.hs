--Implement join :: M (N (M (N a))) -> M (N a), 
--given distrib :: N (M a) -> M (N a) and 
--assuming M and N are instances of Monad.

import Control.Monad

distrib :: (Monad m, Monad n) => n (m a) -> m (n a)
distrib x = do xx <- x
               return xx 