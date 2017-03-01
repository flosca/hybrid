module Util where


(<<) :: Monad m => m b -> m a -> m b
p << q = do
  x <- p
  q
  return x
