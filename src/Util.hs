module Util where


(<<) :: Monad m => m b -> m a -> m b
p << q = do
  x <- p
  q
  return x


fromRight :: Either a b -> b
fromRight (Left _)  = error "fromRight: Argument takes form 'Left _'"
fromRight (Right x) = x
