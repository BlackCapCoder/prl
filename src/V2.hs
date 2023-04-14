module V2 where

data V2 a = V2 { x,y :: a }
  deriving (Show, Eq, Ord, Functor)

instance Applicative V2 where
  pure a = V2 a a
  V2 fl fr <*> V2 al ar = V2 (fl al) (fr ar)

instance Semiring a => Semiring (V2 a) where
  zero = pure zero
  one  = pure one
  fromNatural = pure . fromNatural

  plus  = liftA2 plus
  times = liftA2 times

instance Ring a => Ring (V2 a) where
  negate = fmap negate

