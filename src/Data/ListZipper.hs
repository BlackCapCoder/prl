module Data.ListZipper where


data LZ a = LZ a [a] [a]
  deriving
    ( Show, Eq, Ord, Functor
    )

moveL (LZ a (l:ls) rs) = LZ l ls (a:rs)
moveL z = z

moveR (LZ a ls (r:rs)) = LZ r (a:ls) rs
moveR z = z

edit f (LZ a ls rs) =
  LZ (f a) ls rs

----

instance Pointed LZ where
  point a = LZ a (repeat a) (repeat a)

instance Copointed LZ where
  copoint (LZ a _ _) = a

