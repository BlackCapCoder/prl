{-# LANGUAGE StrictData #-}
module Data.Circular where

import Data.Vector qualified as V
import GHC.Exts as GHC


-- A circular list
--
data Circular a = Circular
   { vec :: V.Vector a
   , cur :: Int
   }
   deriving
     ( Show, Eq, Ord, Functor
     )

----

instance IsList (Circular a) where
  type Item (Circular a) = a

  fromList lst = Circular
    { vec = V.fromList lst
    , cur = 0
    }

  toList Circular {..} =
    [ vec `V.unsafeIndex` mod (i+cur) (V.length vec)
    | i <- [0..pred (V.length vec)]
    ]

instance Foldable Circular where
  toList = GHC.toList
  foldr f z = foldr f z . GHC.toList

instance Pointed Circular where
  point a = Circular (V.singleton a) 0

instance Copointed Circular where
  copoint Circular {..} = vec V.! cur

----

moveL Circular {..}
  | 0 <- cur = Circular { cur = pred (V.length vec), .. }
  | let      = Circular { cur = pred cur, .. }

moveR Circular {..}
  | succ cur == V.length vec = Circular { cur = 0, .. }
  | let                      = Circular { cur = succ cur, .. }

