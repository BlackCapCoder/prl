module Weighted where

import System.Random

newtype Weighted a = Weighted [(Int, a)]
  deriving (Show, Eq, Ord, Functor, Foldable)

total (Weighted xs) =
  foldr (\(w,_) s -> w+s) 0 xs

pick n (Weighted xs) = go 0 xs where
  go x [] = Nothing
  go x ((w,a):as)
    | n <= x+w  = Just a
    | otherwise = go (x+w) as

roll w = do
  n <- randomRIO (0, total w)
  pure $ pick n w

