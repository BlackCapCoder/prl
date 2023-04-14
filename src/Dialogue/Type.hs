module Dialogue.Type where

import Control.Monad.State

-- A dialogue with an NPC
--
data Dialogue m a
   = Say Bool String (Dialogue m a)
   | DSelect [(String, Dialogue m a)] (Dialogue m a)
   | Act (m (Dialogue m a))
   | IntSelect Int Int (Maybe Int -> Dialogue m a)
   | End a
   deriving Functor

instance Functor m => Semigroup (Dialogue m a) where
  l0 <> r = go l0 where
    go = \case
      Say nag s l       -> Say nag s (go l)
      Act m             -> Act (go <$> m)
      DSelect os l      -> DSelect (fmap go <$> os) (go l)
      IntSelect lo hi f -> IntSelect lo hi (go . f)
      End _             -> r

instance (Functor m, Monoid a) => Monoid (Dialogue m a) where
  mempty = End mempty

instance Monad m => Applicative (Dialogue m) where
  pure  = End
  (<*>) = ap

instance Monad m => Monad (Dialogue m) where
  m >>= f = case m of
    End a -> f a
    Act m -> Act (fmap (>>= f) m)
    Say nag s r -> Say nag s (r >>= f)
    DSelect os r -> DSelect (fmap (>>= f) <$> os) (r >>= f)
    IntSelect lo hi g -> IntSelect lo hi (g >=> f)

instance MonadTrans Dialogue where
  lift m = Act (End <$> m)

----

end =
  End ()

nag s =
  Say True s end

say s =
  Say False s end

select os =
  DSelect os end

choose os =
  DSelect (fmap (End . Just) <$> os) (End Nothing)

yesno y n =
  DSelect [("Yes", y), ("No", n)] n

noyes =
  flip yesno

number lo hi f =
  IntSelect lo hi \case
    Just i  -> f i
    Nothing -> end

