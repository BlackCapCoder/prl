module Pokemon.Stat where


data Stat a = Stat
  { hp, att, def, spA, spD, spe :: a }
  deriving (Functor, Foldable, Show, Eq, Ord)

instance Applicative Stat where
  pure a = Stat a a a a a a

  Stat a b c d e f <*> Stat g h i j k l =
    Stat (a g) (b h) (c i) (d j) (e k) (f l)

instance Semiring a => Semiring (Stat a) where
  fromNatural = pure . fromNatural
  plus        = liftA2 plus
  times       = liftA2 times

----

-- Things that can be boosted in a battle
data Boost a = Boost
  { att, def, spA, spD, spe, acc, eva, cri :: a }
  deriving (Functor, Foldable, Show, Eq, Ord)

instance Applicative Boost where
  pure a = Boost a a a a a a a a

  Boost a b c d e f x z <*> Boost g h i j k l y w =
    Boost (a g) (b h) (c i) (d j) (e k) (f l) (x y) (z w)

instance Semiring a => Semiring (Boost a) where
  fromNatural = pure . fromNatural
  plus        = liftA2 plus
  times       = liftA2 times

----

boost2stat Boost {..} = Stat
  {hp = 0, ..}

stat2boost Stat {..} = Boost
  {acc=0, eva=0, cri=0, ..}

