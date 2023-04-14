{-# LANGUAGE OverloadedRecordDot #-}
module Pokemon.Nature where

import Pokemon.Stat

data Nature
   = Hardy
   | Lonely
   | Adamant
   | Naughty
   | Brave
   | Bold
   | Docile
   | Impish
   | Lax
   | Relaxed
   | Modest
   | Mild
   | Bashful
   | Rash
   | Quiet
   | Calm
   | Gentle
   | Careful
   | Quirky
   | Sassy
   | Timid
   | Hasty
   | Jolly
   | Naive
   | Serious
   deriving (Show, Eq, Ord, Enum, Bounded)

----

natureBoost :: Nature -> Stat Float
natureBoost n = set y 0.1 $ set x (-0.1) $ pure 1.0
  where
    (y,x) = divMod (fromEnum n) 5

    set 0 x Stat {..} = Stat { att = att + x, .. }
    set 1 x Stat {..} = Stat { def = def + x, .. }
    set 2 x Stat {..} = Stat { spA = spA + x, .. }
    set 3 x Stat {..} = Stat { spD = spD + x, .. }
    set 4 x Stat {..} = Stat { spe = spe + x, .. }
    set _ _ s = s -- unreachable

-- effective stat for a pokemon
--
calcStat :: Stat Int -> Int -> Nature -> Stat Int -> Stat Int -> Stat Int
calcStat base level (natureBoost->nature) iv ev = Stat
  { hp  = f (base.hp )              (iv.hp ) (ev.hp ) + level + 10
  , att = g (base.att) (nature.att) (iv.att) (ev.att)
  , def = g (base.def) (nature.def) (iv.def) (ev.def)
  , spA = g (base.spA) (nature.spA) (iv.spA) (ev.spA)
  , spD = g (base.spD) (nature.spD) (iv.spD) (ev.spD)
  , spe = g (base.spe) (nature.spe) (iv.spe) (ev.spe)
  }
  where
    f base iv ev =
      floor $ ((2*fi base + fi iv + fi ev/4) * fi level)/100

    g base nature iv ev =
      floor $ nature * fi (5 + f base iv ev)

