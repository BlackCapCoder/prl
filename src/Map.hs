module Map where

import Data.Vector qualified as V
import Data.IntMap qualified as IM
import V2


data Map a = Map
   { content :: V.Vector a
   -- , seen    :: V.Vector Bool
   , stride  :: Int
   }
   deriving (Show, Eq, Ord, Functor)

width =
  stride

height Map {..} =
  V.length content `div` stride

placeMap x y a m
  | x < 0 || y < 0 || x >= width m || y >= height m = m
  | let = m { content = content m `V.update` V.fromList [(y*width m + x, a)] }

----

data Maps a = Maps
   { maps :: IM.IntMap (Map a)
   , cnt  :: Int
   }
   deriving (Show, Eq, Ord, Functor)

insertMap m Maps {..} = Maps
  { cnt  = succ cnt
  , maps = IM.insert cnt m maps
  }

lookupMaps i x y Maps {..} = do
  guard $ x >= 0 && y >= 0
  m <- IM.lookup i maps
  guard $ x < width m && y < height m
  pure $ content m V.! (y * width m + x)

emptyMaps = Maps mempty 0

place i x y a Maps {..}
  | Just m <- IM.lookup i maps
  = Maps { maps = IM.insert i (placeMap x y a m) maps, .. }
  | let = Maps {..}

