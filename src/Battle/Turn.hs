module Battle.Turn where

import Pokemon.PokeAPI qualified as API
import Pokemon.Pokemon
import System.Random


data BattleAction
   = UseMove API.Move
   | Switch  Pokemon
   | UseItem ID
   | Run

data Action = Action
   { user      :: UID
   , action    :: BattleAction
   , selection :: Maybe ()
   , speed     :: Int
   , priority  :: Int
   }

data TurnEx
   = RanAway  -- user ran away in a wild battle
   | Fleed    -- wild pokemon fleed in a wild battle
   | Captured -- user captured the wild pokemon


popAction :: RandomGen g => g -> [Action] -> ((Maybe Action, g), [Action])
popAction g = go (Nothing, g) where
  go a [] = (a, [])
  go (Nothing, g) (a:as) = go (Just a, g) as
  go (Just a, g) (b:bs) =
    case compare (a.priority, a.speed) (b.priority, b.speed) of
      LT -> (a:) <$> go (Just b, g) bs
      GT -> (b:) <$> go (Just a, g) bs
      EQ -> case random g of
        (False, g2) -> (a:) <$> go (Just b, g2) bs
        (True,  g2) -> (b:) <$> go (Just a, g2) bs

runActions :: (RandomGen g, Monad m)
           => g -> [Action] -> (Action -> m a) -> m g

runActions g0 as f =
  case popAction g0 as of
    ((Nothing, g), _ ) -> pure g
    ((Just b,  g), bs) -> f b *> runActions g bs f

