module Pokemon.Pokemon where

import Settings
import Pokemon.Stat
import Pokemon.Nature
import Pokemon.Type
import Pokemon.Level
import Pokemon.PokeAPI hiding (PokemonMove, Pokemon)
import Pokemon.PokeAPI qualified as API
import Data.IntMap qualified as IM
import System.Random
import System.Random.Shuffle
import Data.Text qualified as Text
import Control.Monad.State
import Data.Word
import Data.List qualified as L

type ID  = Int
type UID = Int -- unique ID

data Pokemon = Pokemon
   { id        :: ID
   , uid       :: UID
   , ability   :: ID
   , nature    :: Nature
   , evs       :: Stat Int
   , ivs       :: Stat Int
   , gender    :: PokemonGender
   , happiness :: Word8
   , nickname  :: Maybe String
   , moves     :: [PokemonMove]
   , shiny     :: Bool
   , eggCycles :: Int -- egg cycles left before hatchning
   , level     :: Int
   , totalExp  :: Int
   , heldItem  :: Maybe ID
   , hp        :: Int
   , status    :: Maybe PokemonStatus
   , types     :: [TYPE]
   }
   deriving (Show, Eq, Ord)

data PokemonMove = PokemonMove
   { id     :: ID
   , pp_ups :: Int
   , pp     :: Int
   }
   deriving (Show, Eq, Ord)

data PokemonGender
   = Female
   | Male
   | Genderless
   deriving (Show, Eq, Ord, Enum, Bounded)

genderAbriv = \case
  Female     -> "F"
  Male       -> "M"
  Genderless -> ""

data PokemonStatus
   = Paralysis
   | Poison
   | Freeze
   | Burn
   | Sleep Int
   | Toxic Int
   deriving (Show, Eq, Ord)

statusAbriv = \case
  Paralysis -> "PAR"
  Sleep _   -> "SLP"
  Poison    -> "POI"
  Freeze    -> "FRZ"
  Burn      -> "BRN"
  Toxic _   -> "TOX"


-- calcStat base level (natureBoost->nature) iv ev = Stat

createPokemon Settings{..} uid api@PokeAPI {..} id level_range = do
  let Just mon  = IM.lookup id pokemon
  let Just base = API.getBaseStats api id

  level  <- randomRIO level_range
  gender <- randomRIO (0, 1 ) <&> toEnum
  nature <- randomRIO (0, 15) <&> toEnum

  let as = getPokemonAbilities api id
  ability <- randomRIO (0, length as - 1) <&> \i -> (as !! i).id

  let ms = mapMaybe (\pm -> getMoveByName api pm.move.name)
         $ mon.moves & filter \pm ->
             pm.version_group_details & any \pmv ->
               pmv.level_learned_at <= level &&
               pmv.move_learn_method.name == "level-up"

  let pms = ms <&> \m -> PokemonMove
                { id     = m.id
                , pp     = m.pp
                , pp_ups = 0
                }

  moves <- shuffleM pms <&> take 4

  let types = mon.types <&> \a -> typeFromName a._type.name

  ivs <- if noIVs then pure $ pure 31 else do
    hp  <- randomRIO (0, 31)
    att <- randomRIO (0, 31)
    def <- randomRIO (0, 31)
    spA <- randomRIO (0, 31)
    spD <- randomRIO (0, 31)
    spe <- randomRIO (0, 31)
    pure Stat {..}

  shiny <- do
    roll <- randomRIO (0.0, 1.0)
    pure $ roll <= shinyChance

  let Just growth = getPokemonGrowthRate api mon

  let mon = Pokemon
        { totalExp  = totalExpAtLevel growth level
        , eggCycles = 0
        , heldItem  = Nothing
        , evs       = zero
        , ivs       = ivs
        , happiness = 0
        , nickname  = Nothing
        , hp        = (getStats api mon).hp
        , status    = Nothing
        , ..
        }

  pure mon

getStats :: PokeAPI -> Pokemon -> Stat Int
getStats api Pokemon {..} = stats
  where
    Just base = API.getBaseStats api id
    stats = calcStat base level nature evs ivs

heal :: PokeAPI -> Pokemon -> Pokemon
heal api pok = pok
  { status = Nothing
  , hp     = stats.hp
  , moves  = map restorePP pok.moves
  }
  where
    stats = getStats api pok

    restorePP m = m { pp = pp_max, pp_ups = m.pp_ups }
      where
        Just amove = IM.lookup m.id api.moves
        pp_max = amove.pp

isAsleep mon = case mon.status of
  Just (Sleep _) -> True
  _ -> False

pokemonName api mon
  | mon.eggCycles > 0                         = "egg"
  | Just nick <- mon.nickname                 = nick
  | Just pok  <- IM.lookup mon.id api.pokemon = Text.unpack pok.name
  | otherwise = "UNKNOWN"

addEVs add mon = mon { evs = execState go mon.evs } where
  go = do
    modify \evs -> evs { hp  = min 252 $ evs.hp  + min (510 - sum evs) add.hp , att=evs.att }
    modify \evs -> evs { att = min 252 $ evs.att + min (510 - sum evs) add.att, hp =evs.hp  }
    modify \evs -> evs { def = min 252 $ evs.def + min (510 - sum evs) add.def, hp =evs.hp  }
    modify \evs -> evs { spA = min 252 $ evs.spA + min (510 - sum evs) add.spA, hp =evs.hp  }
    modify \evs -> evs { spD = min 252 $ evs.spD + min (510 - sum evs) add.spD, hp =evs.hp  }
    modify \evs -> evs { spe = min 252 $ evs.spe + min (510 - sum evs) add.spe, hp =evs.hp  }

levelUpMoves :: PokeAPI -> Pokemon -> [Move]
levelUpMoves api pok =
  filter (\m -> m.id `L.notElem` mids) $
  mapMaybe (\pm -> getMoveByName api pm.move.name)
    $ mon.moves & filter \pm ->
        pm.version_group_details & any \pmv ->
          pmv.level_learned_at <= pok.level &&
          pmv.move_learn_method.name == "level-up"
  where
    Just mon = IM.lookup pok.id api.pokemon

    mids = map (.id) pok.moves

levelUpEvolution :: PokeAPI -> Pokemon -> Maybe Pokemon
levelUpEvolution api pok = Nothing

