module Pokemon.Pokemon where

import Pokemon.Stat
import Pokemon.Nature
import Pokemon.Type
import Pokemon.PokeAPI hiding (PokemonMove, Pokemon)
import Pokemon.PokeAPI qualified as API
import Data.IntMap qualified as IM
import System.Random
import System.Random.Shuffle
import Data.Text qualified as Text

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
   , happiness :: Int
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

createPokemon uid api@PokeAPI {..} id level_range = do
  let Just mon  = IM.lookup id pokemon
  let Just base = API.getBaseStats api id

  level  <- randomRIO level_range
  gender <- randomRIO (0, 1 ) <&> toEnum
  nature <- randomRIO (0, 15) <&> toEnum

  let as = getPokemonAbilities api id
  ability <- randomRIO (0, length as - 1) <&> \i -> (as !! i).id

  let stats = calcStat base level nature zero zero

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

  iv_hp  <- randomRIO (0, 31)
  iv_att <- randomRIO (0, 31)
  iv_def <- randomRIO (0, 31)
  iv_spA <- randomRIO (0, 31)
  iv_spD <- randomRIO (0, 31)
  iv_spe <- randomRIO (0, 31)

  let ivs = Stat iv_hp iv_att iv_def iv_spA iv_spD iv_spe

  pure Pokemon
    { totalExp  = 0
    , shiny     = False
    , eggCycles = 0
    , heldItem  = Nothing
    , evs       = zero
    , ivs       = ivs
    , happiness = 0
    , nickname  = Nothing
    , hp        = stats.hp
    , status    = Nothing
    , ..
    }

getStats :: PokeAPI -> Pokemon -> Stat Int
getStats api Pokemon {..} = stats
  where
    Just base = API.getBaseStats api id
    stats = calcStat base level nature evs ivs

heal :: PokeAPI -> Pokemon -> Pokemon
heal api pok = pok
  { status = Nothing
  , hp     = stats.hp
  }
  where
    stats = getStats api pok

isAsleep mon = case mon.status of
  Just (Sleep _) -> True
  _ -> False

pokemonName api mon =
  case mon.nickname of
    Just nick -> nick
    Nothing   ->
      case IM.lookup mon.id api.pokemon of
        Just pok -> Text.unpack pok.name
        Nothing  -> "unknown"


