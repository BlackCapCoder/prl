{-# LANGUAGE TemplateHaskell #-}
module Pokemon.PokeAPI where

import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.ByteString.Lazy qualified as B
import Data.ByteString qualified as BS
import Data.Aeson.TH
import Data.Aeson
import GHC.Generics
import System.Directory
import Data.IntMap qualified as IM
import Pokemon.Stat
import Pokemon.Level

----

data Pokemon = Pokemon
   { id                       :: Int
   , name                     :: Text
   , base_experience          :: Maybe Int
   , height                   :: Int
   , is_default               :: Bool
   , order                    :: Int
   , weight                   :: Int
   , abilities                :: [PokemonAbility]
   , forms                    :: [NamedAPIResource]
   , game_indices             :: [VersionGameIndex]
   , held_items               :: [PokemonHeldItem]
   , location_area_encounters :: Text
   , moves                    :: [PokemonMove]
   , past_types               :: [PokemonTypePast]
   , sprites                  :: PokemonSprites
   , species                  :: NamedAPIResource
   , stats                    :: [PokemonStat]
   , types                    :: [PokemonType]
   }
   deriving (Show, Eq, Ord)

data PokemonAbility = PokemonAbility
   { is_hidden :: Bool
   , slot      :: Int
   , ability   :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

newtype NamedAPIResource = NamedAPIResource
   { name :: Text
   -- , url  :: Text
   }
   deriving (Show, Eq, Ord)

data VersionGameIndex = VersionGameIndex
   { game_index :: Int
   , version    :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

data PokemonHeldItem = PokemonHeldItem
   { item            :: NamedAPIResource
   , version_details :: [PokemonHeldItemVersion]
   }
   deriving (Show, Eq, Ord)

data PokemonHeldItemVersion = PokemonHeldItemVersion
   { version :: NamedAPIResource
   , rarity  :: Int
   }
   deriving (Show, Eq, Ord)

data PokemonMove = PokemonMove
   { move                  :: NamedAPIResource
   , version_group_details :: [PokemonMoveVersion]
   }
   deriving (Show, Eq, Ord)

data PokemonMoveVersion = PokemonMoveVersion
   { move_learn_method :: NamedAPIResource
   , version_group     :: NamedAPIResource
   , level_learned_at  :: Int
   }
   deriving (Show, Eq, Ord)

data PokemonTypePast = PokemonTypePast
   { generation :: NamedAPIResource
   , types      :: [PokemonType]
   }
   deriving (Show, Eq, Ord)

data PokemonType = PokemonType
   { _slot :: Int
   , _type :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)


data PokemonSprites = PokemonSprites
   { front_default      :: Maybe Text
   , front_shiny        :: Maybe Text
   , front_female       :: Maybe Text
   , front_shiny_female :: Maybe Text
   , back_default       :: Maybe Text
   , back_shiny         :: Maybe Text
   , back_female        :: Maybe Text
   , back_shiny_female  :: Maybe Text
   }
   deriving (Show, Eq, Ord)

data PokemonStat = PokemonStat
   { stat      :: NamedAPIResource
   , effort    :: Int
   , base_stat :: Int
   }
   deriving (Show, Eq, Ord)

----

data Ability = Ability
   { id                  :: Int
   , name                :: Text
   , is_main_series      :: Bool
   , generation          :: NamedAPIResource
   , names               :: [Name]
   , effect_entries      :: [VerboseEffect]
   , effect_changes      :: [AbilityEffectChange]
   , flavor_text_entries :: [AbilityFlavorText]
   , pokemon             :: [AbilityPokemon]
   }
   deriving (Show, Eq, Ord)

data Name = Name
   { name     :: Text
   , language :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

data VerboseEffect = VerboseEffect
   { effect       :: Text
   , short_effect :: Text
   , language     :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

data AbilityEffectChange = AbilityEffectChange
   { effect_entries :: [Effect]
   , version_group  :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

data AbilityFlavorText = AbilityFlavorText
   { flavor_text   :: Text
   , language      :: NamedAPIResource
   , version_group :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

data AbilityPokemon = AbilityPokemon
   { is_hidden :: Bool
   , slot      :: Int
   , pokemon   :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

data Effect = Effect
   { effect   :: Text
   , language :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

----

data EvolutionChain = EvolutionChain
   { id                :: Int
   , baby_trigger_item :: Maybe NamedAPIResource
   , chain             :: ChainLink
   }
   deriving (Show, Eq, Ord)

data ChainLink = ChainLink
   { is_baby           :: Bool
   , species           :: NamedAPIResource
   , evolution_details :: [EvolutionDetail]
   , evolves_to        :: [ChainLink]
   }
   deriving (Show, Eq, Ord)

data EvolutionDetail = EvolutionDetail
   { item                    :: Maybe NamedAPIResource
   , trigger                 :: NamedAPIResource
   , gender                  :: Maybe Int
   , held_item               :: Maybe NamedAPIResource
   , known_move              :: Maybe NamedAPIResource
   , known_move_type         :: Maybe NamedAPIResource
   -- , location                :: Maybe Text
   , min_level               :: Maybe Int
   , min_happiness           :: Maybe Int
   , min_beauty              :: Maybe Int
   , min_affection           :: Maybe Int
   , needs_overworld_rain    :: Bool
   , party_species           :: Maybe NamedAPIResource
   , party_type              :: Maybe NamedAPIResource
   , relative_physical_stats :: Maybe Int
   , time_of_day             :: Text
   , trade_species           :: Maybe NamedAPIResource
   , turn_upside_down        :: Bool
   }
   deriving (Show, Eq, Ord)

----

data PokemonSpecies = PokemonSpecies
   { id                     :: Int
   , name                   :: Text
   , order                  :: Int
   , gender_rate            :: Int
   , capture_rate           :: Int
   , base_happiness         :: Maybe Int
   , is_baby                :: Bool
   , is_legendary           :: Bool
   , hatch_counter          :: Maybe Int
   , has_gender_differences :: Bool
   , forms_switchable       :: Bool
   , growth_rate            :: Maybe NamedAPIResource
   , pokedex_numbers        :: [PokemonSpeciesDexEntry]
   , egg_groups             :: [NamedAPIResource]
   , color                  :: NamedAPIResource
   , shape                  :: Maybe NamedAPIResource
   , evolves_from_species   :: Maybe NamedAPIResource
   -- , evolution_chain        :: Maybe APIResource
   , habitat                :: Maybe NamedAPIResource
   , names                  :: [Name]
   , pal_park_encounters    :: [PalParkEncounterArea]
   , flavor_text_entries    :: [FlavorText]
   , form_descriptions      :: [Description]
   , genera                 :: [Genus]
   , varieties              :: [PokemonSpeciesVariety]
   }
   deriving (Show, Eq, Ord)

data Genus = Genus
   { genus    :: Text
   , language :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

data PokemonSpeciesDexEntry = PokemonSpeciesDexEntry
   { entry_number :: Int
   , pokedex      :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

data PalParkEncounterArea = PalParkEncounterArea
   { base_score :: Int
   , rate       :: Int
   , area       :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

data FlavorText = FlavorText
   { flavor_text :: Text
   , language    :: NamedAPIResource
   , version     :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

newtype APIResource = APIResource
   {
   url :: Text
   }
   deriving (Show, Eq, Ord)

data Description = Description
   { description :: Text
   , language    :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

data PokemonSpeciesVariety = PokemonSpeciesVariety
   { is_default :: Bool
   , pokemon    :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

----

data Item = Item
   { id                  :: Int
   , name                :: Text
   , cost                :: Int
   , fling_power         :: Maybe Int
   , fling_effect        :: Maybe NamedAPIResource
   , attributes          :: [NamedAPIResource]
   , category            :: NamedAPIResource
   , effect_entries      :: [VerboseEffect]
   , flavor_text_entries :: [VersionGroupFlavorText]
   , game_indices        :: [GenerationGameIndex]
   , names               :: [Name]
   , sprites             :: ItemSprites
   , held_by_pokemon     :: [ItemHolderPokemon]
   , baby_trigger_for    :: Maybe APIResource
   , machines            :: [MachineVersionDetail]
   }
   deriving (Show, Eq, Ord)

newtype ItemSprites = ItemSprites
   { _default :: Maybe Text
   }
   deriving (Show, Eq, Ord)

data ItemHolderPokemon = ItemHolderPokemon
   { pokemon         :: NamedAPIResource
   , version_details :: [ItemHolderPokemonVersionDetail]
   }
   deriving (Show, Eq, Ord)

data ItemHolderPokemonVersionDetail = ItemHolderPokemonVersionDetail
   { rarity  :: Int
   , version :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

data VersionGroupFlavorText = VersionGroupFlavorText
   { text          :: Text
   , language      :: NamedAPIResource
   , version_group :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

data GenerationGameIndex = GenerationGameIndex
   { game_index :: Int
   , generation :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

data MachineVersionDetail = MachineVersionDetail
   { machine       :: APIResource
   , version_group :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

----

data Move = Move
   { id           :: Int
   , name         :: Text
   , pp           :: Int
   , priority     :: Int
   , power        :: Maybe Int
   , accuracy     :: Maybe Int
   , damage_class :: NamedAPIResource
   , _type        :: Maybe NamedAPIResource
   , names        :: [Name]
   , target       :: NamedAPIResource
   }
   deriving (Show, Eq, Ord)

----

$(deriveJSON defaultOptions 'NamedAPIResource)
$(deriveJSON defaultOptions 'VersionGameIndex)
$(deriveJSON defaultOptions 'PokemonHeldItemVersion)
$(deriveJSON defaultOptions 'PokemonHeldItem)
$(deriveJSON defaultOptions 'PokemonAbility)
$(deriveJSON defaultOptions 'PokemonMoveVersion)
$(deriveJSON defaultOptions 'PokemonMove)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} 'PokemonType)
$(deriveJSON defaultOptions 'PokemonTypePast)
$(deriveJSON defaultOptions 'PokemonSprites)
$(deriveJSON defaultOptions 'PokemonStat)
$(deriveJSON defaultOptions 'Pokemon)

$(deriveJSON defaultOptions 'Name)
$(deriveJSON defaultOptions 'Effect)
$(deriveJSON defaultOptions 'VerboseEffect)
$(deriveJSON defaultOptions 'AbilityEffectChange)
$(deriveJSON defaultOptions 'AbilityFlavorText)
$(deriveJSON defaultOptions 'AbilityPokemon)
$(deriveJSON defaultOptions 'Ability)

$(deriveJSON defaultOptions 'EvolutionDetail)
$(deriveJSON defaultOptions 'ChainLink)
$(deriveJSON defaultOptions 'EvolutionChain)

$(deriveJSON defaultOptions 'APIResource)
$(deriveJSON defaultOptions 'FlavorText)
$(deriveJSON defaultOptions 'Description)
$(deriveJSON defaultOptions 'PokemonSpeciesVariety)
$(deriveJSON defaultOptions 'PalParkEncounterArea)
$(deriveJSON defaultOptions 'PokemonSpeciesDexEntry)
$(deriveJSON defaultOptions 'Genus)
$(deriveJSON defaultOptions 'PokemonSpecies)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} 'ItemSprites)
$(deriveJSON defaultOptions 'VersionGroupFlavorText)
$(deriveJSON defaultOptions 'ItemHolderPokemonVersionDetail)
$(deriveJSON defaultOptions 'ItemHolderPokemon)
$(deriveJSON defaultOptions 'GenerationGameIndex)
$(deriveJSON defaultOptions 'MachineVersionDetail)
$(deriveJSON defaultOptions 'Item)

$(deriveJSON defaultOptions {fieldLabelModifier = \case '_':r -> r; r -> r} 'Move)

----

getPokemon name = do
  decode @Pokemon <$> B.readFile ("data/pokemon/" <> name <> ".json")

getAbility name = do
  decode @Ability <$> B.readFile ("data/ability/" <> name <> ".json")

getEvolutionChain ix = do
  decode @EvolutionChain <$> B.readFile ("data/evolution-chain/" <> ix <> ".json")

getPokemonSpecies name = do
  decode @PokemonSpecies <$> B.readFile ("data/pokemon-species/" <> name <> ".json")

getItem name = do
  decode @Item <$> B.readFile ("data/item/" <> name <> ".json")


getAllPokemon = catMaybes <$> do
  pths <- listDirectory "data/pokemon"
  forM pths \pth -> do
    !bs <- BS.readFile ("data/pokemon/" <> pth)
    pure $ decode @Pokemon $ BS.fromStrict bs

getAbilities = catMaybes <$> do
  pths <- listDirectory "data/ability"
  forM pths \pth -> do
    !bs <- BS.readFile ("data/ability/" <> pth)
    pure $ decode @Ability $ BS.fromStrict bs

getEvolutionChains = catMaybes <$> do
  pths <- listDirectory "data/evolution-chain"
  forM pths \pth -> do
    !bs <- BS.readFile ("data/evolution-chain/" <> pth)
    pure $ decode @EvolutionChain $ BS.fromStrict bs

getAllPokemonSpecies = catMaybes <$> do
  pths <- listDirectory "data/pokemon-species"
  forM pths \pth -> do
    !bs <- BS.readFile ("data/pokemon-species/" <> pth)
    pure $ decode @PokemonSpecies $ BS.fromStrict bs

-- tm100.json fails to parse
getAllItems = catMaybes <$> do
  pths <- listDirectory "data/item"
  forM pths \pth -> do
    !bs <- BS.readFile ("data/item/" <> pth)
    pure $ decode @Item $ BS.fromStrict bs

getAllMoves = catMaybes <$> do
  pths <- listDirectory "data/move"
  forM pths \pth -> do
    !bs <- BS.readFile ("data/move/" <> pth)
    pure $ decode @Move $ BS.fromStrict bs

----

data PokeAPI = PokeAPI
   { pokemon    :: IM.IntMap Pokemon
   , abilities  :: IM.IntMap Ability
   , evolutions :: IM.IntMap EvolutionChain
   , species    :: IM.IntMap PokemonSpecies
   , items      :: IM.IntMap Item
   , moves      :: IM.IntMap Move
   }
   deriving (Show, Eq, Ord)

getPokeAPI :: IO PokeAPI
getPokeAPI = do
  pokemon    <- getAllPokemon        <&> IM.fromList . map \x -> (x.id, x)
  abilities  <- getAbilities         <&> IM.fromList . map \x -> (x.id, x)
  evolutions <- getEvolutionChains   <&> IM.fromList . map \x -> (x.id, x)
  species    <- getAllPokemonSpecies <&> IM.fromList . map \x -> (x.id, x)
  items      <- getAllItems          <&> IM.fromList . map \x -> (x.id, x)
  moves      <- getAllMoves          <&> IM.fromList . map \x -> (x.id, x)
  pure PokeAPI {..}

-- Get the base stats for a pokemon
--
getBaseStats :: PokeAPI -> Int -> Maybe $ Stat Int
getBaseStats PokeAPI {..} id = do
  Pokemon {..} <- IM.lookup id pokemon
  pure (go zero stats)
  where
    go Stat {..} [] = Stat {..}
    go Stat {..} (a:as) =
      case a.stat.name of
        "hp"              -> go Stat {hp =a.base_stat, ..} as
        "attack"          -> go Stat {att=a.base_stat, ..} as
        "defense"         -> go Stat {def=a.base_stat, ..} as
        "special-attack"  -> go Stat {spA=a.base_stat, ..} as
        "special-defense" -> go Stat {spD=a.base_stat, ..} as
        "speed"           -> go Stat {spe=a.base_stat, ..} as
        _                 -> go Stat {..} as

-- Get the EV yield of a pokemon
--
getEvYield :: PokeAPI -> Int -> Maybe $ Stat Int
getEvYield PokeAPI {..} id = do
  Pokemon {..} <- IM.lookup id pokemon
  pure (go zero stats)
  where
    go Stat {..} [] = Stat {..}
    go Stat {..} (a:as) =
      case a.stat.name of
        "hp"              -> go Stat {hp =a.effort, ..} as
        "attack"          -> go Stat {att=a.effort, ..} as
        "defense"         -> go Stat {def=a.effort, ..} as
        "special-attack"  -> go Stat {spA=a.effort, ..} as
        "special-defense" -> go Stat {spD=a.effort, ..} as
        "speed"           -> go Stat {spe=a.effort, ..} as
        _                 -> go Stat {..} as

getAbilityByName PokeAPI {..} name =
  listToMaybe [ a | a <- toList abilities, a.name == name ]

getMoveByName PokeAPI {..} name =
  listToMaybe [ a | a <- toList moves, a.name == name ]

getSpeciesByName PokeAPI {..} name =
  listToMaybe [ a | a <- toList species, a.name == name ]

getPokemonAbilities api@PokeAPI {..} id = join $ maybeToList do
  pok <- IM.lookup id pokemon
  let as = pok.abilities <&> \a -> a.ability.name
  pure $ mapMaybe (getAbilityByName api) as

getPokemonMoves api@PokeAPI {..} (pok :: Pokemon) = do
  pm <- pok.moves
  pure pm

getSpeciesGrowthRate :: PokemonSpecies -> Maybe GrowthRate
getSpeciesGrowthRate spe = spe.growth_rate >>= \a -> case a.name of
  "slow"                -> Just Slow
  "medium-slow"         -> Just MediumSlow
  "medium"              -> Just MediumFast
  "fast"                -> Just Fast
  "slow-then-very-fast" -> Just Erratic
  "fast-then-very-slow" -> Just Fluctuating
  _                     -> Nothing

getPokemonGrowthRate api@PokeAPI {..} (pok :: Pokemon) = do
  spe <- getSpeciesByName api $ pok.species.name
  getSpeciesGrowthRate spe

