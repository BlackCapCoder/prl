{-# LANGUAGE OverloadedRecordDot #-}
module Play where

import Map
import Tile
import V2
import World
import WorldMap
import Render
import Dialogue
import Picture
import UserInput
import Item
import Battle          qualified
import Pokemon.PokeAPI qualified as API
import Pokemon.Pokemon
import Pokemon.Nature
import Pokemon.Stat
import Pokemon.Type as TYPE

import Control.Monad.State
import Data.Bifunctor
import System.Console.ANSI (getTerminalSize)
import System.Exit
import System.IO
import System.Random
import Data.ByteString.Builder qualified as BS
import Data.IntMap             qualified as IM
import Data.Map                qualified as M
import Data.Text               qualified as Text
import Weighted                qualified


-- After an encounter, minimum number of turns before the next encounter
encounterCooldown = 10

-- Percentage chance of gettting an encounter
encounterRate = 10

-- (pokemonID, (min_level, max_level))
encounterTable :: Weighted.Weighted (ID, (Int, Int))
encounterTable = Weighted.Weighted
  [ 1 --> (1, (3, 7))
  , 3 --> (4, (3, 7))
  , 1 --> (7, (3, 7))
  -- , 10 --> (9, (100, 100))
  ]

----

initialWorld = do
  api <- API.getPokeAPI

  mon <- createPokemon 0 api 1 (5, 5)

  Just (h, w) <- getTerminalSize
  let world = World
        { wm         = worldMap
        , pl         = Pos 4 (V2 4 3)
        -- , pl         = Pos 7 (V2 20 10)
        , menuCursor = 0
        , twidth     = w
        , theight    = h
        , npcs       = initialNpcs
        , money      = 1000
        , bagItems   = mempty & IM.insert 0 1
        , pcItems    = mempty & IM.insert 1 1
        , api        = api
        , party      = [mon { status=Nothing }]
        , respawnLoc = pl world
        , encounterGraze = 0
        , unique = 1
        }
  pure world

----

(-->) = (,) ; infixr 0 -->

initialNpcs = M.fromList
  [ Pos 8 (V2 11 2) --> pokecenterDialogue
  , Pos 9 (V2 4  2) --> pokemartDialogue
  , Pos 5 (V2 3  6) --> nag "Hi there"
  , Pos 3 (V2 9  2) --> oakDialogue
  ]

----

pokecenterDialogue = do
  lift $ modify \w -> w { respawnLoc = pl w }
  nag "Welcome to the pokemon center"
  say "Do you want me to heal your Pokemon?"
  noyes end do
    nag "I'll take your Pokemon for a minute"
    nag "..."
    lift healParty
    nag "Thank you for waiting!"
  nag "I hope to see you again!"

----

oakDialogue = do
  nag "What are you waiting for?"
  nag "Grab a Pokemon already!"

----

pokemartDialogue = do
  nag "Welcome to the Pokemart"
  buySellDialogue

buySellDialogue = do
  say "How may I help you?"
  select
    [ "Buy" --> do
      let stock = mapMaybe findItemByName ["Pokeball", "Potion", "Repel"]
      buyDialogue stock
      buySellDialogue
    , "Sell" --> do
      sellDialogue
      buySellDialogue
    ]

buyDialogue (inv :: [(Int, Item)]) = go
  where
    w = maximum $ map (\(_, item) -> length item.name) inv

    inv' = inv <&> \(k, item) ->
      (item.name <> replicate (w - length item.name) ' ' <> "    $" <> show item.price, buy k item)

    buy id item = do
      cash <- lift $ gets money
      let max = div cash item.price

      if max < 1 then do
        nag "You can't afford that!"
      else do
        say $ "How many " <> item.name <> "s do you want?"
        number 0 max \n -> do
          unless (n < 1) do
          let total = item.price * n
          let plural = if n > 1 then "s" else ""
          say $ "Buy " <> show n <> " " <> item.name <> plural <> " for $" <> show total <> "?"
          noyes end do
          lift $ modify \World {..} -> World
            { money = money - total
            , bagItems = IM.unionWith (+) bagItems $ IM.singleton id n
            , ..
            }
      go

    go = do
      cash <- lift $ gets money
      say $ "You have $" <> show cash <> ". What would you like to buy?"
      select inv'

sellDialogue = do
  items <- lift $ gets bagItems
  if null items then nag "You have no items to sell!" else do

  let inv = IM.assocs items
          & mapMaybe \(id, cnt) -> (id,,cnt) <$> findItemByID id
  let w = maximum $ inv <&> \(_, item, _) -> length item.name

  let inv' = inv <&> \(k, item, cnt) -> (,(k,item,cnt)) $ concat
        [ item.name
        , replicate (w - length item.name) ' '
        , "    "
        , show cnt
        , "    $"
        , show (round $ fi item.price * storeMargin)
        ]

  cash <- lift $ gets money
  say $ "You have $" <> show cash <> ". What would you like to sell?"

  choose inv' >>= \case
    Nothing -> end
    Just (id, item, cnt) -> do
      say $ "How many " <> item.name <> "s would you like to sell?"
      number 0 cnt \n -> when (n > 0) do
        let total = n * round do fi item.price * storeMargin
        let plural = if n > 1 then "s" else ""
        say $ "Sell " <> show n <> " " <> item.name <> plural <> " for $" <> show total <> "?"
        noyes end do
        lift $ modify \World{..} -> World
          { money = money + total
          , bagItems = if n == cnt
              then IM.delete id bagItems
              else IM.insert id (cnt-n) bagItems
          , ..
          }
      sellDialogue

----

pcDialogue = do
  say "You booted up the PC"
  select
    [ "Pokemon" --> pcPokemonDialogue *> pcDialogue
    , "Items"   --> pcItemsDialogue *> pcDialogue
    , "Exit"    --> end
    ]

--

pcItemsDialogue = do
  say "What do you want to do?"
  select
    [ "Withdraw" --> pcWithdrawDialogue *> pcItemsDialogue
    , "Deposit"  --> pcDepositDialogue  *> pcItemsDialogue
    , "Exit"     --> end
    ]

pcWithdrawDialogue = do
  items <- lift $ gets pcItems
  if null items then nag "You have no items to withdraw!" else do
  say "Select an item to withdraw"
  pickItem items >>= \case
    Nothing -> end
    Just (id, item, cnt) -> do
      say $ "Withdraw how many " <> item.name <> "s?"
      number 0 cnt \n -> do
        when (n > 0) do
        lift $ modify \World {..} -> World
          { pcItems = if n == cnt
              then IM.delete id pcItems
              else IM.insert id (cnt-n) pcItems
          , bagItems = IM.unionWith (+) bagItems $ IM.singleton id n
          , ..
          }
        end
      pcWithdrawDialogue

pcDepositDialogue = do
  items <- lift $ gets bagItems
  if null items then nag "You have no items to deposit!" else do
  say "Select an item to deposit"
  pickItem items >>= \case
    Nothing -> end
    Just (id, item, cnt) -> do
      say $ "Deposit how many " <> item.name <> "s?"
      number 0 cnt \n -> do
        when (n > 0) do
        lift $ modify \World {..} -> World
          { bagItems = if n == cnt
              then IM.delete id bagItems
              else IM.insert id (cnt-n) bagItems
          , pcItems = IM.unionWith (+) pcItems $ IM.singleton id n
          , ..
          }
        end
      pcDepositDialogue

pickItem bag = do
  let items = flip mapMaybe (IM.assocs bag) \(id, n) -> do
        item <- findItemByID id
        pure (id, item, n)

  let w = maximum $ items <&> \(_, item, _) -> length item.name

  let ls =
        [ (item.name <> replicate (w - length item.name) ' ' <> "    " <> show cnt, x)
        | x@(_, item, cnt) <- items
        ]

  choose ls

--

pcPokemonDialogue = do
  say "What do you want to do?"
  select
    [ "Withdraw" --> pcWithdrawPokemonDialogue *> pcPokemonDialogue
    , "Deposit"  --> pcDepositPokemonDialogue  *> pcPokemonDialogue
    , "Exit"     --> end
    ]

pcWithdrawPokemonDialogue = do
  nag "TODO: Withdraw pokemon"

pcDepositPokemonDialogue = do
  nag "TODO: Deposit pokemon"

----

mainMenu = do
  World {..} <- lift get
  select $ concat
    [ [ "Pokemon" --> pokemonMenu *> mainMenu
      | not $ null party
      ]
    , [ "Bag"     --> viewBag
      , "Money"   --> nag $ "You have $" <> show money
      , "Debug"   --> debugMenu
      , "Exit"    --> end
      ]
    ]

debugMenu = DSelect
  [ "Give $1000" --> lift $ modify \w -> w { money = money w + 1000 }
  , "Heal party" --> lift healParty
  ] mainMenu

viewBag = do
  items <- lift $ gets bagItems
  if null items then nag "You have no items!" else do
  say "Select an item"
  pickItem items >>= \case
    Nothing -> end
    Just (id, item, cnt) -> do
      let plural = if cnt > 1 then "s" else ""
      say $ "What do you want to do with your " <> item.name <> plural <> "?"
      select
        [ "Use"  --> do
            nag "TODO: Use item"
            viewBag
        , "Toss" --> do
            say $ "How many " <> item.name <> "s do you want to toss?"
            number 0 cnt \n -> when (n > 0) do
              lift $ modify \World{..} -> World
                { bagItems = if n == cnt
                    then IM.delete id bagItems
                    else IM.insert id (cnt - n) bagItems
                , ..
                }
            viewBag
        , "Info" --> do
            viewBag
        ]

----

pokemonMenu = do
  World {..} <- lift get

  xs <- lift $ forM (zip [0..] party) \(ix, mon) -> do
    let Just pok = IM.lookup mon.id api.pokemon
    let name | Just n <- mon.nickname = n
             | otherwise              = Text.unpack pok.name
    let stats = getStats api mon
    pure $ (,ix) $ unwords
      [ name
      , "Lv"  <> show mon.level
      , show mon.hp <> "/" <> show stats.hp
      , maybe "" statusAbriv mon.status
      ]

  choose xs >>= \case
    Nothing -> pure ()
    Just ix -> do
      lift $ draw $ picStats api $ party !! ix
      void $ lift $ liftIO acceptInput

  pure ()

----

getUnique = do
  World {..} <- get
  put World { unique = succ unique, .. }
  pure (unique :: UID)

play = evalStateT play'

play' = do
  w <- get

  draw =<< overworld

  liftIO getOverworldInput >>= \case
    Quit     -> pure ()
    OpenMenu -> runDialogue mainMenu *> play'
    Test     -> test *> play'
    Move mx my -> do
      let pl' = Pos (mapID (pl w)) (pos (pl w) + V2 mx my)

      case M.lookup pl' (npcs w) of
        Just d  -> runDialogue d
        Nothing ->
          case lookupMaps (mapID pl') (x (pos pl')) (y (pos pl')) (wm w) of
            Just (Portal _ p) -> do
              put w { pl = p }
            Just t | not (isSolid t) -> do
              put w { pl = pl' }
              moved
              case t of
                Grass -> walkedInGrass
                _     -> pure ()
            Just PC -> runDialogue pcDialogue
            _ -> pure ()

      play'

test = do
  whiteOut
  pure ()

moved = do
  modify \World {..} -> World
    { encounterGraze = max 0 $ pred encounterGraze
    , ..
    }

walkedInGrass = do
  World {..} <- get
  when (encounterGraze < 1) do
  roll <- randomRIO (0, 99)
  when (roll < encounterRate) do
  encounter encounterTable

encounter table = do
  liftIO (Weighted.roll table) >>= \case
    Nothing -> pure ()
    Just (pid, lvl) -> do
      api <- gets api
      uid <- getUnique
      mon <- createPokemon uid api pid lvl
      battle mon
      modify \w -> w
        { encounterGraze = encounterCooldown
        }

battle (mon :: Pokemon) = do
  World {..} <- get
  let b = Battle.newBattle twidth theight api party [mon] True
  Battle.runBattle b
  pure ()


----

-- Bulbasaur M Lv100 SLP [############--] 19/19
--
--     Base IVs EVs | PP    typ cat pwr Move
-- HP  100  31  252 |
-- Att 100+ 31  252 | 10/10 NOR sta -   Growl
-- Def 100  31  252 | 10/10 NOR phy 40  Tackle
-- SpA 100  31  252 | 10/10 GRA phy 45  Vine-whip
-- SpD 100- 31  252 | 10/10 NOR phy 40  Pound
-- Spe 100  31  252 |
--
-- Ability  Overgrow
-- Item     None
-- Exp      1000000

strMonInfo api mon = unwords $ filter (not . null)
  [ pokemonName api mon
  , genderAbriv mon.gender
  , "Lv" <> show mon.level
  , maybe "" statusAbriv mon.status
  , show mon.hp <> "/" <> show stats.hp
  ]
  where
    stats = getStats api mon

pad n str =
  str <> replicate (n - length str) ' '

strMoves api mon =
  "PP    typ cat pwr Move" : "" :
    [ unwords
      [ pad 5 $ show move.pp <> "/" <> show info.pp
      , show $ TYPE.typeFromName $ Text.unpack $ info._type.name
      , cat $ info.damage_class.name
      , pad 3 $ maybe "-" show info.power
      , Text.unpack $ info.name
      ]
    | move <- mon.moves
    , info <- maybeToList $ IM.lookup move.id api.moves
    ]
  where
    cat = \case
      "physical" -> "phy"
      "special"  -> "spe"
      _          -> "sta"

strStats api mon =
  [ "    Base IVs EVs"
  , "HP  " <> pad 5 (show sta.hp  <> sign nat.hp ) <> pad 4 (show mon.ivs.hp ) <> pad 3 (show mon.evs.hp )
  , "Att " <> pad 5 (show sta.att <> sign nat.att) <> pad 4 (show mon.ivs.att) <> pad 3 (show mon.evs.att)
  , "Def " <> pad 5 (show sta.def <> sign nat.def) <> pad 4 (show mon.ivs.def) <> pad 3 (show mon.evs.def)
  , "SpA " <> pad 5 (show sta.spA <> sign nat.spA) <> pad 4 (show mon.ivs.spA) <> pad 3 (show mon.evs.spA)
  , "SpD " <> pad 5 (show sta.spD <> sign nat.spD) <> pad 4 (show mon.ivs.spD) <> pad 3 (show mon.evs.spD)
  , "Spe " <> pad 5 (show sta.spe <> sign nat.spe) <> pad 4 (show mon.ivs.spe) <> pad 3 (show mon.evs.spe)
  ]
  where
    nat = natureBoost mon.nature
    sta = getStats api mon

    sign n = case compare n 1 of
      LT -> "-"
      EQ -> ""
      GT -> "+"

strExtra api mon =
  [ "Ability " <> Text.unpack ability.name
  , "Item    " <> maybe "None" (\i -> Text.unpack i.name) item
  , "Exp     " <> show mon.totalExp
  ]
  where
    Just ability = IM.lookup mon.ability api.abilities
    item = mon.heldItem >>= \id -> IM.lookup id api.items

strStatMoves api mon =
  zipWith (\l r -> l <> " | " <> r)
    do strStats api mon
    do strMoves api mon <> repeat []

picStats api mon = Pictures
  [ Text $ unlines $ strMonInfo api mon : "" : strStatMoves api mon <> [""] <> strExtra api mon
  ]

