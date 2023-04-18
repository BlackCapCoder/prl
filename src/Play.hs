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
import Pokemon.Level
import Pokemon.Type as TYPE
import Settings
import Weighted qualified

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
import StyledString
import Data.List (intercalate, intersperse)

import Data.Vector         qualified as V
import Data.Vector.Mutable qualified as MV


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

-- How often to take damage from poison in the overworld
poisonTickFreq = 10

-- How slow to hatch eggs
eggTickFreq = 10

----

initialWorld = do
  let settings = defaultSettings

  api <- API.getPokeAPI

  mon1 <- createPokemon settings 0 api 1 (15, 15)
  mon2 <- createPokemon settings 1 api 4 (5, 5)
  mon3 <- createPokemon settings 2 api 7 (5, 5)

  Just (h, w) <- getTerminalSize
  let world = World
        { wm             = worldMap
        -- , pl             = Pos 4 (V2 4 3)
        , pl             = Pos 7 (V2 20 10)
        , menuCursor     = 0
        , twidth         = w
        , theight        = h
        , npcs           = initialNpcs
        , money          = 1000
        , bagItems       = mempty & IM.insert 0 1
        , pcItems        = mempty & IM.insert 1 1
        , api            = api
        , party          = [mon1, mon2, mon3]
        , respawnLoc     = pl world
        , encounterGraze = 0
        , unique         = 3
        , settings       = settings
        , stepCount      = 0
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
    let name = pokemonName api mon
    let stats = getStats api mon
    pure $ (,ix) $ unwords
      [ name
      , "Lv"  <> show mon.level
      , show mon.hp <> "/" <> show stats.hp
      , maybe "" statusAbriv mon.status
      ]

  choose xs >>= \case
    Nothing -> pure ()
    Just ix -> select
      [ "Info" --> lift do
          party' <- pokemonInfoScreen api (V.fromList party) ix Nothing Nothing
          put World {party = V.toList party', ..}
      , "Switch" --> lift do
          let (ls, r:rs) = splitAt ix party
          put World {party = r:ls ++ rs, ..}
      ]

  pure ()

pokemonInfoScreen api party cur0 mcur1 mcur2 = do
  let cur = mod cur0 $ V.length party
  draw $ picStats api (party V.! cur) mcur1 mcur2

  inp <- liftIO getMenuInput

  case mcur1 of
    Nothing -> case inp of
      CursorUp   -> pokemonInfoScreen api party (pred cur) mcur1 mcur2
      CursorDown -> pokemonInfoScreen api party (succ cur) mcur1 mcur2
      Select     -> pokemonInfoScreen api party cur (Just 0) mcur2
      Cancel     -> pure party
    Just cur1
      | let f i = mod i $ length $ (party V.! cur).moves ->
      case mcur2 of
        Nothing -> case inp of
          CursorUp   -> pokemonInfoScreen api party cur (Just $ f $ pred cur1) mcur2
          CursorDown -> pokemonInfoScreen api party cur (Just $ f $ succ cur1) mcur2
          Cancel     -> pokemonInfoScreen api party cur Nothing mcur2
          Select     -> pokemonInfoScreen api party cur mcur1 (Just cur1)
        Just cur2 -> case inp of
          CursorUp   -> pokemonInfoScreen api party cur mcur1 (Just $ f $ pred cur2)
          CursorDown -> pokemonInfoScreen api party cur mcur1 (Just $ f $ succ cur2)
          Cancel     -> pokemonInfoScreen api party cur mcur1 Nothing
          Select     -> do
            let v1 = V.fromList $ (party V.! cur).moves
            let v2 = V.update v1 $ V.fromList [(cur1, v1 V.! cur2), (cur2, v1 V.! cur1)]
            let moves' = V.toList v2
            let mon' = (party V.! cur) { moves = moves' }
            let party' = V.update party $ V.fromList [(cur, mon')]

            pokemonInfoScreen api party' cur mcur1 Nothing

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
                _     -> tickSteps
            Just PC -> runDialogue pcDialogue
            _ -> pure ()

      play'

test = do
  whiteOut
  pure ()

moved = do
  modify \World {..} -> World
    { encounterGraze = max 0 $ pred encounterGraze
    , stepCount      = succ stepCount
    , ..
    }

walkedInGrass = do
  World {..} <- get
  when (encounterGraze < 1) do
  roll <- randomRIO (0, 99)
  if roll < encounterRate then
    encounter encounterTable
  else
    tickSteps

encounter table = do
  liftIO (Weighted.roll table) >>= \case
    Nothing -> pure ()
    Just (pid, lvl) -> do
      api <- gets api
      set <- gets settings
      uid <- getUnique
      mon <- createPokemon set uid api pid lvl
      oldParty <- gets party
      battle mon
      newParty <- gets party
      modify \w -> w
        { encounterGraze = encounterCooldown
        }

battle (mon :: Pokemon) = do
  World {..} <- get
  let b = Battle.newBattle api party [mon] True

  let oldParty = party
  Battle.runBattle b
  newParty <- gets (.party)

  -- give back held items
  when settings.keepItems do
    modify \w -> w { party = restoreHeldItems party w.party }

  -- Level-Up evolutions
  newParty' <- forM newParty \mon2 -> do
    if oldParty & any \mon1 -> mon1.uid == mon2.uid && mon1.level < mon2.level
       then evolutionCheck mon2
       else pure mon2

  modify \w -> w { party = newParty' }

evolutionCheck mon1 = do
  World {..} <- get
  case levelUpEvolution api mon1 of
    Nothing -> pure mon1
    Just mon2 -> runDialogue do
      say $ "Evolve " <> pokemonName api mon1 <> "?"
      noyes (pure mon1) do
        pure mon2

restoreHeldItems old0 new0 = foldr go new0 old0 where
  go old news
    | Just item <- old.heldItem = put old.uid (Just item) news
    | otherwise = news

  put uid item (mon:ms)
    | mon.uid == uid = mon {heldItem=item} : ms
    | otherwise = mon : put uid item ms
  put _ _ ms = ms


tickSteps = do
  tickEggs
  tickPoison

tickPoison = do
  World {..} <- get
  when (mod stepCount poisonTickFreq == 0) do

  case settings.poisonInOverworld of
    Nothing   -> pure ()
    Just kill -> do
      let party' = go kill party
      put World { party = party', .. }
      when (all (\mon -> mon.hp == 0) party') whiteOut

  where
    go kill (mon:ms)
      | mon.status /= Just Poison = mon : go kill ms
      | mon.hp > 1 = mon { hp = pred mon.hp, status=mon.status } : go kill ms
      | kill       = mon { hp = 0, status=Nothing } : go kill ms
      | otherwise  = mon { hp = 1, status=Nothing } : go kill ms
    go _ ms = ms

tickEggs = do
  World {..} <- get
  when (mod stepCount eggTickFreq == 0) do
  party' <- forM party \mon -> do
    when (mon.eggCycles == 1) do
      let Just pok = IM.lookup mon.id api.pokemon
      let name     = Text.unpack pok.name
      runDialogue $ nag $ "Your egg hatched into a " <> name <> "!"
    pure mon { eggCycles = max 0 $ pred mon.eggCycles }
  put World {party = party', ..}

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

sstrMonInfo api mon =
  sstr $ strMonInfo api mon

pad n str =
  str <> replicate (n - length str) ' '

spad n (StyledString str) =
  StyledString str <> StyledString (replicate (n - length str) mempty)

sstrMoves api mon cur1 cur2 =
  "PP    typ cat pwr Move" : "" :
    [ sty $ mconcat $ intersperse " "
      [ sstr $ pad 5 $ show move.pp <> "/" <> show info.pp
      , fg (typeColor ty) $ sshow ty
      , cat info.damage_class.name
      , sstr $ pad 3 $ maybe "-" show info.power
      , sstr $ Text.unpack $ info.name
      ]
    | move <- mon.moves
    , info <- maybeToList $ IM.lookup move.id api.moves
    , let ty = maybe NON (\t -> TYPE.typeFromName t.name) info._type
    | i <- [0..]
    , let sty | Just j <- cur1, i==j = style Picture.Bold
              | Just j <- cur2, i==j = style Picture.Bold
              | let = Prelude.id
    ]
  where
    cat = \case
      "physical" -> "phy"
      "special"  -> "spe"
      _          -> "sta"

sstrStats api mon =
  [ "    Base IVs EVs"
  , "HP  " <> spad 5 (sshow sta.hp  <> sign nat.hp ) <> spad 4 (sshow mon.ivs.hp ) <> spad 3 (sshow mon.evs.hp )
  , "Att " <> spad 5 (sshow sta.att <> sign nat.att) <> spad 4 (sshow mon.ivs.att) <> spad 3 (sshow mon.evs.att)
  , "Def " <> spad 5 (sshow sta.def <> sign nat.def) <> spad 4 (sshow mon.ivs.def) <> spad 3 (sshow mon.evs.def)
  , "SpA " <> spad 5 (sshow sta.spA <> sign nat.spA) <> spad 4 (sshow mon.ivs.spA) <> spad 3 (sshow mon.evs.spA)
  , "SpD " <> spad 5 (sshow sta.spD <> sign nat.spD) <> spad 4 (sshow mon.ivs.spD) <> spad 3 (sshow mon.evs.spD)
  , "Spe " <> spad 5 (sshow sta.spe <> sign nat.spe) <> spad 4 (sshow mon.ivs.spe) <> spad 3 (sshow mon.evs.spe)
  ]
  where
    nat = natureBoost mon.nature
    sta = getStats api mon

    sign n = case compare n 1 of
      LT -> fg (RGB 96 96 255) "-"
      EQ -> ""
      GT -> fg (RGB 255 0 0) "+"

sstrExtra api mon =
  [ "Ability " <> sstr do Text.unpack ability.name
  , "Item    " <> sstr do maybe "None" (\i -> Text.unpack i.name) item
  , "Exp     " <> sstr do show mon.totalExp <> "/" <> show expNext
  ]
  where
    Just ability = IM.lookup mon.ability api.abilities
    item = mon.heldItem >>= \id -> IM.lookup id api.items

    Just pok = IM.lookup mon.id api.pokemon
    Just gro = API.getPokemonGrowthRate api pok

    expNext = totalExpAtLevel gro $ min 100 $ succ $ mon.level

sstrStatMoves api mon cur1 cur2 =
  zipWith (\l r -> l <> sstr " | " <> r)
    do sstrStats api mon
    do sstrMoves api mon cur1 cur2 <> repeat mempty

picStats api mon cur1 cur2 = Pictures
  [ slines2pic $ sstrMonInfo api mon : mempty : sstrStatMoves api mon cur1 cur2
              <> [mempty] <> sstrExtra api mon
  -- , Text $ unlines $ strMonInfo api mon : "" : strStatMoves api mon <> [""] <> strExtra api mon
  ]

typeColor = \case
  NON -> RGB 255 255 255
  NOR -> RGB 255 255 255
  FIR -> RGB 255 0   0
  WAT -> RGB 0   255 255
  ELE -> RGB 255 255 0
  GRA -> RGB 0   255 0
  ICE -> RGB 135 206 255
  FIG -> RGB 139 69  19
  POI -> RGB 128 0   128
  GRO -> RGB 210 180 140
  FLY -> RGB 135 206 250
  PSY -> RGB 255 0   255
  BUG -> RGB 144 238 144
  ROC -> RGB 160 82  45
  GHO -> RGB 75  0   130
  DRA -> RGB 75  0   130
  DAR -> RGB 75  0   130
  STE -> RGB 192 192 192
  FAI -> RGB 255 20  147

