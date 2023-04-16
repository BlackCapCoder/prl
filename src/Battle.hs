module Battle
  ( module Battle.Type
  , runBattle
  )
  where

import Battle.Type
import Battle.BasicAttack
import Battle.Move
import Pokemon.Nature
import Pokemon.PokeAPI qualified as API
import Pokemon.Pokemon
import Pokemon.Type as TYPE
import Pokemon.Stat
import Pokemon.Level
import Pokemon.Move qualified as Pok
import Settings
import System.Random
import Prelude hiding (Field)
import Control.Monad.State
import Picture
import ANSI
import Pokemon.Move qualified as Move
import UserInput
import System.IO
import Data.ByteString.Builder qualified as BS
import Data.Text qualified as Text
import Data.IntMap qualified as IM
import World qualified
import Data.List (elem)
import Data.List qualified as L
import StyledString
import Data.Vector qualified as V
import Data.Text qualified as T

----

healthBarWidth    = 40
redBarTreshold    = 0.25
yellowBarTreshold = 0.5

----

runBattle =
  evalStateT battle

battle = do
  World.World {..} <- lift get
  Battle {..} <- get

  when isWild do
    tell $ "A wild " <> pokemonName api mon2.pokemon <> " appeared!"

  do let name = "You"
     tell $ name <> " sent out " <> pokemonName api mon1.pokemon

  mainLoop

  pause
  flushMon
  flushParty

  Battle {party1} <- get
  let loss = all (\m -> m.hp == 0) party1

  when loss do
    lift World.whiteOut

----

data BattleAction
   = UseMove API.Move
   | Switch  Pokemon
   | UseItem ID
   | Run

data BattleResult
   = Won
   | Lost
   | RanAway
   deriving Eq


mainLoop = do
  World.World {settings, api} <- lift get
  Battle {..} <- get

  -- Select actions
  userAction <- selectAction
  foeAction  <- UseMove <$> selectRandomMove mon2

  -- TODO: handle speed

  res1 <- handleBattleAction userAction

  case res1 of

    Just RanAway ->
      tell "You successfully ran away"

    Just Lost -> do
      continue <- onAllyDefeated
      when continue endOfTurn

    Just Won -> do
      continue <- onFoeDefeated
      when continue endOfTurn

    Nothing -> do
      pause
      swapField
      res2 <- handleBattleAction foeAction
      swapField

      case res2 of
        Just RanAway -> do
          tell $ "The opposing " <> pokemonName api mon2.pokemon <> " ran away!"
        Just Lost -> do
          continue <- onFoeDefeated
          when continue endOfTurn
        Just Won -> do
          continue <- onAllyDefeated
          when continue endOfTurn
        Nothing ->
          endOfTurn

----

onFoeDefeated = do
  World.World {settings, api} <- lift get
  Battle      {..}            <- get

  tell $ "The opposing " <> pokemonName api mon2.pokemon <> " fainted"

  -- Grant expreience
  when grantExperience do
    giveExp

  -- Grant EVs
  unless settings.noEVs do
    let Just evs = API.getEvYield api mon2.pokemon.id
    modify \b -> b { mon1 = b.mon1 { pokemon = addEVs evs b.mon1.pokemon } }

  flushMon
  pure False -- continue?

onAllyDefeated = do
  flushMon

  Battle {mon1,party1} <- get
  World.World {settings, api} <- lift get

  let continue = not $ all (\m -> m.hp == 0) party1

  tell $ pokemonName api mon1.pokemon <> " fainted"

  when continue do
    pause
    forcedSwitch

  pure continue

forcedSwitch = do
  World.World {settings, api} <- lift get

  Just mon <- selectSwitch False

  modify \w -> w
    { mon1       = newBattleMon api mon
    , moveCursor = 0
    }

  tell $ pokemonName api mon <> " was sent out!"

endOfTurn = do
  Battle {mon1,mon2} <- get
  mon1' <- tickDrowsy mon1
  mon2' <- tickDrowsy mon2

  modify \Battle {..} -> Battle
    { mon1 = tickTurnMon $ tickCurse mon1'
    , mon2 = tickTurnMon $ tickCurse mon2'
    , ..
    }

  statusDamage
  swapField *> statusDamage *> swapField

  perishTick
  swapField *> perishTick *> swapField

  -- TODO: pokemon may have died

  modify \Battle {..} -> Battle
    { field = tickTurnField field
    , ..
    }

  mainLoop

----

tickDrowsy mon
  | mon.drowsy && isNothing mon.pokemon.status = pure mon
  | otherwise = do
      World.World {settings, api} <- lift get
      tell $ pokemonName api mon.pokemon <> " fell asleep"
      turns <- liftIO $ randomRIO (1, 3)
      pure mon { pokemon = mon.pokemon { status = Just (Sleep turns) } }

tickCurse mon
  | not mon.cursed = mon
  | otherwise      = mon
    { pokemon = mon.pokemon
      { hp = max 0 $ mon.pokemon.hp - max 1 (div mon.stats.hp 4)
      }
    }

tickTurnMon mon = mon
  { enduring    = False
  , protected   = False
  , wideGuard   = False
  , flinched    = False
  , snatching   = False
  , drowsy      = False
  , taunt       = max 0 $ pred mon.taunt
  , torment     = max 0 $ pred mon.torment
  , healBlock   = max 0 $ pred mon.healBlock
  , luckyChant  = max 0 $ pred mon.luckyChant
  , dynamax     = max 0 $ pred mon.dynamax
  -- , confusion   = max 0 $ pred mon.confusion
  , embargo     = max 0 $ pred mon.embargo
  , destinyBond = max 0 $ pred mon.destinyBond
  , magnetRise  = max 0 $ pred mon.magnetRise
  , perishCount = mon.perishCount <&> pred
  , encore = case mon.encore of
      Nothing      -> Nothing
      Just (_, 1)  -> Nothing
      Just (i, n)  -> Just (i, pred n)
  , disabled = goDisabled mon.disabled
  }
  where
    goDisabled [] = []
    goDisabled ((i,n):as)
      | n == 1 = goDisabled as
      | let    = (i, pred n) : goDisabled as

tickTurnLane l = l
  { lightScreen = max 0 $ pred l.lightScreen
  , reflect     = max 0 $ pred l.reflect
  , auroraveil  = max 0 $ pred l.auroraveil
  , tailwind    = max 0 $ pred l.tailwind
  , safeguard   = max 0 $ pred l.safeguard
  }

tickTurnField f = f
  { lane1      = tickTurnLane f.lane1
  , lane2      = tickTurnLane f.lane2
  , trickRoom  = max 0 $ pred f.trickRoom
  , magicRoom  = max 0 $ pred f.magicRoom
  , wonderRoom = max 0 $ pred f.wonderRoom
  , mudSport   = max 0 $ pred f.mudSport
  , gravity    = max 0 $ pred f.gravity
  , weather    = may f.weather
  , terrain    = may f.terrain
  }
  where
    may = \case
      Nothing     -> Nothing
      Just (i, 1) -> Nothing
      Just (i, n) -> Just (i, pred n)

statusDamage = do
  Battle {..} <- get
  let maxhp = mon1.stats.hp
  case mon1.pokemon.status of
    Just Burn      -> editHP \h -> max 0 $ h - max 1 do div maxhp 16
    Just Poison    -> editHP \h -> max 0 $ h - max 1 do div maxhp 8
    Just (Toxic n) -> do
      put Battle {mon1 = mon1 {pokemon = mon1.pokemon { status = Just (Toxic (succ n)) }}, ..}
      -- TODO: Poison heal
      editHP \h -> max 0 $ h - max 1 do div (maxhp*(n+1)) 16
    _ -> pure ()

perishTick = do
  Battle {..} <- get
  when (mon1.perishCount == Just 0) do
  editHP (const 0)

editHP f = do
  modify \b -> b {mon1 = b.mon1 { pokemon = b.mon1.pokemon { hp=f b.mon1.pokemon.hp } }}

----

handleBattleAction act = do
  World.World {settings, api} <- lift get

  case act of
    Run ->
      pure $ Just RanAway

    UseItem item ->
      pure Nothing -- Just <$> useItem item

    Switch  mon  -> do
      Battle {mon1} <- get
      tell $ pokemonName api mon1.pokemon <> " withdrew!"

      modify \b -> b { mon1 = b.mon1 { pokemon = cureToxic b.mon1.pokemon } }
      flushMon
      modify \w -> w
        { mon1       = newBattleMon api mon
        , moveCursor = 0
        }

      tell $ pokemonName api mon <> " was sent out!"
      pure Nothing

    UseMove move -> do
      performMove move
      Battle {..} <- get

      pure if
        | mon1.pokemon.hp <= 0 -> Just Lost
        | mon2.pokemon.hp <= 0 -> Just Won
        | otherwise            -> Nothing

----

pause =
  drawBattle 0 [] *> liftIO acceptInput

tell msg = do
  modify \w -> w { log = msg : w.log }

flushMon = do
  Battle {..} <- get
  modify \b -> b { party1 = updateParty mon1.pokemon b.party1 }

flushParty = do
  Battle {..} <- get
  lift do
    forM_ (updateParty mon1.pokemon party1) \p1 -> do
      modify \w -> w { World.party = updateParty (cureToxic p1) w.party }

updateParty p [] = []
updateParty p (x:xs)
  | x.uid == p.uid = updateMon p x : xs
  | otherwise      = x : updateParty p xs

updateMon p1 p2 = p2
  { hp       = p1.hp
  , status   = p1.status
  , evs      = p1.evs
  , totalExp = p1.totalExp
  , level    = p1.level
  , moves    = p1.moves -- TODO: Transform/mimic may change moves!
  }

cureToxic p = case p.status of
  Just (Toxic _) -> p { status = Just Poison }
  _ -> p

swapField = do
  modify \w -> w
    { mon1 = w.mon2
    , mon2 = w.mon1
    , party1 = w.party2
    , party2 = w.party1
    , field  = w.field
      { lane1 = w.field.lane2
      , lane2 = w.field.lane1
      }
    }

----

selectAction = do
  Battle {..} <- get

  let cur = actionCursor

  drawBattle cur ["Attack", "Bag", "Switch", "Run"]

  liftIO getBattleMenuInput >>= \case
    North | cur == 2 || cur == 3 -> put Battle {actionCursor = cur-2, ..} *> selectAction
    West  | cur == 1 || cur == 3 -> put Battle {actionCursor = cur-1, ..} *> selectAction
    East  | cur == 0 || cur == 2 -> put Battle {actionCursor = cur+1, ..} *> selectAction
    South | cur == 0 || cur == 1 -> put Battle {actionCursor = cur+2, ..} *> selectAction
    BSelect
      | 0 <- cur -> selectMove
      | 1 <- cur -> pure $ UseItem (-1)
      | 2 <- cur -> selectSwitch True >>= \case
          Just mon -> pure (Switch mon)
          Nothing  -> selectAction
      | 3 <- cur -> pure Run
    BCancel
      | isWild -> put Battle {actionCursor = 3, ..} *> selectAction
    _ -> selectAction

selectMove = do
  World.World {..} <- lift get
  Battle {..} <- get

  let cur = moveCursor
  let moves = mapMaybe (\m -> (, "[" <> show m.pp <> "] ") <$> do
          IM.lookup m.id api.moves <* guard (m.pp > 0)
        ) mon1.pokemon.moves

  let moves'
        | null moves = maybeToList $ IM.lookup struggleID api.moves <&> (,"")
        | otherwise  = moves

  let opts = moves' <&> \(m,pp) -> pp <> Text.unpack (moveName m)

  let canSelect n = n < length opts

  drawBattle cur opts

  liftIO getBattleMenuInput >>= \case
    North | cur == 2 || cur == 3, canSelect (cur-2) -> put Battle {moveCursor = cur-2, ..} *> selectMove
    West  | cur == 1 || cur == 3, canSelect (cur-1) -> put Battle {moveCursor = cur-1, ..} *> selectMove
    East  | cur == 0 || cur == 2, canSelect (cur+1) -> put Battle {moveCursor = cur+1, ..} *> selectMove
    South | cur == 0 || cur == 1, canSelect (cur+2) -> put Battle {moveCursor = cur+2, ..} *> selectMove
    BCancel -> selectAction
    BSelect -> pure $ UseMove $ fst $ moves' !! cur
    _       -> selectMove

selectSwitch canCancel = do
  World.World {..} <- lift get
  Battle      {..} <- get

  let f mon = mon.uid /= mon1.pokemon.uid && mon.hp > 0
  let mons  = V.fromList $ filter f party1

  if null mons then pure Nothing else do

  0 & fix \loop cur0 -> do
    let cur = mod cur0 (V.length mons)
    let f i | i == cur = style Picture.Bold | let = Prelude.id
    draw $ slines2pic
      [ f i $ sstr $ pokemonName api $ mons V.! i
      | i <- [0..V.length mons - 1]
      ]

    liftIO getMenuInput >>= \case
      CursorUp   -> loop (pred cur0)
      CursorDown -> loop (succ cur0)
      Select     -> pure $ Just $ mons V.! cur
      Cancel
        | canCancel -> pure Nothing
        | otherwise -> loop cur0

----

performMove move = do
  World.World {..} <- lift get
  Battle {..} <- get

  -- flinch
  if mon1.flinched
  then do
    tell $ pokemonName api mon1.pokemon <> " flinched and couldn't move!"
  else do

  -- statuses that might prevent the move
  case mon1.pokemon.status of
    Just (Sleep n)
      | n < 2 -> do
        put Battle
          { mon1 = mon1
            { pokemon = mon1.pokemon
              { status = Nothing
              }
            }
          , ..
          }
        tell $ pokemonName api mon1.pokemon <> " woke up!"
        performMove' move
      | let -> do
          -- TODO: sleep talk, snore
          put Battle
            { mon1 = mon1
              { pokemon = mon1.pokemon
                { status = Just (Sleep (pred n))
                }
              }
            , ..
            }
          tell $ pokemonName api mon1.pokemon <> " is fast asleep"

    Just Paralysis -> do
      roll <- liftIO $ randomRIO (0.0, 1.0)
      if roll <= settings.paralysisChance then do
        tell $ pokemonName api mon1.pokemon <> " couldn't move because it's paralyzed!"
      else do
        performMove' move

    Just Freeze -> do
      thawRoll <- liftIO $ randomRIO (0.0, 1.0)

      -- TODO: Moves that unfreeze the user
      let thaw = thawRoll <= settings.thawChance

      if not thaw then do
        tell $ pokemonName api mon1.pokemon <> " is frozen"
      else do
        put Battle
          { mon1 = mon1
            { pokemon = mon1.pokemon
              { status = Nothing
              }
            }
          , ..
          }
        tell $ pokemonName api mon1.pokemon <> " thawed!"
        performMove' move

    _ -> performMove' move

performMove' move = do
  World.World {..} <- lift get
  Battle {..} <- get

  -- confusion
  case compare mon1.confusion 0 of
    LT -> performMove'' move
    EQ -> do
      put Battle { mon1 = mon1 { confusion = -1 }, .. }
      tell $ pokemonName api mon1.pokemon <> " snapped out of its confusion"
      performMove'' move
    GT -> do
      put Battle { mon1 = mon1 { confusion = mon1.confusion - 1 }, .. }
      tell $ pokemonName api mon1.pokemon <> " is confused " <> show mon1.confusion
      roll <- liftIO $ randomRIO (0.0, 1.0)
      if roll <= settings.confusionChance then do
        tell $ pokemonName api mon1.pokemon <> " hit itself in its confusion!"
        confusionHit
      else do
        performMove'' move

confusionHit = do

  -- This incorrectly respect things like reflect and attack-boosting abilities
  --
  -- modify \w -> w { mon1 = w.mon2, mon2 = w.mon1 }
  -- runBasicAttackResult =<< basicAttack struggleID NON True 40 mon1 mon1 True
  -- modify \w -> w { mon1 = w.mon2, mon2 = w.mon1 }

  Battle {..} <- get

  let att = fi mon1.stats.att * boostMult mon1.boosts.att
  let def = fi mon1.stats.def * boostMult mon1.boosts.def
  let dmg = max 1 $ round $ damageBase mon1.pokemon.level 40 (round att) (round def)

  put Battle
    { mon1 = mon1
      { pokemon = mon1.pokemon
        { hp = max 0 $ mon1.pokemon.hp - dmg
        }
      }
    , ..
    }

performMove'' move = do
  -- attract

  World.World {..} <- lift get
  Battle {..} <- get

  let Just m = IM.lookup move.id api.moves
  tell $ pokemonName api mon1.pokemon <> " used " <> Text.unpack (moveName m)

  modifyMove move \m -> m { pp = max 0 (pred m.pp) }

  let cat = case m.damage_class.name of
        "physical" -> Move.Physical
        "special"  -> Move.Special
        _          -> Move.Status

  let ty = maybe NON (\t -> TYPE.typeFromName t.name) m._type

  when (cat /= Move.Status) do
    runBasicAttackResult =<< basicAttack move.id ty (cat == Move.Physical) 50 mon1 mon2

  case move2moveDesc move of
    Nothing -> pure ()
    Just md -> do
      tell $ "Effect: " <> show md.move.eff
      runEffect md.move.eff

  pure ()

modifyMove move f = do
  modify \Battle {..} -> Battle
    { mon1 = mon1
      { pokemon = mon1.pokemon
        { moves = go mon1.pokemon.moves
        }
      }
    , ..
    }
  where
    go [] = []
    go (m:ms)
      | m.id == move.id = f m : ms
      | otherwise       = m : go ms

----

capture = do
  Battle {..} <- get
  lift $ modify \w -> w { World.party = World.party w <> [mon2.pokemon] }
  pure Won

----

selectRandomMove mon = do
  World.World {..} <- lift get
  Battle {..} <- get

  let moves = mapMaybe (\m -> IM.lookup m.id api.moves <* guard (m.pp > 0)) mon.pokemon.moves

  if null moves
    then pure $ fromJust $ IM.lookup struggleID api.moves
    else do
    roll <- liftIO $ randomRIO (0, length moves - 1)
    pure $ moves !! roll

----

drawBattle cur opts = do
  World.World {..} <- lift get
  Battle {..} <- get

  let (w1, info1) = monInfo api False healthBarWidth mon2
  let (w2, info2) = monInfo api True  healthBarWidth mon1

  draw $ Pictures
    [ info1
    , Translate (fi (twidth-w2)) 2 info2
    , Translate 0 4
    $ battleMenuPic twidth cur opts
    , Translate 0 9
    $ battleLog (theight-9) log
    ]

----

draw pic = do
  World.World {..} <- lift get
  Battle {..} <- get
  liftIO do
    putStr $ clearScreen <> cursorHome
    BS.hPutBuilder stdout $ drawColor 0 0 twidth theight (toF pic) <> resetStyle
    hFlush stdout

----

healthBar' width hp max dmg = mconcat
  [ "["
  , fg color $ sstr $ replicate boxes '#'
  , fg gray  $ sstr $ replicate slashes '#'
  , sstr $ replicate dashes  ' '
  , "]"
  -- , " ", sshow hp, "/", sshow max
  ]
  where
    perc = fi hp / fi max
    color
      | perc <= redBarTreshold    = RGB 255 0   0
      | perc <= yellowBarTreshold = RGB 255 255 0
      | otherwise                 = RGB 0   255 0

    gray = RGB 64 64 64

    dmgperc = fi dmg / fi max

    boxes   = ceiling $ fi (width - 2) * perc
    slashes = ceiling $ fi (width - 2) * dmgperc
    dashes  = width - 2 - boxes - slashes


healthBar width hp max dmg =
  slines2pic [healthBar' width hp max dmg]

monInfo api right w2 mon = (w3,) $ Pictures
  [ if right then Blank else Text name
  , (if right then Prelude.id else Translate (fi (length name + 1)) 0)
  $ Pictures
    [ Text lvlStr
    , Translate w1 0 $ healthBar w2 hp max dmg
    , case status of
        Nothing -> Blank
        Just s  -> Translate (w1+fi w2+1) 0
                 $ Text (statusAbriv s)
    ]
  , if not right
  then Blank
  else Translate (w1+fi w2+5) 0
     $ Text name
  ]
  where
    w1     = 6
    lvlStr = "Lv" <> show lvl

    w3 = floor w1 + w2 + 5 + length name

    hp     = mon.pokemon.hp
    max    = mon.stats.hp
    dmg    = mon.lastDamage
    lvl    = mon.pokemon.level
    status = mon.pokemon.status
    name   = pokemonName api mon.pokemon

line width =
  Text $ replicate width '-'

battleMenuPic width cur opts = Pictures
  [ line width
  , Translate 0 1 sep
  , Translate 0 2 sep
  , Translate 0 3 $ line width
  , (if cur==0 then fill else Prelude.id) $ Translate (fi x1) 1 $ Text $ get 0
  , (if cur==1 then fill else Prelude.id) $ Translate (fi x2) 1 $ Text $ get 1
  , (if cur==2 then fill else Prelude.id) $ Translate (fi x3) 2 $ Text $ get 2
  , (if cur==3 then fill else Prelude.id) $ Translate (fi x4) 2 $ Text $ get 3
  ]
  where
    half = div width 2
    sep = Pictures
      [ Text "|"
      , Translate (fi half)    0 $ Text "|"
      , Translate (fi width-1) 0 $ Text "|"
      ]

    x1 = div (half - length (get 0)) 2
    x2 = div (half - length (get 1)) 2 + half
    x3 = div (half - length (get 2)) 2
    x4 = div (half - length (get 3)) 2 + half

    get ix | ix >= length opts = ""
           | otherwise         = opts !! ix

    fill = Fill Unset (Set Picture.Bold) None None

battleLog cnt log =
  Text $ unlines $ take cnt log

----

runBasicAttackResult = \case

  Immune _ ->
    tell "It did no damage"

  Protected -> do
    World.World {..} <- lift get
    Battle {..} <- get
    tell $ pokemonName api mon2.pokemon <> " protected itself"

  AttackResult {..} -> do
    modify \Battle {..} -> Battle
      { mon2 = mon2
        { pokemon = mon2.pokemon
          { hp = max 0 $ mon2.pokemon.hp - damage
          , level = mon2.pokemon.level
          }
        , lastDamage = min mon2.pokemon.hp damage
        }
      , ..
      }
    when berryPopped do
      modify \Battle {..} -> Battle
        { mon2 = mon2
          { lastBerry = mon2.pokemon.heldItem
          , pokemon = mon2.pokemon
            { heldItem = Nothing
            }
          }
        , ..
        }
    when critical do
      tell "It's a critical hit!"

    case effectiveness of
      NotVery -> tell "It's not very effective"
      Super   -> tell "It's super effective!"
      _       -> pure ()

----

-- l  - level of defeated pokemon
-- lp - level of victorious pokemon
-- b  - base exp defeated pokemon
-- s  - number of pokemon that participated and have not fained (unless exp all)
--
baseExpGain l lp b s =
  (b * l)/5 * 1/s * ((2*l + 10)/(l + lp + 10))**2.5 + 1

getExpGain = do
  World.World {..} <- lift get
  Battle {..} <- get

  let baseYield = IM.lookup mon2.pokemon.id api.pokemon >>= \pok -> pok.base_experience
  let baseExp = baseExpGain
       do fi mon2.pokemon.level
       do fi mon1.pokemon.level
       do maybe 0 fi baseYield
       do 1

  pure baseExp

giveExp = do
  World.World {..} <- lift get
  Battle {..} <- get
  when (mon1.pokemon.level < 100) do
  exp <- round <$> getExpGain
  tell $ pokemonName api mon1.pokemon <> " gained " <> show exp <> " Exp"

  let Just pok = IM.lookup mon1.pokemon.id api.pokemon
  let Just gro = API.getPokemonGrowthRate api pok

  -- We level-up one level at a time in case there are
  -- new moves to learn
  --
  exp & fix \loop exp -> do
    Battle {..} <- get
    when (exp > 0 && mon1.pokemon.level < 100) do

    let expNext = totalExpAtLevel gro $ succ mon1.pokemon.level
    let dt      = expNext - mon1.pokemon.totalExp

    if expNext - mon1.pokemon.totalExp <= exp then do
      let lvl = succ mon1.pokemon.level
      put Battle
        { mon1 = mon1
          { pokemon = mon1.pokemon
            { level    = lvl
            , totalExp = expNext
            }
          }
        , ..
        }
      tell $ pokemonName api mon1.pokemon <> " is now level " <> show lvl
      onLevelUp
      loop $ exp - dt
    else do
      put Battle
        { mon1 = mon1
          { pokemon = mon1.pokemon
            { totalExp = mon1.pokemon.totalExp + exp
            }
          }
        , ..
        }

onLevelUp = do
  pure ()

----

englishMoveName :: API.Move -> Maybe T.Text
englishMoveName move =
  L.find f move.names <&> \x -> x.name
  where
    f x = x.language.name == "en"

moveName move =
  fromMaybe move.name $ englishMoveName move

move2moveDesc :: API.Move -> Maybe Pok.MoveDesc
move2moveDesc move = L.find (\d -> d.name == n) Pok.moves
  where n = T.unpack $ moveName move

