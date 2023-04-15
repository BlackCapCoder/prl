module Battle
  ( module Battle.Type
  , runBattle
  )
  where

import Battle.Type
import Pokemon.Nature
import Pokemon.PokeAPI qualified as API
import Pokemon.Pokemon
import Pokemon.Type as TYPE
import Pokemon.Stat
import Pokemon.Level
import Settings
import System.Random
import Prelude hiding (Field)
import Control.Monad.State
import Picture
import ANSI
import UserInput
import System.IO
import Data.ByteString.Builder qualified as BS
import Data.Text qualified as Text
import Data.IntMap qualified as IM
import World qualified
import Data.List (elem)
import StyledString
import Data.Vector qualified as V

----

healthBarWidth    = 40
redBarTreshold    = 0.25
yellowBarTreshold = 0.5

----

gutsID         = 62
adaptabilityID = 91
battlearmorID  = 4
shellarmorID   = 75
infiltratorID  = 151
sniperID       = 97
tintedLensID   = 110
fluffyID       = 218
neuroforceID   = 233
icescalesID    = 246
multiscaleID   = 136
shadowshieldID = 231
punkrockID     = 244
filterID       = 111
prismarmorID   = 232
solidrockID    = 116
unawareID      = 109

levitateID     = 26
flashfireID    = 18
waterabsorbID  = 11
stormdrainID   = 114
voltabsorbID   = 10
lightningrodID = 31
sapsipperID    = 157
soundproofID   = 43

struggleID       = 165
hydrostreamID    = -1
collisionCouseID = 878
electroDriftID   = 879
earthquakeID     = 89
magnitudeID      = 222
surfID           = 57
whirlpoolID      = 250
bodyslamID       = 34
stompID          = 23
dragonrushID     = 407
heatcrashID      = 535
heavyslamID      = 484
flyingpressID    = 560
behemothbashID   = 782
behemothbladeID  = 781
dynamaxcannonID  = 744
glaiverushID     = 862

expertBeltID = 245
metronomeID  = 254
lifeOrbID    = 247
airBallonID  = 584

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
      when continue mainLoop

    Just Won -> do
      continue <- onFoeDefeated
      when continue mainLoop

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
          when continue mainLoop
        Just Won -> do
          continue <- onAllyDefeated
          when continue mainLoop
        Nothing ->
          mainLoop

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

  let opts = moves' <&> \(m,pp) -> pp <> Text.unpack m.name

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
  -- runAttackResult =<< basicAttack struggleID NON True 40 mon1 mon1 True
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
  tell $ pokemonName api mon1.pokemon <> " used " <> Text.unpack m.name

  modifyMove move \m -> m { pp = max 0 (pred m.pp) }

  let cat = case m.damage_class.name of
        "physical" -> Physical
        "special"  -> Special
        _          -> Status

  let ty = maybe NON (\t -> TYPE.typeFromName t.name) m._type

  when (cat /= Status) do
    runAttackResult =<< basicAttack move.id ty (cat == Physical) 50 mon1 mon2 False

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

  moveID <- if null moves
    then pure $ fromJust $ IM.lookup struggleID api.moves
    else do
    roll <- liftIO $ randomRIO (0, length moves - 1)
    pure $ moves !! roll

  pure moveID

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

data MoveCategory
   = Physical
   | Special
   | Status
   deriving (Show, Eq, Ord)

----

data WeatherBonus
   = Positive
   | Negative
   | Neutral

type Effectiveness = Ordering

pattern NotVery  = LT :: Effectiveness
pattern Normally = EQ :: Effectiveness
pattern Super    = GT :: Effectiveness

data AttackResult
   = Immune Bool -- if true, the targets ability trigger additional effects
   | Protected
   | AttackResult
     { damage        :: Int
     , effectiveness :: Effectiveness
     , critical      :: Bool
     , berryPopped   :: Bool
     }

-- The base damage calculation, this is simply multiplied by any buffs/debuffs
--
damageBase level power a d =
  2 + (((2*fi level)/5 + 2) * fi power * fi a/fi d)/50

boostMult :: Int -> Float
boostMult n = case compare n 0 of
  EQ -> 1.0
  GT -> 1.0 + fi n/2
  LT -> 1 / (1 - fi n/2)

basicAttack moveID ty isPhysical pow mon1 mon2 neverCrit = do
  World.World {..} <- lift get
  Battle {field} <- get

  ---- Protect

  let isZMove            = False
  let ignoreProtect      = False
  let hasMultipleTargets = False

  if (mon2.protected || (hasMultipleTargets && mon2.wideGuard))
        && not (ignoreProtect || isZMove)
  then pure Protected
  else do

  ---- Immunity

  -- TODO: Scrappy, identify, freeze-dry
  let multType =
        foldr (\t x -> TYPE.effectiveness ty t * x) 1.0 mon2.pokemon.types

  -- tell $ show (ty, mon2.pokemon.types)

  let targetAbility
        | mon2.suppressed = Nothing
        | otherwise       = Just mon2.pokemon.ability

  let isSoundBased = False -- is the move sound based?

  let immunity
        | multType == 0
       || ty == GRO && mon2.pokemon.heldItem == Just airBallonID
       || ty == GRO && targetAbility == Just levitateID
       || ty == WAT && targetAbility == Just stormdrainID
       || isSoundBased && targetAbility == Just soundproofID
        = Just False
        | ty == FIR && targetAbility == Just flashfireID
       || ty == WAT && targetAbility == Just sapsipperID
       || ty == WAT && targetAbility == Just waterabsorbID
       || ty == ELE && targetAbility == Just voltabsorbID
       || ty == ELE && targetAbility == Just lightningrodID
        = Just True
        | otherwise
        = Nothing

  immunity & flip maybe (pure . Immune) do

  ---- Damage

  let isSecondStrikeOfParentialBond        = False
  let targetAbilityIgnored                 = False
  let allyOfTargetHasFriendGuardNotIgnored = False
  let typeResistBerryTriggered             = False
  let alwaysCrit                           = False
  let userIsFocused                        = False
  let dealsPhysicalDamage                  = False -- special move that deals physical damage

  let holdingExpertBelt = mon2.pokemon.heldItem == Just expertBeltID
  let holdingLifeOrb    = mon2.pokemon.heldItem == Just lifeOrbID
  let holdingMetronome  = mon2.pokemon.heldItem == Just metronomeID

  let userAbility
        | mon1.suppressed = Nothing
        | otherwise       = Just mon1.pokemon.ability

  let isSpecial       = not isPhysical
  let isSTAB          = ty `elem` mon1.pokemon.types
  let targetFullHP    = mon2.pokemon.hp      == mon2.stats.hp
  let isBurned        = mon1.pokemon.status  == Just Burn
  let hasAdaptability = userAbility == Just adaptabilityID
  let hasGuts         = userAbility == Just gutsID
  let hasLuckyChant   = mon2.luckyChant         > 0
  let hasReflect      = field.lane2.reflect     > 0
  let hasLightScreen  = field.lane2.lightScreen > 0
  let hasAuroraVeil   = field.lane2.auroraveil  > 0
  let hasGlaiveRush   = mon2.lastMove == Just glaiverushID

  let weatherBonus = case map fst field.weather of
        Just Rain
          | ty == WAT -> Positive
          | ty == FIR -> Negative
        Just Sun
          | ty == FIR                          -> Positive
          | ty == WAT, moveID /= hydrostreamID -> Negative
        _ -> Neutral

  let effectiveness = compare multType 1.0

  let critChance
        | 0 >= mon1.boosts.cri = 1/24
        | 1 <- mon1.boosts.cri = 1/8
        | 2 <- mon1.boosts.cri = 1/2
        | otherwise            = 1

  critRoll <- liftIO $ randomRIO (0.0, 1.0)

  let isCritical
        | neverCrit
       || hasLuckyChant
       || targetAbility `elem` [Just battlearmorID, Just shellarmorID] = False
        | otherwise
        = alwaysCrit || userIsFocused || critRoll < critChance

  ---- Multipliers

  let multTargets
        | hasMultipleTargets = 0.75
        | otherwise          = 1.0

  let multPB
        | isSecondStrikeOfParentialBond = 0.25
        | otherwise                     = 1.0

  let multWeather = case weatherBonus of
        Positive -> 1.5
        Negative -> 0.5
        Neutral  -> 1.0

  let multGlaiveRush
        | hasGlaiveRush = 2.0
        | otherwise     = 1.0

  let multCritical
        | isCritical = settings.critMultiplier
        | otherwise  = 1.0

  multRandom <- if settings.noDamageRanges
    then pure 1.0
    else liftIO $ randomRIO (0.85, 1.0)

  let multSTAB
        | isSTAB = if hasAdaptability then 2.0 else 1.5
        | let    = 1.0

  let multBurn
        | isSpecial || not isBurned || hasGuts = 1.0
        | otherwise                            = 0.5

  let multZMove
        | isZMove && mon2.protected = 0.25
        | otherwise                 = 1.0

  let multScreens
        | userAbility == Just infiltratorID || isCritical = 1.0
        | hasAuroraVeil                = 0.5
        | isPhysical && hasReflect     = 0.5
        | isSpecial  && hasLightScreen = 0.5
        | otherwise                    = 1.0

  let multOther = product
        [ multScreens
        , if mon2.semiInvul == Just Digging && elem moveID [earthquakeID, magnitudeID]
             then 2.0 else 1.0
        , if mon2.semiInvul == Just Diving && elem moveID [surfID, whirlpoolID]
             then 2.0 else 1.0
        , if effectiveness == Super && elem moveID [electroDriftID, collisionCouseID]
             then 5461/4096 else 1.0
        , if mon2.isMinimized && elem moveID [bodyslamID, stompID, dragonrushID, heatcrashID, heavyslamID, flyingpressID]
             then 2.0 else 1.0
        , if mon2.dynamax > 0 && elem moveID [behemothbashID, behemothbladeID, dynamaxcannonID]
             then 2.0 else 1.0
        , if userAbility == Just neuroforceID && effectiveness == Super
             then 1.5 else 1.0
        , if isCritical && userAbility == Just sniperID
             then 1.5 else 1.0
        , if userAbility == Just tintedLensID && effectiveness == NotVery
             then 2.0 else 1.0
        , if ty == FIR && targetAbility == Just fluffyID && not targetAbilityIgnored
             then 2.0 else 1.0
        , if isSpecial && targetAbility == Just icescalesID && not targetAbilityIgnored
             then 0.5 else 1.0
        , if holdingExpertBelt && effectiveness == Super
             then 4915/4096 else 1.0
        , if holdingLifeOrb
             then 5324/4096 else 1.0
        , if holdingMetronome
             then 1 + (819/4096) * fi mon1.echo else 1.0
        , if targetFullHP && ( (targetAbility == Just multiscaleID && not targetAbilityIgnored)
                            || targetAbility == Just shadowshieldID )
             then 0.5 else 1.0
        , if isSoundBased && targetAbility == Just punkrockID && not targetAbilityIgnored
             then 0.5 else 1.0
        , if effectiveness == Super && targetAbility == Just prismarmorID
             then 0.75 else 1.0
        , if effectiveness == Super && elem targetAbility [Just filterID, Just solidrockID]
                                    && not targetAbilityIgnored
             then 0.75 else 1.0
        , if allyOfTargetHasFriendGuardNotIgnored
             then 0.75 else 1.0
        , if typeResistBerryTriggered
             then 0.5 else 1.0
        , if ty == ELE && field.mudSport > 0
             then 1/3 else 1.0
        ]

  ---- boosts

  let ignoreAttBoost = targetAbility == Just unawareID
  let ignoreDefBoost = userAbility   == Just unawareID || isCritical

  let att
       | isPhysical = fi mon1.stats.att * if ignoreAttBoost then 1.0 else boostMult mon1.boosts.att
       | otherwise  = fi mon1.stats.spA * if ignoreAttBoost then 1.0 else boostMult mon1.boosts.spA

  let def
       | isPhysical || dealsPhysicalDamage
       = fi (if field.wonderRoom > 0 then mon2.stats.spD else mon2.stats.def)
       * if ignoreDefBoost then 1.0 else boostMult mon2.boosts.def

       | otherwise
       = fi (if field.wonderRoom > 0 then mon2.stats.def else mon2.stats.spD)
       * if ignoreDefBoost then 1.0 else boostMult mon2.boosts.spD

  ----

  let base = damageBase mon1.pokemon.level pow (round att) (round def)

  let dmg = base
          * multPB
          * multWeather
          * multGlaiveRush
          * multCritical
          * multRandom
          * multSTAB
          * multType
          * multBurn
          * multZMove
          * multOther

  pure AttackResult
    { damage        = 1 `max` round dmg
    , critical      = isCritical
    , berryPopped   = typeResistBerryTriggered
    , effectiveness = effectiveness
    }

runAttackResult = \case

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

