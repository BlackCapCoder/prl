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

----

healthBarWidth  = 20
confusionChance = (1, 3)
paralysisChance = (1, 2)
thawChance      = (1, 5)

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
  Battle {..} <- get

  when isWild do
    tell $ "A wild " <> pokemonName api mon2.pokemon <> " appeared!"

  do let name = "You"
     tell $ name <> " sent out " <> pokemonName api mon1.pokemon

  res <- selectAction
  flushParty

  case res of
    RanAway -> lift $ World.fullscreenMessage "You ran away"
    Won     -> do
      tell "You won!"
      drawBattle 0 []
      liftIO acceptInput
    Lost -> do
      tell "You lost!"
      drawBattle 0 []
      liftIO acceptInput
      lift World.whiteOut


tell msg = do
  modify \w -> w { log = msg : w.log }

----

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
  { hp     = p1.hp
  , status = p1.status
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
      | 0 <- cur -> attackSelected
      | 1 <- cur -> bagSelected
      | 2 <- cur -> switchSelected
      | 3 <- cur -> runSelected
    BCancel
      | isWild -> put Battle {actionCursor = 3, ..} *> selectAction
    _ -> selectAction

selectMove = do
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
    BSelect -> moveSelected0 $ fst $ moves' !! cur
    _       -> selectMove

----

moveSelected0 move = do
  Battle {..} <- get
  move2 <- selectRandomMove mon2

  let spe1 = round $ fi mon1.stats.spe * boostMult mon1.boosts.spe
  let spe2 = round $ fi mon2.stats.spe * boostMult mon2.boosts.spe

  goFirst <- case compare spe1 spe2 of
    LT -> pure False
    GT -> pure True
    EQ -> liftIO randomIO

  if goFirst then do
    moveSelected move
    battleResult >>= \case
      Just r  -> pure r
      Nothing -> do
        drawBattle 0 []
        liftIO acceptInput
        swapField
        moveSelected move2
        swapField
        endOfTurn
  else do
    swapField
    moveSelected move2
    swapField
    battleResult >>= \case
      Just r  -> pure r
      Nothing -> do
        drawBattle 0 []
        liftIO acceptInput
        moveSelected move
        endOfTurn

moveSelected move = do
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
        moveSelected' move
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
      roll <- liftIO $ randomRIO paralysisChance
      if roll==1 then do
        tell $ pokemonName api mon1.pokemon <> " couldn't move because it's paralyzed!"
      else do
        moveSelected' move

    Just Freeze -> do
      thawRoll <- liftIO $ randomRIO thawChance

      -- TODO: Moves that unfreeze the user
      let thaw = thawRoll == 1

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
        moveSelected' move

    _ -> moveSelected' move

moveSelected' move = do
  Battle {..} <- get

  -- confusion
  case compare mon1.confusion 0 of
    LT -> moveSelected'' move
    EQ -> do
      put Battle { mon1 = mon1 { confusion = -1 }, .. }
      tell $ pokemonName api mon1.pokemon <> " snapped out of its confusion"
      moveSelected'' move
    GT -> do
      put Battle { mon1 = mon1 { confusion = mon1.confusion - 1 }, .. }
      tell $ pokemonName api mon1.pokemon <> " is confused " <> show mon1.confusion
      roll <- liftIO $ randomRIO confusionChance
      if roll == 1 then do
        tell $ pokemonName api mon1.pokemon <> " hit itself in its confusion!"
        confusionHit
      else do
        moveSelected'' move

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

moveSelected'' move = do
  -- attract

  Battle {..} <- get

  let Just m = IM.lookup move.id api.moves
  tell $ pokemonName api mon1.pokemon <> " used " <> Text.unpack m.name

  modifyMove move \m -> m { pp = max 0 (pred m.pp) }

  let cat = case m.damage_class.name of
        "physical" -> Physical
        "special"  -> Special
        _          -> Status

  let ty = TYPE.typeFromName m._type.name

  when (cat /= Status) do
    runAttackResult =<< basicAttack move.id ty (cat == Physical) 50 mon1 mon2 False

    pure ()

  -- performMove move


-- performMove move = do
--   when (move.id == struggleID) struggle

struggle = do
  Battle {..} <- get
  put Battle
    { mon1 = mon1
      { pokemon = mon1.pokemon
        { hp = max 0 $ mon1.pokemon.hp - div (mon1.stats.hp) 4
        }
      }
    , ..
    }
  runAttackResult =<< basicAttack struggleID NON True 50 mon1 mon2 False

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

attackSelected =
  selectMove

bagSelected =
  selectAction

switchSelected =
  selectAction

runSelected =
  pure RanAway

----

endOfTurn = do
  Battle {..} <- get

  if | mon1.pokemon.hp == 0 -> pure Lost
     | mon2.pokemon.hp == 0 -> pure Won
     | otherwise            -> selectAction

battleResult = do
  Battle {..} <- get
  pure $ if
    | mon1.pokemon.hp == 0 -> Just Lost
    | mon2.pokemon.hp == 0 -> Just Won
    | otherwise            -> Nothing

data BattleResult
   = Won
   | Lost
   | RanAway

----

selectRandomMove mon = do
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
  Battle {..} <- get
  liftIO do
    putStr $ clearScreen <> cursorHome
    BS.hPutBuilder stdout $ drawColor 0 0 twidth theight (toF pic) <> resetStyle
    hFlush stdout

----

redBarTreshold = 0.25

healthBar width hp max = Pictures
  [ Text $ "[" <> replicate (width - 2) ' ' <> "]"
  , Translate 1 0
  $ Fill Unset None (Set color) None
  $ Text $ replicate boxes '#'
  , Translate (fi boxes + 1) 0
  $ Text $ replicate dashes '-'
  ]
  where
    perc = fi hp / fi max
    color | perc <= redBarTreshold = RGB 255 0 0
          | otherwise              = RGB 0 255 0

    boxes  = floor $ fi (width - 2) * perc
    dashes = width - 2 - boxes

monInfo api right w2 mon = (w3,) $ Pictures
  [ if right then Blank else Text name
  , (if right then Prelude.id else Translate (fi (length name + 1)) 0)
  $ Pictures
    [ Text lvlStr
    , Translate w1 0 $ healthBar w2 hp max
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
        | isCritical = 1.5
        | otherwise  = 1.0

  multRandom <- liftIO $ randomRIO (0.85, 1.0)

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
    Battle {..} <- get
    tell $ pokemonName api mon2.pokemon <> " protected itself"

  AttackResult {..} -> do
    modify \Battle {..} -> Battle
      { mon2 = mon2
        { pokemon = mon2.pokemon
          { hp = max 0 $ mon2.pokemon.hp - damage
          }
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


