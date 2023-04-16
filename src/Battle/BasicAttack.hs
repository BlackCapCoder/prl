module Battle.BasicAttack where

import Battle.Type
import Pokemon.Pokemon
import Pokemon.Type as TYPE
import Pokemon.Stat
import World qualified
import Settings

import Data.List (elem)
import System.Random
import Control.Monad.State

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

----

-- The base damage calculation, this is simply multiplied by any buffs/debuffs
--
damageBase level power a d =
  2 + (((2*fi level)/5 + 2) * fi power * fi a/fi d)/50

boostMult :: Int -> Float
boostMult n = case compare n 0 of
  EQ -> 1.0
  GT -> 1.0 + fi n/2
  LT -> 1 / (1 - fi n/2)


basicAttack moveID ty isPhysical pow mon1 mon2 = do
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
        | hasLuckyChant
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

