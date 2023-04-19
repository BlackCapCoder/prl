{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
module Battle.Move where

import Pokemon.Move as Move
import Pokemon.Pokemon as Pok
import Pokemon.Type
import Pokemon.Stat
import Battle.Type as Bat
import Control.Monad.State
import System.Random
import Data.IntSet qualified as IntSet
import Data.List (elem, notElem)


-- TODO: Boost should be capped at +- 6
-- TODO: magic bounce
-- TODO: frostbite


data MoveEnv m = MoveEnv
   { getUser     :: m BattleMon
   , getTarget   :: m BattleMon
   , putUser     :: BattleMon -> m ()
   , putTarget   :: BattleMon -> m ()
   , tell        :: String    -> m ()
   }


runEffect env@MoveEnv {..} eff = do
  Battle {field,..} <- get

  user <- getUser
  targ <- getTarget

  case eff of
    None   -> pure ()
    l :+ r -> runEffect env l *> runEffect env r
    n :% e -> do
      roll <- liftIO $ randomRIO (0, 99)
      when (roll < n) (runEffect env e)
    Choose [] -> pure ()
    Choose xs -> do
      roll <- liftIO $ randomRIO (0, pred $ length xs)
      runEffect env $ xs !! roll

    Flinch -> putTarget targ { flinched = True }

    Confuse -> do
      if user.confusion > 0 then do
        pure () -- already confused
      else do
        roll <- liftIO $ randomRIO (1, 4)
        putTarget targ { confusion = roll }

    Attract -> do
      if user.pokemon.gender /= Genderless &&
         targ.pokemon.gender /= Genderless &&
         user.pokemon.gender /= targ.pokemon.gender
      then do
        putTarget targ { attraction = IntSet.insert user.pokemon.uid targ.attraction }
      else do
        tell "But it failed.."

    Curse -> do
      if GHO `elem` user.pokemon.types
      then do
        env.putUser user
          { pokemon = user.pokemon
            { hp = max 0 $ user.pokemon.hp - div user.stats.hp 4
            }
          }
        env.putTarget targ { cursed = True }
      else do
        env.putUser user { boosts = user.boosts + zero {att=1,def=1,spe= -1,acc=0} }

    LeechSeed -> do
      unless (GRA `elem` targ.pokemon.types) do -- TODO: handle safety googles?
      putTarget targ { leeched = True }

    Yawn -> do
      unless (isAsleep targ.pokemon) do
      putTarget targ { drowsy = True }

    PerishSong -> do
      when (isNothing targ.perishCount) do
      putTarget targ { perishCount = Just 3 }

    Gravity
      | field.gravity > 0 -> put Battle {field = field { gravity=3 }, ..}
      | otherwise         -> put Battle {field = field { gravity=0 }, ..}

    TrickRoom
      | field.trickRoom > 0 -> put Battle {field = field { trickRoom=3 }, ..}
      | otherwise           -> put Battle {field = field { trickRoom=0 }, ..}

    MagicRoom
      | field.magicRoom > 0 -> put Battle {field = field { magicRoom=3 }, ..}
      | otherwise           -> put Battle {field = field { magicRoom=0 }, ..}

    WonderRoom
      | field.wonderRoom > 0 -> put Battle {field = field { wonderRoom=3 }, ..}
      | otherwise            -> put Battle {field = field { wonderRoom=0 }, ..}

    LuckyChant -> do
      putUser user { luckyChant = 3 }

    EStatus s -> do
      when (isNothing targ.pokemon.status) do
      unless (isImmuneToStatus targ s) do
      case s of
        Move.Paralysis -> putTargetStatus env $ Just Pok.Paralysis
        Move.Burn      -> putTargetStatus env $ Just Pok.Burn
        Move.Freeze    -> putTargetStatus env $ Just Pok.Freeze
        Move.Poison    -> putTargetStatus env $ Just Pok.Poison
        Move.Toxic     -> putTargetStatus env $ Just $ Pok.Toxic 0
        Move.Sleep -> do
          roll <- liftIO $ randomRIO (1, 3)
          putTargetStatus env $ Just $ Pok.Sleep roll -- TODO: early bird, insomnia
        _ -> pure ()

    EWeather w -> editField \f -> f
      { weather = Just case w of
          Move.Rain      -> (Bat.Rain,       3) -- TODO: held items extending weather
          Move.Sun       -> (Bat.Sun,        3)
          Move.Hail      -> (Bat.Hail,       3)
          Move.Sandstorm -> (Bat.Sandstorm,  3)
      }

    ETerrain t -> editField \f -> f
      { terrain = Just case t of
          Move.TGrass    -> (Bat.TGrassy,   3) -- TODO: terrain extenders
          Move.TPsychic  -> (Bat.TPsychic,  3)
          Move.TElectric -> (Bat.TElectric, 3)
          Move.TMisty    -> (Bat.TMisty,    3)
      }

    EScreen Phy -> do
      when (field.lane1.reflect < 1) do
      editLane1 \l -> l { reflect = 3 }

    EScreen Spe -> do
      when (field.lane1.lightScreen < 1) do
      editLane1 \l -> l { lightScreen = 3 }

    EScreen PhySpe -> do
      when (field.lane1.auroraveil < 1) do
      editLane1 \l -> l { auroraveil = 3 }

    EHazard Spikes -> do
      when (field.lane1.spikes < 3) do
      editLane1 \l -> l { spikes = succ l.spikes }

    EHazard ToxicSpikes -> do
      when (field.lane1.toxicSpikes < 2) do
      editLane1 \l -> l { toxicSpikes = succ l.toxicSpikes }

    EHazard Rocks ->
      editLane1 \l -> l { rocks = True }

    EHazard Web ->
      editLane1 \l -> l { web = True }

    EInvul Move.Flying -> putUser user
      { semiInvul = Just Bat.Flying }

    EInvul Move.Digging -> putUser user
      { semiInvul = Just Bat.Digging }

    EInvul Move.Diving -> putUser user
      { semiInvul = Just Bat.Diving }

    EInvul Move.Phantom -> putUser user
      { semiInvul = Just Bat.Phantom }

    PartialTrap -> do
      when (targ.partialTrap < 1) do
      turns <- liftIO $ randomRIO (4,5) -- 7 if target is holding a grip claw
      putTarget targ
        { partialTrap = turns }

    NoSwitch -> putTarget targ
      { blocked = True }

    ClearStatus ->
      putTargetStatus env Nothing

    ClearStatusParty -> editParty2 $
      map \pok -> pok { status = Nothing }

    ClearScreen -> editLane2 \l -> l
      { lightScreen = 0
      , reflect     = 0
      , auroraveil  = 0
      }

    ClearHazard -> editLane2 \l -> l
      { spikes      = 0
      , toxicSpikes = 0
      , rocks       = False
      , web         = False
      }

    AddBoost False b -> putTarget targ
      { boosts = targ.boosts + b }

    AddBoost True b -> putUser user
      { boosts = user.boosts + b }

    AddRandomBoost n -> do
      roll <- liftIO $ randomRIO (1, 5)
      putTarget targ
        { boosts = targ.boosts +
          case roll of
            1 -> zero {acc=0, att=n}
            2 -> zero {acc=0, def=n}
            3 -> zero {acc=0, spA=n}
            4 -> zero {acc=0, spD=n}
            5 -> zero {acc=0, spe=n}
            _ -> error "Unreachable"
        }

    ClearBoost -> putTarget targ
      { boosts = zero }

    InvBoost -> putTarget targ
      { boosts = targ.boosts * pure (-1) }

    CopyBoost -> putUser user
      { boosts = targ.boosts }

    MoveBoost -> do
      putUser   user { boosts = targ.boosts }
      putTarget targ { boosts = zero        }

    SwapBoost -> do
      putUser   user { boosts = targ.boosts }
      putTarget targ { boosts = user.boosts }

    SetAbility a -> putTarget targ
      { pokemon = targ.pokemon { ability = a } }

    CopyAbility Target2User -> putUser user
      { pokemon = user.pokemon { ability = targ.pokemon.ability } }

    CopyAbility User2Target -> putTarget targ
      { pokemon = targ.pokemon { ability = user.pokemon.ability } }

    SwapAbility -> do
      putUser   user { pokemon = user.pokemon { ability = targ.pokemon.ability } }
      putTarget targ { pokemon = targ.pokemon { ability = user.pokemon.ability } }

    SuppressAbility -> putTarget targ
      { suppressed = True }

    SetType ty False -> putTarget targ
      { pokemon = targ.pokemon { types = [ty] } }

    SetType ty True -> putUser user
      { pokemon = user.pokemon { types = [ty] } }

    AddType ty -> putTarget targ
      { pokemon = targ.pokemon { types = take 2 targ.pokemon.types <> [ty] } }

    RemoveType ty -> putTarget targ
      { pokemon = targ.pokemon { types = filter (/=ty) targ.pokemon.types } }

    SwapAttDef -> putTarget targ
      { stats = targ.stats { att = targ.stats.def, def = targ.stats.att, hp=targ.stats.hp } }

    AvgAtt -> do
      let att = div (user.stats.att + targ.stats.att) 2
      let spA = div (user.stats.spA + targ.stats.spA) 2

      putUser   user { stats = user.stats {hp=user.stats.hp, att=att, spA=spA} }
      putTarget targ { stats = targ.stats {hp=targ.stats.hp, att=att, spA=spA} }

    AvgDef -> do
      let def = div (user.stats.def + targ.stats.def) 2
      let spD = div (user.stats.spD + targ.stats.spD) 2

      putUser   user { stats = user.stats {hp=user.stats.hp, def=def, spD=spD} }
      putTarget targ { stats = targ.stats {hp=targ.stats.hp, def=def, spD=spD} }

    SwpAtt -> do
      putUser   user { stats = user.stats {hp=user.stats.hp, att=targ.stats.att, spA=targ.stats.spA} }
      putTarget targ { stats = targ.stats {hp=targ.stats.hp, att=user.stats.att, spA=user.stats.spA} }

    SwpDef -> do
      putUser   user { stats = user.stats {hp=user.stats.hp, def=targ.stats.def, spD=targ.stats.spD} }
      putTarget targ { stats = targ.stats {hp=targ.stats.hp, def=user.stats.def, spD=user.stats.spD} }

    Recover n -> putTarget targ
      { pokemon = targ.pokemon
        { hp = max (fi targ.stats.hp) $ targ.pokemon.hp + round (fi targ.stats.hp * n)
        }
      }

    MatchUserHP -> putTarget targ
      { pokemon = targ.pokemon { hp = min targ.stats.hp user.pokemon.hp } }

    FractionalDamage n -> putTarget targ
      { pokemon = targ.pokemon { hp = round $ fi targ.pokemon.hp * n } }

    FractionalDamageMax n -> putTarget targ
      { pokemon = targ.pokemon
        { hp = max 0 $ targ.pokemon.hp - round (fi targ.stats.hp * n)
        }
      }

    ConstantDamage n -> putTarget targ
      { pokemon = targ.pokemon
        { hp = max 0 $ targ.pokemon.hp - n
        }
      }

    LevelDamage -> putTarget targ
      { pokemon = targ.pokemon
        { hp = max 0 $ targ.pokemon.hp - targ.pokemon.level
        }
      }

    Struggle ->
      putUser user
        { pokemon = user.pokemon
          { hp = max 0 $ user.pokemon.hp - div user.stats.hp 4
          }
        }

    RecoilMax n ->
      putUser user
        { pokemon = user.pokemon
          { hp = max 0 $ user.pokemon.hp - round (fi user.stats.hp * n)
          }
        }

    Protect -> protectTarget env

    WideGuard ->
      putTarget targ { wideGuard = True }

    Substitute -> do
      when (targ.subHP < 1) do
      let health = div targ.stats.hp 4
      when (targ.pokemon.hp > health) do
      putTarget targ
        { subHP   = health
        , pokemon = targ.pokemon { hp = targ.pokemon.hp - health }
        }

    BurnIfBoosted -> do
      when (isNothing targ.pokemon.status && any (>0) targ.boosts) do
      putTargetStatus env (Just Pok.Burn)

    Captivate -> do
      when ( user.pokemon.gender /= Genderless
          && targ.pokemon.gender /= Genderless
          && user.pokemon.gender /= targ.pokemon.gender
           ) do
      putTarget targ
        { boosts = targ.boosts + zero { spA= -2, acc=0 } }

    RemoveItem -> putTarget targ
      { pokemon = targ.pokemon { heldItem = Nothing } }

    StealItem -> do
      putTarget targ { pokemon = targ.pokemon { heldItem = Nothing } }
      when (isNothing user.pokemon.heldItem) do
      putUser user { pokemon = user.pokemon { heldItem = targ.pokemon.heldItem } }

    BellyDrum -> do
      let dmg = div targ.stats.hp 2
      when (targ.pokemon.hp > dmg) do
      putTarget targ
        { pokemon = targ.pokemon { hp = targ.pokemon.hp - dmg }
        , boosts  = targ.boosts { att=6, acc=targ.boosts.acc }
        }

    -- TODO: This will swap things like tailwind, screens and safeguard.
    -- Verify that this is correct behavior
    SwapFieldEffects -> do
      editLane1 (const field.lane2)
      editLane2 (const field.lane1)


    MiracleEye -> putTarget targ
      { miracleEye = True
      , boosts     = targ.boosts { eva = 0 }
      }


    Nightmare -> do
      when (isAsleep targ.pokemon) do
      putTarget targ
        { pokemon = targ.pokemon
          { hp = max 0 $ targ.pokemon.hp - div targ.stats.hp 4
          }
        }

    MoveStatus ->
      case user.pokemon.status of
        Nothing -> pure ()
        Just s  -> do
          unless (targ `isImmuneToStatus'` s) do
          unless (isNothing targ.pokemon.status) do
          putUserStatus   env Nothing
          putTargetStatus env user.pokemon.status

    Refresh -> case user.pokemon.status of
      Just Pok.Paralysis -> putUserStatus env Nothing
      Just Pok.Burn      -> putUserStatus env Nothing
      Just Pok.Poison    -> putUserStatus env Nothing
      Just (Pok.Toxic _) -> putUserStatus env Nothing
      _                  -> tell "But it failed.."

    FilletAway                     -> error "FilletAway"
    Drain _                        -> error "Drain"
    DrainSleeping _                -> error "DrainSleeping"
    PainSplit                      -> error "PainSplit" -- TODO: Lazy
    Switch {}                      -> error "Switch"
    Camouflage                     -> error "Camouflage"
    Precharge                      -> error "Precharge"
    CopyAbility Target2Allies      -> error "CopyAbility Target2Allies"
    AddBoostIfKO _                 -> error "AddBoostIfKO"
    Locked                         -> error "TODO: Locking moves"
    CutPwrUserHP                   -> error "CutPwrUserHP"
    RaisePwrUserHP                 -> error "RaisePwrUserHP"
    AddPwr                         -> error "AddPwr"
    UseDef                         -> error "UseDef"
    DoublePwrNoItem                -> error "DoublePwrNoItem"
    DoublePwrIfHit                 -> error "DoublePwrIfHit"
    DoublePwrIfTargetFaster        -> error "DoublePwrIfTargetFaster"
    DoublePwrIfTargetSlower        -> error "DoublePwrIfTargetSlower"
    DoubleDmgIfTargetStatus        -> error "DoubleDmgIfTargetStatus"
    DoublePwrIfUserStatus          -> error "DoublePwrIfUserStatus"
    DoubleDmgIfDynamax             -> error "DoubleDmgIfDynamax"
    DoublePwrIfTargetHalfHP        -> error "DoublePwrIfTargetHalfHP"
    CrushGrip                      -> error "CrushGrip"
    DoublePowerIfInvul _           -> error "DoublePowerIfInvul"
    PwrLowFriendship               -> error "PwrLowFriendship"
    PwrHighFriendship              -> error "PwrHighFriendship"
    PwrHeavyTarget                 -> error "PwrHeavyTarget"
    PwrHeavyUser                   -> error "PwrHeavyUser"
    PwrInHarshSunlight             -> error "PwrInHarshSunlight"
    EchoPower                      -> error "EchoPower"
    SpeedPower                     -> error "SpeedPower"
    Recoil _                       -> error "Recoil"
    Don'tKill                      -> error "Don'tKill"
    FinalGambit                    -> error "FinalGambit"
    BeakBlast                      -> error "BeakBlast"
    ShieldTrap                     -> error "ShieldTrap"
    Wish                           -> error "Wish"
    Delay2Turns                    -> error "Delay2Turns"
    UserPrimary                    -> error "UserPrimary"
    ExtraType _                    -> error "ExtraType"
    Instruct                       -> error "Instruct"
    AllySwap                       -> error "AllySwap"
    Encore                         -> error "Encore"
    Disable                        -> error "Disable"
    Metronome                      -> error "Metronome"
    Assist                         -> error "Assist"
    AfterYou                       -> error "AfterYou"
    MorpekoMode                    -> error "MorpekoMode"
    Autotomize                     -> error "Autotomize"
    AxeKick                        -> error "AxeKick"
    BaddyBad                       -> error "BaddyBad"
    BeatUp                         -> error "BeatUp"
    Belch                          -> error "Belch"
    Bestow                         -> error "Bestow"
    BodyPress                      -> error "BodyPress"
    EatBerry _                     -> error "EatBerry"
    ChillyReception                -> error "ChillyReception"
    BoostSuperEffective            -> error "BoostSuperEffective"
    Comeuppance                    -> error "Comeuppance"
    Conversion                     -> error "Conversion"
    Conversion2                    -> error "Conversion2"
    Copycat                        -> error "Copycat"
    CoreEnforcer                   -> error "CoreEnforcer"
    CraftyShield                   -> error "CraftyShield"
    Defog                          -> error "Defog"
    RemovePP _                     -> error "RemovePP"
    ExpandingForce                 -> error "ExpandingForce"
    NoFleeingNextTurn              -> error "NoFleeingNextTurn"
    FirePledge                     -> error "FirePledge"
    WaterPledge                    -> error "WaterPledge"
    GrassPledge                    -> error "GrassPledge"
    Bide                           -> error "Bide"
    MirrorCoat                     -> error "MirrorCoat"
    Counter                        -> error "Counter"
    DamageWithSplinters            -> error "DamageWithSplinters"
    DieHealSwitchIn                -> error "DieHealSwitchIn"
    SplashDamage                   -> error "SplashDamage"
    Fling                          -> error "Fling"
    FloralHealing                  -> error "FloralHealing"
    FloralShield                   -> error "FloralShield"
    FlinchIfHit                    -> error "FlinchIfHit"
    FollowMe                       -> error "FollowMe"
    Foresight                      -> error "Foresight"
    UseTargetAtt                   -> error "UseTargetAtt"
    SuperEffectiveAgainst _        -> error "SuperEffectiveAgainst _"
    FusionBolt                     -> error "FusionBolt"
    FusionFlare                    -> error "FusionFlare"
    GearUp                         -> error "GearUp"
    MagneticFlux                   -> error "MagneticFlux"
    NoSpam                         -> error "NoSpam"
    GlaiveRush                     -> error "GlaiveRush"
    GlitzyGlow                     -> error "GlitzyGlow"
    GrassyGlide                    -> error "GrassyGlide"
    Grudge                         -> error "Grudge"
    GyroBall                       -> error "GyroBall"
    HappyHour                      -> error "HappyHour"
    HelpingHand                    -> error "HelpingHand"
    HiddenPower                    -> error "HiddenPower"
    DamageUserIfMiss _             -> error "DamageUserIfMiss"
    Scaling5Turns                  -> error "Scaling5Turns"
    DoubleDmgIfDefenceCurlUsed     -> error "DoubleDmgIfDefenceCurlUsed"
    Imprison                       -> error "Imprison"
    RemoveBerry _                  -> error "RemoveBerry"
    IonDeluge                      -> error "IonDeluge"
    NoSwitchUserAndTarget          -> error "NoSwitchUserAndTarget"
    Judgment                       -> error "Judgement"
    JungleHealing                  -> error "JungleHealing"
    DoublePwrIfUserDebuff          -> error "DoublePwrIfUserDebuff"
    AllOtherMovesUsed              -> error "AllOtherMovesUsed"
    LastRespects                   -> error "LastRespects"
    LightThatBurnsTheSky           -> error "LightThatBurnsTheSky"
    Magnitude                      -> error "Magnitude"
    MeFirst                        -> error "MeFirst"
    MatchTarget'sDamage _          -> error "MatchTarget'sDamage"
    Mimic                          -> error "Mimic"
    MirrorMove                     -> error "MirrorMove"
    PwrInTerrain _                 -> error "PwrInTerrain"
    RecoverWeather                 -> error "RecoverWeather"
    MultiAttack                    -> error "MultiAttack"
    NaturalGift                    -> error "NaturalGift"
    NaturePower                    -> error "NaturePower"
    Uproar                         -> error "Uproar"
    CopyType                       -> error "CopyType"
    SwapOffDef                     -> error "SwapOffDef"
    SwpSpe                         -> error "SwpSpe"
    DoublePwrIfTargetPoison        -> error "DoublePwrIfTargetPoison"
    IfUserHitByContactMove _       -> error "IfUserHitByContactMove"
    SwapItem                       -> error "SwapItem"
    DoublePwrInTerrain _           -> error "DoublePwrInTerrain"
    Octolock                       -> error "Octolock"
    PayDay                         -> error "PayDay"
    DoublePwrIfUserAttacked        -> error "DoublePwrIfUserAttacked"
    UseHighestOfAttSpA             -> error "UseHighestOfAttSpA"
    PwrHighBond                    -> error "PwrHighBond"
    PollenPuff                     -> error "PollenPuff"
    Poltergeist                    -> error "Poltergeist"
    Powder                         -> error "Powder"
    Present                        -> error "Present"
    Psywave                        -> error "Psywave"
    Punishment                     -> error "Punishment"
    Purify                         -> error "Purify"
    Pursuit                        -> error "Pursuit"
    Rage                           -> error "Rage"
    RageFist                       -> error "RageFist"
    RagingBull                     -> error "RagingBull"
    RagingFury                     -> error "RagingFury"
    Recycle                        -> error "Recycle"
    SleepFor2Turns                 -> error "SleepFor2Turns"
    DoubleDmgIfAllyFaintedLastTurn -> error "DoubleDmgIfAllyFaintedLastTurn"
    ReviveAllyToHalfHP             -> error "ReviveAllyToHalfHP"
    GroundFor1Turn                 -> error "GroundFor1Turn"
    Rototiller                     -> error "Rototiller"
    Round                          -> error "Round"
    SaltCure                       -> error "SaltCure"
    SecretPower                    -> error "SecretPower"
    ShedTail                       -> error "ShedTail"
    ShellSideArm                   -> error "ShellSideArm"
    ShellTrap                      -> error "ShellTrap"
    ShoreUp                        -> error "ShoreUp"
    Sketch                         -> error "Sketch"
    SkyDrop                        -> error "SkyDrop"
    HitInvul _                     -> error "HitInvul"
    SleepTalk                      -> error "SleepTalk"
    GroundFlying                   -> error "GroundFlying"
    SmellingSalts                  -> error "SmellingSalts"
    IgnoreFollowMe                 -> error "IgnoreFollowMe"
    FailIfNotAsleep                -> error "FailIfNotAsleep"
    Snowscape                      -> error "Snowscape"
    ChargeIfNotSun                 -> error "ChargeIfNotSun"
    StealStatBoosts                -> error "StealStatBoosts"
    Spite                          -> error "Spite"
    SpringtideStorm                -> error "SpringtideStorm"
    DoublePwrIfLastMoveFailed      -> error "DoublePwrIfLastMoveFailed"
    StrengthSap                    -> error "StrengthSap"
    SuckerPunch                    -> error "SuckerPunch"
    Synchronoise                   -> error "Synchronoise"
    TechnoBlast                    -> error "TechnoBlast"
    TeraBlast                      -> error "TeraBlast"
    TerrainPulse                   -> error "TerrainPulse"
    TroatChop                      -> error "TroatChop"
    PerfectAccuracyInWeather _     -> error "PerfectAccuracyInWeather"
    RemoveSubstitute               -> error "RemoveSubstitute"
    TripleAxel                     -> error "TripleAxel"
    TripleKick                     -> error "TripleKick"
    TrumpCard                      -> error "TrumpCard"
    Transform                      -> error "Transform"
    VenomDrench                    -> error "VenomDrench"
    WakeUpSlap                     -> error "WakeUpSlap"
    WeatherBall                    -> error "WeatherBall"
    WringOut                       -> error "WringOut"
    Feint                          -> error "Feint"
    IfUserHit _                    -> error "IfUserHit"

    HealBurn -> case targ.pokemon.status of
      Just Pok.Burn -> putTargetStatus env Nothing
      _             -> pure ()

    Stockpile -> putTarget targ { stockpiles = min 3 $ succ targ.stockpiles }
    SpitUp    -> error "SpitUp"
    Swallow   -> error "Swallow"

    Torment         -> putTarget targ { torment     = True }
    Quash           -> putTarget targ { quashed     = True }
    TarShot         -> putTarget targ { tarShot     = True }
    Endure          -> putTarget targ { enduring    = True }
    LockOn          -> putTarget targ { lockedOn    = True }
    MagicCoat       -> putTarget targ { magicCoat   = True }
    Charge          -> putTarget targ { charged     = True }
    AquaRing        -> putTarget targ { aquaRing    = True }
    Ingrain         -> putTarget targ { ingrained   = True }
    Mist            -> putTarget targ { mist        = True }
    LaserFocus      -> putTarget targ { focused     = True }
    DefenceCurlUsed -> putTarget targ { defenceCurl = True }
    Snatch          -> putTarget targ { snatching   = True }
    OdorSleuth      -> putTarget targ { odorSleuth  = True }
    MagnetRise      -> putTarget targ { magnetRise  = 5 }
    Telekinesis     -> putTarget targ { telekinesis = 3 }
    DestinyBond     -> putTarget targ { destinyBond = 1 }
    HealBlock       -> putTarget targ { healBlock   = 5 }
    Taunt           -> putTarget targ { taunt       = if targ.hasMoved then 4 else 3 }
    Embargo         -> putTarget targ { embargo     = 3 }
    QuickGuard      -> editLane2 \l -> l { quickGuard = True }
    Safeguard       -> editLane2 \l -> l { safeguard  = 3 }
    Tailwind        -> editLane2 \l -> l { tailwind   = 3 }
    WaterSport      -> editField \f -> f { waterSport = 5 }
    MudSport        -> editField \l -> l { mudSport   = 5 }
    ClearTerrain    -> editField \f -> f { terrain    = Nothing }

    Recharge -> putUser user { recharge = 2 }
    UserDies -> putUser user { pokemon  = user.pokemon { hp = 0 } }

----

-- editUser f =
--   modify \b -> b { mon1 = f b.mon1 }

-- editTarget f =
--   modify \b -> b { mon2 = f b.mon2 }

editField f =
  modify \b -> b { field = f b.field }

editLane1 f =
  editField \x -> x { lane1 = f x.lane1 }

editLane2 f =
  editField \x -> x { lane2 = f x.lane2 }

editParty1 f =
  modify \b -> b { party1 = f b.party1 }

editParty2 f =
  modify \b -> b { party2 = f b.party2 }

putUserStatus MoveEnv {..} s = do
  user <- getUser
  putUser user { pokemon = user.pokemon { status = s } }

putTargetStatus MoveEnv {..} s = do
  targ <- getTarget
  putTarget targ { pokemon = targ.pokemon { status = s } }

protectTarget MoveEnv {..} = do
  targ <- getTarget
  let successRate = (1/3) ** fi targ.protections
  roll <- liftIO $ randomRIO (0.0, 1.0)
  when (roll <= successRate) do
  putTarget targ
    { protected   = True
    , protections = succ targ.protections
    }

-- TODO: Insomnia
isImmuneToStatus pok = \case
  Move.Paralysis -> ELE `elem` pok.pokemon.types
  Move.Burn      -> FIR `elem` pok.pokemon.types
  Move.Freeze    -> ICE `elem` pok.pokemon.types
  Move.Poison    -> POI `elem` pok.pokemon.types
  Move.Toxic     -> POI `elem` pok.pokemon.types
  _              -> False

isImmuneToStatus' pok = \case
  Pok.Paralysis -> ELE `elem` pok.pokemon.types
  Pok.Burn      -> FIR `elem` pok.pokemon.types
  Pok.Freeze    -> ICE `elem` pok.pokemon.types
  Pok.Poison    -> POI `elem` pok.pokemon.types
  Pok.Toxic _   -> POI `elem` pok.pokemon.types
  _             -> False

