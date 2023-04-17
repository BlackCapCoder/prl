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
   { getUser   :: m BattleMon
   , getTarget :: m BattleMon
   , putUser   :: BattleMon -> m ()
   , putTarget :: BattleMon -> m ()
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
      if ( user.pokemon.gender /= Genderless
         && targ.pokemon.gender /= Genderless
         && user.pokemon.gender /= targ.pokemon.gender
         )
      then do
        putTarget targ { attraction = IntSet.insert user.pokemon.uid targ.attraction }
      else do
        pure () -- no attraction

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

    Tailwind -> put Battle
      { field = field { lane1 = field.lane1 {tailwind=3} }, .. }

    LuckyChant -> do
      putUser user { luckyChant = 3 }

    EStatus s -> do
      when (isNothing targ.pokemon.status) do
      case s of
        Move.Paralysis
          | ELE `notElem` targ.pokemon.types -> putTargetStatus env $ Just Pok.Paralysis
        Move.Burn
          | FIR `notElem` targ.pokemon.types -> putTargetStatus env $ Just Pok.Burn
        Move.Freeze
          | ICE `notElem` targ.pokemon.types -> putTargetStatus env $ Just Pok.Freeze
        Move.Poison
          | POI `notElem` targ.pokemon.types -> putTargetStatus env $ Just Pok.Poison
        Move.Toxic
          | POI `notElem` targ.pokemon.types -> putTargetStatus env $ Just $ Pok.Toxic 0
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

    ELocking _ -> error "TODO: Locking damage-over-time moves"

    NoSwitch -> putTarget targ
      { blocked = True }

    Locked -> error "TODO: Locking moves"

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

    AddBoostIfKO _ -> error "AddBoostIfKO"

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

    IgnoreBoosts -> error "IgnoreBoosts"

    SetAbility a -> putTarget targ
      { pokemon = targ.pokemon { ability = a } }

    CopyAbility Target2Allies -> error "CopyAbility Target2Allies"

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

    Camouflage -> error "Camouflage"
    Recharge   -> error "Recharge"
    Precharge  -> error "Precharge"

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

    Switch {} -> error "Switch"

    Recover n -> putTarget targ
      { pokemon = targ.pokemon
        { hp = max (fi targ.stats.hp) $ targ.pokemon.hp + round (fi targ.stats.hp * n)
        }
      }

    Drain _         -> error "Drain"
    DrainSleeping _ -> error "DrainSleeping"
    PainSplit       -> error "PainSplit" -- TODO: Lazy

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

    CutPwrUserHP   -> error "CutPwrUserHP"
    RaisePwrUserHP -> error "RaisePwrUserHP"
    AddPwr         -> error "AddPwr"
    UseDef         -> error "UseDef"

    DoublePwrNoItem          -> error "DoublePwrNoItem"
    DoublePwrIfHit           -> error "DoublePwrIfHit"
    DoublePwrIfTargetFaster  -> error "DoublePwrIfTargetFaster"
    DoublePwrIfTargetSlower  -> error "DoublePwrIfTargetSlower"
    DoubleDmgIfTargetStatus  -> error "DoubleDmgIfTargetStatus"
    DoublePwrIfUserStatus    -> error "DoublePwrIfUserStatus"
    DoubleDmgIfDynamax       -> error "DoubleDmgIfDynamax"
    DoublePwrIfTargetHalfHP  -> error "DoublePwrIfTargetHalfHP"
    CrushGrip                -> error "CrushGrip"
    DoublePowerIfInvul _     -> error "DoublePowerIfInvul"

    PwrLowFriendship   -> error "PwrLowFriendship"
    PwrHighFriendship  -> error "PwrHighFriendship"
    PwrHeavyTarget     -> error "PwrHeavyTarget"
    PwrHeavyUser       -> error "PwrHeavyUser"
    PwrInHarshSunlight -> error "PwrInHarshSunlight"

    EchoPower  -> error "EchoPower"
    SpeedPower -> error "SpeedPower"

    Recoil _ -> error "Recoil"

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

    Protect ->
      protectTarget env

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

    IgnoreProtect _ -> error "IgnoreProtect"

    Endure -> putTarget targ
      { enduring = True }

    Don'tKill   -> error "Don'tKill"
    FinalGambit -> error "FinalGambit"
    BeakBlast   -> error "BeakBlast"
    ShieldTrap  -> error "ShieldTrap"

    DestinyBond -> putTarget targ
      { destinyBond = 1 }

    Wish        -> error "Wish"
    Delay2Turns -> error "Delay2Turns"
    UserPrimary -> error "UserPrimary"
    ExtraType _ -> error "ExtraType"

    Snatch -> putTarget targ
      { snatching = True }

    Instruct -> error "Instruct"
    AllySwap -> error "AllySwap"

    Taunt   -> putTarget targ { taunt  = 3 }
    Embargo -> putTarget targ { embargo = 3 }
    Encore      -> error "Encore"
    Disable     -> error "Disable"
    Metronome   -> error "Metronome"
    Assist      -> error "Assist"
    AfterYou    -> error "AfterYou"
    MorpekoMode -> error "MorpekoMode"

    UserDies -> putUser user
      { pokemon = user.pokemon { hp = 0 } }

    Autotomize -> error "Autotomize"
    AxeKick    -> error "AxeKick"
    BaddyBad   -> error "BaddyBad"
    BeatUp     -> error "BeatUp"
    Belch      -> error "Belch"
    Bestow     -> error "Bestow"
    BodyPress  -> error "BodyPress"
    EatBerry   -> error "EatBerry"

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

    Charge -> putTarget targ { charged = True }

    ChillyReception     -> error "ChillyReception"
    BoostSuperEffective -> error "BoostSuperEffective"
    Comeuppance         -> error "Comeuppance"
    Conversion          -> error "Conversion"
    Conversion2         -> error "Conversion2"
    Copycat             -> error "Copycat"
    CoreEnforcer        -> error "CoreEnforcer"

    RemoveItem -> putTarget targ
      { pokemon = targ.pokemon { heldItem = Nothing } }

    StealItem -> do
      putTarget targ { pokemon = targ.pokemon { heldItem = Nothing } }
      when (isNothing user.pokemon.heldItem) do
      putUser user { pokemon = user.pokemon { heldItem = targ.pokemon.heldItem } }

    Safeguard -> editLane2 \l -> l
      { safeguard = 3 }

    CraftyShield      -> error "CraftyShield"
    Defog             -> error "Defog"
    RemovePP _        -> error "RemovePP"
    ExpandingForce    -> error "ExpandingForce"
    NoFleeingNextTurn -> error "NoFleeingNextTurn"

    FirePledge  -> error "FirePledge"
    WaterPledge -> error "WaterPledge"
    GrassPledge -> error "GrassPledge"

    BellyDrum -> do
      let dmg = div targ.stats.hp 2
      when (targ.pokemon.hp > dmg) do
      putTarget targ
        { pokemon = targ.pokemon { hp = targ.pokemon.hp - dmg }
        , boosts  = targ.boosts { att=6, acc=targ.boosts.acc }
        }

    FilletAway -> error "FilletAway"

    -- TODO: This will swap things like tailwind, screens and safeguard.
    -- Verify that this is correct behavior
    SwapFieldEffects -> do
      editLane1 (const field.lane2)
      editLane2 (const field.lane1)

    Bide       -> error "Bide"
    MirrorCoat -> error "MirrorCoat"
    Counter    -> error "Counter"

    AquaRing -> putTarget targ { aquaRing  = True }
    Ingrain  -> putTarget targ { ingrained = True }

    DamageWithSplinters -> error "DamageWithSplinters"
    DieHealSwitchIn     -> error "DieHealSwitchIn"
    SplashDamage        -> error "SplashDamage"
    Fling               -> error "Fling"
    FloralHealing       -> error "FloralHealing"
    FloralShield        -> error "FloralShield"

    FlinchIfHit             -> error "FlinchIfHit"
    FollowMe                -> error "FollowMe"
    Foresight               -> error "Foresight"
    UseTargetAtt            -> error "UseTargetAtt"
    SuperEffectiveAgainst _ -> error "SuperEffectiveAgainst _"

    FusionBolt  -> error "FusionBolt"
    FusionFlare -> error "FusionFlare"

    GearUp       -> error "GearUp"
    MagneticFlux -> error "MagneticFlux"

    NoSpam      -> error "NoSpam"
    GlaiveRush  -> error "GlaiveRush"
    GlitzyGlow  -> error "GlitzyGlow"
    GrassyGlide -> error "GrassyGlide"
    Grudge      -> error "Grudge"
    GyroBall    -> error "GyroBall"
    HappyHour   -> error "HappyHour"
    HealBlock   -> putTarget targ { healBlock = 5 }
    HelpingHand -> error "HelpingHand"
    HiddenPower -> error "HiddenPower"

    DamageUserIfMiss _ -> error "DamageUserIfMiss"
    Scaling5Turns -> error "Scaling5Turns"
    DefenceCurlUsed -> putTarget targ { defenceCurl = True }
    DoubleDmgIfDefenceCurlUsed -> error "DoubleDmgIfDefenceCurlUsed"
    ClearTerrain -> editField \f -> f { terrain = Nothing }
    Imprison -> error "Imprison"
    RemoveBerry _ -> error "RemoveBerry"

    IonDeluge -> error "IonDeluge"
    NoSwitchUserAndTarget -> error "NoSwitchUserAndTarget"
    Judgement -> error "Judgement"
    JungleHealing -> error "JungleHealing"
    LaserFocus -> putTarget targ { focused = True }
    DoublePwrIfUserDebuff -> error "DoublePwrIfUserDebuff"
    AllOtherMovesUsed -> error "AllOtherMovesUsed"
    LastRespects -> error "LastRespects"
    LightThatBurnsTheSky -> error "LightThatBurnsTheSky"
    LockOn -> putTarget targ { lockedOn = True }
    MagicCoat -> putTarget targ { magicCoat = True }
    MagnetRise -> putTarget targ { magnetRise = 5 }
    Magnitude -> error "Magnitude"
    MeFirst -> error "MeFirst"
    MatchTarget'sDamage _ -> error "MatchTarget'sDamage"
    Mimic -> error "Mimic"
    MiracleEye -> putTarget targ
      { miracleEye = True
      , boosts = targ.boosts { eva = 0 }
      }
    MirrorMove -> error "MirrorMove"
    Mist -> putTarget targ { mist=True }

    PwrInTerrain _ -> error "PwrInTerrain"
    IgnoreAbility -> error "IgnoreAbility"

    RecoverWeather -> error "RecoverWeather"
    MudSport -> editField \l -> l { mudSport = 3 }
    MultiAttack -> error "MultiAttack"
    NaturalGift -> error "NaturalGift"
    NaturePower -> error "NaturePower"

    Nightmare -> do
      when (isAsleep targ.pokemon) do
      putTarget targ
        { pokemon = targ.pokemon
          { hp = max 0 $ targ.pokemon.hp - div targ.stats.hp 4
          }
        }

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

