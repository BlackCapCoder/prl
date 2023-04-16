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


runEffect eff = do
  Battle {..} <- get

  case eff of
    None   -> pure ()
    l :+ r -> runEffect l *> runEffect r
    n :% e -> do
      roll <- liftIO $ randomRIO (0, 99)
      when (roll < n) (runEffect e)
    Choose [] -> pure ()
    Choose xs -> do
      roll <- liftIO $ randomRIO (0, pred $ length xs)
      runEffect $ xs !! roll

    Flinch -> editTarget \mon -> mon { flinched = True }

    Confuse -> do
      if mon1.confusion > 0 then do
        pure () -- already confused
      else do
        roll <- liftIO $ randomRIO (1, 4)
        editTarget \mon -> mon { confusion = roll }

    Attract -> do
      if ( mon1.pokemon.gender /= Genderless
         && mon2.pokemon.gender /= Genderless
         && mon1.pokemon.gender /= mon2.pokemon.gender
         )
      then do
        editTarget \mon -> mon { attraction = IntSet.insert mon1.pokemon.uid mon.attraction }
      else do
        pure () -- no attraction

    Curse -> do
      if GHO `elem` mon1.pokemon.types
      then do
        editUser \u -> u
          { pokemon = u.pokemon
            { hp = max 0 $ u.pokemon.hp - div u.stats.hp 4
            }
          }
        editTarget \mon -> mon { cursed = True }
      else do
        editUser \mon -> mon { boosts = mon.boosts + zero {att=1,def=1,spe= -1,acc=0} }

    LeechSeed -> do
      unless (GRA `elem` mon2.pokemon.types) do -- TODO: handle safety googles?
      editTarget \mon -> mon { leeched = True }

    Yawn -> do
      unless (isAsleep mon2.pokemon) do
      editTarget \mon -> mon { drowsy = True }

    PerishSong -> do
      when (isNothing mon2.perishCount) do
      editTarget \mon -> mon { perishCount = Just 3 }

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
      editUser \mon -> mon { luckyChant = 3 }

    EStatus s -> do
      when (isNothing mon2.pokemon.status) do
      case s of
        Move.Paralysis
          | ELE `notElem` mon2.pokemon.types -> putTargetStatus $ Just Pok.Paralysis
        Move.Burn
          | FIR `notElem` mon2.pokemon.types -> putTargetStatus $ Just Pok.Burn
        Move.Freeze
          | ICE `notElem` mon2.pokemon.types -> putTargetStatus $ Just Pok.Freeze
        Move.Poison
          | POI `notElem` mon2.pokemon.types -> putTargetStatus $ Just Pok.Poison
        Move.Toxic
          | POI `notElem` mon2.pokemon.types -> putTargetStatus $ Just $ Pok.Toxic 0
        Move.Sleep -> do
          roll <- liftIO $ randomRIO (1, 3)
          putTargetStatus $ Just $ Pok.Sleep roll -- TODO: early bird, insomnia
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

    EInvul Move.Flying -> editUser \u -> u
      { semiInvul = Just Bat.Flying }

    EInvul Move.Digging -> editUser \u -> u
      { semiInvul = Just Bat.Digging }

    EInvul Move.Diving -> editUser \u -> u
      { semiInvul = Just Bat.Diving }

    EInvul Move.Phantom -> editUser \u -> u
      { semiInvul = Just Bat.Phantom }

    ELocking _ -> error "TODO: Locking damage-over-time moves"

    NoSwitch -> editTarget \t -> t
      { blocked = True }

    Locked -> error "TODO: Locking moves"

    ClearStatus ->
      putTargetStatus Nothing

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

    AddBoost False b -> editTarget \t -> t
      { boosts = t.boosts + b }

    AddBoost True b -> editUser \u -> u
      { boosts = u.boosts + b }

    AddBoostIfKO _ -> error "AddBoostIfKO"

    AddRandomBoost n -> do
      roll <- liftIO $ randomRIO (1, 5)
      editTarget \t -> t
        { boosts = t.boosts +
          case roll of
            1 -> zero {acc=0, att=n}
            2 -> zero {acc=0, def=n}
            3 -> zero {acc=0, spA=n}
            4 -> zero {acc=0, spD=n}
            5 -> zero {acc=0, spe=n}
        }

    ClearBoost -> editTarget \t -> t
      { boosts = zero }

    InvBoost -> editTarget \t -> t
      { boosts = t.boosts * pure (-1) }

    CopyBoost -> editUser \u -> u
      { boosts = mon2.boosts }

    MoveBoost -> do
      editUser   \u -> u { boosts = mon2.boosts }
      editTarget \t -> t { boosts = zero        }

    SwapBoost -> do
      editUser   \u -> u { boosts = mon2.boosts }
      editTarget \t -> t { boosts = mon1.boosts }

    IgnoreBoosts -> error "IgnoreBoosts"

    SetAbility a -> editTarget \t -> t
      { pokemon = t.pokemon { ability = a } }

    CopyAbility Target2Allies -> error "CopyAbility Target2Allies"

    CopyAbility Target2User -> editUser \u -> u
      { pokemon = u.pokemon { ability = mon2.pokemon.ability } }

    CopyAbility User2Target -> editTarget \t -> t
      { pokemon = t.pokemon { ability = mon1.pokemon.ability } }

    SwapAbility -> do
      editUser   \u -> u { pokemon = u.pokemon { ability = mon2.pokemon.ability } }
      editTarget \t -> t { pokemon = t.pokemon { ability = mon1.pokemon.ability } }

    SuppressAbility -> editTarget \t -> t
      { suppressed = True }

    SetType ty False -> editTarget \t -> t
      { pokemon = t.pokemon { types = [ty] } }

    SetType ty True -> editUser \u -> u
      { pokemon = u.pokemon { types = [ty] } }

    AddType ty -> editTarget \t -> t
      { pokemon = t.pokemon { types = take 2 t.pokemon.types <> [ty] } }

    RemoveType ty -> editTarget \t -> t
      { pokemon = t.pokemon { types = filter (/=ty) t.pokemon.types } }

    Camouflage -> error "Camouflage"
    Recharge   -> error "Recharge"
    Precharge  -> error "Precharge"

    SwapAttDef -> editTarget \t -> t
      { stats = t.stats { att = t.stats.def, def = t.stats.att, hp=t.stats.hp } }

    AvgAtt -> do
      let att = div (mon1.stats.att + mon2.stats.att) 2
      let spA = div (mon1.stats.spA + mon2.stats.spA) 2

      editUser   \u -> u { stats = u.stats {hp=u.stats.hp, att=att, spA=spA} }
      editTarget \u -> u { stats = u.stats {hp=u.stats.hp, att=att, spA=spA} }

    AvgDef -> do
      let def = div (mon1.stats.def + mon2.stats.def) 2
      let spD = div (mon1.stats.spD + mon2.stats.spD) 2

      editUser   \u -> u { stats = u.stats {hp=u.stats.hp, def=def, spD=spD} }
      editTarget \u -> u { stats = u.stats {hp=u.stats.hp, def=def, spD=spD} }

    SwpAtt -> do
      editUser   \u -> u { stats = u.stats {hp=u.stats.hp, att=mon2.stats.att, spA=mon2.stats.spA} }
      editTarget \u -> u { stats = u.stats {hp=u.stats.hp, att=mon1.stats.att, spA=mon1.stats.spA} }

    SwpDef -> do
      editUser   \u -> u { stats = u.stats {hp=u.stats.hp, def=mon2.stats.def, spD=mon2.stats.spD} }
      editTarget \u -> u { stats = u.stats {hp=u.stats.hp, def=mon1.stats.def, spD=mon1.stats.spD} }

    Switch {} -> error "Switch"

    Recover n -> editTarget \t -> t
      { pokemon = t.pokemon
        { hp = max (fi mon2.stats.hp) $ t.pokemon.hp + round (fi mon2.stats.hp * n)
        }
      }

    Drain _ -> error "Drain"
    DrainSleeping _ -> error "DrainSleeping"
    PainSplit -> error "PainSplit" -- TODO: Lazy

    MatchUserHP -> editTarget \t -> t
      { pokemon = t.pokemon { hp = min t.stats.hp mon1.pokemon.hp } }

    FractionalDamage n -> editTarget \t -> t
      { pokemon = t.pokemon { hp = round $ fi t.pokemon.hp * n } }

    FractionalDamageMax n -> editTarget \t -> t
      { pokemon = t.pokemon
        { hp = max 0 $ t.pokemon.hp - round (fi mon2.stats.hp * n)
        }
      }

    ConstantDamage n -> editTarget \t -> t
      { pokemon = t.pokemon
        { hp = max 0 $ t.pokemon.hp - n
        }
      }

    LevelDamage -> editTarget \t -> t
      { pokemon = t.pokemon
        { hp = max 0 $ t.pokemon.hp - mon2.pokemon.level
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
      editUser \u -> u
        { pokemon = u.pokemon
          { hp = max 0 $ u.pokemon.hp - div u.stats.hp 4
          }
        }

    RecoilMax n ->
      editUser \u -> u
        { pokemon = u.pokemon
          { hp = max 0 $ u.pokemon.hp - round (fi u.stats.hp * n)
          }
        }

    Protect ->
      protectTarget

    -- TODO: Attack drop
    KingShield ->
      protectTarget

    -- TODO: Poison
    BanefulBunker ->
      protectTarget

    WideGuard ->
      editTarget \t -> t { wideGuard = True }

    Substitute -> do
      when (mon2.subHP < 1) do
      let health = div mon2.stats.hp 4
      when (mon2.pokemon.hp > health) do
      editTarget \t -> t
        { subHP = health
        , pokemon = t.pokemon { hp = t.pokemon.hp - health }
        }

    IgnoreProtect _ -> error "IgnoreProtect"

    Endure -> editTarget \t -> t
      { enduring = True }

    Don'tKill -> error "Don'tKill"

    FinalGambit -> error "FinalGambit"
    BeakBlast -> error "BeakBlast"
    ShieldTrap -> error "ShieldTrap"

    DestinyBond -> editTarget \t -> t
      { destinyBond = 1 }

    Wish -> error "Wish"
    Delay2Turns -> error "Delay2Turns"

    UserPrimary -> error "UserPrimary"
    ExtraType _ -> error "ExtraType"

    Snatch -> editTarget \t -> t
      { snatching = True }

    Instruct -> error "Instruct"
    AllySwap -> error "AllySwap"

    Taunt   -> editTarget \t -> t { taunt  = 3 }
    Embargo -> editTarget \t -> t { embargo = 3 }
    Encore  -> error "Encore"
    Disable -> error "Disable"
    Metronome -> error "Metronome"
    Assist -> error "Assist"
    AfterYou -> error "AfterYou"
    MorpekoMode -> error "MorpekoMode"

    UserDies -> editUser \u -> u
      { pokemon = u.pokemon { hp = 0 } }

    Autotomize -> error "Autotomize"
    AxeKick -> error "AxeKick"
    BaddyBad -> error "BaddyBad"
    BeatUp -> error "BeatUp"
    Belch -> error "Belch"
    Bestow -> error "Bestow"
    BodyPress -> error "BodyPress"
    EatBerry -> error "EatBerry"

    BurnIfBoosted -> do
      when (isNothing mon2.pokemon.status && any (>0) mon2.boosts) do
      putTargetStatus (Just Pok.Burn)

    Captivate -> do
      when ( mon1.pokemon.gender /= Genderless
          && mon2.pokemon.gender /= Genderless
          && mon1.pokemon.gender /= mon2.pokemon.gender
           ) do
      editTarget \t -> t
        { boosts = t.boosts + zero { spA= -2, acc=0 } }

    Charge -> editTarget \t -> t { charged = True }

    ChillyReception -> error "ChillyReception"
    BoostSuperEffective -> error "BoostSuperEffective"
    Comeuppance -> error "Comeuppance"
    Conversion -> error "Conversion"
    Conversion2 -> error "Conversion2"
    Copycat -> error "Copycat"
    CoreEnforcer -> error "CoreEnforcer"

    RemoveItem -> editTarget \t -> t
      { pokemon = t.pokemon { heldItem = Nothing } }

    StealItem -> do
      editTarget \t -> t { pokemon = t.pokemon { heldItem = Nothing } }
      when (isNothing mon1.pokemon.heldItem) do
      editUser \u -> u { pokemon = u.pokemon { heldItem = mon2.pokemon.heldItem } }

    Safeguard -> editLane2 \l -> l
      { safeguard = 3 }

    CraftyShield -> error "CraftyShield"
    Defog -> error "Defog"
    RemovePP _ -> error "RemovePP"
    ExpandingForce -> error "ExpandingForce"
    NoFleeingNextTurn -> error "NoFleeingNextTurn"

    FirePledge  -> error "FirePledge"
    WaterPledge -> error "WaterPledge"
    GrassPledge -> error "GrassPledge"

    BellyDrum -> do
      let dmg = div mon2.stats.hp 2
      when (mon2.pokemon.hp > dmg) do
      editTarget \t -> t
        { pokemon = t.pokemon { hp = t.pokemon.hp - dmg }
        , boosts  = t.boosts { att=6, acc=t.boosts.acc }
        }

    FilletAway -> error "FilletAway"

    -- TODO: This will swap things like tailwind, screens and safeguard.
    -- Verify that this is correct behavior
    SwapFieldEffects -> do
      editLane1 (const field.lane2)
      editLane2 (const field.lane1)

    Bide -> error "Bide"
    MirrorCoat -> error "MirrorCoat"
    Counter -> error "Counter"

    AquaRing -> editTarget \t -> t { aquaRing  = True }
    Ingrain  -> editTarget \t -> t { ingrained = True }

    DamageWithSplinters -> error "DamageWithSplinters"
    DieHealSwitchIn -> error "DieHealSwitchIn"
    SplashDamage -> error "SplashDamage"
    Fling -> error "Fling"
    FloralHealing -> error "FloralHealing"
    FloralShield -> error "FloralShield"

    FlinchIfHit -> error "FlinchIfHit"
    FollowMe -> error "FollowMe"
    Foresight -> error "Foresight"
    UseTargetAtt -> error "UseTargetAtt"
    SuperEffectiveAgainst _ -> error "SuperEffectiveAgainst _"

    FusionBolt -> error "FusionBolt"
    FusionFlare -> error "FusionFlare"

    GearUp -> error "GearUp"
    MagneticFlux -> error "MagneticFlux"

    NoSpam -> error "NoSpam"
    GlaiveRush -> error "GlaiveRush"
    GlitzyGlow -> error "GlitzyGlow"
    GrassyGlide -> error "GrassyGlide"
    Grudge -> error "Grudge"
    GyroBall -> error "GyroBall"
    HappyHour -> error "HappyHour"
    HealBlock -> editTarget \t -> t { healBlock = 5 }
    HelpingHand -> error "HelpingHand"
    HiddenPower -> error "HiddenPower"

    DamageUserIfMiss _ -> error "DamageUserIfMiss"
    Scaling5Turns -> error "Scaling5Turns"
    DefenceCurlUsed -> editTarget \t -> t { defenceCurl = True }
    DoubleDmgIfDefenceCurlUsed -> error "DoubleDmgIfDefenceCurlUsed"
    ClearTerrain -> editField \f -> f { terrain = Nothing }
    Imprison -> error "Imprison"
    RemoveBerry _ -> error "RemoveBerry"

    IonDeluge -> error "IonDeluge"
    NoSwitchUserAndTarget -> error "NoSwitchUserAndTarget"
    Judgement -> error "Judgement"
    JungleHealing -> error "JungleHealing"
    LaserFocus -> editTarget \t -> t { focused = True }
    DoublePwrIfUserDebuff -> error "DoublePwrIfUserDebuff"
    AllOtherMovesUsed -> error "AllOtherMovesUsed"
    LastRespects -> error "LastRespects"
    LightThatBurnsTheSky -> error "LightThatBurnsTheSky"
    LockOn -> editTarget \t -> t { lockedOn = True }
    MagicCoat -> editTarget \t -> t { magicCoat = True }
    MagnetRise -> editTarget \t -> t { magnetRise = 5 }
    Magnitude -> error "Magnitude"
    MeFirst -> error "MeFirst"
    MatchTarget'sDamage _ -> error "MatchTarget'sDamage"
    Mimic -> error "Mimic"
    MiracleEye -> editTarget \t -> t
      { miracleEye = True
      , boosts = t.boosts { eva = 0 }
      }
    MirrorMove -> error "MirrorMove"
    Mist -> editTarget \t -> t { mist=True }

    PwrInTerrain _ -> error "PwrInTerrain"
    IgnoreAbility -> error "IgnoreAbility"

    RecoverWeather -> error "RecoverWeather"
    MudSport -> editField \l -> l { mudSport = 3 }
    MultiAttack -> error "MultiAttack"
    NaturalGift -> error "NaturalGift"
    NaturePower -> error "NaturePower"

    Nightmare -> do
      when (isAsleep mon2.pokemon) do
      editTarget \t -> t
        { pokemon = t.pokemon
          { hp = max 0 $ t.pokemon.hp - div t.stats.hp 4
          }
        }

----

editUser f =
  modify \b -> b { mon1 = f b.mon1 }

editTarget f =
  modify \b -> b { mon2 = f b.mon2 }

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

putTargetStatus s =
  editTarget \mon -> mon { pokemon = mon.pokemon { status = s } }

-- TODO: May fail if used consequtively
protectTarget = do
  Battle {mon2} <- get
  let successRate = (1/3) ** fi mon2.protections
  roll <- liftIO $ randomRIO (0.0, 1.0)
  when (roll <= successRate) do
  editTarget \t -> t
    { protected   = True
    , protections = succ t.protections
    }

