{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
module Pokemon.Move where

import Pokemon.Type
import Pokemon.Stat hiding (Stat (..))
import Data.Word
import Data.Int
import Data.Bits ((.|.), (.&.))
import Data.List qualified as L
import Prelude   hiding (Category)

type Ability = Int

data Category
   = Physical
   | Special
   | Status
   deriving (Show, Eq, Ord, Enum, Bounded)

data Move = Move
   { ty      :: TYPE
   , cat     :: Category
   , pow     :: Word8  -- The base power (BP) of a move
   , acc     :: Word8  -- probability of the move hitting the target (0=Always /= 100 due to evasion)
   , crit    :: Word8  -- 1="high crit chance", alwaysCrit="always crits"
   , hits    :: Word8  -- Number of times to perform the move, between 2 and N (N if N<2)
   , pri     :: Int8   -- Priority bracket. In each bracket, faster pokemon attack first (unless trick room)
   , flags   :: Word8
   , targ    :: Target -- The target(s) of the move
   , eff     :: Effect -- additional effects if the move is successful
   }
   deriving (Show, Eq, Ord)

neverMiss  = 0 -- acc
alwaysCrit = 6 -- crit

-- Foe  Foe  Foe
-- Self Ally Ally
-- Wide
type Target = Word8

pattern SELF     = 0b0001000 :: Target
pattern ADJACENT = 0b1100100 :: Target
pattern ALL      = 0b1111110 :: Target
pattern ADJFOES  = 0b1100000 :: Target
pattern ALLIES   = 0b0000110 :: Target
pattern WIDE     = 0b0000001 :: Target -- hit all targets if set, otherwise choose one

-- Fields
--
pattern DANCE   = 0b1       -- dance moves are copied by pokemon with dancer
pattern SOUND   = 0b10      -- sound-based moves ignore substitues, but pokemon with soundproof are immune
pattern BULLET  = 0b100     -- pokemon with bulletproof are immune
pattern POWDER  = 0b1000    -- pokemon holding safety googles are immune
pattern CONTACT = 0b10000   -- if a move makes contact it may trigger additional effects
pattern ZMOVE   = 0b100000  -- z-moves can hit through protect at reduced damage
pattern TURN1   = 0b1000000 -- wether the move can only be used on the first turn

infixr 5 :%
infixr 4 :+

data Effect
   = None             -- No effect
   | Effect :+ Effect -- Do both effects
   | Int :% Effect    -- Roll a number between 0-99, if less than X do effect
   | Choose [Effect]  -- Randomly pick an effect

   | Flinch
   | Confuse
   | Attract
   | Curse
   | LeechSeed
   | Yawn
   | PerishSong

   | Gravity
   | TrickRoom
   | MagicRoom
   | WonderRoom
   | Tailwind
   | LuckyChant -- foes cannot land crits for 5 turns

   | EStatus  Status
   | EWeather Weather
   | ETerrain Terrain
   | EScreen  Screen
   | EHazard  Hazard
   | EInvul   Invulnerable -- enter a semi-invulnerable state. The move is delayed until next
                           -- turn, then the semi-invulnerable state ends.
   | ELocking LockingMove -- prevent target from switching out AND damage over time (5 turns)
                          -- each of these can be stacked!

   | NoSwitch -- prevent target from switching out (mean look, block, ..)
   | Locked   -- user is locked to performing the move for 2 or 3 turns, then become confused (outrage, petal dance)

   | ClearStatus
   | ClearStatusParty -- all ally pokemon in party
   | ClearScreen
   | ClearHazard

   | AddBoost Bool (Boost Int) -- if true, target the user
   | AddBoostIfKO  (Boost Int) -- give user boost if attack KO's
   | AddRandomBoost Int

   | ClearBoost   -- all boosts are set to 0
   | InvBoost     -- all boosts are multiplied by -1
   | CopyBoost    -- user copies boosts from target
   | MoveBoost    -- move boosts from target to user
   | SwapBoost    -- swap boosts of target and user
   | IgnoreBoosts -- attack ignore boosts

   | SetAbility  Ability -- worry seed, entrainment
   | CopyAbility CopyAbility
   | SwapAbility
   | SuppressAbility -- Targets ability is suppressed. This effectively prevents the target
                     -- from having an ability at all, even if it is changed

   | SetType TYPE Bool -- set type of target or the user if True (soak, electrify, burn up)
   | AddType TYPE      -- add a third type to the target (forest curse, trick or treat)
   | RemoveType TYPE   -- user loses the given type
   | Camouflage        -- change user's type based on location

   | Recharge  -- The move is performed, but the user must skip the next turn
   | Precharge -- The user skips this turn, the move is performed next turn

   | SwapAttDef -- swap att and def
   | AvgAtt     -- average att and spA with target
   | SwpAtt     -- swap att and spA with targer
   | AvgDef     -- average def and spD with target
   | SwpDef     -- swap def and spD with targer

   -- target must switch with an ally at the end of the move.
   -- if 'user', the user of the move switches, regardless of target
   -- if 'random', a random ally is used, otherwise an ally must be choosen
   -- if 'keepBoost', any boosts are passed on to the new pokemon
   | Switch { user, random, keepBoost :: Bool }

   | Recover Float       -- recover a percentage of max health
   | Drain   Float       -- recover a percentage of damage inflicted
   | DrainSleeping Float -- like drain, if target is asleep
   | PainSplit           -- average missing health with target
   | MatchUserHP         -- Set target's HP to that of the user

   | FractionalDamage    Float -- current HP is multiplied by X
   | FractionalDamageMax Float -- target takes damage a fraction its max HP

   | ConstantDamage Int -- always deal a constant amount of damage (dragon rage, sonic boom)
   | LevelDamage        -- always deal damage equal to the users level (night shade)

   | CutPwrUserHP   -- power is multiplied by currentHP/maxHP (water spout, eruption, dragon energy)
   | RaisePwrUserHP -- power is increased on lower HP (flail, reversal)
   | AddPwr         -- add 20 power for each stat boost (stored power, power trip)
   | UseDef         -- use physical defence, even if the move is special

   | DoublePwrNoItem         -- double power if no held item
   | DoublePwrIfHit          -- double power if user hit on the same turn
   | DoublePwrIfTargetFaster -- double power if user attacks after the target
   | DoublePwrIfTargetSlower -- double power if user attacks before the target
   | DoubleDmgIfTargetStatus -- double damage if target has a status
   | DoublePwrIfUserStatus   -- double damage if user   has a status
   | DoubleDmgIfDynamax      -- double damage if target is dynamaxed
   | DoublePwrIfTargetHalfHP -- double power if target has 1/2 HP or less
   | CrushGrip               -- more powerful when opponent has higher HP
   | DoublePowerIfInvul Invulnerable

   | PwrLowFriendship  -- (frustration)
   | PwrHighFriendship -- (return)
   | PwrHeavyTarget    -- The heavier the opponent, the stronger the attack
   | PwrHeavyUser      -- More power the heavier the user
   | PwrInHarshSunlight

   | EchoPower  -- power up consecutive hits (echoed voice, fury cutter)
   | SpeedPower -- more power the faster the user (electro ball)

   | Recoil Float -- user is damaged by a fraction of damage dealt
   | Struggle     -- user loses 1/4th of maximum hp
   | RecoilMax Float -- user is damaged by a fraction of it's own max HP

   | Protect       -- protect, detect
   | KingShield    -- like protect, but contact moves receive a negative attack boost
   | BanefulBunker -- like protect, but contact moves receive poison status
   | WideGuard     -- like protect, but only for moves that hit multiple pokemon
   | Substitute
   | IgnoreProtect Bool -- the move can hit through protect (phantom force, horn drill, ..)
                        -- if True, the move fails if the target does not protect (feint)

   | Endure      -- user survive at at-least 1 HP, may fail if used in succession
   | Don'tKill   -- Target survives at at-least 1 HP
   | FinalGambit -- user faints, target take damage equal to users HP

   | BeakBlast   -- if hit, attacker is burned
   | ShieldTrap  -- if attacked, attacker is damaged
   | DestinyBond -- if killed, attacker is killed. Lasts 2 turns

   | Wish        -- Heal pokemon in the spot targeted in 2 turns
   | Delay2Turns -- Damage pokemon in the spot targeted in 2 turns

   | UserPrimary    -- the type of the move is the users primary type
   | ExtraType TYPE -- the move has an extra type (flying press)

   | Snatch
   | Instruct
   | AllySwap
   | Taunt
   | Encore
   | Disable
   | Embargo
   | Metronome
   | Assist
   | UserDies -- user dies
   | AfterYou
   | MorpekoMode
   | Autotomize -- reduce weight by 100kg and raise speed 2 stages
   | AxeKick -- user loses HP if it misses, may confuse
   | BaddyBad -- reduces damage from physical attacks
   | BeatUp
   | Belch -- User must have consumed a berry
   | Bestow -- User gives held item to target
   | BodyPress -- The higher the user's defence, the stronger the attack
   | EatBerry
   | BurnIfBoosted
   | Captivate -- sharpely lower spA if opposite gender
   | Charge -- raises spD and next elec move deals more damage
   | ChillyReception -- switch out and summon a snowstorm lasting 5 turns
   | BoostSuperEffective -- boosted even more if it's super-effective (lollision course, electro drift)
   | Comeuppance -- deals more damage to the opponent that last inflicted damage on it
   | Conversion -- change user's type to that of its first move
   | Conversion2 -- users type changes to become resistent to opponents last move
   | Copycat -- copy most recently used move (by any pokemon)
   | CoreEnforcer -- suppress targets ability if it has already moved
   | RemoveItem
   | StealItem
   | Safeguard
   | CraftyShield
   | Defog
   | RemovePP Int -- Remove n PP from target's last-used move
   | ExpandingForce -- Increases power and hits all opponents on psychic terrain
   | NoFleeingNextTurn -- (fairy lock)

   | FirePledge
   | WaterPledge
   | GrassPledge

   | BellyDrum -- Take 50% damage, max att
   | FilletAway -- Lower hp but sharply boost att, spA, spe

   | SwapFieldEffects -- (court change)

   | Bide -- User takes damage for 2 turns, then strikes back double
   | MirrorCoat
   | Counter

   | AquaRing
   | Ingrain

   | DamageWithSplinters -- target takes damage from splinters each turn

   | DieHealSwitchIn
   | SplashDamage -- 1/16 damage to AllyNotUser, or Allies if an ally is targeted (flame burst)

   | Fling
   | FloralHealing -- restore half HP, "more" in grassy terrain
   | FloralShield  -- sharply raises defence of all grass types on the field

   | FlinchIfHit
   | FollowMe
   | Foresight
   | UseTargetAtt -- (foul play)

   | SuperEffectiveAgainst TYPE -- This attack is additionally super-effective against the given type

   | FusionBolt
   | FusionFlare

   | GearUp       -- raise att,spA of all ally pokemon with Plus or Minus
   | MagneticFlux -- raise def,spD of plus/minus

   | NoSpam -- move cannot be used twice in a row
   | GlaiveRush
   | GlitzyGlow -- Reduces damage from special attacks
   | GrassyGlide -- High priority during grassy terrain
   | Grudge -- if the user faints after using this move, the PP of the killing move is depleted
   | GyroBall
   | HappyHour -- double prize money
   | HealBlock -- prevent from using healing moves for 5 turns
   | HelpingHand -- multiple power of target's attack by 1.5
   | HiddenPower -- type and power depends on user's IVs

   | DamageUserIfMiss Float

   | Scaling5Turns -- Lock user in to the attack for 5 turns, damage doubles each turn
   | DefenceCurlUsed -- Flag the user as having used defence curl
   | DoubleDmgIfDefenceCurlUsed
   | ClearTerrain
   | Imprison
   | RemoveBerry {user :: Bool}

   | IonDeluge -- Changes normal moves to electric ones

   | NoSwitchUserAndTarget -- (jaw lock)
   | Judgement -- type depends on arceus plate being held
   | JungleHealing -- Restore team's HP and cures status conditions
   | LaserFocus -- next hit is guaranteed to crit
   | DoublePwrIfUserDebuff -- double power if user received a debuff this turn
   | AllOtherMovesUsed -- Move fails unless all other moves have been used (last resort)
   | LastRespects -- increase power the more allies have been defeated
   | LightThatBurnsTheSky -- ignores target ability; uses highest attack stat
   | LockOn -- next hit is guaranteed to hit
   | MagicCoat -- reflects status conditions back to the attacker
   | MagnetRise -- user becomes immune to ground moves for 5 turns
   | Magnitude -- hits with random power
   | MeFirst -- copy opponent's attack with 1.5x power
   | MatchTarget'sDamage Float -- deal damage equal to X times the opponent's attack
   | Mimic -- copy the target's last move, replacing mimic
   | MiracleEye -- clear targets evasion, remove dark-type's psychic immunity
   | MirrorMove -- perform the target's last move
   | Mist -- User's stat changes cannot be changed "for a period of time"

   | PwrInTerrain Terrain -- power increases in the given terrain (misty explosion)
   | IgnoreAbility -- Ignore the targets ability

   | RecoverWeather -- Recover an amount of health that varies with weather
   | MudSport -- Weakens the power of Electric-type moves
   | MultiAttack -- Type varies depending on held memory item
   | NaturalGift -- power and type depends on held berry
   | NaturePower -- Uses a certain move based on current terrain
   | Nightmare -- 0.25 damage to sleeping target

   deriving (Show, Eq, Ord)

pattern OHKO   = FractionalDamage 1.0
pattern HalfHP = FractionalDamage 0.5


data Terrain
   = TGrass
   | TPsychic
   | TElectric
   | TMisty
   deriving (Show, Eq, Ord, Enum, Bounded)

data Status
   = Paralysis
   | Burn
   | Frostbite
   | Freeze
   | Sleep
   | Poison
   | Toxic
   deriving (Show, Eq, Ord, Enum, Bounded)

data Weather
   = Rain
   | Hail
   | Sun
   | Sandstorm
   deriving (Show, Eq, Ord, Enum, Bounded)

data Screen
   = Phy    -- reflect
   | Spe    -- light screen
   | PhySpe -- aurora veil
   deriving (Show, Eq, Ord, Enum, Bounded)

data Hazard
   = Spikes
   | ToxicSpikes
   | Rocks
   | Web
   deriving (Show, Eq, Ord, Enum, Bounded)

data Invulnerable
   = Flying  -- fly, sky drop
   | Diving  -- dive
   | Digging -- dig
   | Phantom -- phantom force
   deriving (Show, Eq, Ord, Enum, Bounded)

data CopyAbility
   = Target2Allies
   | Target2User
   | User2Target
   deriving (Show, Eq, Ord, Enum, Bounded)

data LockingMove
   = Whirlpool
   | Firespin
   | Bind
   | Clamp
   | Infestation
   | MagmaStorm
   deriving (Show, Eq, Ord, Enum, Bounded)

----

isAttack move =
  cat move /= Status

isWide move =
  targ move .&. WIDE /= 0

----

data MoveDesc = MoveDesc
   { name :: String -- name of the move
   , pp   :: Int    -- base PP
   , move :: Move   -- things that happen upon selection, one per turn
   }
   deriving (Show, Eq, Ord)

instance Semigroup Effect where (<>) = (:+)
instance Monoid    Effect where mempty = None

----

celebrate = Move
  { ty      = NOR
  , cat     = Status
  , pow     = 0
  , acc     = neverMiss
  , crit    = 0
  , pri     = 0
  , targ    = SELF
  , hits    = 1
  , eff     = None
  , flags  = 0
  }

tackle = Move
  { ty      = NOR
  , cat     = Physical
  , pow     = 40
  , acc     = 100
  , crit    = 0
  , pri     = 0
  , targ    = ADJACENT
  , hits    = 1
  , eff     = None
  , flags  = 0
  }


moves =
  [ MoveDesc "10,000,000 Volt Thunderbolt" 1 tackle
      {ty=ELE, flags=ZMOVE, cat=Special, crit=1, pow=195}
  , MoveDesc "Absorb" 25 tackle
      {ty=GRA, cat=Special, eff=Drain 0.5, pow=20}
  , MoveDesc "Accelerock" 20 tackle
      {ty=ROC, pow=40, pri=1, flags=CONTACT}
  , MoveDesc "Acid" 30 tackle
      {ty=POI, cat=Special, pow=40, eff=10 :% AddBoost False zero {spD = -1}}
  , MoveDesc "Acid Armor" 20 celebrate
      {ty=POI, eff=AddBoost True zero {def = 2}}
  -- acid downpour
  , MoveDesc "Acid Spray" 20 tackle
      {ty=POI, cat=Special, pow=40, eff=AddBoost False zero {spD = -2}}
  , MoveDesc "Acrobatics" 15 tackle
      {ty=FLY, pow=55, eff=DoublePwrNoItem, flags=CONTACT}
  , MoveDesc "Acupressure" 30 celebrate
      {ty=NOR, eff=AddRandomBoost 2}
  , MoveDesc "Aerial Ace" 20 tackle
      {ty=FLY, pow=60, acc=neverMiss, flags=CONTACT}
  , MoveDesc "Aeroblast" 5 tackle
      {ty=FLY, cat=Special, pow=100, acc=95, crit=1}
  , MoveDesc "After You" 15 celebrate
      {ty=NOR, targ=ADJACENT, eff=AfterYou}
  , MoveDesc "Agility" 30 celebrate
      {ty=PSY, eff=AddBoost True zero {spe = 2}}
  , MoveDesc "Air Cutter" 25 tackle
      {ty=FLY, cat=Special, pow=60, acc=95, crit=1}
  , MoveDesc "Air Slash" 15 tackle
      {ty=FLY, cat=Special, pow=75, acc=95, eff=30 :% Flinch}
  -- all-out plummeling
  , MoveDesc "Ally Switch" 15 celebrate
      {ty=PSY, eff=AllySwap, targ=SELF}
  , MoveDesc "Amnesia" 20 celebrate
      {ty=PSY, eff=AddBoost True zero {spD=2}}
  , MoveDesc "Anchor Shot" 20 tackle
      {ty=STE, pow=80, eff=NoSwitch, flags=CONTACT}
  , MoveDesc "Ancient Power" 5 tackle
      {ty=ROC, pow=60, cat=Special, eff=10 :% AddBoost True one {eva=0,acc=0,cri=0}}
  , MoveDesc "Apple Acid" 10 tackle
      {ty=GRA, cat=Special, pow=80, eff=AddBoost False zero {spD = -1}}
  , MoveDesc "Aqua Cutter" 20 tackle
      {ty=WAT, pow=70, crit=1}
  , MoveDesc "Aqua Jet" 20 tackle
      {ty=WAT, pow=40, pri=1, flags=CONTACT}
  , MoveDesc "Aqua Ring" 20 celebrate
      {ty=WAT, eff=AquaRing}
  , MoveDesc "Aqua Step" 10 tackle
      {ty=WAT, pow=80, eff=AddBoost True zero {spe=1}, flags=CONTACT}
  , MoveDesc "Aqua Tail" 10 tackle
      {ty=WAT, pow=90, flags=CONTACT}
  , MoveDesc "Arm Thrust" 20 tackle
      {ty=FIG, pow=15, hits=5, flags=CONTACT}
  , MoveDesc "Armor Cannon" 5 tackle
      {ty=FIR, pow=120, cat=Special, eff=AddBoost True zero {def= -1, spD= -1}}
  , MoveDesc "Aromatherapy" 5 celebrate
      {ty=GRA, eff=ClearStatusParty}
  , MoveDesc "Aromatic Mist" 20 celebrate
      {ty=FAI, eff=AddBoost False zero {spD=1}, targ=ADJACENT}
  , MoveDesc "Assist" 20 celebrate
      {ty=NOR, eff=Assist}
  , MoveDesc "Assurance" 10 tackle
      {ty=DAR, pow=60, eff=DoublePwrIfHit, flags=CONTACT}
  , MoveDesc "Astonish" 15 tackle
      {ty=GHO, pow=30, eff=30 :% Flinch, flags=CONTACT}
  , MoveDesc "Astrial Barrage" 5 tackle
      {ty=GHO, pow=120, cat=Special}
  , MoveDesc "Attack Order" 15 tackle
      {ty=BUG, pow=90, crit=1}
  , MoveDesc "Attract" 15 celebrate
      {ty=NOR, targ=ADJACENT, eff=Attract}
  , MoveDesc "Aura Sphere" 20 tackle
      {ty=FIG, cat=Special, acc=neverMiss, pow=80}
  , MoveDesc "Aura Wheel" 10 tackle
      {ty=ELE, pow=110, eff=MorpekoMode}
  , MoveDesc "Aurora Beam" 20 tackle
      {ty=ICE, cat=Special, pow=65, eff=10 :% AddBoost False zero {att= -1}}
  , MoveDesc "Aurora Veil" 20 celebrate
      {ty=ICE, eff=EScreen PhySpe}
  , MoveDesc "Autotomize" 15 celebrate
      {ty=STE, eff=Autotomize}
  , MoveDesc "Avalanche" 10 tackle
      {ty=ICE, pow=60, eff=DoublePwrIfHit, flags=CONTACT}
  , MoveDesc "Axe Kick" 10 tackle
      {ty=FIG, pow=120, eff=90 :% AxeKick, flags=CONTACT}

  , MoveDesc "Baby-Doll Eyes" 30 celebrate
      {ty=FAI, pri=1, eff=AddBoost False zero {att= -1}}
  , MoveDesc "Baddy Bad" 15 tackle
      {ty=DAR, cat=Special, pow=90, eff=BaddyBad}
  , MoveDesc "Baneful Bunker" 10 celebrate
      {ty=POI, eff=BanefulBunker}
  , MoveDesc "Barb Barrage" 10 tackle
      {ty=POI, pow=60, eff=DoubleDmgIfTargetStatus}
  , MoveDesc "Barrage" 20 tackle
      {ty=NOR, pow=15, acc=85, hits=5}
  , MoveDesc "Barrier" 20 celebrate
      {ty=PSY, eff=AddBoost True zero {def=2}}
  , MoveDesc "Baton Pass" 40 celebrate
      {ty=NOR, eff=Switch True False True}
  , MoveDesc "Beak Blast" 15 tackle
      {ty=FLY, pow=100, eff=BeakBlast, pri= -3}
  , MoveDesc "Beat Up" 10 tackle
      {ty=DAR, pow=0, eff=BeatUp}
  , MoveDesc "Behemoth Bash" 5 tackle
      {ty=STE, pow=100, eff=DoubleDmgIfDynamax, flags=CONTACT}
  , MoveDesc "Behemoth Blade" 5 tackle
      {ty=STE, pow=100, eff=DoubleDmgIfDynamax, flags=CONTACT}
  , MoveDesc "Belch" 10 tackle
      {ty=STE, pow=120, cat=Special, eff=Belch}
  , MoveDesc "Belly Drum" 10 celebrate
      {ty=NOR, eff=BellyDrum}
  , MoveDesc "Bestow" 15 celebrate
      {ty=NOR, eff=Bestow}
  , MoveDesc "Bide" 10 tackle
      {ty=NOR, eff=Bide, flags=CONTACT}
  , MoveDesc "Bind" 20 tackle
      {ty=NOR, acc=85, eff=ELocking Bind, flags=CONTACT}
  , MoveDesc "Bite" 25 tackle
      {ty=DAR, pow=60, eff=30 :% Flinch, flags=CONTACT}
  , MoveDesc "Bitter Blade" 10 tackle
      {ty=FIR, pow=90, eff=Drain 0.5, flags=CONTACT}
  , MoveDesc "Bitter Malice" 10 tackle
      {ty=GHO, pow=75, cat=Special, eff=DoubleDmgIfTargetStatus}
  -- black hole eclipse
  , MoveDesc "Blast Burn" 5 tackle
      {ty=FIR, pow=150, cat=Special, acc=90, eff=Recharge}
  , MoveDesc "Blaze Kick" 10 tackle
      {ty=FIR, pow=85, acc=90, crit=1, eff=10 :% EStatus Burn, flags=CONTACT}
  , MoveDesc "Blazing Torque" 10 tackle
      {ty=FIR, pow=80}
  , MoveDesc "Bleakwind Storm" 10 tackle
      {ty=FLY, pow=100, acc=80, eff=10 :% EStatus Frostbite}
  , MoveDesc "Blizzard" 5 tackle
      {ty=ICE, pow=110, cat=Special, acc=70, eff=10 :% EStatus Freeze}
  , MoveDesc "Block" 5 celebrate
      {ty=NOR, targ=ADJACENT, eff=NoSwitch}
  -- bloom doom
  , MoveDesc "Blue Flare" 5 tackle
      {ty=FIR, pow=130, cat=Special, acc=85, eff=20 :% EStatus Burn}
  , MoveDesc "Body Press" 10 tackle
      {ty=FIG, pow=80, eff=BodyPress, flags=CONTACT}
  , MoveDesc "Body Slam" 15 tackle
      {ty=NOR, pow=85, eff=30 :% EStatus Paralysis, flags=CONTACT}
  , MoveDesc "Bolt Beak" 10 tackle
      {ty=ELE, pow=85, eff=DoublePwrIfTargetFaster, flags=CONTACT}
  , MoveDesc "Bolt Strike" 5 tackle
      {ty=ELE, pow=130, acc=85, eff=20 :% EStatus Paralysis, flags=CONTACT}
  , MoveDesc "Bone Club" 20 tackle
      {ty=GRO, pow=65, acc=85, eff=10 :% Flinch}
  , MoveDesc "Bone Rush" 10 tackle
      {ty=GRO, pow=25, acc=90, hits=5}
  , MoveDesc "Bonemerang" 10 tackle
      {ty=GRO, pow=50, acc=90, hits=2}
  , MoveDesc "Boomburst" 10 tackle
      {ty=NOR, pow=140, cat=Special, targ=ADJACENT .|. WIDE, flags=SOUND}
  , MoveDesc "Bounce" 5 tackle
      {pow = 85, ty=FLY, acc=85, eff=EInvul Flying :+ 30 :% EStatus Paralysis, flags=CONTACT}
  , MoveDesc "Bouncy Bubble" 15 tackle
      {ty=WAT, pow=90, cat=Special, eff=Drain 0.5}
  , MoveDesc "Branch Poke" 40 tackle
      {ty=GRA, pow=40, flags=CONTACT}
  , MoveDesc "Brave Bird" 15 tackle
      {ty=FLY, pow=120, eff=Recoil (1/3), flags=CONTACT}
  , MoveDesc "Breaking Swipe" 15 tackle
      {ty=DRA, pow=60, targ=ADJFOES .|. WIDE, flags=CONTACT}
  -- breakneck blitz
  , MoveDesc "Brick Break" 15 tackle
      {ty=FIG, pow=75, eff=ClearScreen, flags=CONTACT}
  , MoveDesc "Brine" 10 tackle
      {ty=WAT, pow=65, eff=DoublePwrIfTargetHalfHP, cat=Special}
  , MoveDesc "Brutal Swing" 20 tackle
      {ty=DAR, pow=60, targ=ADJACENT .|. WIDE, flags=CONTACT}
  , MoveDesc "Bubble" 30 tackle
      {ty=WAT, pow=40, cat=Special, eff=10 :% AddBoost False zero {spe= -1}}
  , MoveDesc "Bubble Beam" 20 tackle
      {ty=WAT, pow=65, cat=Special, eff=10 :% AddBoost False zero {spe= -1}}
  , MoveDesc "Bug Bite" 20 tackle
      {ty=BUG, pow=60, eff=EatBerry, flags=CONTACT}
  , MoveDesc "Bug Buzz" 10 tackle
      {ty=BUG, pow=90, cat=Special, eff=10 :% AddBoost False zero {spD= -1}}
  , MoveDesc "Bulk Up" 20 celebrate
      {ty=FIG, eff=AddBoost True zero {att=1, def=1}}
  , MoveDesc "Bulldoze" 20 tackle
      {ty=GRO, pow=60, targ=ADJACENT .|. WIDE, eff=AddBoost False zero {spe= -1}}
  , MoveDesc "Bullet Punch" 30 tackle
      {ty=STE, pow=40, pri=1, flags=CONTACT}
  , MoveDesc "Bullet Seed" 30 tackle
      {ty=GRA, pow=25, hits=5}
  , MoveDesc "Burn Up" 5 tackle
      {ty=FIR, pow=130, cat=Special, eff=SetType NON True}
  , MoveDesc "Burning Jealousy" 5 tackle
      {ty=FIR, pow=70, cat=Special, targ=ADJFOES .|. WIDE, eff=BurnIfBoosted}
  , MoveDesc "Buggy Buzz" 15 tackle
      {ty=ELE, pow=90, cat=Special, eff=EStatus Paralysis}

  , MoveDesc "Calm Mind" 20 celebrate
      {ty=PSY, eff=AddBoost True zero{spA=1, spD=1}}
  , MoveDesc "Camouflage" 20 celebrate
      {ty=NOR, eff=Camouflage}
  , MoveDesc "Captivate" 20 celebrate
      {ty=NOR, acc=100, targ=ADJACENT, eff=Captivate}
  , MoveDesc "Catastropika" 1 tackle
      {ty=ELE, pow=210, flags=CONTACT .|. ZMOVE}
  , MoveDesc "Ceaseless Edge" 15 tackle
      {ty=DAR, pow=65, acc=90, crit=1, eff=DamageWithSplinters, flags=CONTACT}
  , MoveDesc "Celebrate" 40 celebrate ,
      MoveDesc "Charge" 20 celebrate {ty=ELE, eff=Charge} ,
          MoveDesc "Charge Beam" 10 tackle
              {ty=ELE, pow=50, cat=Special, acc=90, eff=70 :% AddBoost True zero {spA=1}}
  , MoveDesc "Charm" 20 celebrate
      {ty=NOR, targ=ADJACENT, eff=AddBoost False zero {att= -2}}
  , MoveDesc "Chatter" 20 tackle
      {ty=FLY, pow=65, cat=Special, acc=100, eff=Confuse}
  , MoveDesc "Chilling Water" 20 tackle
      {ty=WAT, pow=50, cat=Special, eff=AddBoost False zero {att= -1}}
  , MoveDesc "Chilly Reception" 10 celebrate
      {ty=ICE, eff=ChillyReception}
  , MoveDesc "Chip Away" 20 tackle
      {ty=NOR, pow=70, eff=IgnoreBoosts, flags=CONTACT}
  , MoveDesc "Chloroblast" 5 tackle
      {ty=GRA, pow=150, acc=95, cat=Special, eff=Recoil 0.5} -- TODO: Recoil is a constant 0.5x of users maximum HP
  , MoveDesc "Circle Throw" 10 tackle
      {ty=FIG, pow=60, acc=90, eff=Switch False True False, flags=CONTACT}
  , MoveDesc "Clamp" 15 tackle
      {ty=WAT, pow=35, acc=85, eff=ELocking Clamp, flags=CONTACT}
  , MoveDesc "Clanging Scales" 5 tackle
      {ty=DRA, pow=110, cat=Special, eff=AddBoost True zero {def= -1}, targ=ADJFOES .|. WIDE}
  , MoveDesc "Clangorous Soul" 5 celebrate
      {ty=DRA, eff=AddBoost True zero {att=1,def=1,spA=1,spD=1,spe=1} :+ FractionalDamageMax (1/3)}
  , MoveDesc "Clangorous Soulblaze" 1 tackle
      {ty=DRA, pow=185, cat=Special, flags=ZMOVE}
  , MoveDesc "Clear Smog" 15 tackle
      {ty=POI, pow=50, cat=Special, eff=ClearBoost}
  , MoveDesc "Close Combat" 5 tackle
      {ty=FIG, pow=120, eff=AddBoost True zero {def= -1, spD= -1}, flags=CONTACT}
  , MoveDesc "Coaching" 10 celebrate
      {ty=FIG, targ=ALLIES .|. WIDE, eff=AddBoost False zero {att=1,def=1}}
  , MoveDesc "Coil" 20 celebrate
      {ty=POI, eff=AddBoost True zero {att=1,def=1,acc=1}}
  , MoveDesc "Collision Course" 5 tackle
      {ty=FIG, pow=100, eff=BoostSuperEffective, flags=CONTACT}
  , MoveDesc "Collision Torque" 10 tackle
      {ty=FIG, pow=100}
  , MoveDesc "Comet Punch" 15 tackle
      {ty=NOR, pow=18, acc=85, hits=5, flags=CONTACT}
  , MoveDesc "Comeuppance" 10 tackle
      {ty=DAR, pow=1, eff=Comeuppance, flags=CONTACT}
  , MoveDesc "Confide" 20 celebrate
      {ty=NOR, targ=ADJACENT, eff=AddBoost False zero {spA= -1}}
  , MoveDesc "Confuse Ray" 10 celebrate
      {ty=GHO, targ=ADJACENT, eff=Confuse}
  , MoveDesc "Confusion" 25 tackle
      {ty=PSY, pow=50, cat=Special, eff=10 :% Confuse}
  , MoveDesc "Constrict" 35 tackle
      {ty=NOR, pow=10, eff=10 :% AddBoost False zero {spe= -1}, flags=CONTACT}
  -- continental crush
  , MoveDesc "Conversion" 30 celebrate
      {ty=NOR, eff=Conversion}
  , MoveDesc "Conversion 2" 30 celebrate
      {ty=NOR, eff=Conversion2}
  , MoveDesc "Copycat" 20 celebrate
      {ty=NOR, eff=Copycat}
  , MoveDesc "Core Enforcer" 10 tackle
      {ty=DRA, cat=Special, pow=100, eff=CoreEnforcer}
  -- corkscrew crash
  , MoveDesc "Corrosive Gas" 40 celebrate
      {ty=POI, targ=ADJACENT, eff=RemoveItem}
  , MoveDesc "Cosmic Power" 20 celebrate
      {ty=PSY, eff=AddBoost True zero {def=1, spD=1}}
  , MoveDesc "Cotton Guard" 10 celebrate
      {ty=GRA, eff=AddBoost True zero {def=3}}
  , MoveDesc "Cotton Spore" 40 celebrate
      {ty=GRA, targ=ADJACENT, eff=AddBoost False zero {spe= -2}, flags=POWDER}
  , MoveDesc "Counter" 20 celebrate
      {ty=FIG, cat=Physical, eff=Counter, flags=CONTACT}
  , MoveDesc "Court Change" 10 celebrate
      {ty=NOR, eff=SwapFieldEffects}
  , MoveDesc "Covet" 25 tackle
      {ty=NOR, pow=60, eff=StealItem, flags=CONTACT}
  , MoveDesc "Crabhammer" 10 tackle
      {ty=WAT, pow=100, acc=90, crit=1, flags=CONTACT}
  , MoveDesc "Crafty Shield" 10 celebrate
      {ty=FAI, eff=CraftyShield}
  , MoveDesc "Cross Chop" 5 tackle
      {ty=FIG, pow=100, acc=80, crit=1, flags=CONTACT}
  , MoveDesc "Cross Poison" 20 tackle
      {ty=POI, pow=70, crit=1, eff=10 :% EStatus Poison, flags=CONTACT}
  , MoveDesc "Crunch" 15 tackle
      {ty=DAR, pow=80, eff=20 :% AddBoost False zero {def= -1}, flags=CONTACT}
  , MoveDesc "Crush Claw" 10 tackle
      {ty=NOR, pow=75, acc=95, eff=50 :% AddBoost False zero {def= -1}, flags=CONTACT}
  , MoveDesc "Crush Grip" 5 tackle
      {ty=NOR, pow=0, eff=CrushGrip, flags=CONTACT}
  , MoveDesc "Curse" 10 celebrate
      {ty=GHO, eff=Curse}
  , MoveDesc "Cut" 30 tackle
      {ty=NOR, pow=50, acc=95, flags=CONTACT}

  , MoveDesc "Dark Pulse" 15 tackle
      {ty=DAR, pow=80, cat=Special, eff=20 :% Flinch}
  , MoveDesc "Dark Void" 10 celebrate
      {ty=DAR, targ=ADJFOES .|. WIDE, eff=EStatus Sleep}
  , MoveDesc "Darkest Lariat" 10 tackle
      {ty=DAR, pow=85, eff=IgnoreBoosts, flags=CONTACT}
  , MoveDesc "Dazzling Gleam" 10 tackle
      {ty=FAI, pow=80, targ=ADJFOES .|. WIDE}
  , MoveDesc "Decorate" 15 celebrate
      {ty=FAI, targ=ADJACENT, eff=AddBoost False zero {att=2, spA=2}}
  , MoveDesc "Defend Order" 10 celebrate
      {ty=BUG, eff=AddBoost True zero {def=1, spD=1}}
  , MoveDesc "Defence Curl" 40 celebrate
      {ty=NOR, eff=AddBoost True zero {def=1} :+ DefenceCurlUsed}
  , MoveDesc "Defog" 15 celebrate
      {ty=FLY, targ=ADJACENT, eff=Defog}
  , MoveDesc "Destiny Bond" 5 celebrate
      {ty=GHO, eff=DestinyBond}
  , MoveDesc "Detect" 5 celebrate
      {ty=FIG, eff=Protect}
  -- devastating drake
  , MoveDesc "Diamond Storm" 5 tackle
      {ty=ROC, pow=100, acc=95, targ=ADJFOES .|. WIDE, eff=50 :% AddBoost True zero {def=2}}
  , MoveDesc "Dig" 10 tackle
      {pow = 80, ty=GRO, flags=CONTACT, eff = EInvul Digging}
  , MoveDesc "Dire Claw" 15 tackle
      {ty=POI, pow=80, crit=1, flags=CONTACT, eff=50 :% Choose [EStatus Poison, EStatus Paralysis, Yawn]}
  , MoveDesc "Disable" 20 celebrate
      {ty=NOR, targ=ADJACENT, eff=Disable}
  , MoveDesc "Disarming Voice" 15 tackle
      {ty=FAI, cat=Special, acc=neverMiss, pow=40}
  , MoveDesc "Discharge" 15 tackle
      {ty=ELE, cat=Special, targ=ADJACENT .|. WIDE, pow=80, eff=30 :% EStatus Paralysis}
  , MoveDesc "Dive" 10 tackle
      {pow = 80, ty=WAT, eff = EInvul Diving, flags=CONTACT}
  , MoveDesc "Dizzy Punch" 10 tackle
      {ty=NOR, pow=70, eff=20 :% Confuse, flags=CONTACT}
  , MoveDesc "Doodle" 10 celebrate
      {ty=NOR, targ=ADJACENT, eff=CopyAbility Target2Allies}
  , MoveDesc "Doom Desire" 5 tackle
      {ty=STE, pow=140, cat=Special, eff=Delay2Turns}
  , MoveDesc "Double Hit" 10 tackle
      {ty=NOR, pow=35, acc=90, hits=2, flags=CONTACT}
  , MoveDesc "Double Iron Bash" 5 tackle
      {ty=STE, pow=60, hits=2, eff=30 :% Flinch, flags=CONTACT}
  , MoveDesc "Double Kick" 30 tackle
      {ty=FIG, pow=30, hits=2, flags=CONTACT}
  , MoveDesc "Double Shock" 5 tackle
      {ty=ELE, pow=120, eff=RemoveType ELE, flags=CONTACT}
  , MoveDesc "Double Slap" 10 tackle
      {ty=NOR, pow=15, hits=5, acc=85, flags=CONTACT}
  , MoveDesc "Double Team" 15 celebrate
      {ty=NOR, eff=AddBoost True zero {eva=1}}
  , MoveDesc "Double-Edge" 15 tackle
      {ty=NOR, pow=120, eff=Recoil (1/3), flags=CONTACT}
  , MoveDesc "Draco Meteor" 5 tackle
      {ty=DRA, pow=130, cat=Special, eff=AddBoost True zero { spA= -2 }, acc=90}
  , MoveDesc "Dragon Ascent" 5 tackle
      {ty=DRA, pow=120, eff=AddBoost True zero {def= -1, spD= -1}, flags=CONTACT}
  , MoveDesc "Dragon Breath" 20 tackle
      {ty=DRA, pow=60, cat=Special, eff=30 :% EStatus Paralysis}
  , MoveDesc "Dragon Claw" 15 tackle
      {ty=DRA, pow=80, flags=CONTACT}
  , MoveDesc "Dragon Dance" 20 celebrate
      {ty=DRA, eff=AddBoost True zero {att=1, spe=1}, flags=DANCE}
  , MoveDesc "Dragon Darts" 10 tackle
      {ty=DRA, pow=50, hits=2}
  , MoveDesc "Dragon Energy" 5 tackle
      {ty=DRA, pow=150, eff=CutPwrUserHP, cat=Special}
  , MoveDesc "Dragon Hammer" 15 tackle
      {ty=DRA, pow=90, flags=CONTACT}
  , MoveDesc "Dragon Pulse" 10 tackle
      {ty=DRA, pow=80, cat=Special}
  , MoveDesc "Dragon Rage" 10 tackle
      {ty=DRA, pow=0, cat=Special, eff=ConstantDamage 40}
  , MoveDesc "Dragon Rush" 10 tackle
      {ty=DRA, pow=100, acc=75, eff=20 :% Flinch, flags=CONTACT}
  , MoveDesc "Dragon Tail" 10 tackle
      {ty=DRA, pow=60, acc=90, eff=Switch False True False, flags=CONTACT}
  , MoveDesc "Drain Punch" 10 tackle
      {ty=FIG, pow=75, eff=Drain 0.5, flags=CONTACT}
  , MoveDesc "Draining Kiss" 10 tackle
      {ty=FIG, pow=50, cat=Special, eff=Drain 0.75, flags=CONTACT}
  , MoveDesc "Dream Eater" 15 tackle
      {ty=PSY, pow=100, cat=Special, eff=DrainSleeping 0.5}
  , MoveDesc "Drill Peck" 20 tackle
      {ty=FLY, pow=80, flags=CONTACT}
  , MoveDesc "Drill Run" 10 tackle
      {ty=GRO, pow=80, acc=95, crit=1, flags=CONTACT}
  , MoveDesc "Drum Beating" 10 tackle
      {ty=GRA, pow=80, eff=AddBoost False zero {spe= -1}}
  , MoveDesc "Dual Chop" 15 tackle
      {ty=DRA, pow=40, acc=90, hits=2, flags=CONTACT}
  , MoveDesc "Dual Wingbeat" 10 tackle
      {ty=FLY, pow=40, acc=90, hits=2, flags=CONTACT}
  , MoveDesc "Dynamax Cannon" 5 tackle
      {ty=DRA, pow=100, cat=Special, eff=DoubleDmgIfDynamax}
  , MoveDesc "Dynamic Punch" 5 tackle
      {ty=FIG, pow=100, acc=50, eff=Confuse, flags=CONTACT}

  , MoveDesc "Earth Power" 10 tackle
      {ty=GRO, pow=90, cat=Special, eff=10 :% AddBoost False zero {spD= -1}}
  , MoveDesc "Earthquake" 10 tackle
      {ty=GRO, pow=100, targ=ADJACENT .|. WIDE, eff=DoublePowerIfInvul Digging}
  , MoveDesc "Echoed Voice" 15 tackle
      {ty=NOR, cat=Special, pow=40, eff=EchoPower}
  , MoveDesc "Eerie Impulse" 15 celebrate
      {ty=ELE, targ=ADJACENT, eff=AddBoost False zero {spA= -2}}
  , MoveDesc "Eerie Spell" 5 tackle
      {ty=PSY, cat=Special, pow=80, eff=RemovePP 3}
  , MoveDesc "Egg Bomb" 10 tackle
      {ty=NOR, pow=100, acc=75}
  , MoveDesc "Electric Terrain" 10 celebrate
      {ty=ELE, eff=ETerrain TElectric}
  , MoveDesc "Electrify" 20 celebrate
      {ty=ELE, targ=ADJACENT, eff=SetType ELE False}
  , MoveDesc "Electro Ball" 10 tackle
      {ty=ELE, cat=Special, pow=0, eff=SpeedPower}
  , MoveDesc "Electro Drift" 5 tackle
      {ty=ELE, cat=Special, pow=100, eff=BoostSuperEffective, flags=CONTACT}
  , MoveDesc "Electroweb" 15 tackle
      {ty=ELE, cat=Special, pow=55, acc=95, eff=AddBoost False zero {spe= -1}, targ=ADJFOES .|. WIDE}
  , MoveDesc "Embargo" 15 celebrate
      {ty=DAR, targ=ADJACENT, eff=Embargo}
  , MoveDesc "Ember" 25 tackle
      {ty=FIR, cat=Special, eff=10 :% EStatus Burn}
  , MoveDesc "Encore" 5 celebrate
      {ty=NOR, targ=ADJACENT, eff=Encore}
  , MoveDesc "Endeavor" 5 tackle
      {ty=NOR, pow=0, eff=MatchUserHP, flags=CONTACT}
  , MoveDesc "Endure" 10 celebrate
      {ty=NOR, eff=Endure}
  , MoveDesc "Energy Ball" 10 tackle
      {ty=GRA, pow=90, eff=10 :% AddBoost False zero {spD= -1}}
  , MoveDesc "Entrainment" 15 celebrate
      {ty=NOR, eff=CopyAbility User2Target}
  , MoveDesc "Eruption" 5 tackle
      {ty=FIR, cat=Special, targ=ADJFOES .|. WIDE, eff=CutPwrUserHP, pow=150}
  , MoveDesc "Esper Wing" 10 tackle
      {ty=PSY, cat=Special, pow=80, crit=1, eff=AddBoost True zero {spe=1}}
  , MoveDesc "Eternalbeam" 5 tackle
      {ty=DRA, cat=Special, pow=160, acc=90, eff=Recharge}
  , MoveDesc "Expanding Force" 10 tackle
      {ty=PSY, cat=Special, pow=80, eff=ExpandingForce}
  , MoveDesc "Explosion" 5 tackle
      {ty=NOR, pow = 250, eff = UserDies, targ=ADJACENT .|. WIDE}
  , MoveDesc "Extrasensory" 20 tackle
      {ty=PSY, pow = 80, eff=10 :% Flinch}
  -- extreme evoboost
  , MoveDesc "Extreme Speed" 5 tackle
      {ty=NOR, pow = 80, pri=1, flags=CONTACT}

  , MoveDesc "Facade" 20 tackle
      {ty=NOR, pow=70, eff=DoublePwrIfUserStatus, flags=CONTACT}
  , MoveDesc "Fairy Lock" 10 celebrate
      {ty=FAI, targ=ADJACENT, eff=NoFleeingNextTurn}
  , MoveDesc "Fairy Wind" 30 tackle
      {ty=FAI, pow=40, cat=Special}
  , MoveDesc "Fake Out" 10 tackle
      {ty=NOR, pow=40, pri=3, eff=Flinch, flags=CONTACT .|. TURN1}
  , MoveDesc "Fake Tears" 20 celebrate
      {ty=DAR, targ=ADJACENT, eff=AddBoost False zero {spD= -2}}
  , MoveDesc "False Surrender" 10 tackle
      {ty=DAR, pow=80, acc=neverMiss, flags=CONTACT}
  , MoveDesc "False Swipe" 40 tackle
      {ty=NOR, pow=40, eff=Don'tKill, flags=CONTACT}
  , MoveDesc "Feater Dance" 15 celebrate
      {ty=FLY, targ=ADJACENT, flags=DANCE, eff=AddBoost False zero {att= -2}}
  , MoveDesc "Feint" 10 tackle
      {ty=NOR, pow=30, eff=IgnoreProtect True}
  , MoveDesc "Feint Attack" 20 tackle
      {ty=DAR, pow=60, acc=neverMiss, flags=CONTACT}
  , MoveDesc "Fell Stinger" 25 tackle
      {ty=BUG, pow=50, eff=AddBoostIfKO zero {att=3}, flags=CONTACT}
  , MoveDesc "Fiery Dance" 10 tackle
      {ty=FIR, cat=Special, pow=80, flags=DANCE, eff=50 :% AddBoost True zero {spA=1}}
  , MoveDesc "Fiery Wrath" 10 tackle
      {ty=DAR, cat=Special, pow=90, eff=20 :% Flinch}
  , MoveDesc "Fillet Away" 10 celebrate
      {ty=NOR, eff=FilletAway}
  , MoveDesc "Final Gambit" 5 tackle
      {ty=FIG, eff=FinalGambit}
  , MoveDesc "Fire Blast" 5 tackle
      {ty=FIR, cat=Special, pow=110, acc=85, eff=10 :% EStatus Burn}
  , MoveDesc "Fire Fang" 15 tackle
      {ty=FIR, pow=65, acc=95, eff=10 :% Flinch :+ 10 :% EStatus Burn, flags=CONTACT}
  , MoveDesc "Fire Lash" 15 tackle
      {ty=FIR, pow=80, eff=AddBoost False zero {def= -1}, flags=CONTACT}
  , MoveDesc "Fire Pledge" 10 tackle
      {ty=FIR, pow=80, cat=Special, eff=FirePledge}
  , MoveDesc "Fire Punch" 15 tackle
      {ty=FIR, pow=75, eff=10 :% EStatus Burn, flags=CONTACT}
  , MoveDesc "Fire Spin" 15 tackle
      {ty=FIR, pow=35, cat=Special, eff=ELocking Firespin}
  , MoveDesc "First Impression" 10 tackle
      {ty=BUG, pow=90, flags=CONTACT .|. TURN1}
  , MoveDesc "Fishious Rend" 10 tackle
      {ty=WAT, pow=85, eff=DoublePwrIfTargetSlower, flags=CONTACT}
  , MoveDesc "Fissure" 5 tackle
      {ty=GRO, pow=0, acc=30, eff=OHKO}
  , MoveDesc "Flail" 15 tackle
      {ty=NOR, pow=0, eff=RaisePwrUserHP, flags=CONTACT}
  , MoveDesc "Flame Burst" 15 tackle
      {ty=FIR, pow=70, cat=Special, eff=SplashDamage}
  , MoveDesc "Flame Charge" 20 tackle
      {ty=FIR, pow=50, eff=AddBoost True zero {spe=1}, flags=CONTACT}
  , MoveDesc "Flame Wheel" 25 tackle
      {ty=FIR, pow=60, eff=10 :% EStatus Burn, flags=CONTACT}
  , MoveDesc "Flamethrower" 15 tackle
      {ty=FIR, pow=90, cat=Special, eff=10 :% EStatus Burn}
  , MoveDesc "Flare Blitz" 15 tackle
      {ty=FIR, pow=120, eff=Recoil (1/3) :+ 10 :% EStatus Burn, flags=CONTACT}
  , MoveDesc "Flash" 20 celebrate
      {ty=NOR, targ=ADJACENT, eff=AddBoost False zero {att=0, acc= -1}}
  , MoveDesc "Flash Cannon" 10 tackle
      {ty=STE, pow=80, cat=Special, eff=10 :% AddBoost False zero {spD= -1}}
  , MoveDesc "Flatter" 15 celebrate
      {ty=DAR, targ=ADJACENT, eff=Confuse :+ AddBoost False zero {spA=1}}
  , MoveDesc "Fleur Cannon" 5 tackle
      {ty=FAI, pow=130, cat=Special, acc=90, eff=AddBoost True zero {spA= -2}}
  , MoveDesc "Fling" 10 tackle
      {ty=DAR, pow=0, eff=Fling}
  , MoveDesc "Flip Turn" 20 tackle
      {ty=WAT, pow=60, eff=Switch True False False, flags=CONTACT}
  , MoveDesc "Floaty Fall" 15 tackle
      {ty=FLY, pow=90, acc=95, eff=30 :% Flinch, flags=CONTACT}
  , MoveDesc "Floral Healing" 10 celebrate
      {ty=FAI, targ=ADJACENT, eff=FloralHealing}
  , MoveDesc "Floral Shield" 10 celebrate
      {ty=FAI, targ=ALL .|. WIDE, eff=FloralShield}
  , MoveDesc "Flower Trick" 10 tackle
      {ty=GRA, pow=70, acc=neverMiss, crit=alwaysCrit}
  , MoveDesc "Fly" 15 tackle
      {pow = 90, ty=FLY, acc=95, flags=CONTACT, eff = EInvul Flying}
  , MoveDesc "Flying Press" 5 tackle
      {ty=FIG, pow=100, acc=95, eff=ExtraType FLY, flags=CONTACT}
  , MoveDesc "Focus Blast" 5 tackle
      {ty=FIG, cat=Special, pow=120, acc=70, eff=10 :% AddBoost False zero {spD= -1}}
  , MoveDesc "Focus Energy" 30 celebrate
      {ty=NOR, eff=AddBoost True zero {cri=1}}
  , MoveDesc "Focus Punch" 20 tackle
      {ty=FIG, pow=150, eff=FlinchIfHit, flags=CONTACT}
  , MoveDesc "Follow Me" 20 celebrate
      {ty=NOR, eff=FollowMe}
  , MoveDesc "Force Palm" 10 tackle
      {ty=FIG, pow=60, eff=30 :% EStatus Paralysis, flags=CONTACT}
  , MoveDesc "Foresight" 40 celebrate
      {ty=NOR, targ=ADJACENT, eff=Foresight}
  , MoveDesc "Forest's Curse" 20 celebrate
      {ty=GRA, targ=ADJACENT, eff=AddType GRA}
  , MoveDesc "Foul Play" 15 tackle
      {ty=DAR, pow=95, eff=UseTargetAtt, flags=CONTACT}
  , MoveDesc "Freeze Shock" 5 tackle
      {ty=ICE, pow=140, acc=90, eff=Precharge :+ 30 :% EStatus Paralysis}
  , MoveDesc "Freeze-Dry" 20 tackle
      {ty=ICE, pow=70, cat=Special, eff=SuperEffectiveAgainst WAT :+ 10 :% EStatus Freeze}
  , MoveDesc "Freezing Glare" 10 tackle
      {ty=PSY, pow=90, cat=Special, eff=10 :% EStatus Freeze}
  , MoveDesc "Freezy Frost" 15 tackle
      {ty=ICE, pow=90, cat=Special, targ=ALL .|. WIDE, eff=ClearBoost}
  , MoveDesc "Frenzy Plant" 5 tackle
      {ty=GRA, pow=150, acc=90, cat=Special, eff=Recharge}
  , MoveDesc "Frost Breath" 10 tackle
      {ty=ICE, pow=60, acc=90, cat=Special, crit=alwaysCrit}
  , MoveDesc "Frustration" 20 tackle
      {ty=NOR, pow=0, eff=PwrLowFriendship, flags=CONTACT}
  , MoveDesc "Fury Attack" 20 tackle
      {ty=NOR, pow=15, acc=85, hits=5, flags=CONTACT}
  , MoveDesc "Fury Cutter" 20 tackle
      {ty=BUG, pow=40, acc=95, eff=EchoPower, flags=CONTACT}
  , MoveDesc "Fury Swipes" 15 tackle
      {ty=NOR, pow=18, acc=80, hits=5, flags=CONTACT}
  , MoveDesc "Fusion Bolt" 5 tackle
      {ty=ELE, pow=100, eff=FusionBolt}
  , MoveDesc "Fusion Flare" 5 tackle
      {ty=FIR, cat=Special, pow=100, eff=FusionFlare}
  , MoveDesc "Future Sight" 10 tackle
      {ty=PSY, cat=Special, pow=120, eff=Delay2Turns}

  -- G-Max moves ...

  , MoveDesc "Gastro Acid" 10 celebrate
      {ty=POI, targ=ADJACENT, eff=SuppressAbility}
  , MoveDesc "Gear Grind" 15 tackle
      {ty=STE, pow=50, hits=2, acc=85, flags=CONTACT}
  , MoveDesc "Gear Up" 20 celebrate
      {ty=STE, eff=GearUp, targ=WIDE .|. SELF .|. ALLIES}
  , MoveDesc "Genesis Supernova" 1 tackle
      {ty=PSY, pow=185, flags=ZMOVE}
  , MoveDesc "Geomancy" 10 celebrate
      {ty=FAI, eff=Precharge :+ AddBoost True zero {spA=2,spD=2,spe=2}}
  , MoveDesc "Giga Drain" 10 tackle
      {ty=GRA, pow=75, cat=Special, eff=Drain 0.5}
  , MoveDesc "Giga Impact" 5 tackle
      {ty=NOR, pow=150, acc=90, eff=Recharge, flags=CONTACT}
  , MoveDesc "Gigaton Hammer" 5 tackle
      {ty=STE, pow=160, eff=NoSpam}
  -- Gigavolt Havoc
  , MoveDesc "Glacial Lance" 5 tackle
      {ty=ICE, pow=120}
  , MoveDesc "Glaciate" 10 tackle
      {ty=ICE, pow=65, cat=Special, acc=95, eff=AddBoost False zero {spe= -1}}
  , MoveDesc "Glaive Rush" 5 tackle
      {ty=DRA, pow=120, eff=GlaiveRush, flags=CONTACT}
  , MoveDesc "Glare" 30 celebrate
      {ty=NOR, targ=ADJACENT, eff=EStatus Paralysis}
  , MoveDesc "Glitzy Glow" 15 tackle
      {ty=PSY, cat=Special, pow=90, eff=GlitzyGlow}
  , MoveDesc "Grass Knot" 20 tackle
      {ty=GRA, cat=Special, pow=0, eff=PwrHeavyTarget, flags=CONTACT}
  , MoveDesc "Grass Pledge" 10 tackle
      {ty=GRA, pow=80, cat=Special, eff=GrassPledge}
  , MoveDesc "Grass Whistle" 15 celebrate
      {ty=GRA, targ=ADJACENT, eff=EStatus Sleep, acc=55}
  , MoveDesc "Grassy Glide" 20 tackle
      {ty=GRA, pow=60, eff=GrassyGlide, flags=CONTACT}
  , MoveDesc "Grassy Terrain" 10 celebrate
      {ty=GRA, eff=ETerrain TGrass}
  , MoveDesc "Grav Apple" 10 tackle
      {ty=GRA, pow=80, eff=AddBoost False zero {def= -1}}
  , MoveDesc "Gravity" 5 celebrate
      {ty=PSY, eff=Gravity}
  , MoveDesc "Growl" 40 celebrate
      {ty=NOR, targ=ADJACENT, eff=AddBoost False zero {att= -1}}
  , MoveDesc "Growth" 20 celebrate
      {ty=NOR, eff=AddBoost True zero {att=1, spA=1}}
  , MoveDesc "Grudge" 5 celebrate
      {ty=GHO, targ=ADJACENT, eff=Grudge}
  , MoveDesc "Guard Split" 10 celebrate
      {ty=PSY, targ=ADJACENT, eff=AvgDef}
  , MoveDesc "Guard Swap" 10 celebrate
      {ty=PSY, targ=ADJACENT, eff=SwpDef}
  -- Guardian of Alola
  , MoveDesc "Guillotine" 5 tackle
      {ty=NOR, pow=0, eff=OHKO, acc=30, flags=CONTACT}
  , MoveDesc "Gunk Shot" 5 tackle
      {ty=POI, pow=120, acc=80, eff=30 :% EStatus Poison}
  , MoveDesc "Gust" 35 tackle
      {ty=FLY, pow=40, cat=Special, eff=DoublePowerIfInvul Flying}
  , MoveDesc "Gyro Ball" 5 tackle
      {ty=STE, pow=0, eff=GyroBall, flags=CONTACT}

  , MoveDesc "Hail" 10 celebrate
      {ty=ICE, eff=EWeather Hail}
  , MoveDesc "Hammer Arm" 10 tackle
      {ty=FIG, pow=100, acc=90, eff=AddBoost True zero {spe= -1}, flags=CONTACT}
  , MoveDesc "Happy Hour" 30 celebrate
      {ty=NOR, eff=HappyHour}
  , MoveDesc "Harden" 30 celebrate
      {ty=NOR, eff=AddBoost True zero {def=1}}
  , MoveDesc "Haze" 30 celebrate
      {ty=ICE, eff=ClearBoost, targ=ALL .|. WIDE}
  , MoveDesc "Head Charge" 15 tackle
      {ty=NOR, pow=120, eff=Recoil (1/4), flags=CONTACT}
  , MoveDesc "Head Smash" 5 tackle
      {ty=ROC, pow=150, acc=80, eff=Recoil (1/2), flags=CONTACT}
  , MoveDesc "Headbutt" 15 tackle
      {ty=NOR, pow=70, eff=30 :% Flinch, flags=CONTACT}
  , MoveDesc "Headlong Rush" 5 tackle
      {ty=GRO, pow=120, eff=AddBoost True zero {def= -1}, flags=CONTACT}
  , MoveDesc "Heal Bell" 5 celebrate
      {ty=NOR, eff=ClearStatusParty}
  , MoveDesc "Heal Block" 15 celebrate
      {ty=PSY, targ=ADJACENT, eff=HealBlock}
  , MoveDesc "Heal Order" 10 celebrate
      {ty=BUG, eff=Recover 0.5}
  , MoveDesc "Heal Pulse" 10 celebrate
      {ty=PSY, targ=ADJACENT, eff=Recover 0.5}
  , MoveDesc "Healing Wish" 10 celebrate
      {ty=PSY, eff=DieHealSwitchIn}
  , MoveDesc "Heart Stamp" 25 tackle
      {ty=PSY, pow=60, eff=30 :% Flinch, flags=CONTACT}
  , MoveDesc "Heart Swap" 10 celebrate
      {ty=PSY, targ=ADJACENT, eff=SwapBoost}
  , MoveDesc "Heat Crash" 10 tackle
      {ty=FIR, pow=0, eff=PwrHeavyUser, flags=CONTACT}
  , MoveDesc "Heat Wave" 10 tackle
      {ty=FIR, pow=95, cat=Special, acc=90, targ=ADJFOES .|. WIDE, eff=10 :% EStatus Burn}
  , MoveDesc "Heavy Slam" 10 tackle
      {ty=STE, pow=0, eff=PwrHeavyUser, flags=CONTACT}
  , MoveDesc "HelpingHand" 20 celebrate
      {ty=NOR, targ=ADJACENT, eff=HelpingHand, pri=5}
  , MoveDesc "Hex" 10 tackle
      {ty=GHO, pow=60, eff=DoubleDmgIfTargetStatus}
  , MoveDesc "Hidden Power" 15 tackle
      {ty=NOR, pow=60, cat=Special, eff=HiddenPower}
  , MoveDesc "High Horsepower" 10 tackle
      {ty=GRO, pow=95, acc=95, flags=CONTACT}
  , MoveDesc "High Jump Kick" 10 tackle
      {ty=FIG, pow=130, acc=90, eff=DamageUserIfMiss 0.5, flags=CONTACT}
  , MoveDesc "Hold Back" 40 tackle
      {ty=NOR, pow=40, eff=Don'tKill, flags=CONTACT}
  , MoveDesc "Hold Hands" 40 celebrate
      {ty=NOR, targ=ALLIES}
  , MoveDesc "Hone Claws" 15 celebrate
      {ty=DAR, eff=AddBoost True zero {att=1, acc=1}}
  , MoveDesc "Horn Attack" 25 tackle
      {ty=NOR, pow=65, flags=CONTACT}
  , MoveDesc "Horn Drill" 5 tackle
      {ty=NOR, pow=0, acc=30, eff=OHKO, flags=CONTACT}
  , MoveDesc "Horn Leech" 10 tackle
      {ty=GRA, pow=75, eff=Drain 0.5, flags=CONTACT}
  , MoveDesc "Howl" 40 celebrate
      {ty=NOR, targ=ALLIES .|. SELF .|. WIDE, eff=AddBoost False zero {att=1}}
  , MoveDesc "Hurricane" 10 tackle
      {ty=FLY, cat=Special, acc=70, pow=110, eff=30 :% Confuse}
  , MoveDesc "Hydro Cannon" 5 tackle
      {ty=WAT, cat=Special, acc=90, pow=150, eff=Recharge}
  , MoveDesc "Hydro Pump" 5 tackle
      {ty=WAT, cat=Special, acc=80, pow=110}
  , MoveDesc "Hydro Steam" 15 tackle
      {ty=WAT, cat=Special, pow=80, eff=PwrInHarshSunlight}
  -- Hydro Vortex
  , MoveDesc "Hyper Beam" 5 tackle
      {ty=NOR, cat=Special, pow=150, eff=Recharge, acc=90}
  , MoveDesc "Hyper Drill" 5 tackle
      {ty=NOR, pow=100, eff=IgnoreProtect False, flags=CONTACT}
  , MoveDesc "Hyper Fang" 15 tackle
      {ty=NOR, pow=80, acc=90, eff=10 :% Flinch, flags=CONTACT}
  , MoveDesc "Hyper Voice" 10 tackle
      {ty=NOR, pow=90, cat=Special, targ=ADJFOES .|. WIDE, flags=SOUND}
  , MoveDesc "Hyperspace Fury" 5 tackle
      {ty=DAR, pow=100, acc=neverMiss, eff=IgnoreProtect False :+ AddBoost True zero {def= -1}}
  , MoveDesc "Hyperspace Hole" 5 tackle
      {ty=PSY, cat=Special, acc=neverMiss, pow=80, eff=IgnoreProtect False}
  , MoveDesc "Hypnosis" 20 celebrate
      {ty=PSY, acc=60, eff=EStatus Sleep, targ=ADJACENT}

  , MoveDesc "Ice Ball" 20 tackle
      {ty=ICE, pow=30, acc=90, eff=Scaling5Turns, flags=CONTACT}
  , MoveDesc "Ice Beam" 10 tackle
      {ty=ICE, pow=90, cat=Special, eff=10 :% EStatus Freeze}
  , MoveDesc "Ice Burn" 5 tackle
      {ty=ICE, pow=140, cat=Special, acc=90, eff=Precharge :+ 30 :% EStatus Burn}
  , MoveDesc "Ice Fang" 15 tackle
      {ty=ICE, pow=65, acc=95, eff=10 :% Flinch :+ 10 :% EStatus Freeze}
  , MoveDesc "Ice Hammer" 10 tackle
      {ty=ICE, pow=100, acc=90, eff=AddBoost True zero {def= -1}}
  , MoveDesc "Ice Punch" 15 tackle
      {ty=ICE, pow=75, eff=10 :% EStatus Freeze}
  , MoveDesc "Ice Shard" 30 tackle
      {ty=ICE, pow=40, pri=1}
  , MoveDesc "Ice Spinner" 30 tackle
      {ty=ICE, pow=80, eff=ClearTerrain}
  , MoveDesc "Icicle Crash" 10 tackle
      {ty=ICE, pow=85, acc=90, eff=30 :% Flinch}
  , MoveDesc "Icicle Spear" 10 tackle
      {ty=ICE, pow=25, hits=5}
  , MoveDesc "Icy Wind" 15 tackle
      {ty=ICE, cat=Special, pow=55, targ=ADJFOES .|. WIDE, eff=AddBoost False zero {spe= -1}}
  , MoveDesc "Imprison" 15 celebrate
      {ty=PSY, targ=ADJACENT, eff=Imprison}
  , MoveDesc "Incinerate" 15 tackle
      {ty=FIR, cat=Special, pow=60, eff=RemoveBerry False}
  , MoveDesc "Infernal Parade" 15 tackle
      {ty=GHO, cat=Special, pow=60, eff=DoubleDmgIfTargetStatus}
  , MoveDesc "Inferno" 5 tackle
      {ty=FIR, cat=Special, pow=100, acc=50, eff=EStatus Burn}
  -- Inferno overdrive
  , MoveDesc "Infestation" 20 tackle
      {ty=BUG, cat=Special, pow=20, eff=ELocking Infestation}
  , MoveDesc "Ingrain" 20 celebrate
      {ty=GRA, eff=Ingrain}
  , MoveDesc "Instruct" 15 celebrate
      {ty=PSY, eff=Instruct, targ=ADJACENT}
  , MoveDesc "Ion Deluge" 25 celebrate
      {ty=ELE, eff=IonDeluge}
  , MoveDesc "Iron Defence" 15 celebrate
      {ty=STE, eff=AddBoost True zero {def=2}}
  , MoveDesc "Iron Head" 15 tackle
      {ty=STE, eff=30 :% Flinch, pow=80}
  , MoveDesc "Iron Tail" 15 tackle
      {ty=STE, pow=100, acc=75, eff=30 :% AddBoost False zero {def= -1}}

  , MoveDesc "Jaw Lock" 10 tackle
      {ty=DAR, pow=80, eff=NoSwitchUserAndTarget}
  , MoveDesc "Jet Punch" 15 tackle
      {ty=WAT, pow=60, pri=1}
  , MoveDesc "Judgement" 10 tackle
      {ty=NOR, pow=100, eff=Judgement, cat=Special}
  , MoveDesc "Jump Kick" 10 tackle
      {ty=FIG, pow=100, acc=95, eff=DamageUserIfMiss 0.5}
  , MoveDesc "Jungle Healing" 10 celebrate
      {ty=GRA, eff=JungleHealing}

  , MoveDesc "Karate Chop" 25 tackle
      {ty=FIG, pow=50, crit=1}
  , MoveDesc "Kinesis" 15 celebrate
      {ty=PSY, acc=80, targ=ADJACENT, eff=AddBoost False zero {acc= -1}}
  , MoveDesc "King's Shield" 10 celebrate
      {ty=PSY, eff=KingShield}
  , MoveDesc "Knock Off" 20 tackle
      {ty=DAR, pow=65, eff=RemoveItem}
  , MoveDesc "Kowtow Cleave" 10 tackle
      {ty=DAR, pow=85, acc=neverMiss}

  , MoveDesc "Land's Wrath" 10 tackle
      {ty=GRO, pow=90}
  , MoveDesc "Laser Focus" 30 celebrate
      {ty=NOR, targ=ADJACENT, eff=LaserFocus}
  , MoveDesc "Lash Out" 5 tackle
      {ty=DAR, pow=75, eff=DoublePwrIfUserDebuff}
  , MoveDesc "Last Resort" 5 tackle
      {ty=NOR, pow=140, eff=AllOtherMovesUsed}
  , MoveDesc "Last Respects" 10 tackle
      {ty=GHO, pow=50, eff=LastRespects}
  , MoveDesc "Lava Plume" 15 tackle
      {ty=FIR, cat=Special, pow=80, eff=30 :% EStatus Burn}
  , MoveDesc "Leaf Blade" 15 tackle
      {ty=GRA, pow=90, crit=1}
  , MoveDesc "Leaf Storm" 5 tackle
      {ty=GRA, cat=Special, pow=130, acc=90, eff=AddBoost True zero {spA= -2}}
  , MoveDesc "Leaf Tornado" 10 tackle
      {ty=GRA, cat=Special, pow=65, acc=90, eff=50 :% AddBoost False zero {acc= -1}}
  , MoveDesc "Leafage" 40 tackle
      {ty=GRA, pow=40}
  , MoveDesc "Leech Life" 10 tackle
      {ty=BUG, pow=80, eff=Drain 0.5}
  , MoveDesc "Leech Seed" 10 celebrate
      {ty=GRA, acc=90, eff=LeechSeed}
  , MoveDesc "Leer" 30 celebrate
      {ty=NOR, eff=AddBoost False zero {def= -1}}
  , MoveDesc "Let's Snuggle Forever" 1 tackle
      {ty=FAI, flags=ZMOVE, pow=190}
  , MoveDesc "Lick" 30 tackle
      {ty=GHO, pow=30, eff=30 :% EStatus Paralysis}
  , MoveDesc "Life Dew" 10 celebrate
      {ty=WAT, targ=ALLIES .|. SELF .|. WIDE, eff=Recover 0.25}
  , MoveDesc "Light of Ruin" 5 tackle
      {ty=FAI, cat=Special, pow=140, acc=90, eff=Recoil 0.5}
  , MoveDesc "Light Screen" 30 celebrate
      {ty=PSY, eff=EScreen Spe}
  , MoveDesc "Light That Burns the Sky" 1 tackle
      {ty=PSY, flags=ZMOVE, pow=200, eff=LightThatBurnsTheSky}
  , MoveDesc "Liquidation" 10 tackle
      {ty=WAT, flags=CONTACT, pow=85, eff=20 :% AddBoost False zero {def= -1}}
  , MoveDesc "Lock-On" 5 celebrate
      {ty=NOR, targ=ADJACENT, eff=LockOn}
  , MoveDesc "Lovely Kiss" 10 celebrate
      {ty=NOR, targ=ADJACENT, acc=75, eff=EStatus Sleep}
  , MoveDesc "Low Kick" 20 tackle
      {ty=FIG, pow=0, eff=PwrHeavyTarget}
  , MoveDesc "Low Sweep" 20 tackle
      {ty=FIG, pow=65, eff=AddBoost False zero {spe= -1}}
  , MoveDesc "Lucky Chant" 30 celebrate
      {ty=NOR, eff=LuckyChant}
  , MoveDesc "Lumina Crash" 10 tackle
      {ty=PSY, cat=Special, pow=80, eff=AddBoost False zero {spD= -2}}
  , MoveDesc "Lunar Blessing" 5 celebrate
      {ty=PSY, eff=ClearStatus :+ Recover 0.5}
  , MoveDesc "Lunar Dance" 10 celebrate
      {ty=PSY, flags=DANCE, eff=DieHealSwitchIn}
  , MoveDesc "Lunge" 15 tackle
      {ty=BUG, pow=80, eff=AddBoost False zero {att= -1}}
  , MoveDesc "Luster Purge" 5 tackle
      {ty=PSY, pow=70, eff=50 :% AddBoost False zero {spD= -1}}

  , MoveDesc "Mach Punch" 30 tackle
      {ty=FIG, pow=40, pri=1, flags=CONTACT}
  , MoveDesc "Magic Coat" 15 celebrate
      {ty=PSY, eff=MagicCoat, pri=4}
  , MoveDesc "Magic Powder" 20 celebrate
      {ty=PSY, targ=ADJACENT, eff=SetType PSY False}
  , MoveDesc "Magic Room" 10 celebrate
      {ty=PSY, targ=ADJACENT, eff=MagicRoom}
  , MoveDesc "Magical Leaf" 20 tackle
      {ty=GRA, cat=Special, pow=60, targ=WIDE .|. ADJFOES, acc=0}
  , MoveDesc "Magical Torque" 10 tackle
      {ty=FAI, pow=100}
  , MoveDesc "Magma Storm" 5 tackle
      {ty=FIR, cat=Special, pow=100, acc=75, eff=ELocking MagmaStorm}
  , MoveDesc "Magnet Bomb" 20 tackle
      {ty=STE, pow=60, acc=0}
  , MoveDesc "Magnet Rise" 10 celebrate
      {ty=ELE, eff=MagnetRise}
  , MoveDesc "Magnetic Flux" 20 celebrate
      {ty=ELE, eff=MagneticFlux, targ=WIDE .|. SELF .|. ALLIES}
  , MoveDesc "Magnitude" 30 tackle
      {ty=GRO, pow=0, eff=Magnitude, targ=WIDE .|. ADJACENT}
  , MoveDesc "Make It Rain" 5 tackle -- TODO: Extra money
      {ty=STE, cat=Special, pow=120, targ=WIDE .|. ADJFOES, eff=AddBoost True zero {spA= -1}}
  , MoveDesc "Malicious Moonsault" 1 tackle
      {ty=DAR, pow=180, flags=CONTACT .|. ZMOVE }
  , MoveDesc "Mat Block" 10 celebrate
      {ty=FIG, targ=SELF .|. ALLIES .|. WIDE, flags=TURN1, eff=Protect}
  -- Max z-moves ...
  , MoveDesc "Me First" 20 celebrate
      {ty=NOR, targ=ADJACENT, eff=MeFirst}
  , MoveDesc "Mean Look" 5 celebrate
      {ty=NOR, targ=ADJACENT, eff=NoSwitch}
  , MoveDesc "Meditate" 40 celebrate
      {ty=PSY, eff=AddBoost True zero {att=1}}
  , MoveDesc "Mega Drain" 15 tackle
      {ty=GRA, pow=40, cat=Special, eff=Drain 0.5}
  , MoveDesc "Mega Kick" 5 tackle
      {ty=NOR, pow=120, acc=75}
  , MoveDesc "Mega Punch" 20 tackle
      {ty=NOR, pow=80, acc=85}
  , MoveDesc "Megahorn" 10 tackle
      {ty=BUG, pow=120, acc=85}
  , MoveDesc "Memento" 10 celebrate
      {ty=DAR, pow=120, acc=85, eff=UserDies :+ AddBoost False zero {att= -2, spA= -2}}
  , MoveDesc "Menacing Moonraze Maelstrom" 1 tackle
      {ty=GHO, pow=200, flags=ZMOVE, cat=Special }
  , MoveDesc "Metal Burst" 10 tackle
      {ty=STE, pow=0, eff=MatchTarget'sDamage 1.5}
  , MoveDesc "Metal Claw" 35 tackle
      {ty=STE, pow=50, acc=95, eff=10 :% AddBoost True zero {att=1}}
  , MoveDesc "Metal Sound" 40 celebrate
      {ty=STE, targ=ADJACENT, acc=85, eff=AddBoost False zero {spD= -2}}
  , MoveDesc "Meteor Assault" 5 tackle
      {ty=FIG, pow=150, eff=Recharge}
  , MoveDesc "Meteor Beam" 5 tackle
      {ty=ROC, cat=Special, pow=120, acc=90, eff=AddBoost True zero {spA=1} :+ Precharge}
  , MoveDesc "Meteor Mash" 10 tackle
      {ty=STE, pow=90, acc=90, eff=10 :% AddBoost True zero {att=1}}
  , MoveDesc "Metronome" 10 celebrate
      {ty=NOR, eff=Metronome}
  , MoveDesc "Milk Drink" 5 celebrate
      {ty=NOR, eff=Recover 0.5}
  , MoveDesc "Mimic" 10 celebrate
      {ty=NOR, eff=Mimic}
  , MoveDesc "Mind Blown" 5 tackle
      {ty=FIR, cat=Special, pow=150, eff=RecoilMax 0.5, targ=WIDE .|. ADJACENT}
  , MoveDesc "Mind Reader" 5 celebrate
      {ty=PSY, eff=LockOn}
  , MoveDesc "Minimize" 10 celebrate
      {ty=NOR, eff=AddBoost True zero {eva=1}}
  , MoveDesc "Miracle Eye" 40 celebrate
      {ty=NOR, targ=ADJACENT, eff=MiracleEye}
  , MoveDesc "Mirror Coat" 20 tackle
      {ty=NOR, pow=0, cat=Special, eff=MirrorCoat}
  , MoveDesc "Mirror Move" 20 celebrate
      {ty=FLY, eff=MirrorMove}
  , MoveDesc "Mirror Shot" 10 tackle
      {ty=STE, pow=65, acc=85, eff=30 :% AddBoost False zero {acc= -1}}
  , MoveDesc "Mist" 30 celebrate
      {ty=ICE, eff=Mist}
  , MoveDesc "Mist Ball" 5 tackle
      {ty=PSY, cat=Special, pow=70, eff=50 :% AddBoost False zero {spA= -1}}
  , MoveDesc "Misty Explosion" 5 tackle
      {ty=FAI, cat=Special, pow=100, targ=WIDE .|. ADJACENT, eff=UserDies :+ PwrInTerrain TMisty}
  , MoveDesc "Misty Terrain" 10 celebrate
      {ty=FAI, eff=ETerrain TMisty}
  , MoveDesc "Moonblast" 15 tackle
      {ty=FAI, cat=Special, pow=95, eff=30 :% AddBoost False zero {spA= -1}}
  , MoveDesc "Moongeist Ball" 5 tackle
      {ty=GHO, cat=Special, pow=100, eff=IgnoreAbility}
  , MoveDesc "Moonlight" 5 celebrate
      {ty=FAI, eff=RecoverWeather}
  , MoveDesc "Morning Sun" 5 celebrate
      {ty=NOR, eff=RecoverWeather}
  , MoveDesc "Mortal Spin" 15 tackle -- TODO: Removes entry hazards and trap move effects, and poisons opposing Pokémon.
      {ty=POI, pow=30, targ=WIDE .|. ADJFOES, eff=ClearHazard :+ EStatus Poison}
  , MoveDesc "Mountain Gale" 10 tackle
      {ty=ICE, pow=100, acc=85, eff=AddBoost True zero {spe= -1}}
  , MoveDesc "Mud Bomb" 10 tackle
      {ty=GRO, cat=Special, pow=65, acc=85, eff=30 :% AddBoost False zero {acc= -1}}
  , MoveDesc "Mud Shot" 15 tackle
      {ty=GRO, cat=Special, pow=55, acc=95, eff=AddBoost False zero {spe= -1}}
  , MoveDesc "Mud Sport" 15 celebrate
      {ty=GRO, eff=MudSport}
  , MoveDesc "Mud-Slap" 10 tackle
      {ty=GRO, cat=Special, pow=20, eff=AddBoost False zero {acc= -1}}
  , MoveDesc "Muddy Water" 10 tackle
      {ty=WAT, cat=Special, targ=WIDE .|. ADJFOES, pow=90, acc=85, eff=AddBoost False zero {acc= -1}}
  , MoveDesc "Multi-Attack" 10 tackle
      {ty=NOR, pow=120, eff=MultiAttack}
  , MoveDesc "Mystical Fire" 10 tackle
      {ty=FIR, cat=Special, pow=75, eff=AddBoost False zero {spA= -1}}
  , MoveDesc "Mystical Power" 10 tackle
      {ty=PSY, cat=Special, pow=70, acc=90, eff=Choose [AddBoost True zero {att=1}, AddBoost True zero {spA=1}]}

  , MoveDesc "Nasty Plot" 10 celebrate
      {ty=DAR, eff=AddBoost True zero {spA=2}}
  , MoveDesc "Natural Gift" 10 tackle
      {ty=NOR, pow=0, eff=NaturalGift}
  , MoveDesc "Nature Power" 20 celebrate
      {ty=NOR, eff=NaturePower}
  , MoveDesc "Nature's Madness" 20 tackle
      {ty=FAI, pow=0, cat=Special, eff=HalfHP}
  , MoveDesc "Needle Arm" 15 tackle
      {ty=GRA, pow=60, eff=30 :% Flinch}
  -- never ending nightmare
  , MoveDesc "Night Daze" 10 tackle
      {ty=DAR, cat=Special, pow=85, acc=95, eff=40 :% AddBoost False zero {acc= -1}}
  , MoveDesc "Night Shade" 15 tackle
      {ty=GHO, cat=Special, pow=0, eff=LevelDamage}
  , MoveDesc "Night Slash" 15 tackle
      {ty=DAR, pow=70, crit=1}
  , MoveDesc "Nightmare" 15 tackle
      {ty=GHO, cat=Special, pow=0, eff=Nightmare}
  , MoveDesc "No Retreat" 5 celebrate
      {ty=FIG, eff=NoSwitch :+ AddBoost True zero {att=1,def=1,spA=1,spD=1,spe=1}}
  , MoveDesc "Noble Roar" 30 celebrate
      {ty=NOR, targ=ADJACENT, eff=AddBoost False zero {att= -1, spA= -1}}
  , MoveDesc "Noxious Torque" 10 tackle
      {ty=POI, pow=100}
  , MoveDesc "Nuzzle" 20 tackle
      {ty=ELE, pow=20, eff=EStatus Paralysis}


   -- | Switch { user, random, keepBoost :: Bool }

  , MoveDesc "Rollout" 20 tackle
      {ty=ROC, pow=30, acc=90, eff=Scaling5Turns :+ DoubleDmgIfDefenceCurlUsed}
  , MoveDesc "Return" 20 tackle
      {ty=NOR, pow=0, eff=PwrHighFriendship}
  , MoveDesc "Rage Powder" 20 celebrate
      {ty=GRA, eff=FollowMe, flags=POWDER}
  , MoveDesc "Trick or Treat" 20 celebrate
      {ty=GHO, targ=ADJACENT, eff=AddType GHO}

  , MoveDesc "Water Pledge" 10 tackle
      {ty=WAT, pow=80, cat=Special, eff=WaterPledge}

  , MoveDesc "Quiver Dance" 20 celebrate
      {ty=BUG, flags=DANCE, eff=AddBoost True zero {spe=1, spA=1, spD=1}}
  , MoveDesc "Swords Dance" 20 celebrate
      {ty=NOR, flags=DANCE, eff=AddBoost True zero {att=2}}
  , MoveDesc "Teeter Dance" 20 celebrate
      {ty=NOR, flags=DANCE, targ=ADJACENT .|. WIDE, eff=Confuse}
  , MoveDesc "Victory Dance" 10 celebrate
      {ty=FIG, flags=DANCE, eff=AddBoost True zero {att=1, def=1}}
  , MoveDesc "Petal Dance" 10 tackle
      {ty=GRA, cat=Special, pow=120, flags=DANCE, eff=Locked}
  , MoveDesc "Revelation Dance" 15 tackle
      {ty=NOR, cat=Special, pow=90, flags=DANCE, eff=UserPrimary}


  , MoveDesc "Role Play" 15 celebrate
      {ty=NOR, eff=CopyAbility Target2User}
  , MoveDesc "Skill Swap" 15 celebrate
      {ty=NOR, eff=SwapAbility}

  , MoveDesc "Tackle"  35 tackle
      { flags=CONTACT }
  , MoveDesc "Struggle" 0 tackle
      {pow = 50, eff = Struggle, ty = NON}

  , MoveDesc "Phantom Force" 10 tackle
      {pow = 90, ty=GHO, eff=EInvul Phantom :+ IgnoreProtect False}

  , MoveDesc "Self-Destruct" 5 tackle
      {pow = 200, eff = UserDies, targ=ADJACENT .|. WIDE}

  , MoveDesc "Tri Attack" 10 tackle
      {ty=NOR, cat=Special, pow=80, eff=20 :% Choose [EStatus Paralysis, EStatus Burn, EStatus Freeze]}
  ]

-- Contact moves:
--
-- Accelerock
-- Acrobatics
-- Aerial Ace
-- Anchor Shot
-- Aqua Jet
-- Aqua Step
-- Aqua Tail
-- Arm Thrust
-- Assurance
-- Astonish
-- Avalanche
-- Axe Kick
-- Behemoth Bash
-- Behemoth Blade
-- Bide
-- Bind
-- Bite
-- Bitter Blade
-- Blaze Kick
-- Body Press
-- Body Slam
-- Bolt Beak
-- Bolt Strike
-- Bounce
-- Branch Poke
-- Brave Bird
-- Breaking Swipe
-- Brick Break
-- Brutal Swing
-- Bug Bite
-- Bullet Punch
-- Catastropika
-- Ceaseless Edge
-- Chip Away
-- Circle Throw
-- Clamp
-- Close Combat
-- Collision Course
-- Comet Punch
-- Comeuppance
-- Constrict
-- Counter
-- Covet
-- Crabhammer
-- Cross Chop
-- Cross Poison
-- Crunch
-- Crush Claw
-- Crush Grip
-- Cut
-- Darkest Lariat
-- Dig
-- Dire Claw
-- Dive
-- Dizzy Punch
-- Double Hit
-- Double Iron Bash
-- Double Kick
-- Double Shock
-- Double Slap
-- Double-Edge
-- Dragon Ascent
-- Dragon Claw
-- Dragon Hammer
-- Dragon Rush
-- Dragon Tail
-- Drain Punch
-- Draining Kiss
-- Drill Peck
-- Drill Run
-- Dual Chop
-- Dual Wingbeat
-- Dynamic Punch
-- Electro Drift
-- Endeavor
-- Extreme Speed
-- Facade
-- Fake Out
-- False Surrender
-- False Swipe
-- Feint Attack
-- Fell Stinger
-- Fire Fang
-- Fire Lash
-- Fire Punch
-- First Impression
-- Fishious Rend
-- Flail
-- Flame Charge
-- Flame Wheel
-- Flare Blitz
-- Flip Turn
-- Floaty Fall
-- Fly
-- Flying Press
-- Focus Punch
-- Force Palm
-- Foul Play
-- Frustration
-- Fury Attack
-- Fury Cutter
-- Fury Swipes
-- Gear Grind
-- Giga Impact
-- Glaive Rush
-- Grass Knot
-- Grassy Glide
-- Guillotine
-- Gyro Ball
-- Hammer Arm
-- Head Charge
-- Head Smash
-- Headbutt
-- Headlong Rush
-- Heart Stamp
-- Heat Crash
-- Heavy Slam
-- High Horsepower
-- High Jump Kick
-- Hold Back
-- Horn Attack
-- Horn Drill
-- Horn Leech
-- Hyper Drill
-- Hyper Fang
-- Ice Ball
-- Ice Fang
-- Ice Hammer
-- Ice Punch
-- Ice Spinner
-- Infestation
-- Iron Head
-- Iron Tail
-- Jaw Lock
-- Jet Punch
-- Jump Kick
-- Karate Chop
-- Knock Off
-- Kowtow Cleave
-- Lash Out
-- Last Resort
-- Leaf Blade
-- Leech Life
-- Let's Snuggle Forever
-- Lick
-- Liquidation
-- Low Kick
-- Low Sweep
-- Lunge
-- Mach Punch
-- Malicious Moonsault
-- Mega Kick
-- Mega Punch
-- Megahorn
-- Metal Claw
-- Meteor Mash
-- Mortal Spin
-- Multi-Attack
-- Needle Arm
-- Night Slash
-- Nuzzle
-- Outrage
-- Payback
-- Peck
-- Petal Dance
-- Phantom Force
-- Plasma Fists
-- Play Rough
-- Pluck
-- Poison Fang
-- Poison Jab
-- Poison Tail
-- Population Bomb
-- Pounce
-- Pound
-- Power Trip
-- Power Whip
-- Power-Up Punch
-- Psychic Fangs
-- Psyshield Bash
-- Pulverizing Pancake
-- Punishment
-- Pursuit
-- Quick Attack
-- Rage
-- Rage Fist
-- Raging Bull
-- Rapid Spin
-- Razor Shell
-- Retaliate
-- Return
-- Revenge
-- Reversal
-- Rock Climb
-- Rock Smash
-- Rolling Kick
-- Rollout
-- Sacred Sword
-- Scratch
-- Searing Sunraze Smash
-- Seismic Toss
-- Shadow Claw
-- Shadow Force
-- Shadow Punch
-- Shadow Sneak
-- Sizzly Slide
-- Skitter Smack
-- Skull Bash
-- Sky Drop
-- Sky Uppercut
-- Slam
-- Slash
-- Smart Strike
-- Smelling Salts
-- Snap Trap
-- Solar Blade
-- Soul-Stealing 7-Star Strike
-- Spark
-- Spectral Thief
-- Spin Out
-- Spirit Break
-- Steamroller
-- Steel Roller
-- Steel Wing
-- Stomp
-- Stomping Tantrum
-- Stone Axe
-- Storm Throw
-- Strength
-- Struggle
-- Submission
-- Sucker Punch
-- Sunsteel Strike
-- Super Fang
-- Superpower
-- Surging Strikes
-- Tackle
-- Tail Slap
-- Take Down
-- Thief
-- Thrash
-- Throat Chop
-- Thunder Fang
-- Thunder Punch
-- Thunderous Kick
-- Trailblaze
-- Triple Axel
-- Triple Dive
-- Triple Kick
-- Trop Kick
-- Trump Card
-- U-turn
-- V-create
-- Veevee Volley
-- Vine Whip
-- Vise Grip
-- Vital Throw
-- Volt Tackle
-- Wake-Up Slap
-- Waterfall
-- Wave Crash
-- Wicked Blow
-- Wild Charge
-- Wing Attack
-- Wood Hammer
-- Wrap
-- Wring Out
-- X-Scissor
-- Zen Headbutt
-- Zing Zap
-- Zippy Zap

moveByName n =
  move <$> L.find (\MoveDesc {..} -> name == n) moves

