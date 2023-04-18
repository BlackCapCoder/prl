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
type FLAGS   = Word16

simpleID = -1
insomniaID = -1


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
   , hits    :: (Word8,Word8)  -- Number of times to perform the move, between 2 and N (N if N<2)
   , pri     :: Int8   -- Priority bracket. In each bracket, faster pokemon attack first (unless trick room)
   , flags   :: FLAGS
   , targ    :: Target -- The target(s) of the move
   , eff     :: Effect -- additional effects if the move is successful
   }
   deriving (Show, Eq, Ord)

neverMiss  = 0 -- acc
alwaysCrit = 4 -- crit

-- Foe  Foe  Foe
-- Self Ally Ally
-- Wide
type Target = Word8

pattern SELF     = 0b0001000 :: Target
pattern ADJACENT = 0b1100100 :: Target
pattern ALL      = 0b1111110 :: Target
pattern ADJFOES  = 0b1100000 :: Target
pattern FOES     = 0b1110000 :: Target
pattern ALLIES   = 0b0000110 :: Target
pattern WIDE     = 0b0000001 :: Target -- hit all targets if set, otherwise choose one

-- Fields
--
pattern DANCE   = 0b1                :: FLAGS -- dance moves are copied by pokemon with dancer
pattern SOUND   = 0b10               :: FLAGS -- sound-based moves ignore substitues, but pokemon with soundproof are immune
pattern BULLET  = 0b100              :: FLAGS -- pokemon with bulletproof are immune
pattern POWDER  = 0b1000             :: FLAGS -- pokemon holding safety googles are immune
pattern CONTACT = 0b10000            :: FLAGS -- if a move makes contact it may trigger additional effects
pattern ZMOVE   = 0b100000           :: FLAGS -- z-moves can hit through protect at reduced damage
pattern TURN1   = 0b1000000          :: FLAGS -- wether the move can only be used on the first turn
pattern IGNSUB  = 0b10000000         :: FLAGS -- move can hit through substitute
pattern PULSE   = 0b100000000        :: FLAGS -- aura and pulse based move (boosted by Mega Launcher)
pattern BITE    = 0b1000000000       :: FLAGS -- bite based move (boosted by Strong Jaw)
pattern EXPLODE = 0b10000000000      :: FLAGS -- explosive moves cannot be used with Damp present
pattern PUNCH   = 0b100000000000     :: FLAGS -- punching moves (iron fist, punching glove)
pattern SLICE   = 0b1000000000000    :: FLAGS -- slicing moves (sharpness)
pattern WIND    = 0b10000000000000   :: FLAGS -- wind moves (wind power, wind rider)
pattern SNATCH  = 0b100000000000000  :: FLAGS -- move can be stolen by snatch
pattern MCOAT   = 0b1000000000000000 :: FLAGS -- move is affected by magic coat/magic bounce

-- Edit: Sound-based moves DOES NOT hit through substitute. It just so happens that
-- every sound-based move except for howl do

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
   | Uproar   -- Locks for 3 turns, prevents sleep, no confusion

   | ClearStatus
   | ClearStatusParty -- all ally pokemon in party
   | ClearScreen
   | ClearHazard
   | MoveStatus -- transfer users status to target

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
   | CopyType          -- user copies the target's type

   | Recharge  -- The move is performed, but the user must skip the next turn
   | Precharge -- The user skips this turn, the move is performed next turn

   | SwapAttDef -- swap att and def (power trick)
   | SwapOffDef -- swap users offensive/defensive stats (power shift)
   | AvgAtt     -- average att and spA with target (power split)
   | SwpAtt     -- swap att and spA with targer (power swap)
   | AvgDef     -- average def and spD with target
   | SwpDef     -- swap def and spD with targer
   | SwpSpe     -- swap spe with targer

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
   | DoublePwrIfTargetPoison

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
   | WideGuard     -- like protect, but only for moves that hit multiple pokemon
   | Substitute
   | IgnoreProtect -- the move can hit through protect (phantom force, horn drill, ..)
   | Feint

   -- Any foe that hits the user with a contact move, even if the user is protected,
   -- will recieve the following effect. The target of the effect is the foe
   --
   | IfUserHitByContactMove Effect

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
   | EatBerry Bool -- if true, user eats the targets berry, if false, target eat its own berry
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
   | SwapItem
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
   | Judgment -- type depends on arceus plate being held
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

   | PwrInTerrain Terrain -- power increases in the given terrain (misty explosion, psyblade)
   | DoublePwrInTerrain Terrain
   | IgnoreAbility -- Ignore the targets ability

   | RecoverWeather -- Recover an amount of health that varies with weather
   | MudSport -- Weakens the power of Electric-type moves
   | MultiAttack -- Type varies depending on held memory item
   | NaturalGift -- power and type depends on held berry
   | NaturePower -- Uses a certain move based on current terrain
   | Nightmare -- 0.25 damage to sleeping target

   | Octolock -- lower target def,spD every turn, prevent switching out
   | OdorSleuth -- Reset target eva. allow normal and fight to hit ghost

   | PayDay -- money after battle depending on users level
   | DoublePwrIfUserAttacked -- on this turn
   | UseHighestOfAttSpA
   | PwrHighBond -- Power increases when player's bond is stronger

   | PollenPuff -- damage if foe, heal if ally
   | Poltergeist -- fail if target does not have a held item
   | Powder -- damages pokemon using fire-type moves
   | Present -- random damage or heal (present)
   | Psywave -- Inflict damage 50%-150% of user's level
   | Punishment -- Power increases when opponent's stats have been raised
   | Purify -- heal target's status condition. If successful, restore users HP
   | Pursuit -- if target switches out, hit with double power

   | Quash
   | QuickGuard

   | Rage -- raise user's attack when hit (rage)
   | RageFist -- The more times the user has been hit by attacks, the greater the move's power
   | RagingBull -- Type depends on the user's form
   | RagingFury -- User keeps repeating the same move over and over
   | Recycle -- User's held item is restored
   | Refresh -- Cures paralysis, poison and burns (not freeze or sleep, toxic?)
   | SleepFor2Turns -- Sleep for exactly 2 turns, overwriting old status condition (rest)
   | DoubleDmgIfAllyFaintedLastTurn

   | ReviveAllyToHalfHP
   | GroundFor1Turn -- remove flying type for 1 turn
   | Rototiller -- Raise att,spA of grass types
   | Round -- power "increases" if an ally uses round on the same turn.
           -- All ally round users attack immediately after the fastest ally

   | SaltCure -- Deals damage each turn; steel and water types are more affected
   | SecretPower -- Effects of the attack vary with location
   | ShedTail -- Create a substitute, then switch to a teammate.
              -- Check if this works like baton pass

   | ShellSideArm -- "May" poison. Inflict either Special/Physical damage, whichever is better
   | ShellTrap -- Deal "more" damage if hit by a Physical move
   | ShoreUp -- Recover, but heal more in a sandstorm
   | Sketch -- Permanently replace the user's move with the target's last used move
   | SkyDrop -- Both user and target gets Flying semiInvul.
             -- If target is slower than the user, it skips its attack.
             -- Next turn, the user attacks the target

   | HitInvul Invulnerable -- Attack can hit target in the given invul state,
                           -- but there are no additional effects

   | SleepTalk -- use a random known move, excluding sleep talk
   | GroundFlying -- target (flying-types) become vulnerable to ground-type moves
                  -- TODO: Does this apply to levitate too?

   | SmellingSalts -- double power if target is paralysed. Cure the paralysis
   | IgnoreFollowMe
   | FailIfNotAsleep
   | Snowscape -- raises defence of ice-types for 5 turns
   | ChargeIfNotSun
   | HealBurn
   | StealStatBoosts -- removes the targets possitive boosts and adds them onto the user

   | SpitUp
   | Swallow
   | Stockpile
   | Spite
   | SpringtideStorm
   | DoublePwrIfLastMoveFailed
   | StrengthSap -- heal user by the same amount as the target's attack stat
   | SuckerPunch
   | Synchronoise -- hits all adjacent pokemon that share a type with the user

   | TarShot -- makes the target weaker to fire-type moves
   | TechnoBlast -- type depends on held drive
   | Telekinesis -- ignore targets eva for 3 turns, add Ground immunity (to target) and immunity to arena trap
   | TeraBlast -- change type when user has terastallized
   | TerrainPulse -- type and power changes depending on current terrain
   | TroatChop -- prevent use of sound moves for 2 turns
   | PerfectAccuracyInWeather Weather
   | RemoveSubstitute
   | TripleAxel -- A consecutive three-kick attack that becomes more powerful with each successful hit
   | TripleKick -- Hits 3 times, with each kick's BP increasing by 10.
                -- Each hit has its own acc check, if one misses the move ends

   | TrumpCard -- Lower the PP, the higher the power
   | Transform
   | Torment
   | VenomDrench -- Lower poisoned opponents spA, spe

   | WakeUpSlap -- Double power if target asleep, but wakes it up
   | WaterSport -- Weaken fire-type moves
   | WeatherBall
   | WringOut -- higher damage the more user HP

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
   | SandTomb
   | SnapTrap
   | ThunderCage
   | Wrap
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
  , hits    = (1, 1)
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
  , hits    = (1, 1)
  , eff     = None
  , flags  = 0
  }


moves =
  [ MoveDesc "10,000,000 Volt Thunderbolt" 1 tackle
      {ty=ELE, flags=ZMOVE, cat=Special, crit=1, pow=195, acc=neverMiss}
  , MoveDesc "Absorb" 25 tackle
      {ty=GRA, cat=Special, eff=Drain 0.5, pow=20}
  , MoveDesc "Accelerock" 20 tackle
      {ty=ROC, pow=40, pri=1, flags=CONTACT}
  , MoveDesc "Acid" 30 tackle
      {ty=POI, cat=Special, pow=40, eff=10 :% AddBoost False zero {spD = -1}}
  , MoveDesc "Acid Armor" 20 celebrate
      {ty=POI, eff=AddBoost True zero {def = 2}, flags=SNATCH}
  -- acid downpour
  , MoveDesc "Acid Spray" 20 tackle
      {ty=POI, cat=Special, pow=40, eff=AddBoost False zero {spD = -2}, flags=BULLET}
  , MoveDesc "Acrobatics" 15 tackle
      {ty=FLY, pow=55, eff=DoublePwrNoItem, flags=CONTACT}
  , MoveDesc "Acupressure" 30 celebrate
      {ty=NOR, eff=AddRandomBoost 2}
  , MoveDesc "Aerial Ace" 20 tackle
      {ty=FLY, pow=60, acc=neverMiss, flags=CONTACT .|. SLICE}
  , MoveDesc "Aeroblast" 5 tackle
      {ty=FLY, cat=Special, pow=100, acc=95, crit=1}
  , MoveDesc "After You" 15 celebrate
      {ty=NOR, targ=ADJACENT, eff=AfterYou, flags=IGNSUB, acc=neverMiss}
  , MoveDesc "Agility" 30 celebrate
      {ty=PSY, eff=AddBoost True zero {spe = 2}, flags=SNATCH}
  , MoveDesc "Air Cutter" 25 tackle
      {ty=FLY, cat=Special, pow=60, acc=95, crit=1, flags=SLICE .|. WIND}
  , MoveDesc "Air Slash" 15 tackle
      {ty=FLY, cat=Special, pow=75, acc=95, eff=30 :% Flinch, flags=SLICE}
  -- all-out plummeling
  , MoveDesc "Ally Switch" 15 celebrate
      {ty=PSY, eff=AllySwap, targ=SELF, pri=2}
  , MoveDesc "Amnesia" 20 celebrate
      {ty=PSY, eff=AddBoost True zero {spD=2}, flags=SNATCH}
  , MoveDesc "Anchor Shot" 20 tackle
      {ty=STE, pow=80, eff=NoSwitch, flags=CONTACT}
  , MoveDesc "Ancient Power" 5 tackle
      {ty=ROC, pow=60, cat=Special, eff=10 :% omniboost}
  , MoveDesc "Apple Acid" 10 tackle
      {ty=GRA, cat=Special, pow=80, eff=AddBoost False zero {spD = -1}}
  , MoveDesc "Aqua Cutter" 20 tackle
      {ty=WAT, pow=70, crit=1, flags=SLICE}
  , MoveDesc "Aqua Jet" 20 tackle
      {ty=WAT, pow=40, pri=1, flags=CONTACT}
  , MoveDesc "Aqua Ring" 20 celebrate
      {ty=WAT, eff=AquaRing, flags=SNATCH}
  , MoveDesc "Aqua Step" 10 tackle
      {ty=WAT, pow=80, eff=AddBoost True zero {spe=1}, flags=CONTACT .|. DANCE}
  , MoveDesc "Aqua Tail" 10 tackle
      {ty=WAT, pow=90, flags=CONTACT}
  , MoveDesc "Arm Thrust" 20 tackle
      {ty=FIG, pow=15, hits=(2,5), flags=CONTACT}
  , MoveDesc "Armor Cannon" 5 tackle
      {ty=FIR, pow=120, cat=Special, eff=AddBoost True zero {def= -1, spD= -1}}
  , MoveDesc "Aromatherapy" 5 celebrate
      {ty=GRA, eff=ClearStatusParty, flags=SNATCH}
  , MoveDesc "Aromatic Mist" 20 celebrate
      {ty=FAI, eff=AddBoost False zero {spD=1}, targ=ADJACENT, flags=IGNSUB}
  , MoveDesc "Assist" 20 celebrate
      {ty=NOR, eff=Assist}
  , MoveDesc "Assurance" 10 tackle
      {ty=DAR, pow=60, eff=DoublePwrIfHit, flags=CONTACT}
  , MoveDesc "Astonish" 15 tackle
      {ty=GHO, pow=30, eff=30 :% Flinch, flags=CONTACT}
  , MoveDesc "Astral Barrage" 5 tackle
      {ty=GHO, pow=120, cat=Special}
  , MoveDesc "Attack Order" 15 tackle
      {ty=BUG, pow=90, crit=1}
  , MoveDesc "Attract" 15 celebrate
      {ty=NOR, targ=ADJACENT, eff=Attract, flags=IGNSUB .|. MCOAT}
  , MoveDesc "Aura Sphere" 20 tackle
      {ty=FIG, cat=Special, acc=neverMiss, pow=80, flags=PULSE .|. BULLET}
  , MoveDesc "Aura Wheel" 10 tackle
      {ty=ELE, pow=110, eff=MorpekoMode}
  , MoveDesc "Aurora Beam" 20 tackle
      {ty=ICE, cat=Special, pow=65, eff=10 :% AddBoost False zero {att= -1}, flags=SNATCH}
  , MoveDesc "Aurora Veil" 20 celebrate
      {ty=ICE, eff=EScreen PhySpe}
  , MoveDesc "Autotomize" 15 celebrate
      {ty=STE, eff=Autotomize, flags=SNATCH}
  , MoveDesc "Avalanche" 10 tackle
      {ty=ICE, pow=60, eff=DoublePwrIfHit, flags=CONTACT, pri= -4}
  , MoveDesc "Axe Kick" 10 tackle
      {ty=FIG, pow=120, eff=90 :% AxeKick, flags=CONTACT}

  , MoveDesc "Baby-Doll Eyes" 30 celebrate
      {ty=FAI, pri=1, eff=AddBoost False zero {att= -1}, flags=MCOAT}
  , MoveDesc "Baddy Bad" 15 tackle
      {ty=DAR, cat=Special, pow=80, eff=BaddyBad, acc=neverMiss}
  , MoveDesc "Baneful Bunker" 10 celebrate
      {ty=POI, eff=Protect :+ IfUserHitByContactMove do EStatus Poison, pri=4}
  , MoveDesc "Barb Barrage" 10 tackle
      {ty=POI, pow=60, eff=DoubleDmgIfTargetStatus}
  , MoveDesc "Barrage" 20 tackle
      {ty=NOR, pow=15, acc=85, hits=(2,5), flags=BULLET}
  , MoveDesc "Barrier" 20 celebrate
      {ty=PSY, eff=AddBoost True zero {def=2}, flags=SNATCH}
  , MoveDesc "Baton Pass" 40 celebrate
      {ty=NOR, eff=Switch True False True}
  , MoveDesc "Beak Blast" 15 tackle
      {ty=FLY, pow=100, eff=BeakBlast, pri= -3}
  , MoveDesc "Beat Up" 10 tackle
      {ty=DAR, pow=0, eff=BeatUp}
  , MoveDesc "Behemoth Bash" 5 tackle
      {ty=STE, pow=100, eff=DoubleDmgIfDynamax, flags=CONTACT}
  , MoveDesc "Behemoth Blade" 5 tackle
      {ty=STE, pow=100, eff=DoubleDmgIfDynamax, flags=CONTACT .|. SLICE}
  , MoveDesc "Belch" 10 tackle
      {ty=POI, pow=120, cat=Special, eff=Belch}
  , MoveDesc "Belly Drum" 10 celebrate
      {ty=NOR, eff=BellyDrum, flags=SNATCH}
  , MoveDesc "Bestow" 15 celebrate
      {ty=NOR, eff=Bestow, flags=IGNSUB, acc=neverMiss}
  , MoveDesc "Bide" 10 tackle
      {ty=NOR, eff=Bide, flags=CONTACT, pri=1, acc=neverMiss}
  , MoveDesc "Bind" 20 tackle
      {ty=NOR, pow=15, acc=85, eff=ELocking Bind, flags=CONTACT}
  , MoveDesc "Bite" 25 tackle
      {ty=DAR, pow=60, eff=30 :% Flinch, flags=CONTACT .|. BITE}
  , MoveDesc "Bitter Blade" 10 tackle
      {ty=FIR, pow=90, eff=Drain 0.5, flags=CONTACT .|. SLICE}
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
      {ty=FLY, cat=Special, pow=100, acc=80, eff=10 :% EStatus Frostbite, flags=WIND}
  , MoveDesc "Blizzard" 5 tackle
      {ty=ICE, pow=110, cat=Special, acc=70, eff=10 :% EStatus Freeze :+ PerfectAccuracyInWeather Hail, flags=WIND}
  , MoveDesc "Block" 5 celebrate
      {ty=NOR, targ=ADJACENT, eff=NoSwitch, acc=neverMiss, flags=MCOAT}
  -- bloom doom
  , MoveDesc "Blue Flare" 5 tackle
      {ty=FIR, pow=130, cat=Special, acc=85, eff=20 :% EStatus Burn}
  , MoveDesc "Body Press" 10 tackle
      {ty=FIG, pow=80, eff=BodyPress, flags=CONTACT}
  , MoveDesc "Body Slam" 15 tackle
      {ty=NOR, pow=85, eff=30 :% EStatus Paralysis, flags=CONTACT, acc=neverMiss}
  , MoveDesc "Bolt Beak" 10 tackle
      {ty=ELE, pow=85, eff=DoublePwrIfTargetFaster, flags=CONTACT}
  , MoveDesc "Bolt Strike" 5 tackle
      {ty=ELE, pow=130, acc=85, eff=20 :% EStatus Paralysis, flags=CONTACT}
  , MoveDesc "Bone Club" 20 tackle
      {ty=GRO, pow=65, acc=85, eff=10 :% Flinch}
  , MoveDesc "Bone Rush" 10 tackle
      {ty=GRO, pow=25, acc=90, hits=(2,5)}
  , MoveDesc "Bonemerang" 10 tackle
      {ty=GRO, pow=50, acc=90, hits=(2,2)}
  , MoveDesc "Boomburst" 10 tackle
      {ty=NOR, pow=140, cat=Special, targ=ADJACENT .|. WIDE, flags=SOUND .|. IGNSUB}
  , MoveDesc "Bounce" 5 tackle
      {pow = 85, ty=FLY, acc=85, eff=EInvul Flying :+ 30 :% EStatus Paralysis, flags=CONTACT}
  , MoveDesc "Bouncy Bubble" 20 tackle
      {ty=WAT, pow=60, cat=Special, eff=Drain 0.5, acc=neverMiss}
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
      {ty=BUG, pow=60, eff=EatBerry True, flags=CONTACT}
  , MoveDesc "Bug Buzz" 10 tackle
      {ty=BUG, pow=90, cat=Special, eff=10 :% AddBoost False zero {spD= -1}, flags=SOUND .|. IGNSUB}
  , MoveDesc "Bulk Up" 20 celebrate
      {ty=FIG, eff=AddBoost True zero {att=1, def=1}, flags=SNATCH}
  , MoveDesc "Bulldoze" 20 tackle
      {ty=GRO, pow=60, targ=ADJACENT .|. WIDE, eff=AddBoost False zero {spe= -1}}
  , MoveDesc "Bullet Punch" 30 tackle
      {ty=STE, pow=40, pri=1, flags=CONTACT .|. PUNCH}
  , MoveDesc "Bullet Seed" 30 tackle
      {ty=GRA, pow=25, hits=(2,5), flags=BULLET}
  , MoveDesc "Burn Up" 5 tackle
      {ty=FIR, pow=130, cat=Special, eff=SetType NON True}
  , MoveDesc "Burning Jealousy" 5 tackle
      {ty=FIR, pow=70, cat=Special, targ=ADJFOES .|. WIDE, eff=BurnIfBoosted}
  , MoveDesc "Buzzy Buzz" 20 tackle
      {ty=ELE, pow=60, cat=Special, eff=EStatus Paralysis, acc=neverMiss}

  , MoveDesc "Calm Mind" 20 celebrate
      {ty=PSY, eff=AddBoost True zero{spA=1, spD=1}, flags=SNATCH}
  , MoveDesc "Camouflage" 20 celebrate
      {ty=NOR, eff=Camouflage, flags=SNATCH}
  , MoveDesc "Captivate" 20 celebrate
      {ty=NOR, acc=100, targ=ADJACENT, eff=Captivate, flags=MCOAT}
  , MoveDesc "Catastropika" 1 tackle
      {ty=ELE, pow=210, flags=CONTACT .|. ZMOVE}
  , MoveDesc "Ceaseless Edge" 15 tackle
      {ty=DAR, pow=65, acc=90, crit=1, eff=DamageWithSplinters, flags=CONTACT .|. SLICE}
  , MoveDesc "Celebrate" 40 celebrate
  , MoveDesc "Charge" 20 celebrate
      {ty=ELE, eff=Charge, flags=SNATCH}
  , MoveDesc "Charge Beam" 10 tackle
      {ty=ELE, pow=50, cat=Special, acc=90, eff=70 :% AddBoost True zero {spA=1}}
  , MoveDesc "Charm" 20 celebrate
      {ty=FAI, targ=ADJACENT, eff=AddBoost False zero {att= -2}, flags=MCOAT}
  , MoveDesc "Chatter" 20 tackle
      {ty=FLY, pow=65, cat=Special, acc=100, eff=Confuse, flags=SOUND .|. IGNSUB}
  , MoveDesc "Chilling Water" 20 tackle
      {ty=WAT, pow=50, cat=Special, eff=AddBoost False zero {att= -1}}
  , MoveDesc "Chilly Reception" 10 celebrate
      {ty=ICE, eff=ChillyReception}
  , MoveDesc "Chip Away" 20 tackle
      {ty=NOR, pow=70, eff=IgnoreBoosts, flags=CONTACT}
  , MoveDesc "Chloroblast" 5 tackle
      {ty=GRA, pow=150, acc=95, cat=Special, eff=Recoil 0.5} -- TODO: Recoil is a constant 0.5x of users maximum HP
  , MoveDesc "Circle Throw" 10 tackle
      {ty=FIG, pow=60, acc=90, eff=Switch False True False, flags=CONTACT, pri= -6}
  , MoveDesc "Clamp" 15 tackle
      {ty=WAT, pow=35, acc=85, eff=ELocking Clamp, flags=CONTACT}
  , MoveDesc "Clanging Scales" 5 tackle
      {ty=DRA, pow=110, cat=Special, eff=AddBoost True zero {def= -1}, targ=ADJFOES .|. WIDE, flags=SOUND .|. IGNSUB}
  , MoveDesc "Clangorous Soul" 5 celebrate
      {ty=DRA, eff=AddBoost True zero {att=1,def=1,spA=1,spD=1,spe=1} :+ FractionalDamageMax (1/3), flags=SOUND .|. IGNSUB .|. DANCE}
  , MoveDesc "Clangorous Soulblaze" 1 tackle
      {ty=DRA, pow=185, cat=Special, flags=ZMOVE .|. SOUND .|. IGNSUB, acc=neverMiss}
  , MoveDesc "Clear Smog" 15 tackle
      {ty=POI, pow=50, cat=Special, eff=ClearBoost, acc=neverMiss}
  , MoveDesc "Close Combat" 5 tackle
      {ty=FIG, pow=120, eff=AddBoost True zero {def= -1, spD= -1}, flags=CONTACT}
  , MoveDesc "Coaching" 10 celebrate
      {ty=FIG, targ=ALLIES .|. WIDE, eff=AddBoost False zero {att=1,def=1}}
  , MoveDesc "Coil" 20 celebrate
      {ty=POI, eff=AddBoost True zero {att=1,def=1,acc=1}, flags=SNATCH}
  , MoveDesc "Collision Course" 5 tackle
      {ty=FIG, pow=100, eff=BoostSuperEffective, flags=CONTACT}
  , MoveDesc "Combat Torque" 10 tackle
      {ty=FIG, pow=100}
  , MoveDesc "Comet Punch" 15 tackle
      {ty=NOR, pow=18, acc=85, hits=(2,5), flags=CONTACT .|. PUNCH}
  , MoveDesc "Comeuppance" 10 tackle
      {ty=DAR, pow=1, eff=Comeuppance, flags=CONTACT}
  , MoveDesc "Confide" 20 celebrate
      {ty=NOR, targ=ADJACENT, eff=AddBoost False zero {spA= -1}, flags=SOUND .|. IGNSUB .|. MCOAT, acc=neverMiss}
  , MoveDesc "Confuse Ray" 10 celebrate
      {ty=GHO, targ=ADJACENT, eff=Confuse, flags=MCOAT}
  , MoveDesc "Confusion" 25 tackle
      {ty=PSY, pow=50, cat=Special, eff=10 :% Confuse}
  , MoveDesc "Constrict" 35 tackle
      {ty=NOR, pow=10, eff=10 :% AddBoost False zero {spe= -1}, flags=CONTACT}
  -- continental crush
  , MoveDesc "Conversion" 30 celebrate
      {ty=NOR, eff=Conversion, flags=SNATCH}
  , MoveDesc "Conversion 2" 30 celebrate
      {ty=NOR, eff=Conversion2, flags=IGNSUB, acc=neverMiss}
  , MoveDesc "Copycat" 20 celebrate
      {ty=NOR, eff=Copycat}
  , MoveDesc "Core Enforcer" 10 tackle
      {ty=DRA, cat=Special, pow=100, eff=CoreEnforcer}
  -- corkscrew crash
  , MoveDesc "Corrosive Gas" 40 celebrate
      {ty=POI, targ=ADJACENT, eff=RemoveItem, flags=MCOAT}
  , MoveDesc "Cosmic Power" 20 celebrate
      {ty=PSY, eff=AddBoost True zero {def=1, spD=1}, flags=SNATCH}
  , MoveDesc "Cotton Guard" 10 celebrate
      {ty=GRA, eff=AddBoost True zero {def=3}, flags=SNATCH}
  , MoveDesc "Cotton Spore" 40 celebrate
      {ty=GRA, targ=ADJACENT, eff=AddBoost False zero {spe= -2}, flags=POWDER .|. MCOAT}
  , MoveDesc "Counter" 20 celebrate
      {ty=FIG, cat=Physical, eff=Counter, flags=CONTACT, pri= -5}
  , MoveDesc "Court Change" 10 celebrate
      {ty=NOR, eff=SwapFieldEffects}
  , MoveDesc "Covet" 25 tackle
      {ty=NOR, pow=60, eff=StealItem, flags=CONTACT}
  , MoveDesc "Crabhammer" 10 tackle
      {ty=WAT, pow=100, acc=90, crit=1, flags=CONTACT}
  , MoveDesc "Crafty Shield" 10 celebrate
      {ty=FAI, eff=CraftyShield, pri=3}
  , MoveDesc "Cross Chop" 5 tackle
      {ty=FIG, pow=100, acc=80, crit=1, flags=CONTACT}
  , MoveDesc "Cross Poison" 20 tackle
      {ty=POI, pow=70, crit=1, eff=10 :% EStatus Poison, flags=CONTACT .|. SLICE}
  , MoveDesc "Crunch" 15 tackle
      {ty=DAR, pow=80, eff=20 :% AddBoost False zero {def= -1}, flags=CONTACT .|. BITE}
  , MoveDesc "Crush Claw" 10 tackle
      {ty=NOR, pow=75, acc=95, eff=50 :% AddBoost False zero {def= -1}, flags=CONTACT}
  , MoveDesc "Crush Grip" 5 tackle
      {ty=NOR, pow=0, eff=CrushGrip, flags=CONTACT}
  , MoveDesc "Curse" 10 celebrate
      {ty=GHO, eff=Curse, flags=IGNSUB}
  , MoveDesc "Cut" 30 tackle
      {ty=NOR, pow=50, acc=95, flags=CONTACT .|. SLICE}

  , MoveDesc "Dark Pulse" 15 tackle
      {ty=DAR, pow=80, cat=Special, eff=20 :% Flinch, flags=PULSE}
  , MoveDesc "Dark Void" 10 celebrate
      {ty=DAR, targ=ADJFOES .|. WIDE, eff=EStatus Sleep, flags=MCOAT}
  , MoveDesc "Darkest Lariat" 10 tackle
      {ty=DAR, pow=85, eff=IgnoreBoosts, flags=CONTACT}
  , MoveDesc "Dazzling Gleam" 10 tackle
      {ty=FAI, cat=Special, pow=80, targ=ADJFOES .|. WIDE}
  , MoveDesc "Decorate" 15 celebrate
      {ty=FAI, targ=ADJACENT, eff=AddBoost False zero {att=2, spA=2}}
  , MoveDesc "Defend Order" 10 celebrate
      {ty=BUG, eff=AddBoost True zero {def=1, spD=1}, flags=SNATCH}
  , MoveDesc "Defense Curl" 40 celebrate -- TODO: Typo
      {ty=NOR, eff=AddBoost True zero {def=1} :+ DefenceCurlUsed, flags=SNATCH}
  , MoveDesc "Defog" 15 celebrate
      {ty=FLY, targ=ADJACENT, eff=Defog, acc=neverMiss, flags=MCOAT}
  , MoveDesc "Destiny Bond" 5 celebrate
      {ty=GHO, eff=DestinyBond}
  , MoveDesc "Detect" 5 celebrate
      {ty=FIG, eff=Protect, pri=4}
  -- devastating drake
  , MoveDesc "Diamond Storm" 5 tackle
      {ty=ROC, pow=100, acc=95, targ=ADJFOES .|. WIDE, eff=50 :% AddBoost True zero {def=2}}
  , MoveDesc "Dig" 10 tackle
      {pow = 80, ty=GRO, flags=CONTACT, eff = EInvul Digging}
  , MoveDesc "Dire Claw" 15 tackle
      {ty=POI, pow=80, crit=1, flags=CONTACT, eff=50 :% Choose [EStatus Poison, EStatus Paralysis, Yawn]}
  , MoveDesc "Disable" 20 celebrate
      {ty=NOR, targ=ADJACENT, eff=Disable, flags=IGNSUB .|. MCOAT}
  , MoveDesc "Disarming Voice" 15 tackle
      {ty=FAI, cat=Special, acc=neverMiss, pow=40, flags=SOUND .|. IGNSUB}
  , MoveDesc "Discharge" 15 tackle
      {ty=ELE, cat=Special, targ=ADJACENT .|. WIDE, pow=80, eff=30 :% EStatus Paralysis}
  , MoveDesc "Dive" 10 tackle
      {pow = 80, ty=WAT, eff = EInvul Diving, flags=CONTACT}
  , MoveDesc "Dizzy Punch" 10 tackle
      {ty=NOR, pow=70, eff=20 :% Confuse, flags=CONTACT .|. PUNCH}
  , MoveDesc "Doodle" 10 celebrate
      {ty=NOR, targ=ADJACENT, eff=CopyAbility Target2Allies}
  , MoveDesc "Doom Desire" 5 tackle
      {ty=STE, pow=140, cat=Special, eff=Delay2Turns}
  , MoveDesc "Double Hit" 10 tackle
      {ty=NOR, pow=35, acc=90, hits=(2,2), flags=CONTACT}
  , MoveDesc "Double Iron Bash" 5 tackle
      {ty=STE, pow=60, hits=(2,2), eff=30 :% Flinch, flags=CONTACT .|. PUNCH}
  , MoveDesc "Double Kick" 30 tackle
      {ty=FIG, pow=30, hits=(2,2), flags=CONTACT}
  , MoveDesc "Double Shock" 5 tackle
      {ty=ELE, pow=120, eff=RemoveType ELE, flags=CONTACT}
  , MoveDesc "Double Slap" 10 tackle
      {ty=NOR, pow=15, hits=(2,5), acc=85, flags=CONTACT}
  , MoveDesc "Double Team" 15 celebrate
      {ty=NOR, eff=AddBoost True zero {eva=1}, flags=SNATCH}
  , MoveDesc "Double-Edge" 15 tackle
      {ty=NOR, pow=120, eff=Recoil (1/3), flags=CONTACT}
  , MoveDesc "Draco Meteor" 5 tackle
      {ty=DRA, pow=130, cat=Special, eff=AddBoost True zero { spA= -2 }, acc=90}
  , MoveDesc "Dragon Ascent" 5 tackle
      {ty=FLY, pow=120, eff=AddBoost True zero {def= -1, spD= -1}, flags=CONTACT}
  , MoveDesc "Dragon Breath" 20 tackle
      {ty=DRA, pow=60, cat=Special, eff=30 :% EStatus Paralysis}
  , MoveDesc "Dragon Claw" 15 tackle
      {ty=DRA, pow=80, flags=CONTACT}
  , MoveDesc "Dragon Dance" 20 celebrate
      {ty=DRA, eff=AddBoost True zero {att=1, spe=1}, flags=DANCE .|. SNATCH}
  , MoveDesc "Dragon Darts" 10 tackle
      {ty=DRA, pow=50, hits=(2,2)}
  , MoveDesc "Dragon Energy" 5 tackle
      {ty=DRA, pow=150, eff=CutPwrUserHP, cat=Special}
  , MoveDesc "Dragon Hammer" 15 tackle
      {ty=DRA, pow=90, flags=CONTACT}
  , MoveDesc "Dragon Pulse" 10 tackle
      {ty=DRA, pow=85, cat=Special, flags=PULSE}
  , MoveDesc "Dragon Rage" 10 tackle
      {ty=DRA, pow=0, cat=Special, eff=ConstantDamage 40}
  , MoveDesc "Dragon Rush" 10 tackle -- perfect accuracy and double power if target minimized
      {ty=DRA, pow=100, acc=75, eff=20 :% Flinch, flags=CONTACT}
  , MoveDesc "Dragon Tail" 10 tackle
      {ty=DRA, pow=60, acc=90, eff=Switch False True False, flags=CONTACT, pri= -6}
  , MoveDesc "Drain Punch" 10 tackle
      {ty=FIG, pow=75, eff=Drain 0.5, flags=CONTACT .|. PUNCH}
  , MoveDesc "Draining Kiss" 10 tackle
      {ty=FAI, pow=50, cat=Special, eff=Drain 0.75, flags=CONTACT}
  , MoveDesc "Dream Eater" 15 tackle
      {ty=PSY, pow=100, cat=Special, eff=DrainSleeping 0.5}
  , MoveDesc "Drill Peck" 20 tackle
      {ty=FLY, pow=80, flags=CONTACT}
  , MoveDesc "Drill Run" 10 tackle
      {ty=GRO, pow=80, acc=95, crit=1, flags=CONTACT}
  , MoveDesc "Drum Beating" 10 tackle
      {ty=GRA, pow=80, eff=AddBoost False zero {spe= -1}}
  , MoveDesc "Dual Chop" 15 tackle
      {ty=DRA, pow=40, acc=90, hits=(2,2), flags=CONTACT}
  , MoveDesc "Dual Wingbeat" 10 tackle
      {ty=FLY, pow=40, acc=90, hits=(2,2), flags=CONTACT}
  , MoveDesc "Dynamax Cannon" 5 tackle
      {ty=DRA, pow=100, cat=Special, eff=DoubleDmgIfDynamax}
  , MoveDesc "Dynamic Punch" 5 tackle
      {ty=FIG, pow=100, acc=50, eff=Confuse, flags=CONTACT .|. PUNCH}

  , MoveDesc "Earth Power" 10 tackle
      {ty=GRO, pow=90, cat=Special, eff=10 :% AddBoost False zero {spD= -1}}
  , MoveDesc "Earthquake" 10 tackle
      {ty=GRO, pow=100, targ=ADJACENT .|. WIDE, eff=DoublePowerIfInvul Digging}
  , MoveDesc "Echoed Voice" 15 tackle
      {ty=NOR, cat=Special, pow=40, eff=EchoPower, flags=SOUND .|. IGNSUB}
  , MoveDesc "Eerie Impulse" 15 celebrate
      {ty=ELE, targ=ADJACENT, eff=AddBoost False zero {spA= -2}, flags=MCOAT}
  , MoveDesc "Eerie Spell" 5 tackle
      {ty=PSY, cat=Special, pow=80, eff=RemovePP 3, flags=SOUND .|. IGNSUB}
  , MoveDesc "Egg Bomb" 10 tackle
      {ty=NOR, pow=100, acc=75, flags=BULLET}
  , MoveDesc "Electric Terrain" 10 celebrate
      {ty=ELE, eff=ETerrain TElectric}
  , MoveDesc "Electrify" 20 celebrate
      {ty=ELE, targ=ADJACENT, eff=SetType ELE False, acc=neverMiss}
  , MoveDesc "Electro Ball" 10 tackle
      {ty=ELE, cat=Special, pow=0, eff=SpeedPower, flags=BULLET}
  , MoveDesc "Electro Drift" 5 tackle
      {ty=ELE, cat=Special, pow=100, eff=BoostSuperEffective, flags=CONTACT}
  , MoveDesc "Electroweb" 15 tackle
      {ty=ELE, cat=Special, pow=55, acc=95, eff=AddBoost False zero {spe= -1}, targ=ADJFOES .|. WIDE}
  , MoveDesc "Embargo" 15 celebrate
      {ty=DAR, targ=ADJACENT, eff=Embargo, flags=MCOAT}
  , MoveDesc "Ember" 25 tackle
      {ty=FIR, cat=Special, eff=10 :% EStatus Burn}
  , MoveDesc "Encore" 5 celebrate
      {ty=NOR, targ=ADJACENT, eff=Encore, flags=IGNSUB .|. MCOAT}
  , MoveDesc "Endeavor" 5 tackle
      {ty=NOR, pow=0, eff=MatchUserHP, flags=CONTACT}
  , MoveDesc "Endure" 10 celebrate
      {ty=NOR, eff=Endure, pri=4}
  , MoveDesc "Energy Ball" 10 tackle
      {ty=GRA, cat=Special, pow=90, eff=10 :% AddBoost False zero {spD= -1}, flags=BULLET}
  , MoveDesc "Entrainment" 15 celebrate
      {ty=NOR, eff=CopyAbility User2Target, flags=MCOAT}
  , MoveDesc "Eruption" 5 tackle
      {ty=FIR, cat=Special, targ=ADJFOES .|. WIDE, eff=CutPwrUserHP, pow=150}
  , MoveDesc "Esper Wing" 10 tackle
      {ty=PSY, cat=Special, pow=80, crit=1, eff=AddBoost True zero {spe=1}}
  , MoveDesc "Eternabeam" 5 tackle
      {ty=DRA, cat=Special, pow=160, acc=90, eff=Recharge}
  , MoveDesc "Expanding Force" 10 tackle
      {ty=PSY, cat=Special, pow=80, eff=ExpandingForce}
  , MoveDesc "Explosion" 5 tackle
      {ty=NOR, pow = 250, eff = UserDies, targ=ADJACENT .|. WIDE, flags=EXPLODE}
  , MoveDesc "Extrasensory" 20 tackle
      {ty=PSY, cat=Special, pow=80, eff=10 :% Flinch}
  -- extreme evoboost
  , MoveDesc "Extreme Speed" 5 tackle
      {ty=NOR, pow = 80, pri=2, flags=CONTACT}

  , MoveDesc "Facade" 20 tackle
      {ty=NOR, pow=70, eff=DoublePwrIfUserStatus, flags=CONTACT}
  , MoveDesc "Fairy Lock" 10 celebrate
      {ty=FAI, targ=ADJACENT, eff=NoFleeingNextTurn, flags=IGNSUB}
  , MoveDesc "Fairy Wind" 30 tackle
      {ty=FAI, pow=40, cat=Special, flags=WIND}
  , MoveDesc "Fake Out" 10 tackle
      {ty=NOR, pow=40, pri=3, eff=Flinch, flags=CONTACT .|. TURN1}
  , MoveDesc "Fake Tears" 20 celebrate
      {ty=DAR, targ=ADJACENT, eff=AddBoost False zero {spD= -2}, flags=MCOAT}
  , MoveDesc "False Surrender" 10 tackle
      {ty=DAR, pow=80, acc=neverMiss, flags=CONTACT}
  , MoveDesc "False Swipe" 40 tackle
      {ty=NOR, pow=40, eff=Don'tKill, flags=CONTACT}
  , MoveDesc "Feather Dance" 15 celebrate
      {ty=FLY, targ=ADJACENT, flags=DANCE .|. MCOAT, eff=AddBoost False zero {att= -2}}
  , MoveDesc "Feint" 10 tackle
      {ty=NOR, pow=30, eff=Feint, pri=2}
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
      {ty=FIG, cat=Special, eff=FinalGambit}
  , MoveDesc "Fire Blast" 5 tackle
      {ty=FIR, cat=Special, pow=110, acc=85, eff=10 :% EStatus Burn}
  , MoveDesc "Fire Fang" 15 tackle
      {ty=FIR, pow=65, acc=95, eff=10 :% Flinch :+ 10 :% EStatus Burn, flags=CONTACT .|. BITE}
  , MoveDesc "Fire Lash" 15 tackle
      {ty=FIR, pow=80, eff=AddBoost False zero {def= -1}, flags=CONTACT}
  , MoveDesc "Fire Pledge" 10 tackle
      {ty=FIR, pow=80, cat=Special, eff=FirePledge}
  , MoveDesc "Fire Punch" 15 tackle
      {ty=FIR, pow=75, eff=10 :% EStatus Burn, flags=CONTACT .|. PUNCH}
  , MoveDesc "Fire Spin" 15 tackle
      {ty=FIR, pow=35, cat=Special, eff=ELocking Firespin}
  , MoveDesc "First Impression" 10 tackle
      {ty=BUG, pow=90, flags=CONTACT .|. TURN1, pri=2}
  , MoveDesc "Fishious Rend" 10 tackle
      {ty=WAT, pow=85, eff=DoublePwrIfTargetSlower, flags=CONTACT .|. BITE}
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
      {ty=NOR, targ=ADJACENT, eff=AddBoost False zero {att=0, acc= -1}, flags=MCOAT}
  , MoveDesc "Flash Cannon" 10 tackle
      {ty=STE, pow=80, cat=Special, eff=10 :% AddBoost False zero {spD= -1}}
  , MoveDesc "Flatter" 15 celebrate
      {ty=DAR, targ=ADJACENT, eff=Confuse :+ AddBoost False zero {spA=1}, flags=MCOAT}
  , MoveDesc "Fleur Cannon" 5 tackle
      {ty=FAI, pow=130, cat=Special, acc=90, eff=AddBoost True zero {spA= -2}}
  , MoveDesc "Fling" 10 tackle
      {ty=DAR, pow=0, eff=Fling}
  , MoveDesc "Flip Turn" 20 tackle
      {ty=WAT, pow=60, eff=Switch True False False, flags=CONTACT}
  , MoveDesc "Floaty Fall" 15 tackle
      {ty=FLY, pow=90, acc=95, eff=30 :% Flinch, flags=CONTACT}
  , MoveDesc "Floral Healing" 10 celebrate
      {ty=FAI, targ=ADJACENT, eff=FloralHealing, acc=neverMiss, flags=MCOAT}
  , MoveDesc "Flower Shield" 10 celebrate
      {ty=FAI, targ=ALL .|. WIDE, eff=FloralShield}
  , MoveDesc "Flower Trick" 10 tackle
      {ty=GRA, pow=70, acc=neverMiss, crit=alwaysCrit}
  , MoveDesc "Fly" 15 tackle
      {pow = 90, ty=FLY, acc=95, flags=CONTACT, eff = EInvul Flying}
  , MoveDesc "Flying Press" 10 tackle -- perfect accuracy and double damage if minimized
      {ty=FIG, pow=100, acc=95, eff=ExtraType FLY, flags=CONTACT}
  , MoveDesc "Focus Blast" 5 tackle
      {ty=FIG, cat=Special, pow=120, acc=70, eff=10 :% AddBoost False zero {spD= -1}, flags=BULLET}
  , MoveDesc "Focus Energy" 30 celebrate
      {ty=NOR, eff=AddBoost True zero {cri=1}, flags=SNATCH}
  , MoveDesc "Focus Punch" 20 tackle
      {ty=FIG, pow=150, eff=FlinchIfHit, flags=CONTACT .|. PUNCH, pri= -3}
  , MoveDesc "Follow Me" 20 celebrate
      {ty=NOR, eff=FollowMe, pri=2}
  , MoveDesc "Force Palm" 10 tackle
      {ty=FIG, pow=60, eff=30 :% EStatus Paralysis, flags=CONTACT}
  , MoveDesc "Foresight" 40 celebrate
      {ty=NOR, targ=ADJACENT, eff=Foresight, flags=IGNSUB .|. MCOAT, acc=neverMiss}
  , MoveDesc "Forest's Curse" 20 celebrate
      {ty=GRA, targ=ADJACENT, eff=AddType GRA, flags=MCOAT}
  , MoveDesc "Foul Play" 15 tackle
      {ty=DAR, pow=95, eff=UseTargetAtt, flags=CONTACT}
  , MoveDesc "Freeze Shock" 5 tackle
      {ty=ICE, pow=140, acc=90, eff=Precharge :+ 30 :% EStatus Paralysis}
  , MoveDesc "Freeze-Dry" 20 tackle
      {ty=ICE, pow=70, cat=Special, eff=SuperEffectiveAgainst WAT :+ 10 :% EStatus Freeze}
  , MoveDesc "Freezing Glare" 10 tackle
      {ty=PSY, pow=90, cat=Special, eff=10 :% EStatus Freeze}
  , MoveDesc "Freezy Frost" 10 tackle
      {ty=ICE, pow=100, cat=Special, targ=ALL .|. WIDE, eff=ClearBoost, acc=neverMiss}
  , MoveDesc "Frenzy Plant" 5 tackle
      {ty=GRA, pow=150, acc=90, cat=Special, eff=Recharge}
  , MoveDesc "Frost Breath" 10 tackle
      {ty=ICE, pow=60, acc=90, cat=Special, crit=alwaysCrit}
  , MoveDesc "Frustration" 20 tackle
      {ty=NOR, pow=0, eff=PwrLowFriendship, flags=CONTACT}
  , MoveDesc "Fury Attack" 20 tackle
      {ty=NOR, pow=15, acc=85, hits=(2,5), flags=CONTACT}
  , MoveDesc "Fury Cutter" 20 tackle
      {ty=BUG, pow=40, acc=95, eff=EchoPower, flags=CONTACT .|. SLICE}
  , MoveDesc "Fury Swipes" 15 tackle
      {ty=NOR, pow=18, acc=80, hits=(2,5), flags=CONTACT}
  , MoveDesc "Fusion Bolt" 5 tackle
      {ty=ELE, pow=100, eff=FusionBolt}
  , MoveDesc "Fusion Flare" 5 tackle
      {ty=FIR, cat=Special, pow=100, eff=FusionFlare}
  , MoveDesc "Future Sight" 10 tackle
      {ty=PSY, cat=Special, pow=120, eff=Delay2Turns}

  -- G-Max moves ...

  , MoveDesc "Gastro Acid" 10 celebrate
      {ty=POI, targ=ADJACENT, eff=SuppressAbility, flags=MCOAT}
  , MoveDesc "Gear Grind" 15 tackle
      {ty=STE, pow=50, hits=(2,2), acc=85, flags=CONTACT}
  , MoveDesc "Gear Up" 20 celebrate
      {ty=STE, eff=GearUp, targ=WIDE .|. SELF .|. ALLIES, flags=IGNSUB .|. SNATCH}
  , MoveDesc "Genesis Supernova" 1 tackle
      {ty=PSY, cat=Special, pow=185, flags=ZMOVE}
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
      {ty=ICE, pow=130}
  , MoveDesc "Glaciate" 10 tackle
      {ty=ICE, pow=65, cat=Special, acc=95, eff=AddBoost False zero {spe= -1}}
  , MoveDesc "Glaive Rush" 5 tackle
      {ty=DRA, pow=120, eff=GlaiveRush, flags=CONTACT}
  , MoveDesc "Glare" 30 celebrate
      {ty=NOR, targ=ADJACENT, eff=EStatus Paralysis, flags=MCOAT}
  , MoveDesc "Glitzy Glow" 15 tackle
      {ty=PSY, cat=Special, pow=80, eff=GlitzyGlow, acc=neverMiss}
  , MoveDesc "Grass Knot" 20 tackle
      {ty=GRA, cat=Special, pow=0, eff=PwrHeavyTarget, flags=CONTACT}
  , MoveDesc "Grass Pledge" 10 tackle
      {ty=GRA, pow=80, cat=Special, eff=GrassPledge}
  , MoveDesc "Grass Whistle" 15 celebrate
      {ty=GRA, targ=ADJACENT, eff=EStatus Sleep, acc=55, flags=SOUND .|. IGNSUB .|. MCOAT}
  , MoveDesc "Grassy Glide" 20 tackle
      {ty=GRA, pow=70, eff=GrassyGlide, flags=CONTACT}
  , MoveDesc "Grassy Terrain" 10 celebrate
      {ty=GRA, eff=ETerrain TGrass}
  , MoveDesc "Grav Apple" 10 tackle
      {ty=GRA, pow=80, eff=AddBoost False zero {def= -1}}
  , MoveDesc "Gravity" 5 celebrate
      {ty=PSY, eff=Gravity}
  , MoveDesc "Growl" 40 celebrate
      {ty=NOR, targ=ADJACENT, eff=AddBoost False zero {att= -1}, flags=SOUND .|. IGNSUB .|. MCOAT}
  , MoveDesc "Growth" 20 celebrate
      {ty=NOR, eff=AddBoost True zero {att=1, spA=1}, flags=SNATCH}
  , MoveDesc "Grudge" 5 celebrate
      {ty=GHO, targ=ADJACENT, eff=Grudge}
  , MoveDesc "Guard Split" 10 celebrate
      {ty=PSY, targ=ADJACENT, eff=AvgDef, acc=neverMiss}
  , MoveDesc "Guard Swap" 10 celebrate
      {ty=PSY, targ=ADJACENT, eff=SwpDef, flags=IGNSUB, acc=neverMiss}
  -- Guardian of Alola
  , MoveDesc "Guillotine" 5 tackle
      {ty=NOR, pow=0, eff=OHKO, acc=30, flags=CONTACT}
  , MoveDesc "Gunk Shot" 5 tackle
      {ty=POI, pow=120, acc=80, eff=30 :% EStatus Poison}
  , MoveDesc "Gust" 35 tackle
      {ty=FLY, pow=40, cat=Special, eff=DoublePowerIfInvul Flying, flags=WIND}
  , MoveDesc "Gyro Ball" 5 tackle
      {ty=STE, pow=0, eff=GyroBall, flags=CONTACT .|. BULLET}

  , MoveDesc "Hail" 10 celebrate
      {ty=ICE, eff=EWeather Hail}
  , MoveDesc "Hammer Arm" 10 tackle
      {ty=FIG, pow=100, acc=90, eff=AddBoost True zero {spe= -1}, flags=CONTACT .|. PUNCH}
  , MoveDesc "Happy Hour" 30 celebrate
      {ty=NOR, eff=HappyHour}
  , MoveDesc "Harden" 30 celebrate
      {ty=NOR, eff=AddBoost True zero {def=1}, flags=SNATCH}
  , MoveDesc "Haze" 30 celebrate
      {ty=ICE, eff=ClearBoost, targ=ALL .|. WIDE, flags=IGNSUB}
  , MoveDesc "Head Charge" 15 tackle
      {ty=NOR, pow=120, eff=Recoil (1/4), flags=CONTACT}
  , MoveDesc "Head Smash" 5 tackle
      {ty=ROC, pow=150, acc=80, eff=Recoil (1/2), flags=CONTACT}
  , MoveDesc "Headbutt" 15 tackle
      {ty=NOR, pow=70, eff=30 :% Flinch, flags=CONTACT}
  , MoveDesc "Headlong Rush" 5 tackle
      {ty=GRO, pow=120, eff=AddBoost True zero {def= -1}, flags=CONTACT .|. PUNCH}
  , MoveDesc "Heal Bell" 5 celebrate
      {ty=NOR, eff=ClearStatusParty, flags=SOUND .|. IGNSUB .|. SNATCH}
  , MoveDesc "Heal Block" 15 celebrate
      {ty=PSY, targ=ADJACENT, eff=HealBlock, flags=MCOAT}
  , MoveDesc "Heal Order" 10 celebrate
      {ty=BUG, eff=Recover 0.5, flags=SNATCH}
  , MoveDesc "Heal Pulse" 10 celebrate
      {ty=PSY, targ=ADJACENT, eff=Recover 0.5, flags=PULSE .|. MCOAT, acc=neverMiss}
  , MoveDesc "Healing Wish" 10 celebrate
      {ty=PSY, eff=DieHealSwitchIn, flags=SNATCH}
  , MoveDesc "Heart Stamp" 25 tackle
      {ty=PSY, pow=60, eff=30 :% Flinch, flags=CONTACT}
  , MoveDesc "Heart Swap" 10 celebrate
      {ty=PSY, targ=ADJACENT, eff=SwapBoost, flags=IGNSUB, acc=neverMiss}
  , MoveDesc "Heat Crash" 10 tackle
      {ty=FIR, pow=0, eff=PwrHeavyUser, flags=CONTACT, acc=neverMiss}
  , MoveDesc "Heat Wave" 10 tackle
      {ty=FIR, pow=95, cat=Special, acc=90, targ=ADJFOES .|. WIDE, eff=10 :% EStatus Burn, flags=WIND}
  , MoveDesc "Heavy Slam" 10 tackle
      {ty=STE, pow=0, eff=PwrHeavyUser, flags=CONTACT}
  , MoveDesc "Helping Hand" 20 celebrate
      {ty=NOR, targ=ADJACENT, eff=HelpingHand, pri=5, flags=IGNSUB}
  , MoveDesc "Hex" 10 tackle
      {ty=GHO, cat=Special, pow=65, eff=DoubleDmgIfTargetStatus}
  , MoveDesc "Hidden Power" 15 tackle
      {ty=NOR, pow=60, cat=Special, eff=HiddenPower}
  , MoveDesc "High Horsepower" 10 tackle
      {ty=GRO, pow=95, acc=95, flags=CONTACT}
  , MoveDesc "High Jump Kick" 10 tackle
      {ty=FIG, pow=130, acc=90, eff=DamageUserIfMiss 0.5, flags=CONTACT}
  , MoveDesc "Hold Back" 40 tackle
      {ty=NOR, pow=40, eff=Don'tKill, flags=CONTACT}
  , MoveDesc "Hold Hands" 40 celebrate
      {ty=NOR, targ=ALLIES, flags=IGNSUB}
  , MoveDesc "Hone Claws" 15 celebrate
      {ty=DAR, eff=AddBoost True zero {att=1, acc=1}, flags=SNATCH}
  , MoveDesc "Horn Attack" 25 tackle
      {ty=NOR, pow=65, flags=CONTACT}
  , MoveDesc "Horn Drill" 5 tackle
      {ty=NOR, pow=0, acc=30, eff=OHKO, flags=CONTACT}
  , MoveDesc "Horn Leech" 10 tackle
      {ty=GRA, pow=75, eff=Drain 0.5, flags=CONTACT}
  , MoveDesc "Howl" 40 celebrate
      {ty=NOR, targ=ALLIES .|. SELF .|. WIDE, eff=AddBoost False zero {att=1}, flags=SOUND .|. SNATCH}
  , MoveDesc "Hurricane" 10 tackle
      {ty=FLY, cat=Special, acc=70, pow=110
      ,eff = 30 :% Confuse
          :+ PerfectAccuracyInWeather Rain
      ,flags=WIND
      }
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
      {ty=NOR, pow=100, eff=IgnoreProtect, flags=CONTACT}
  , MoveDesc "Hyper Fang" 15 tackle
      {ty=NOR, pow=80, acc=90, eff=10 :% Flinch, flags=CONTACT .|. BITE}
  , MoveDesc "Hyper Voice" 10 tackle
      {ty=NOR, pow=90, cat=Special, targ=ADJFOES .|. WIDE, flags=SOUND .|. IGNSUB}
  , MoveDesc "Hyperspace Fury" 5 tackle
      {ty=DAR, pow=100, acc=neverMiss, eff=IgnoreProtect :+ AddBoost True zero {def= -1}, flags=IGNSUB}
  , MoveDesc "Hyperspace Hole" 5 tackle
      {ty=PSY, cat=Special, acc=neverMiss, pow=80, eff=IgnoreProtect, flags=IGNSUB}
  , MoveDesc "Hypnosis" 20 celebrate
      {ty=PSY, acc=60, eff=EStatus Sleep, targ=ADJACENT, flags=MCOAT}

  , MoveDesc "Ice Ball" 20 tackle
      {ty=ICE, pow=30, acc=90, eff=Scaling5Turns, flags=CONTACT .|. BULLET}
  , MoveDesc "Ice Beam" 10 tackle
      {ty=ICE, pow=90, cat=Special, eff=10 :% EStatus Freeze}
  , MoveDesc "Ice Burn" 5 tackle
      {ty=ICE, pow=140, cat=Special, acc=90, eff=Precharge :+ 30 :% EStatus Burn}
  , MoveDesc "Ice Fang" 15 tackle
      {ty=ICE, pow=65, acc=95, eff=10 :% Flinch :+ 10 :% EStatus Freeze, flags=CONTACT .|. BITE}
  , MoveDesc "Ice Hammer" 10 tackle
      {ty=ICE, pow=100, acc=90, eff=AddBoost True zero {def= -1}, flags=CONTACT .|. PUNCH}
  , MoveDesc "Ice Punch" 15 tackle
      {ty=ICE, pow=75, eff=10 :% EStatus Freeze, flags=CONTACT .|. PUNCH}
  , MoveDesc "Ice Shard" 30 tackle
      {ty=ICE, pow=40, pri=1}
  , MoveDesc "Ice Spinner" 15 tackle
      {ty=ICE, pow=80, eff=ClearTerrain, flags=CONTACT}
  , MoveDesc "Icicle Crash" 10 tackle
      {ty=ICE, pow=85, acc=90, eff=30 :% Flinch}
  , MoveDesc "Icicle Spear" 30 tackle
      {ty=ICE, pow=25, hits=(2,5)}
  , MoveDesc "Icy Wind" 15 tackle
      {ty=ICE, cat=Special, pow=55, targ=ADJFOES .|. WIDE, eff=AddBoost False zero {spe= -1}, flags=WIND}
  , MoveDesc "Imprison" 10 celebrate
      {ty=PSY, targ=ADJACENT, eff=Imprison, flags=IGNSUB .|. SNATCH}
  , MoveDesc "Incinerate" 15 tackle
      {ty=FIR, cat=Special, pow=60, eff=RemoveBerry False}
  , MoveDesc "Infernal Parade" 15 tackle
      {ty=GHO, cat=Special, pow=60, eff=DoubleDmgIfTargetStatus}
  , MoveDesc "Inferno" 5 tackle
      {ty=FIR, cat=Special, pow=100, acc=50, eff=EStatus Burn}
  -- Inferno overdrive
  , MoveDesc "Infestation" 20 tackle
      {ty=BUG, cat=Special, pow=20, eff=ELocking Infestation, flags=CONTACT}
  , MoveDesc "Ingrain" 20 celebrate
      {ty=GRA, eff=Ingrain, flags=SNATCH}
  , MoveDesc "Instruct" 15 celebrate
      {ty=PSY, eff=Instruct, targ=ADJACENT, flags=IGNSUB}
  , MoveDesc "Ion Deluge" 25 celebrate
      {ty=ELE, eff=IonDeluge, pri=1}
  , MoveDesc "Iron Defense" 15 celebrate
      {ty=STE, eff=AddBoost True zero {def=2}, flags=SNATCH}
  , MoveDesc "Iron Head" 15 tackle
      {ty=STE, eff=30 :% Flinch, pow=80, flags=CONTACT}
  , MoveDesc "Iron Tail" 15 tackle
      {ty=STE, pow=100, acc=75, eff=30 :% AddBoost False zero {def= -1}, flags=CONTACT}

  , MoveDesc "Jaw Lock" 10 tackle
      {ty=DAR, pow=80, eff=NoSwitchUserAndTarget, flags=CONTACT .|. BITE}
  , MoveDesc "Jet Punch" 15 tackle
      {ty=WAT, pow=60, pri=1, flags=CONTACT .|. PUNCH}
  , MoveDesc "Judgment" 10 tackle
      {ty=NOR, pow=100, eff=Judgment, cat=Special}
  , MoveDesc "Jump Kick" 10 tackle
      {ty=FIG, pow=100, acc=95, eff=DamageUserIfMiss 0.5, flags=CONTACT}
  , MoveDesc "Jungle Healing" 10 celebrate
      {ty=GRA, eff=JungleHealing}

  , MoveDesc "Karate Chop" 25 tackle
      {ty=FIG, pow=50, crit=1, flags=CONTACT}
  , MoveDesc "Kinesis" 15 celebrate
      {ty=PSY, acc=80, targ=ADJACENT, eff=AddBoost False zero {acc= -1}, flags=MCOAT}
  , MoveDesc "King's Shield" 10 celebrate
      {ty=PSY, eff=Protect :+ IfUserHitByContactMove do AddBoost True zero {att= -1}}

  , MoveDesc "Knock Off" 20 tackle
      {ty=DAR, pow=65, eff=RemoveItem, flags=CONTACT}
  , MoveDesc "Kowtow Cleave" 10 tackle
      {ty=DAR, pow=85, acc=neverMiss, flags=CONTACT .|. SLICE}

  , MoveDesc "Land's Wrath" 10 tackle
      {ty=GRO, pow=90}
  , MoveDesc "Laser Focus" 30 celebrate
      {ty=NOR, targ=ADJACENT, eff=LaserFocus, flags=SNATCH}
  , MoveDesc "Lash Out" 5 tackle
      {ty=DAR, pow=75, eff=DoublePwrIfUserDebuff, flags=CONTACT}
  , MoveDesc "Last Resort" 5 tackle
      {ty=NOR, pow=140, eff=AllOtherMovesUsed, flags=CONTACT}
  , MoveDesc "Last Respects" 10 tackle
      {ty=GHO, pow=50, eff=LastRespects}
  , MoveDesc "Lava Plume" 15 tackle
      {ty=FIR, cat=Special, pow=80, eff=30 :% EStatus Burn}
  , MoveDesc "Leaf Blade" 15 tackle
      {ty=GRA, pow=90, crit=1, flags=CONTACT .|. SLICE}
  , MoveDesc "Leaf Storm" 5 tackle
      {ty=GRA, cat=Special, pow=130, acc=90, eff=AddBoost True zero {spA= -2}}
  , MoveDesc "Leaf Tornado" 10 tackle
      {ty=GRA, cat=Special, pow=65, acc=90, eff=50 :% AddBoost False zero {acc= -1}}
  , MoveDesc "Leafage" 40 tackle
      {ty=GRA, pow=40}
  , MoveDesc "Leech Life" 10 tackle
      {ty=BUG, pow=80, eff=Drain 0.5, flags=CONTACT}
  , MoveDesc "Leech Seed" 10 celebrate
      {ty=GRA, acc=90, eff=LeechSeed, flags=MCOAT}
  , MoveDesc "Leer" 30 celebrate
      {ty=NOR, eff=AddBoost False zero {def= -1}, flags=MCOAT}
  , MoveDesc "Let's Snuggle Forever" 1 tackle
      {ty=FAI, flags=ZMOVE, pow=190}
  , MoveDesc "Lick" 30 tackle
      {ty=GHO, pow=30, eff=30 :% EStatus Paralysis, flags=CONTACT}
  , MoveDesc "Life Dew" 10 celebrate
      {ty=WAT, targ=ALLIES .|. SELF .|. WIDE, eff=Recover 0.25, acc=neverMiss}
  , MoveDesc "Light of Ruin" 5 tackle
      {ty=FAI, cat=Special, pow=140, acc=90, eff=Recoil 0.5}
  , MoveDesc "Light Screen" 30 celebrate
      {ty=PSY, eff=EScreen Spe, flags=SNATCH}
  , MoveDesc "Light That Burns the Sky" 1 tackle
      {ty=PSY, cat=Special, flags=ZMOVE, pow=200, eff=LightThatBurnsTheSky, acc=neverMiss}
  , MoveDesc "Liquidation" 10 tackle
      {ty=WAT, flags=CONTACT, pow=85, eff=20 :% AddBoost False zero {def= -1}}
  , MoveDesc "Lock-On" 5 celebrate
      {ty=NOR, targ=ADJACENT, eff=LockOn, acc=neverMiss}
  , MoveDesc "Lovely Kiss" 10 celebrate
      {ty=NOR, targ=ADJACENT, acc=75, eff=EStatus Sleep, flags=MCOAT}
  , MoveDesc "Low Kick" 20 tackle
      {ty=FIG, pow=0, eff=PwrHeavyTarget, flags=CONTACT}
  , MoveDesc "Low Sweep" 20 tackle
      {ty=FIG, pow=65, eff=AddBoost False zero {spe= -1}, flags=CONTACT}
  , MoveDesc "Lucky Chant" 30 celebrate
      {ty=NOR, eff=LuckyChant, flags=SNATCH}
  , MoveDesc "Lumina Crash" 10 tackle
      {ty=PSY, cat=Special, pow=80, eff=AddBoost False zero {spD= -2}}
  , MoveDesc "Lunar Blessing" 5 celebrate
      {ty=PSY, eff=ClearStatus :+ Recover 0.5}
  , MoveDesc "Lunar Dance" 10 celebrate
      {ty=PSY, flags=DANCE .|. SNATCH, eff=DieHealSwitchIn}
  , MoveDesc "Lunge" 15 tackle
      {ty=BUG, pow=80, eff=AddBoost False zero {att= -1}, flags=CONTACT}
  , MoveDesc "Luster Purge" 5 tackle
      {ty=PSY, cat=Special, pow=70, eff=50 :% AddBoost False zero {spD= -1}}

  , MoveDesc "Mach Punch" 30 tackle
      {ty=FIG, pow=40, pri=1, flags=CONTACT .|. PUNCH}
  , MoveDesc "Magic Coat" 15 celebrate
      {ty=PSY, eff=MagicCoat, pri=4}
  , MoveDesc "Magic Powder" 20 celebrate
      {ty=PSY, targ=ADJACENT, eff=SetType PSY False, flags=POWDER .|. MCOAT}
  , MoveDesc "Magic Room" 10 celebrate
      {ty=PSY, targ=ADJACENT, eff=MagicRoom}
  , MoveDesc "Magical Leaf" 20 tackle
      {ty=GRA, cat=Special, pow=60, targ=WIDE .|. ADJFOES, acc=neverMiss}
  , MoveDesc "Magical Torque" 10 tackle
      {ty=FAI, pow=100}
  , MoveDesc "Magma Storm" 5 tackle
      {ty=FIR, cat=Special, pow=100, acc=75, eff=ELocking MagmaStorm}
  , MoveDesc "Magnet Bomb" 20 tackle
      {ty=STE, pow=60, acc=neverMiss, flags=BULLET}
  , MoveDesc "Magnet Rise" 10 celebrate
      {ty=ELE, eff=MagnetRise, flags=SNATCH}
  , MoveDesc "Magnetic Flux" 20 celebrate
      {ty=ELE, eff=MagneticFlux, targ=WIDE .|. SELF .|. ALLIES, flags=IGNSUB .|. SNATCH}
  , MoveDesc "Magnitude" 30 tackle
      {ty=GRO, pow=0, eff=Magnitude, targ=WIDE .|. ADJACENT}
  , MoveDesc "Make It Rain" 5 tackle -- TODO: Extra money
      {ty=STE, cat=Special, pow=120, targ=WIDE .|. ADJFOES, eff=AddBoost True zero {spA= -1}}
  , MoveDesc "Malicious Moonsault" 1 tackle
      {ty=DAR, pow=180, flags=CONTACT .|. ZMOVE, acc=neverMiss}
  , MoveDesc "Mat Block" 10 celebrate -- TODO: The TURN1 check applies to the user, not the snatcher?
      {ty=FIG, targ=SELF .|. ALLIES .|. WIDE, flags=TURN1 .|. SNATCH, eff=Protect}
  -- Max z-moves ...
  , MoveDesc "Me First" 20 celebrate
      {ty=NOR, targ=ADJACENT, eff=MeFirst, flags=IGNSUB, acc=neverMiss}
  , MoveDesc "Mean Look" 5 celebrate
      {ty=NOR, targ=ADJACENT, eff=NoSwitch, acc=neverMiss, flags=MCOAT}
  , MoveDesc "Meditate" 40 celebrate
      {ty=PSY, eff=AddBoost True zero {att=1}, flags=SNATCH}
  , MoveDesc "Mega Drain" 15 tackle
      {ty=GRA, pow=40, cat=Special, eff=Drain 0.5}
  , MoveDesc "Mega Kick" 5 tackle
      {ty=NOR, pow=120, acc=75, flags=CONTACT}
  , MoveDesc "Mega Punch" 20 tackle
      {ty=NOR, pow=80, acc=85, flags=CONTACT .|. PUNCH}
  , MoveDesc "Megahorn" 10 tackle
      {ty=BUG, pow=120, acc=85, flags=CONTACT}
  , MoveDesc "Memento" 10 celebrate
      {ty=DAR, pow=120, acc=neverMiss, eff=UserDies :+ AddBoost False zero {att= -2, spA= -2}}
  , MoveDesc "Menacing Moonraze Maelstrom" 1 tackle
      {ty=GHO, pow=200, flags=ZMOVE, cat=Special, acc=neverMiss}
  , MoveDesc "Metal Burst" 10 tackle
      {ty=STE, pow=0, eff=MatchTarget'sDamage 1.5}
  , MoveDesc "Metal Claw" 35 tackle
      {ty=STE, pow=50, acc=95, eff=10 :% AddBoost True zero {att=1}, flags=CONTACT}
  , MoveDesc "Metal Sound" 40 celebrate
      {ty=STE, targ=ADJACENT, acc=85, eff=AddBoost False zero {spD= -2}, flags=SOUND .|. IGNSUB .|. MCOAT}
  , MoveDesc "Meteor Assault" 5 tackle
      {ty=FIG, pow=150, eff=Recharge}
  , MoveDesc "Meteor Beam" 10 tackle
      {ty=ROC, cat=Special, pow=120, acc=90, eff=AddBoost True zero {spA=1} :+ Precharge}
  , MoveDesc "Meteor Mash" 10 tackle
      {ty=STE, pow=90, acc=90, eff=10 :% AddBoost True zero {att=1}, flags=CONTACT .|. PUNCH}
  , MoveDesc "Metronome" 10 celebrate
      {ty=NOR, eff=Metronome}
  , MoveDesc "Milk Drink" 10 celebrate
      {ty=NOR, eff=Recover 0.5, flags=SNATCH}
  , MoveDesc "Mimic" 10 celebrate
      {ty=NOR, eff=Mimic, acc=neverMiss}
  , MoveDesc "Mind Blown" 5 tackle
      {ty=FIR, cat=Special, pow=150, eff=RecoilMax 0.5, targ=WIDE .|. ADJACENT, flags=EXPLODE}
  , MoveDesc "Mind Reader" 5 celebrate
      {ty=NOR, eff=LockOn, acc=neverMiss}
  , MoveDesc "Minimize" 10 celebrate
      {ty=NOR, eff=AddBoost True zero {eva=1}, flags=SNATCH}
  , MoveDesc "Miracle Eye" 40 celebrate
      {ty=PSY, targ=ADJACENT, eff=MiracleEye, acc=neverMiss, flags=MCOAT}
  , MoveDesc "Mirror Coat" 20 tackle
      {ty=PSY, pow=0, cat=Special, eff=MirrorCoat, pri= -5}
  , MoveDesc "Mirror Move" 20 celebrate
      {ty=FLY, eff=MirrorMove}
  , MoveDesc "Mirror Shot" 10 tackle
      {ty=STE, cat=Special, pow=65, acc=85, eff=30 :% AddBoost False zero {acc= -1}}
  , MoveDesc "Mist" 30 celebrate
      {ty=ICE, eff=Mist, flags=SNATCH}
  , MoveDesc "Mist Ball" 5 tackle
      {ty=PSY, cat=Special, pow=70, eff=50 :% AddBoost False zero {spA= -1}, flags=BULLET}
  , MoveDesc "Misty Explosion" 5 tackle
      {ty=FAI, cat=Special, pow=100, targ=WIDE .|. ADJACENT, eff=UserDies :+ PwrInTerrain TMisty, flags=EXPLODE}
  , MoveDesc "Misty Terrain" 10 celebrate
      {ty=FAI, eff=ETerrain TMisty}
  , MoveDesc "Moonblast" 15 tackle
      {ty=FAI, cat=Special, pow=95, eff=30 :% AddBoost False zero {spA= -1}}
  , MoveDesc "Moongeist Beam" 5 tackle
      {ty=GHO, cat=Special, pow=100, eff=IgnoreAbility}
  , MoveDesc "Moonlight" 5 celebrate
      {ty=FAI, eff=RecoverWeather, flags=SNATCH}
  , MoveDesc "Morning Sun" 5 celebrate
      {ty=NOR, eff=RecoverWeather, flags=SNATCH}
  , MoveDesc "Mortal Spin" 15 tackle -- TODO: Removes entry hazards and trap move effects, and poisons opposing Pokémon.
      {ty=POI, pow=30, targ=WIDE .|. ADJFOES, eff=ClearHazard :+ EStatus Poison, flags=CONTACT}
  , MoveDesc "Mountain Gale" 10 tackle
      {ty=ICE, pow=100, acc=85, eff=AddBoost True zero {spe= -1}}
  , MoveDesc "Mud Bomb" 10 tackle
      {ty=GRO, cat=Special, pow=65, acc=85, eff=30 :% AddBoost False zero {acc= -1}, flags=BULLET}
  , MoveDesc "Mud Shot" 15 tackle
      {ty=GRO, cat=Special, pow=55, acc=95, eff=AddBoost False zero {spe= -1}}
  , MoveDesc "Mud Sport" 15 celebrate
      {ty=GRO, eff=MudSport}
  , MoveDesc "Mud-Slap" 10 tackle
      {ty=GRO, cat=Special, pow=20, eff=AddBoost False zero {acc= -1}}
  , MoveDesc "Muddy Water" 10 tackle
      {ty=WAT, cat=Special, targ=WIDE .|. ADJFOES, pow=90, acc=85, eff=AddBoost False zero {acc= -1}}
  , MoveDesc "Multi-Attack" 10 tackle
      {ty=NOR, pow=120, eff=MultiAttack, flags=CONTACT}
  , MoveDesc "Mystical Fire" 10 tackle
      {ty=FIR, cat=Special, pow=75, eff=AddBoost False zero {spA= -1}}
  , MoveDesc "Mystical Power" 10 tackle
      {ty=PSY, cat=Special, pow=70, acc=90, eff=Choose [AddBoost True zero {att=1}, AddBoost True zero {spA=1}]}

  , MoveDesc "Nasty Plot" 20 celebrate
      {ty=DAR, eff=AddBoost True zero {spA=2}, flags=SNATCH}
  , MoveDesc "Natural Gift" 15 tackle
      {ty=NOR, pow=0, eff=NaturalGift}
  , MoveDesc "Nature Power" 20 celebrate
      {ty=NOR, eff=NaturePower}
  , MoveDesc "Nature's Madness" 20 tackle
      {ty=FAI, pow=0, cat=Special, eff=HalfHP}
  , MoveDesc "Needle Arm" 15 tackle
      {ty=GRA, pow=60, eff=30 :% Flinch, flags=CONTACT}
  -- never ending nightmare
  , MoveDesc "Night Daze" 10 tackle
      {ty=DAR, cat=Special, pow=85, acc=95, eff=40 :% AddBoost False zero {acc= -1}}
  , MoveDesc "Night Shade" 15 tackle
      {ty=GHO, cat=Special, pow=0, eff=LevelDamage}
  , MoveDesc "Night Slash" 15 tackle
      {ty=DAR, pow=70, crit=1, flags=CONTACT .|. SLICE}
  , MoveDesc "Nightmare" 15 tackle
      {ty=GHO, cat=Status, eff=Nightmare, acc=neverMiss}
  , MoveDesc "No Retreat" 5 celebrate
      {ty=FIG, eff=NoSwitch :+ omniboost}
  , MoveDesc "Noble Roar" 30 celebrate
      {ty=NOR, targ=ADJACENT, eff=AddBoost False zero {att= -1, spA= -1}, flags=SOUND .|. MCOAT}
  , MoveDesc "Noxious Torque" 10 tackle
      {ty=POI, pow=100}
  , MoveDesc "Nuzzle" 20 tackle
      {ty=ELE, pow=20, eff=EStatus Paralysis}

  , MoveDesc "Oblivion Wing" 10 tackle
      {ty=FLY, cat=Special, pow=80, eff=Drain 0.75}
  , MoveDesc "Obstruct" 10 celebrate
      {ty=DAR, eff=Protect :+ IfUserHitByContactMove do AddBoost False zero {def= -1}, pri=4}
  , MoveDesc "Oceanic Operetta" 1 tackle
      {ty=WAT, flags=ZMOVE, cat=Special, pow=195, acc=neverMiss}
  , MoveDesc "Octazooka" 10 tackle
      {ty=WAT, cat=Special, pow=65, acc=85, eff=50 :% AddBoost False zero {acc= -1}, flags=BULLET}
  , MoveDesc "Octolock" 15 celebrate
      {ty=FIG, targ=ADJACENT, eff=Octolock}
  , MoveDesc "Odor Sleuth" 40 celebrate
      {ty=NOR, targ=ADJACENT, eff=OdorSleuth, acc=neverMiss, flags=MCOAT}
  , MoveDesc "Ominous Wind" 5 tackle
      {ty=GHO, cat=Special, pow=60, eff=10 :% omniboost}
  , MoveDesc "Order Up" 10 tackle
      {ty=DRA, pow=80}
  , MoveDesc "Origin Pulse" 10 tackle
      {ty=WAT, cat=Special, pow=110, acc=85, targ=ADJFOES .|. WIDE, flags=PULSE}
  , MoveDesc "Outrage" 10 tackle
      {ty=DRA, pow=120, eff=Locked, flags=CONTACT}
  , MoveDesc "Overdrive" 10 tackle
      {ty=ELE, cat=Special, pow=80, targ=ADJFOES .|. WIDE, flags=SOUND}
  , MoveDesc "Overheat" 5 tackle
      {ty=FIR, cat=Special, pow=130, acc=90, eff=AddBoost True zero {spA= -2}}

  , MoveDesc "Pain Split" 20 celebrate
      {ty=NOR, targ=ADJACENT, eff=PainSplit, acc=neverMiss}
  , MoveDesc "Parabolic Charge" 20 tackle
      {ty=ELE, cat=Special, pow=65, eff=Drain 0.5}
  , MoveDesc "Parting Shot" 20 celebrate
      {ty=DAR, targ=ADJACENT, eff=AddBoost False zero {att= -2, spA= -2} :+ Switch True False False, flags=SOUND .|. IGNSUB .|. MCOAT}
  , MoveDesc "Pay Day" 20 tackle
      {ty=NOR, pow=40, eff=PayDay}
  , MoveDesc "Payback" 10 tackle
      {ty=DAR, pow=50, eff=DoublePwrIfUserAttacked, flags=CONTACT}
  , MoveDesc "Peck" 35 tackle
      {ty=FLY, pow=35, flags=CONTACT}
  , MoveDesc "Perish Song" 5 celebrate
      {ty=NOR, targ=ALL .|. WIDE, eff=PerishSong, flags=SOUND .|. IGNSUB}
  , MoveDesc "Petal Blizzard" 15 tackle
      {ty=GRA, targ=ADJACENT .|. WIDE, pow=90, flags=WIND}
  , MoveDesc "Petal Dance" 10 tackle
      {ty=GRA, cat=Special, pow=120, flags=DANCE .|. CONTACT, eff=Locked}
  , MoveDesc "Phantom Force" 10 tackle
      {pow=90, ty=GHO, eff=EInvul Phantom :+ IgnoreProtect, flags=CONTACT, acc=neverMiss}
  , MoveDesc "Photon Geyser" 5 tackle
      {ty=PSY, cat=Special, pow=100, eff=UseHighestOfAttSpA}
  , MoveDesc "Pika Papow" 20 tackle
      {ty=ELE, cat=Special, pow=20, acc=neverMiss, eff=PwrHighBond}
  , MoveDesc "Pin Missile" 20 tackle
      {ty=BUG, pow=25, hits=(2,5), acc=95}
  , MoveDesc "Plasma Fists" 15 tackle
      {ty=ELE, pow=100, eff=IonDeluge, flags=CONTACT .|. PUNCH}
  , MoveDesc "Play Nice" 20 celebrate
      {ty=NOR, acc=neverMiss, targ=ADJACENT, eff=AddBoost False zero {att= -1}, flags=IGNSUB .|. MCOAT}
  , MoveDesc "Play Rough" 10 tackle
      {ty=FAI, acc=90, pow=90, eff=10 :% AddBoost False zero {att= -1}, flags=CONTACT}
  , MoveDesc "Pluck" 20 tackle
      {ty=FLY, pow=60, eff=EatBerry True, flags=CONTACT}
  , MoveDesc "Poison Fang" 15 tackle
      {ty=POI, pow=50, eff=50 :% EStatus Toxic, flags=CONTACT .|. BITE}
  , MoveDesc "Poison Gas" 40 celebrate
      {ty=POI, targ=ADJACENT, acc=90, eff=EStatus Poison, flags=MCOAT}
  , MoveDesc "Poison Jab" 20 tackle
      {ty=POI, pow=80, eff=30 :% EStatus Poison, flags=CONTACT}
  , MoveDesc "Poison Powder" 35 celebrate
      {ty=POI, targ=ADJACENT, acc=75, eff=EStatus Poison, flags=POWDER .|. MCOAT}
  , MoveDesc "Poison Sting" 35 tackle
      {ty=POI, pow=15, eff=30 :% EStatus Poison}
  , MoveDesc "Poison Tail" 25 tackle
      {ty=POI, pow=50, crit=1, eff=10 :% EStatus Poison, flags=CONTACT}
  , MoveDesc "Pollen Puff" 15 tackle
      {ty=BUG, cat=Special, pow=90, eff=PollenPuff, flags=BULLET}
  , MoveDesc "Poltergeist" 5 tackle
      {ty=GHO, pow=110, acc=90, eff=Poltergeist}
  , MoveDesc "Population Bomb" 10 tackle
      {ty=NOR, pow=20, acc=90, hits=(1,10), flags=CONTACT .|. SLICE}
  , MoveDesc "Pounce" 20 tackle
      {ty=BUG, pow=50, eff=AddBoost False zero {spe= -1}, flags=CONTACT}
  , MoveDesc "Pound" 35 tackle
      {ty=NOR, pow=40, flags=CONTACT}
  , MoveDesc "Powder" 20 celebrate
      {ty=BUG, targ=ALL .|. WIDE, eff=Powder, pri=1, flags=IGNSUB .|. POWDER .|. MCOAT}
  , MoveDesc "Powder Snow" 25 tackle
      {ty=ICE, cat=Special, pow=40, eff=10 :% EStatus Freeze}
  , MoveDesc "Power Gem" 20 tackle
      {ty=ROC, cat=Special, pow=80}
  , MoveDesc "Power Shift" 10 celebrate
      {ty=NOR, eff=SwapOffDef}
  , MoveDesc "Power Split" 10 celebrate
      {ty=PSY, targ=ADJACENT, eff=AvgAtt, acc=neverMiss}
  , MoveDesc "Power Swap" 10 celebrate
      {ty=PSY, targ=ADJACENT, eff=SwpAtt, flags=IGNSUB, acc=neverMiss}
  , MoveDesc "Power Trick" 10 celebrate
      {ty=PSY, eff=SwapAttDef, flags=SNATCH}
  , MoveDesc "Power Trip" 10 tackle
      {ty=DAR, pow=20, eff=AddPwr, flags=CONTACT}
  , MoveDesc "Power Whip" 10 tackle
      {ty=GRA, pow=120, acc=85, flags=CONTACT}
  , MoveDesc "Power-Up Punch" 20 tackle
      {ty=FIG, pow=40, eff=AddBoost True zero{att=1}, flags=CONTACT .|. PUNCH}
  , MoveDesc "Precipice Blades" 10 tackle
      {ty=GRO, pow=120, acc=85, targ=ADJACENT .|. WIDE}
  , MoveDesc "Present" 15 tackle
      {ty=NOR, pow=0, acc=90, eff=Present}
  , MoveDesc "Prismatic Laser" 10 tackle
      {ty=PSY, cat=Special, pow=160, eff=Recharge}
  , MoveDesc "Protect" 10 celebrate
      {ty=NOR, eff=Protect, pri=4}
  , MoveDesc "Psybeam" 20 tackle
      {ty=PSY, cat=Special, pow=65, eff=10 :% Confuse}
  , MoveDesc "Psyblade" 15 tackle
      {ty=PSY, pow=80, eff=PwrInTerrain TPsychic, flags=SLICE}
  , MoveDesc "Psych Up" 10 celebrate -- TODO: How does this work with snatch?
      {ty=NOR, targ=ADJACENT, eff=CopyBoost, flags=IGNSUB .|. SNATCH, acc=neverMiss}
  , MoveDesc "Psychic" 10 tackle
      {ty=PSY, cat=Special, pow=90, eff=10 :% AddBoost False zero{spD= -1}}
  , MoveDesc "Psychic Fangs" 10 tackle
      {ty=PSY, pow=85, eff=ClearScreen, flags=CONTACT .|. BITE}
  , MoveDesc "Psychic Terrain" 10 celebrate
      {ty=PSY, eff=ETerrain TPsychic}
  , MoveDesc "Psycho Boost" 5 tackle
      {ty=PSY, cat=Special, pow=140, acc=90, eff=AddBoost True zero {spA= -2}}
  , MoveDesc "Psycho Cut" 20 tackle
      {ty=PSY, pow=70, crit=1, flags=SLICE}
  , MoveDesc "Psycho Shift" 10 celebrate
      {ty=PSY, targ=ADJACENT, eff=MoveStatus}
  , MoveDesc "Psyshield Bash" 10 tackle
      {ty=PSY, pow=70, acc=90, eff=AddBoost True zero {def=1, spD=1}, flags=CONTACT}
  , MoveDesc "Psyshock" 10 tackle
      {ty=PSY, cat=Special, pow=80, eff=UseDef}
  , MoveDesc "Psystrike" 10 tackle
      {ty=PSY, cat=Special, pow=100, eff=UseDef}
  , MoveDesc "Psywave" 15 tackle
      {ty=PSY, cat=Special, pow=0, eff=Psywave}
  , MoveDesc "Pulverizing Pancake" 1 tackle
      {ty=NOR, pow=210, flags=ZMOVE .|. CONTACT, acc=neverMiss}
  , MoveDesc "Punishment" 5 tackle
      {ty=DAR, pow=0, eff=Punishment, flags=CONTACT}
  , MoveDesc "Purify" 20 celebrate
      {ty=POI, targ=ADJACENT, eff=Purify, flags=MCOAT}
  , MoveDesc "Pursuit" 20 tackle
      {ty=DAR, pow=40, eff=Pursuit, flags=CONTACT, acc=neverMiss}
  , MoveDesc "Pyro Ball" 5 tackle
      {ty=FIR, pow=120, acc=90, eff=10 :% EStatus Burn, flags=BULLET}

  , MoveDesc "Quash" 15 celebrate
      {ty=DAR, targ=ADJACENT, eff=Quash}
  , MoveDesc "Quick Attack" 30 tackle
      {ty=NOR, pow=40, pri=1, flags=CONTACT}
  , MoveDesc "Quick Guard" 15 celebrate
      {ty=FIG, eff=QuickGuard, pri=3, flags=SNATCH}
  , MoveDesc "Quiver Dance" 20 celebrate
      {ty=BUG, flags=DANCE .|. SNATCH, eff=AddBoost True zero {spe=1, spA=1, spD=1}}

  , MoveDesc "Rage" 20 tackle
      {ty=NOR, pow=20, eff=Rage, flags=CONTACT}
  , MoveDesc "Rage Fist" 10 tackle
      {ty=GHO, pow=50, eff=RageFist, flags=CONTACT .|. PUNCH}
  , MoveDesc "Rage Powder" 20 celebrate
      {ty=BUG, eff=FollowMe, flags=POWDER, pri=2} -- TODO: Make sure safety-googles ignores this!
  , MoveDesc "Raging Bull" 10 tackle -- TODO: Does this ignore protect?
      {ty=NOR, pow=90, eff=IgnoreProtect :+ ClearScreen :+ RagingBull, flags=CONTACT}
  , MoveDesc "Raging Fury" 10 tackle
      {ty=FIR, pow=120, eff=RagingFury}
  , MoveDesc "Rain Dance" 5 celebrate
      {ty=WAT, eff=EWeather Rain}
  , MoveDesc "Rapid Spin" 40 tackle
      {ty=NOR, pow=50, eff=ClearHazard, flags=CONTACT} -- TODO: Clear the user's hazards
  , MoveDesc "Razor Leaf" 25 tackle
      {ty=GRA, pow=55, acc=95, crit=1, flags=SLICE}
  , MoveDesc "Razor Shell" 10 tackle
      {ty=WAT, pow=75, acc=95, eff=50 :% AddBoost False zero{def= -1}, flags=CONTACT .|. SLICE}
  , MoveDesc "Razor Wind" 10 tackle
      {ty=NOR, cat=Special, pow=80, eff=Precharge, crit=1}
  , MoveDesc "Recover" 10 celebrate
      {ty=NOR, eff=Recover 0.5, flags=SNATCH}
  , MoveDesc "Recycle" 10 celebrate
      {ty=NOR, eff=Recycle, flags=SNATCH}
  , MoveDesc "Reflect" 20 celebrate
      {ty=PSY, eff=EScreen Phy}
  , MoveDesc "Reflect Type" 15 celebrate
      {ty=NOR, targ=ADJACENT, eff=CopyType, flags=IGNSUB, acc=neverMiss}
  , MoveDesc "Refresh" 20 celebrate
      {ty=NOR, eff=Refresh, flags=SNATCH}
  , MoveDesc "Relic Song" 10 tackle
      {ty=NOR, cat=Special, pow=75, targ=ADJFOES .|. WIDE, eff=10 :% EStatus Sleep, flags=SOUND .|. IGNSUB}
  , MoveDesc "Rest" 10 celebrate
      {ty=PSY, eff=Recover 1.0 :+ SleepFor2Turns, flags=SNATCH}
  , MoveDesc "Retaliate" 5 tackle
      {ty=NOR, pow=70, eff=DoubleDmgIfAllyFaintedLastTurn}
  , MoveDesc "Return" 20 tackle
      {ty=NOR, pow=0, eff=PwrHighFriendship, flags=CONTACT}
  , MoveDesc "Revelation Dance" 15 tackle
      {ty=NOR, cat=Special, pow=90, flags=DANCE, eff=UserPrimary}
  , MoveDesc "Revenge" 10 tackle
      {ty=FIG, pow=60, eff=DoublePwrIfHit, pri= -4, flags=CONTACT} -- TODO: Power "increases" if user was hit first
  , MoveDesc "Reversal" 15 tackle
      {ty=FIG, pow=0, eff=RaisePwrUserHP, flags=CONTACT}
  , MoveDesc "Revival Blessing" 1 celebrate
      {ty=NOR, eff=ReviveAllyToHalfHP}
  , MoveDesc "Rising Voltage" 20 tackle
      {ty=ELE, cat=Special, pow=70, eff=DoublePwrInTerrain TElectric}
  , MoveDesc "Roar" 20 celebrate
      {ty=NOR, targ=ADJACENT, eff=Switch False True False, pri= -6, flags=SOUND .|. IGNSUB .|. MCOAT, acc=neverMiss}
  , MoveDesc "Roar of Time" 5 tackle
      {ty=DRA, cat=Special, pow=150, acc=90, eff=Recharge}
  , MoveDesc "Rock Blast" 10 tackle
      {ty=ROC, pow=25, acc=90, hits=(2,5), flags=BULLET}
  , MoveDesc "Rock Climb" 20 tackle
      {ty=NOR, pow=90, acc=85, eff=20 :% Confuse, flags=CONTACT}
  , MoveDesc "Rock Polish" 20 celebrate
      {ty=ROC, eff=AddBoost True zero{spe=2}, flags=SNATCH}
  , MoveDesc "Rock Slide" 10 tackle
      {ty=ROC, pow=75, acc=90, targ=ADJFOES .|. WIDE, eff=30 :% Flinch}
  , MoveDesc "Rock Smash" 15 tackle
      {ty=FIG, pow=40, eff=50 :% AddBoost False zero {def= -1}, flags=CONTACT}
  , MoveDesc "Rock Throw" 15 tackle
      {ty=ROC, pow=50, acc=90}
  , MoveDesc "Rock Tomb" 15 tackle
      {ty=ROC, pow=60, acc=95, eff=AddBoost False zero {spe= -1}}
  , MoveDesc "Rock Wrecker" 5 tackle
      {ty=ROC, pow=150, acc=90, eff=Recharge, flags=BULLET}
  , MoveDesc "Role Play" 10 celebrate
      {ty=PSY, eff=CopyAbility Target2User, flags=IGNSUB, acc=neverMiss}
  , MoveDesc "Rolling Kick" 15 tackle
      {ty=FIG, pow=60, acc=85, eff=30 :% Flinch, flags=CONTACT}
  , MoveDesc "Rollout" 20 tackle
      {ty=ROC, pow=30, acc=90, eff=Scaling5Turns :+ DoubleDmgIfDefenceCurlUsed, flags=CONTACT}
  , MoveDesc "Roost" 10 celebrate
      {ty=FLY, eff=GroundFor1Turn :+ Recover 0.5, flags=SNATCH}
  , MoveDesc "Rototiller" 10 celebrate
      {ty=GRO, eff=Rototiller, targ=ALL .|. WIDE}
  , MoveDesc "Round" 15 tackle
      {ty=NOR, cat=Special, pow=60, eff=Round, flags=SOUND .|. IGNSUB}
  , MoveDesc "Ruination" 10 tackle
      {ty=DAR, cat=Special, pow=1, acc=90, eff=HalfHP}

  , MoveDesc "Sacred Fire" 5 tackle
      {ty=FIR, pow=100, acc=95, eff=50 :% EStatus Burn}
  , MoveDesc "Sacred Sword" 15 tackle
      {ty=FIG, pow=90, eff=IgnoreBoosts, flags=CONTACT .|. SLICE}
  , MoveDesc "Safeguard" 25 celebrate
      {ty=NOR, eff=Safeguard, flags=SNATCH}
  , MoveDesc "Salt Cure" 15 tackle
      {ty=ROC, pow=40, eff=SaltCure}
  , MoveDesc "Sand Attack" 15 celebrate
      {ty=GRO, targ=ADJACENT, eff=AddBoost False zero {acc= -1}, flags=MCOAT}
  , MoveDesc "Sand Tomb" 15 tackle
      {ty=GRO, pow=35, acc=85, eff=ELocking SandTomb}
  , MoveDesc "Sandsear Storm" 10 tackle
      {ty=GRO, cat=Special, pow=100, acc=80, eff=EStatus Burn, flags=WIND}
  , MoveDesc "Sandstorm" 10 celebrate
      {ty=ROC, eff=EWeather Sandstorm, flags=WIND}
  , MoveDesc "Sappy Seed" 10 tackle
      {ty=GRA, pow=100, eff=LeechSeed, acc=neverMiss} -- TODO: Verify that this does not stack with leech seed
  -- savage spin-out
  , MoveDesc "Scald" 15 tackle
      {ty=WAT, cat=Special, pow=80, eff=30 :% EStatus Burn}
  , MoveDesc "Scale Shot" 20 tackle
      {ty=DRA, pow=25, acc=90, hits=(2,5), eff=AddBoost True zero{spe=1, def= -1}}
  , MoveDesc "Scary Face" 10 celebrate
      {ty=NOR, targ=ADJACENT, eff=AddBoost False zero{spe= -2}, flags=MCOAT}
  , MoveDesc "Scorching Sands" 10 tackle
      {ty=GRO, cat=Special, pow=70, eff=EStatus Burn} -- TODO: "May" burn target (but 100%)
  , MoveDesc "Scratch" 35 tackle
      {ty=NOR, pow=40, flags=CONTACT}
  , MoveDesc "Screech" 40 celebrate
      {ty=NOR, acc=85, targ=ADJACENT, eff=AddBoost False zero{def= -2}, flags=SOUND .|. IGNSUB .|. MCOAT}
  , MoveDesc "Searing Shot" 5 tackle
      {ty=FIR, cat=Special, pow=100, eff=30 :% EStatus Burn, flags=BULLET}
  , MoveDesc "Searing Sunraze Smash" 1 tackle
      {ty=STE, pow=200, flags=ZMOVE .|. CONTACT, acc=neverMiss}
  , MoveDesc "Secret Power" 20 tackle
      {ty=NOR, pow=70, eff=SecretPower}
  , MoveDesc "Secret Sword" 10 tackle
      {ty=FIG, cat=Special, pow=85, eff=UseDef}
  , MoveDesc "Seed Bomb" 15 tackle
      {ty=GRA, pow=80, flags=BULLET}
  , MoveDesc "Seed Flare" 5 tackle
      {ty=GRA, cat=Special, pow=120, acc=85, eff=40 :% AddBoost False zero{spD= -1}}
  , MoveDesc "Seismic Toss" 20 tackle
      {ty=FIG, pow=0, eff=LevelDamage, flags=CONTACT}
  , MoveDesc "Self-Destruct" 5 tackle
      {ty=NOR, pow=200, eff=UserDies, targ=ADJACENT .|. WIDE, flags=EXPLODE}
  , MoveDesc "Shadow Ball" 15 tackle
      {ty=GHO, cat=Special, pow=80, eff=20 :% AddBoost False zero{spD= -1}, flags=BULLET}
  , MoveDesc "Shadow Bone" 10 tackle
      {ty=GHO, pow=85, eff=20 :% AddBoost False zero{def= -1}}
  , MoveDesc "Shadow Claw" 15 tackle
      {ty=GHO, pow=70, crit=1, flags=CONTACT}
  , MoveDesc "Shadow Force" 5 tackle
      {ty=GHO, pow=120, eff=EInvul Phantom :+ IgnoreProtect, flags=CONTACT, acc=neverMiss}
  , MoveDesc "Shadow Punch" 20 tackle
      {ty=GHO, pow=60, acc=neverMiss, flags=CONTACT .|. PUNCH}
  , MoveDesc "Shadow Sneak" 30 tackle
      {ty=GHO, pow=40, pri=1, flags=CONTACT}
  , MoveDesc "Sharpen" 30 celebrate
      {ty=NOR, eff=AddBoost True zero {att=1}, flags=SNATCH}
  -- Shattered Psyche
  , MoveDesc "Shed Tail" 10 celebrate
      {ty=NOR, eff=ShedTail}
  , MoveDesc "Sheer Cold" 5 tackle
      {ty=ICE, cat=Special, pow=0, acc=30, eff=OHKO}
  , MoveDesc "Shell Side Arm" 10 tackle
      {ty=POI, cat=Special, pow=90, eff=ShellSideArm}
  , MoveDesc "Shell Smash" 15 celebrate
      {ty=NOR, eff=AddBoost True zero {att=2,spA=2,spe=2,def= -1,spD= -1}, flags=SNATCH}
  , MoveDesc "Shell Trap" 5 tackle
      {ty=FIR, cat=Special, pow=150, eff=ShellTrap, pri= -3}
  , MoveDesc "Shelter" 10 celebrate
      {ty=STE, eff=AddBoost True zero {def=1, eva=1}}
  , MoveDesc "Shift Gear" 10 celebrate
      {ty=STE, eff=AddBoost True zero {att=1, spe=2}, flags=SNATCH}
  , MoveDesc "Shock Wave" 20 tackle
      {ty=ELE, cat=Special, pow=60, acc=neverMiss}
  , MoveDesc "Shore Up" 10 celebrate
      {ty=GRO, eff=ShoreUp, flags=SNATCH}
  , MoveDesc "Signal Beam" 15 tackle
      {ty=BUG, cat=Special, pow=75, eff=10 :% Confuse}
  , MoveDesc "Silk Trap" 10 celebrate
      {ty=BUG, eff=Protect :+ IfUserHitByContactMove do AddBoost True zero {spe=1}, pri=4}
  , MoveDesc "Silver Wind" 5 tackle
      {ty=BUG, cat=Special, pow=60, eff=10 :% omniboost}
  , MoveDesc "Simple Beam" 15 celebrate
      {ty=NOR, targ=ADJACENT, eff=SetAbility simpleID, flags=MCOAT}
  , MoveDesc "Sing" 15 celebrate
      {ty=NOR, targ=ADJACENT, acc=55, eff=EStatus Sleep, flags=SOUND .|. IGNSUB .|. MCOAT}
  , MoveDesc "Sinister Arrow Raid" 1 tackle
      {ty=GHO, pow=180, flags=ZMOVE, acc=neverMiss}
  , MoveDesc "Sizzly Slide" 20 tackle
      {ty=FIR, pow=60, eff=EStatus Burn, flags=CONTACT, acc=neverMiss}
  , MoveDesc "Sketch" 1 celebrate
      {ty=NOR, targ=ADJACENT, eff=Sketch, flags=IGNSUB, acc=neverMiss}
  , MoveDesc "Skill Swap" 10 celebrate
      {ty=PSY, targ=ADJACENT, eff=SwapAbility, flags=IGNSUB, acc=neverMiss}
  , MoveDesc "Skitter Smack" 10 tackle
      {ty=BUG, pow=70, acc=90, eff=AddBoost False zero {spA= -1}, flags=CONTACT}
  , MoveDesc "Skull Bash" 10 tackle -- TODO: boost on first turn, attack on second
      {ty=NOR, pow=130, eff=AddBoost True zero{def=1} :+ Precharge, flags=CONTACT}
  , MoveDesc "Sky Attack" 5 tackle -- TODO: Flinch on second turn if it hits
      {ty=FLY, pow=140, acc=90, crit=1, eff=Precharge :+ 30 :% Flinch}
  , MoveDesc "Sky Drop" 10 tackle
      {ty=FLY, pow=60, eff=SkyDrop, flags=CONTACT}
  , MoveDesc "Sky Uppercut" 15 tackle
      {ty=FIG, pow=85, acc=90, eff=HitInvul Flying, flags=CONTACT .|. PUNCH}
  , MoveDesc "Slack Off" 10 celebrate
      {ty=NOR, eff=Recover 0.5, flags=SNATCH}
  , MoveDesc "Slam" 20 tackle
      {ty=NOR, pow=80, acc=75, flags=CONTACT}
  , MoveDesc "Slash" 20 tackle
      {ty=NOR, pow=70, crit=1, flags=CONTACT .|. SLICE}
  , MoveDesc "Sleep Powder" 15 celebrate
      {ty=GRA, acc=75, targ=ADJACENT, flags=POWDER .|. MCOAT, eff=EStatus Sleep}
  , MoveDesc "Sleep Talk" 10 celebrate
      {ty=NOR, eff=FailIfNotAsleep :+ SleepTalk}
  , MoveDesc "Sludge" 20 tackle
      {ty=POI, cat=Special, pow=65, eff=30 :% EStatus Poison}
  , MoveDesc "Sludge Bomb" 10 tackle
      {ty=POI, cat=Special, pow=90, eff=30 :% EStatus Poison, flags=BULLET}
  , MoveDesc "Sludge Wave" 10 tackle
      {ty=POI, cat=Special, pow=95, eff=10 :% EStatus Poison, targ=ADJACENT .|. WIDE}
  , MoveDesc "Smack Down" 15 tackle
      {ty=ROC, pow=50, eff=GroundFlying}
  , MoveDesc "Smart Strike" 10 tackle
      {ty=STE, pow=70, acc=neverMiss, flags=CONTACT}
  , MoveDesc "Smelling Salts" 10 tackle
      {ty=NOR, pow=70, eff=SmellingSalts, flags=CONTACT}
  , MoveDesc "Smog" 20 tackle
      {ty=POI, cat=Special, pow=30, acc=70, eff=40 :% EStatus Poison}
  , MoveDesc "Smokescreen" 20 celebrate
      {ty=NOR, targ=ADJACENT, eff=AddBoost False zero {acc= -1}, flags=MCOAT}
  , MoveDesc "Snap Trap" 15 tackle
      {ty=GRA, pow=35, eff=ELocking SnapTrap, flags=CONTACT}
  , MoveDesc "Snarl" 15 tackle
      {ty=DAR, cat=Special, pow=55, acc=95, targ=ADJFOES .|. WIDE, eff=AddBoost False zero{spA= -1}, flags=SOUND .|. IGNSUB}
  , MoveDesc "Snatch" 10 celebrate
      {ty=DAR, eff=Snatch, pri=4}
  , MoveDesc "Snipe Shot" 15 tackle
      {ty=WAT, cat=Special, pow=80, crit=1, eff=IgnoreFollowMe}
  , MoveDesc "Snore" 15 tackle
      {ty=NOR, cat=Special, pow=50, eff=FailIfNotAsleep :+ 30 :% Flinch, flags=SOUND .|. IGNSUB}
  , MoveDesc "Snowscape" 10 celebrate
      {ty=ICE, eff=Snowscape}
  , MoveDesc "Soak" 20 celebrate
      {ty=WAT, targ=ADJACENT, eff=SetType WAT False, flags=MCOAT}
  , MoveDesc "Soft-Boiled" 10 celebrate
      {ty=NOR, eff=Recover 0.5, flags=SNATCH}
  , MoveDesc "Solar Beam" 10 tackle
      {ty=GRA, cat=Special, pow=120, eff=ChargeIfNotSun}
  , MoveDesc "Solar Blade" 10 tackle
      {ty=GRA, pow=125, eff=ChargeIfNotSun, flags=CONTACT .|. SLICE}
  , MoveDesc "Sonic Boom" 20 tackle
      {ty=NOR, cat=Special, pow=0, acc=90, eff=ConstantDamage 20}
  , MoveDesc "Soul-Stealing 7-Star Strike" 1 tackle
      {ty=GHO, pow=195, flags=ZMOVE .|. CONTACT, acc=neverMiss}
  , MoveDesc "Spacial Rend" 5 tackle
      {ty=DRA, cat=Special, pow=100, acc=95, crit=1}
  , MoveDesc "Spark" 20 tackle
      {ty=ELE, pow=65, eff=20 :% EStatus Paralysis, flags=CONTACT}
  , MoveDesc "Sparkling Aria" 10 tackle
      {ty=WAT, cat=Special, pow=90, eff=HealBurn, flags=SOUND .|. IGNSUB}
  , MoveDesc "Sparkly Swirl" 5 tackle
      {ty=FAI, cat=Special, pow=120, eff=ClearStatusParty, acc=neverMiss}
  , MoveDesc "Spectral Thief" 10 tackle
      {ty=GHO, pow=90, eff=StealStatBoosts, flags=CONTACT .|. IGNSUB}
  , MoveDesc "Speed Swap" 10 celebrate
      {ty=PSY, targ=ADJACENT, eff=SwpSpe, flags=IGNSUB, acc=neverMiss}
  , MoveDesc "Spicy Extract" 15 celebrate
      {ty=GRA, targ=ADJACENT, eff=AddBoost False zero{def= -3, att=2}, acc=neverMiss}
  , MoveDesc "Spider Web" 10 celebrate
      {ty=BUG, targ=ADJACENT, eff=NoSwitch, acc=neverMiss, flags=MCOAT}
  , MoveDesc "Spike Cannon" 15 tackle
      {ty=NOR, pow=20, hits=(2,5)}
  , MoveDesc "Spikes" 20 celebrate
      {ty=GRO, targ=FOES .|. WIDE, eff=EHazard Spikes, flags=MCOAT}
  , MoveDesc "Spiky Shield" 10 celebrate
      {ty=GRA, pri=4, eff=Protect :+ IfUserHitByContactMove do
        FractionalDamageMax (1/8) }
  , MoveDesc "Spin Out" 5 tackle
      {ty=STE, pow=100, eff=AddBoost True zero {spe= -2}, flags=CONTACT}
  , MoveDesc "Spirit Break" 15 tackle
      {ty=FAI, pow=75, eff=AddBoost False zero {spA= -1}, flags=CONTACT}
  , MoveDesc "Spirit Shackle" 10 tackle
      {ty=GHO, pow=80, eff=NoSwitch}
  , MoveDesc "Spit Up" 10 tackle
      {ty=NOR, cat=Special, pow=0, eff=SpitUp}
  , MoveDesc "Spit Up" 10 tackle
      {ty=NOR, cat=Special, pow=0, eff=SpitUp}
  , MoveDesc "Spite" 10 celebrate
      {ty=GHO, targ=ADJACENT, eff=Spite, flags=IGNSUB .|. MCOAT}
  , MoveDesc "Splash" 40 celebrate
      {ty=NOR}
  , MoveDesc "Splintered Stormshards" 1 tackle
      {ty=ROC, pow=190, flags=ZMOVE}
  , MoveDesc "Splishy Splash" 15 tackle
      {ty=WAT, cat=Special, pow=90, eff=30 :% EStatus Paralysis}
  , MoveDesc "Spore" 15 celebrate
      {ty=GRA, targ=ADJACENT, eff=EStatus Sleep, flags=POWDER .|. MCOAT}
  , MoveDesc "Spotlight" 15 celebrate
      {ty=NOR, targ=ADJACENT, eff=FollowMe, pri=3, flags=MCOAT}
  , MoveDesc "Springtide Storm" 5 tackle
      {ty=FAI, cat=Special, pow=100, acc=80, eff=SpringtideStorm, flags=WIND}
  , MoveDesc "Stealth Rock" 20 celebrate
      {ty=ROC, targ=FOES .|. WIDE, eff=EHazard Rocks, flags=MCOAT}
  , MoveDesc "Steam Eruption" 5 tackle
      {ty=WAT, cat=Special, pow=110, acc=95, eff=30 :% EStatus Burn}
  , MoveDesc "Steamroller" 20 tackle
      {ty=BUG, pow=65, eff=30 :% Flinch, flags=CONTACT, acc=neverMiss}
  , MoveDesc "Steel Beam" 5 tackle
      {ty=STE, cat=Special, pow=140, acc=95} -- TODO: User loses 50% of its HP
  , MoveDesc "Steel Roller" 5 tackle
      {ty=STE, pow=130, flags=CONTACT} -- TODO: Fail if no terrain
  , MoveDesc "Steel Wing" 25 tackle
      {ty=STE, pow=70, acc=90, eff=10 :% AddBoost True zero {def=1}, flags=CONTACT}
  , MoveDesc "Sticky Web" 20 celebrate
      {ty=BUG, targ=FOES .|. WIDE, eff=EHazard Web, flags=MCOAT}
  , MoveDesc "Stockpile" 20 celebrate
      {ty=NOR, eff=Stockpile, flags=SNATCH}
  , MoveDesc "Stoked Sparksurfer" 1 tackle
      {ty=ELE, cat=Special, pow=175, flags=ZMOVE, acc=neverMiss}
  , MoveDesc "Stomp" 20 tackle
      {ty=NOR, pow=65, eff=30 :% Flinch, flags=CONTACT, acc=neverMiss}
  , MoveDesc "Stomping Tantrum" 10 tackle
      {ty=GRO, pow=75, eff=DoublePwrIfLastMoveFailed, flags=CONTACT}
  , MoveDesc "Stone Axe" 15 tackle
      {ty=ROC, pow=65, acc=90, crit=1, eff=DamageWithSplinters, flags=CONTACT .|. SLICE}
  , MoveDesc "Stone Edge" 5 tackle
      {ty=ROC, pow=100, acc=80, crit=1}
  , MoveDesc "Stored Power" 10 tackle
      {ty=PSY, cat=Special, pow=20, eff=AddPwr}
  , MoveDesc "Storm Throw" 10 tackle
      {ty=FIG, pow=60, crit=alwaysCrit, flags=CONTACT}
  , MoveDesc "Strange Steam" 10 tackle
      {ty=FAI, cat=Special, pow=90, acc=95, eff=20 :% Confuse}
  , MoveDesc "Strength" 15 tackle
      {ty=NOR, pow=80, flags=CONTACT}
  , MoveDesc "Strength Sap" 10 celebrate
      {ty=GRA, targ=ADJACENT, eff=StrengthSap :+ AddBoost False zero {att= -1}, flags=MCOAT}
  , MoveDesc "String Shot" 40 celebrate
      {ty=BUG, targ=ADJACENT, acc=95, eff=AddBoost False zero {spe= -2}, flags=MCOAT}
  , MoveDesc "Struggle" 1 tackle
      {ty=NON, pow=50, eff=Struggle, flags=CONTACT, acc=neverMiss}
  , MoveDesc "Struggle Bug" 20 tackle
      {ty=BUG, cat=Special, pow=50, eff=AddBoost False zero {spA= -1}}
  , MoveDesc "Stuff Cheeks" 10 celebrate
      {ty=NOR, eff=EatBerry True :+ AddBoost True zero {def=2}}
  , MoveDesc "Stun Spore" 30 celebrate
      {ty=GRA, targ=ADJACENT, acc=75, eff=EStatus Paralysis, flags=POWDER .|. MCOAT}
  , MoveDesc "Submission" 20 tackle
      {ty=FIG, pow=80, acc=80, eff=Recoil 0.25, flags=CONTACT}
  , MoveDesc "Substitute" 10 celebrate
      {ty=NOR, eff=Substitute, flags=SNATCH}
  -- subzero slammer
  , MoveDesc "Sucker Punch" 5 tackle
      {ty=DAR, pow=70, pri=1, eff=SuckerPunch, flags=CONTACT}
  , MoveDesc "Sunny Day" 5 celebrate
      {ty=FIR, eff=EWeather Sun}
  , MoveDesc "Sunsteel Strike" 5 tackle
      {ty=STE, pow=100, eff=IgnoreAbility, flags=CONTACT}
  , MoveDesc "Super Fang" 10 tackle
      {ty=NOR, pow=0, acc=90, eff=HalfHP, flags=CONTACT}
  , MoveDesc "Superpower" 5 tackle
      {ty=FIG, pow=120, eff=AddBoost False zero {att= -1, def= -1}, flags=CONTACT}
  , MoveDesc "Supersonic" 20 celebrate
      {ty=NOR, targ=ADJACENT, acc=55, eff=Confuse, flags=SOUND .|. IGNSUB .|. MCOAT}
  -- supersonic skystrike
  , MoveDesc "Surf" 15 tackle
      {ty=WAT, cat=Special, pow=90, targ=ADJACENT .|. WIDE}
  , MoveDesc "Surging Strikes" 5 tackle
      {ty=WAT, pow=25, crit=alwaysCrit, eff=IgnoreBoosts, flags=CONTACT .|. PUNCH}
  , MoveDesc "Swagger" 15 celebrate
      {ty=NOR, targ=ADJACENT, acc=85, eff=Confuse :+ AddBoost False zero {att=2}, flags=MCOAT}
  , MoveDesc "Swallow" 10 celebrate
      {ty=NOR, eff=Swallow, flags=SNATCH}
  , MoveDesc "Sweet Kiss" 10 celebrate
      {ty=FAI, acc=75, eff=Confuse, targ=ADJACENT, flags=MCOAT}
  , MoveDesc "Sweet Scent" 20 celebrate
      {ty=NOR, targ=ADJACENT, eff=AddBoost False zero {eva= -1}, flags=MCOAT}
  , MoveDesc "Swift" 20 tackle
      {ty=NOR, cat=Special, pow=60, acc=neverMiss, targ=ADJFOES .|. WIDE}
  , MoveDesc "Switcheroo" 10 celebrate
      {ty=DAR, targ=ADJACENT, eff=SwapItem}
  , MoveDesc "Swords Dance" 20 celebrate
      {ty=NOR, flags=DANCE .|. SNATCH, eff=AddBoost True zero {att=2}}
  , MoveDesc "Synchronoise" 10 tackle
      {ty=PSY, cat=Special, pow=120, eff=Synchronoise, targ=ADJACENT .|. WIDE}
  , MoveDesc "Synthesis" 5 celebrate
      {ty=GRA, eff=RecoverWeather, flags=SNATCH}

  , MoveDesc "Tackle" 35 tackle
      {flags=CONTACT}
  , MoveDesc "Tail Glow" 20 celebrate
      {ty=BUG, eff=AddBoost True zero {spA=3}, flags=SNATCH}
  , MoveDesc "Tail Slap" 10 tackle
      {ty=NOR, pow=25, acc=85, hits=(2,5), flags=CONTACT}
  , MoveDesc "Tail Whip" 30 celebrate
      {ty=NOR, targ=ADJACENT, eff=AddBoost False zero {def= -1}, flags=MCOAT}
  , MoveDesc "Tailwind" 15 celebrate
      {ty=FLY, targ=ALLIES .|. WIDE, eff=Tailwind, flags=WIND .|. SNATCH}
  , MoveDesc "Take Down" 20 tackle
      {ty=NOR, pow=90, acc=85, eff=Recoil 0.25, flags=CONTACT}
  , MoveDesc "Take Heart" 10 celebrate -- TODO: Verify the boost
      {ty=PSY, eff=ClearStatus :+ AddBoost True zero {att=1,spA=1,def=1,spD=1}}
  , MoveDesc "Tar Shot" 15 celebrate
      {ty=ROC, targ=ADJACENT, eff=TarShot :+ AddBoost False zero{spe= -1}, flags=MCOAT}
  , MoveDesc "Taunt" 20 celebrate
      {ty=DAR, targ=ADJACENT, eff=Taunt, flags=IGNSUB .|. MCOAT}
  , MoveDesc "Tearful Look" 20 celebrate
      {ty=NOR, targ=ADJACENT, eff=AddBoost False zero {att= -1, spA= -1}, flags=MCOAT}
  , MoveDesc "Teatime" 10 celebrate
      {ty=NOR, targ=ALL .|. WIDE, eff=EatBerry False}
  , MoveDesc "Techno Blast" 5 tackle
      {ty=NOR, cat=Special, pow=120, eff=TechnoBlast}
  -- tectonic rage
  , MoveDesc "Teeter Dance" 20 celebrate
      {ty=NOR, flags=DANCE, targ=ADJACENT .|. WIDE, eff=Confuse}
  , MoveDesc "Telekinesis" 15 celebrate
      {ty=PSY, targ=ADJACENT, eff=Telekinesis, acc=neverMiss, flags=MCOAT}
  , MoveDesc "Teleport" 20 celebrate
      {ty=PSY, eff=Switch True False False, pri= -6}
  , MoveDesc "Tera Blast" 10 tackle
      {ty=NOR, cat=Special, pow=80, eff=TeraBlast}
  , MoveDesc "Terrain Pulse" 10 tackle
      {ty=NOR, cat=Special, pow=50, eff=TerrainPulse, flags=PULSE}
  , MoveDesc "Thief" 25 tackle
      {ty=DAR, pow=60, eff=StealItem, flags=CONTACT}
  , MoveDesc "Thousand Arrows" 10 tackle -- TODO: Make sure this can hit flying-types
      {ty=GRO, pow=90, targ=ADJFOES .|. WIDE, eff=GroundFlying}
  , MoveDesc "Thousand Waves" 10 tackle
      {ty=GRO, pow=90, targ=ADJFOES .|. WIDE, eff=NoSwitch}
  , MoveDesc "Thrash" 10 tackle
      {ty=NOR, pow=120, eff=Locked, flags=CONTACT}
  , MoveDesc "Throat Chop" 15 tackle
      {ty=DAR, pow=80, eff=TroatChop, flags=CONTACT}
  , MoveDesc "Thunder" 10 tackle
      { ty=ELE, cat=Special, pow=110, acc=70
      , eff = 30 :% EStatus Paralysis
           :+ DoublePowerIfInvul       Flying
           :+ PerfectAccuracyInWeather Rain
      }
  , MoveDesc "Thunder Cage" 15 tackle
      {ty=ELE, cat=Special, pow=80, acc=90, eff=ELocking ThunderCage}
  , MoveDesc "Thunder Fang" 15 tackle
      {ty=ELE, pow=65, acc=95, eff=10 :% Flinch :+ 10 :% EStatus Paralysis, flags=CONTACT .|. BITE}
  , MoveDesc "Thunder Punch" 15 tackle
      {ty=ELE, pow=75, eff=10 :% EStatus Paralysis, flags=CONTACT .|. PUNCH}
  , MoveDesc "Thunder Shock" 30 tackle
      {ty=ELE, cat=Special, pow=40, eff=10 :% EStatus Paralysis}
  , MoveDesc "Thunder Wave" 20 celebrate
      {ty=ELE, targ=ADJACENT, acc=90, eff=EStatus Paralysis, flags=MCOAT}
  , MoveDesc "Thunderbolt" 15 tackle
      {ty=ELE, cat=Special, pow=90, eff=10 :% EStatus Paralysis}
  , MoveDesc "Thunderous Kick" 10 tackle
      {ty=FIG, pow=90, eff=AddBoost False zero {def= -1}, flags=CONTACT}
  , MoveDesc "Tickle" 20 celebrate
      {ty=NOR, targ=ADJACENT, eff=AddBoost False zero {att= -1, def= -1}, flags=MCOAT}
  , MoveDesc "Tidy Up" 10 celebrate
      {ty=NOR, eff=ClearHazard :+ RemoveSubstitute :+ AddBoost True zero {att=1, spe=1}}
  , MoveDesc "Topsy-Turvy" 20 celebrate
      {ty=DAR, targ=ADJACENT, eff=InvBoost, acc=neverMiss, flags=MCOAT}
  , MoveDesc "Torch Song" 10 tackle
      {ty=FIR, cat=Special, pow=80, eff=AddBoost True zero{spA= 1}, flags=SOUND}
  , MoveDesc "Torment" 15 celebrate
      {ty=DAR, targ=ADJACENT, eff=Torment, flags=IGNSUB .|. MCOAT}
  , MoveDesc "Toxic" 10 celebrate -- TODO: never misses if user is a poison-type
      {ty=POI, targ=ADJACENT, acc=90, eff=EStatus Toxic, flags=MCOAT}
  , MoveDesc "Toxic Spikes" 20 celebrate
      {ty=POI, targ=FOES .|. WIDE, eff=EHazard ToxicSpikes, flags=MCOAT}
  , MoveDesc "Toxic Thread" 20 celebrate
      {ty=POI, targ=ADJACENT, eff=EStatus Poison :+ AddBoost False zero {spe= -1}, flags=MCOAT}
  , MoveDesc "Trailblaze" 20 tackle
      {ty=GRA, pow=50, eff=AddBoost True zero {spe=1}, flags=CONTACT}
  , MoveDesc "Transform" 10 celebrate
      {ty=NOR, targ=ADJACENT, eff=Transform, acc=neverMiss}
  , MoveDesc "Tri Attack" 10 tackle
      {ty=NOR, cat=Special, pow=80, eff=20 :% Choose [EStatus Paralysis, EStatus Burn, EStatus Freeze]}
  , MoveDesc "Trick" 10 celebrate
      {ty=PSY, targ=ADJACENT, eff=SwapItem}
  , MoveDesc "Trick Room" 5 celebrate
      {ty=PSY, eff=TrickRoom, pri= -7}
  , MoveDesc "Trick-or-Treat" 20 celebrate
      {ty=GHO, targ=ADJACENT, eff=AddType GHO, flags=MCOAT}
  , MoveDesc "Triple Arrows" 10 tackle
      {ty=FIG, pow=90, eff=AddBoost True zero {cri=1} :+ AddBoost False zero {def= -1}}
  , MoveDesc "Triple Axel" 10 tackle
      {ty=ICE, pow=20, acc=90, eff=TripleAxel, flags=CONTACT}
  , MoveDesc "Triple Dive" 10 tackle
      {ty=WAT, pow=30, acc=95, hits=(3,3), flags=CONTACT}
  , MoveDesc "Triple Kick" 10 tackle
      {ty=FIG, pow=10, acc=90, eff=TripleKick, flags=CONTACT}
  , MoveDesc "Trop Kick" 15 tackle
      {ty=GRA, pow=70, eff=AddBoost False zero {att= -1}, flags=CONTACT}
  , MoveDesc "Trump Card" 5 tackle
      {ty=NOR, cat=Special, targ=ADJACENT, pow=0, acc=neverMiss, eff=TrumpCard, flags=CONTACT}
  , MoveDesc "Twin Beam" 10 tackle
      {ty=PSY, cat=Special, pow=40, hits=(2,2)}
  , MoveDesc "Twineedle" 20 tackle
      {ty=BUG, pow=25, hits=(2,2), eff=20 :% EStatus Poison}
  -- Twinkle Tackle
  , MoveDesc "Twister" 20 tackle
      {ty=DRA, cat=Special, pow=40, eff=DoublePowerIfInvul Flying, flags=WIND}

  , MoveDesc "U-turn" 20 tackle
      {ty=BUG, pow=70, eff=Switch True False False, flags=CONTACT}
  , MoveDesc "Uproar" 10 tackle
      {ty=NOR, cat=Special, pow=90, eff=Uproar, flags=SOUND .|. IGNSUB}
  , MoveDesc "V-create" 5 tackle
      {ty=FIR, pow=180, acc=95, eff=AddBoost True zero {def= -1, spD= -1, spe= -1}, flags=CONTACT}
  , MoveDesc "Vacuum Wave" 30 tackle
      {ty=FIG, cat=Special, pow=40, pri=1}
  , MoveDesc "Veevee Volley" 20 tackle
      {ty=NOR, pow=0, acc=neverMiss, eff=PwrHighBond, flags=CONTACT}
  , MoveDesc "Venom Drench" 20 celebrate
      {ty=POI, targ=ADJACENT, eff=VenomDrench}
  , MoveDesc "Venoshock" 10 tackle
      {ty=POI, cat=Special, pow=65, eff=DoublePwrIfTargetPoison}
  , MoveDesc "Victory Dance" 10 celebrate
      {ty=FIG, flags=DANCE, eff=AddBoost True zero {att=1, def=1}}
  , MoveDesc "Vine Whip" 25 tackle
      {ty=GRA, pow=45, flags=CONTACT}
  , MoveDesc "Vise Grip" 30 tackle
      {ty=NOR, pow=55, flags=CONTACT}
  , MoveDesc "Vital Throw" 10 tackle
      {ty=FIG, pow=70, acc=neverMiss, pri= -1, flags=CONTACT}
  , MoveDesc "Volt Switch" 20 tackle
      {ty=ELE, cat=Special, pow=70, eff=Switch True False False}
  , MoveDesc "Volt Tackle" 15 tackle
      {ty=ELE, pow=120, eff=Recoil (1/3) :+ 10 :% EStatus Paralysis, flags=CONTACT}

  , MoveDesc "Wake-Up Slap" 10 tackle
      {ty=FIG, pow=70, eff=WakeUpSlap, flags=CONTACT}
  , MoveDesc "Water Gun" 25 tackle
      {ty=WAT, cat=Special, pow=40}
  , MoveDesc "Water Pledge" 10 tackle
      {ty=WAT, cat=Special, pow=80, eff=WaterPledge}
  , MoveDesc "Water Pulse" 20 tackle
      {ty=WAT, cat=Special, pow=60, eff=20 :% Confuse, flags=PULSE}
  , MoveDesc "Water Shuriken" 20 tackle
      {ty=WAT, cat=Special, pow=15, hits=(2,5), pri=1}
  , MoveDesc "Water Sport" 15 celebrate
      {ty=WAT, eff=WaterSport}
  , MoveDesc "Water Spout" 5 tackle
      {ty=WAT, cat=Special, pow=150, eff=CutPwrUserHP}
  , MoveDesc "Waterfall" 15 tackle
      {ty=WAT, pow=80, eff=20 :% Flinch, flags=CONTACT}
  , MoveDesc "Wave Crash" 10 tackle -- TODO: How much recoil? Raises user's "action speed"
      {ty=WAT, pow=120, eff=Recoil (1/3), flags=CONTACT}
  , MoveDesc "Weather Ball" 10 tackle
      {ty=NOR, cat=Special, pow=50, eff=WeatherBall, flags=BULLET}
  , MoveDesc "Whirlpool" 15 tackle
      {ty=WAT, cat=Special, pow=35, acc=85, eff=ELocking Whirlpool}
  , MoveDesc "Whirlwind" 20 celebrate
      {ty=NOR, targ=ADJACENT, eff=Switch False True False, pri= -6, flags=IGNSUB .|. WIND}
  , MoveDesc "Wicked Blow" 5 tackle
      {ty=DAR, pow=80, crit=alwaysCrit, eff=IgnoreBoosts, flags=CONTACT .|. PUNCH}
  , MoveDesc "Wicked Torque" 10 tackle
      {ty=DAR, pow=80}
  , MoveDesc "Wide Guard" 10 celebrate
      {ty=ROC, targ=ALLIES .|. WIDE, eff=WideGuard, pri=3, flags=SNATCH}
  , MoveDesc "Wild Charge" 15 tackle
      {ty=ELE, pow=90, eff=Recoil (1/4), flags=CONTACT}
  , MoveDesc "Wildbolt Storm" 10 tackle
      {ty=ELE, cat=Special, pow=100, acc=80, eff=EStatus Paralysis, flags=WIND}
  , MoveDesc "Will-O-Wisp" 15 celebrate
      {ty=FIR, targ=ADJACENT, acc=85, eff=EStatus Burn}
  , MoveDesc "Wing Attack" 35 tackle
      {ty=FLY, pow=60, flags=CONTACT}
  , MoveDesc "Wish" 10 celebrate
      {ty=NOR, eff=Wish, flags=SNATCH}
  , MoveDesc "Withdraw" 40 celebrate
      {ty=WAT, eff=AddBoost True zero {def=1}, flags=SNATCH}
  , MoveDesc "Wonder Room" 10 celebrate
      {ty=PSY, eff=WonderRoom}
  , MoveDesc "Wood Hammer" 15 tackle
      {ty=GRA, pow=120, eff=Recoil (1/3), flags=CONTACT}
  , MoveDesc "Work Up" 30 celebrate
      {ty=NOR, eff=AddBoost True zero {att=1, spA=1}, flags=SNATCH}
  , MoveDesc "Worry Seed" 10 celebrate
      {ty=GRA, targ=ADJACENT, eff=SetAbility insomniaID, flags=MCOAT}
  , MoveDesc "Wrap" 20 tackle
      {ty=NOR, pow=15, acc=90, eff=ELocking Wrap, flags=CONTACT}
  , MoveDesc "Wring Out" 5 tackle
      {ty=NOR, cat=Special, pow=0, eff=WringOut, flags=CONTACT}
  , MoveDesc "X-Scissor" 15 tackle
      {ty=BUG, pow=80, flags=CONTACT .|. SLICE}
  , MoveDesc "Yawn" 10 celebrate
      {ty=NOR, targ=ADJACENT, eff=Yawn, acc=neverMiss, flags=MCOAT}
  , MoveDesc "Zap Cannon" 5 tackle
      {ty=ELE, cat=Special, pow=120, acc=50, eff=EStatus Paralysis, flags=BULLET}
  , MoveDesc "Zen Headbutt" 15 tackle
      {ty=PSY, pow=80, acc=90, eff=20 :% Flinch, flags=CONTACT}
  , MoveDesc "Zing Zap" 10 tackle
      {ty=ELE, pow=80, eff=30 :% Flinch, flags=CONTACT}
  , MoveDesc "Zippy Zap" 10 tackle
      {ty=ELE, pow=80, crit=alwaysCrit, pri=2, flags=CONTACT}
  ]

omniboost =
  AddBoost True zero {att=1,def=1,spA=1,spD=1,spe=1}

moveByName n =
  move <$> L.find (\MoveDesc {..} -> name == n) moves

