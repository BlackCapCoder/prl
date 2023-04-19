module Battle.Type where

import Pokemon.Nature
import Pokemon.PokeAPI qualified as API
import Pokemon.Pokemon
import Pokemon.Stat
import Prelude hiding (Field)
import Data.IntSet (IntSet)


type Countdown = Int
type Countup   = Int
type Turn      = Int

----

data BattleMon = BattleMon
   { pokemon     :: Pokemon -- This might change, but the uid will refer to the original pokemon
   , stats       :: Stat  Int
   , boosts      :: Boost Int
   , lastMove    :: Maybe ID
   , perishCount :: Maybe Countdown
   , encore      :: Maybe (ID, Countdown)
   , disabled    :: [(ID, Countdown)]
   , imprissoned :: [UID] -- pokemon that have used imprisson on us
   , taunt       :: Countdown
   , torment     :: Bool
   , echo        :: Countup  -- consequtive turns having used the same move
   , blocked     :: Bool     -- prevented from switching out
   , choice      :: Maybe ID -- locked into a move due to a held choice item
   , suppressed  :: Bool -- ability suppressed
   , transformed :: Bool -- is this pokemon transformed?
   , healBlock   :: Countdown -- prevented from using healing moves?
   , leeched     :: Bool -- suffering from leech seed?
   , ingrained   :: Bool
   , aquaRing    :: Bool
   , drowsy      :: Bool -- due to a yawn
   , lastBerry   :: Maybe ID -- lost berries can be recovered
   , luckyChant  :: Countdown
   , dynamax     :: Countdown
   , isMinimized :: Bool
   , semiInvul   :: Maybe SemiInvul
   , protected   :: Bool
   , wideGuard   :: Bool
   , flinched    :: Bool
   , confusion   :: Countdown
   , embargo     :: Countdown
   , lastDamage  :: Int
   , attraction  :: IntSet -- uid's of pokemon we are attracted to
   , cursed      :: Bool
   , subHP       :: Int -- substitute HP
   , enduring    :: Bool
   , destinyBond :: Countdown
   , snatching   :: Bool
   , charged     :: Bool
   , defenceCurl :: Bool
   , focused     :: Bool
   , lockedOn    :: Bool
   , magicCoat   :: Bool
   , magnetRise  :: Countdown
   , miracleEye  :: Bool
   , mist        :: Bool -- can be passed with baton pass
   , protections :: Int  -- consequtive turns having used protection moves
   , quashed     :: Bool
   , tarShot     :: Bool
   , telekinesis :: Countdown -- Telekinesis has lots of rules- check bulbapedia!
   , partialTrap :: Countdown -- partial trapping moves (bind, whirlpool, magma storm..)
   , stockpiles  :: Int
   , recharge    :: Countdown -- skip turn unless 0
   , odorSleuth  :: Bool
   , outraged    :: Countdown -- user is locked into performing their most recent move
                              -- for N more turns, then become confused
   , hasMoved    :: Bool -- has moved yet this turn
   }

newBattleMon api pok = BattleMon
  { pokemon     = pok
  , stats       = getStats api pok
  , boosts      = zero
  , perishCount = Nothing
  , encore      = Nothing
  , taunt       = 0
  , torment     = False
  , blocked     = False
  , choice      = Nothing
  , echo        = 0
  , lastMove    = Nothing
  , suppressed  = False
  , disabled    = []
  , leeched     = False
  , imprissoned = []
  , transformed = False
  , ingrained   = False
  , aquaRing    = False
  , drowsy      = False
  , lastBerry   = Nothing
  , luckyChant  = 0
  , dynamax     = 0
  , isMinimized = False
  , semiInvul   = Nothing
  , protected   = False
  , wideGuard   = False
  , flinched    = False
  , confusion   = -1
  , embargo     = 0
  , lastDamage  = 0
  , attraction  = mempty
  , cursed      = False
  , subHP       = 0
  , enduring    = False
  , destinyBond = 0
  , snatching   = False
  , charged     = False
  , healBlock   = 0
  , defenceCurl = False
  , focused     = False
  , lockedOn    = False
  , magicCoat   = False
  , magnetRise  = 0
  , miracleEye  = False
  , mist        = False
  , protections = 0
  , quashed     = False
  , tarShot     = False
  , telekinesis = 0
  , partialTrap = 0
  , stockpiles  = 0
  , recharge    = 0
  , odorSleuth  = False
  , outraged    = 0
  , hasMoved    = False
  }

----

data Field = Field
   { lane1, lane2 :: FieldLane
   , weather      :: Maybe (Weather, Countdown)
   , terrain      :: Maybe (Terrain, Countdown)
   , trickRoom    :: Countdown
   , magicRoom    :: Countdown
   , wonderRoom   :: Countdown
   , mudSport     :: Countdown
   , waterSport   :: Countdown
   , gravity      :: Countdown
   }

newField = Field
  { lane1   = newFieldLane
  , lane2   = newFieldLane
  , weather = Nothing
  , terrain = Nothing
  , trickRoom  = 0
  , magicRoom  = 0
  , wonderRoom = 0
  , gravity    = 0
  , mudSport   = 0
  , waterSport = 0
  }


data FieldLane = FieldLane
   { lightScreen      :: Countdown
   , reflect          :: Countdown
   , auroraveil       :: Countdown
   , tailwind         :: Countdown
   , safeguard        :: Countdown
   , spikes           :: Int
   , toxicSpikes      :: Int
   , rocks            :: Bool
   , web              :: Bool
   , quickGuard       :: Bool
   , alliesDefeated   :: Int
   , lastAllyDefeated :: Turn
   }

newFieldLane = FieldLane
  { lightScreen      = 0
  , reflect          = 0
  , auroraveil       = 0
  , tailwind         = 0
  , spikes           = 0
  , toxicSpikes      = 0
  , safeguard        = 0
  , rocks            = False
  , web              = False
  , quickGuard       = False
  , alliesDefeated   = 0
  , lastAllyDefeated = -1
  }

data Weather
   = Rain
   | Sun
   | Hail
   | Sandstorm

data Terrain
   = TPsychic
   | TMisty
   | TGrassy
   | TElectric

data SemiInvul
   = Diving
   | Flying
   | Digging
   | Phantom
   deriving (Eq, Ord)

----

data Battle = Battle
   { field           :: Field
   , mon1, mon2      :: BattleMon
   , party1, party2  :: [Pokemon]
   , log             :: [String]
   , isWild          :: Bool
   , actionCursor    :: Int
   , moveCursor      :: Int
   , grantExperience :: Bool
   , turnCount       :: Int
   }

newBattle api party1 party2 wild = Battle
  { field           = newField
  , mon1            = newBattleMon api sendOut1
  , mon2            = newBattleMon api sendOut2
  , log             = []
  , isWild          = wild
  , actionCursor    = 0
  , moveCursor      = 0
  , grantExperience = True
  , turnCount       = 1
  , ..
  }
  where
    sendOut1:_ = dropWhile (\m -> m.hp < 1) party1
    sendOut2:_ = dropWhile (\m -> m.hp < 1) party2

