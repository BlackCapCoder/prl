module Battle.Type where

import Pokemon.Nature
import Pokemon.PokeAPI qualified as API
import Pokemon.Pokemon
import Pokemon.Stat
import Prelude hiding (Field)


type Countdown = Int
type Countup   = Int

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
   , torment     :: Countdown
   , echo        :: Countup  -- consequtive turns having used the same move
   , blocked     :: Bool     -- prevented from switching out
   , choice      :: Maybe ID -- locked into a move due to a held choice item
   , suppressed  :: Bool -- ability suppressed
   , transformed :: Bool -- is this pokemon transformed?
   , healBlocked :: Bool -- prevented from using healing moves?
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
   }

newBattleMon api pok = BattleMon
  { pokemon     = pok
  , stats       = getStats api pok
  , boosts      = zero
  , perishCount = Nothing
  , encore      = Nothing
  , taunt       = 0
  , torment     = 0
  , blocked     = False
  , choice      = Nothing
  , echo        = 0
  , lastMove    = Nothing
  , suppressed  = False
  , disabled    = []
  , leeched     = False
  , imprissoned = []
  , transformed = False
  , healBlocked = False
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
   }

newField = Field
  { lane1   = newFieldLane
  , lane2   = newFieldLane
  , weather = Nothing
  , terrain = Nothing
  , trickRoom  = 0
  , magicRoom  = 0
  , wonderRoom = 0
  , mudSport   = 0
  }


data FieldLane = FieldLane
   { lightScreen :: Countdown
   , reflect     :: Countdown
   , auroraveil  :: Countdown
   , tailwind    :: Countdown
   , spikes      :: Int
   , toxicSpikes :: Int
   , rocks       :: Bool
   , web         :: Bool
   }

newFieldLane = FieldLane
  { lightScreen = 0
  , reflect     = 0
  , auroraveil  = 0
  , tailwind    = 0
  , spikes      = 0
  , toxicSpikes = 0
  , rocks       = False
  , web         = False
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
   }

newBattle api party1 party2 wild = Battle
  { field = newField
  , mon1  = newBattleMon api (head party1)
  , mon2  = newBattleMon api (head party2)
  , log   = []
  , isWild = wild
  , actionCursor = 0
  , moveCursor = 0
  , grantExperience = True
  , ..
  }

