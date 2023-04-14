module Pokemon.Type where

import Data.Vector qualified as V


data TYPE
   = NON -- typeless
   | NOR
   | FIR
   | WAT
   | ELE
   | GRA
   | ICE
   | FIG
   | POI
   | GRO
   | FLY
   | PSY
   | BUG
   | ROC
   | GHO
   | DRA
   | DAR
   | STE
   | FAI
   deriving (Show, Eq, Ord, Enum, Bounded)

ntypes =
  fromEnum (maxBound :: TYPE) + 1

typeName = \case
   NON -> "typeless"
   NOR -> "normal"
   FIR -> "fire"
   WAT -> "water"
   ELE -> "electric"
   GRA -> "grass"
   ICE -> "ice"
   FIG -> "fighting"
   POI -> "poison"
   GRO -> "ground"
   FLY -> "flying"
   PSY -> "psychic"
   BUG -> "bug"
   ROC -> "rock"
   GHO -> "ghost"
   DRA -> "dragon"
   DAR -> "dark"
   STE -> "steel"
   FAI -> "fairy"

typeFromName = \case
  "normal"   -> NOR
  "fire"     -> FIR
  "water"    -> WAT
  "electric" -> ELE
  "grass"    -> GRA
  "ice"      -> ICE
  "fighting" -> FIG
  "poison"   -> POI
  "ground"   -> GRO
  "flying"   -> FLY
  "psychic"  -> PSY
  "bug"      -> BUG
  "rock"     -> ROC
  "ghost"    -> GHO
  "dragon"   -> DRA
  "dark"     -> DAR
  "steel"    -> STE
  "fairy"    -> FAI
  _          -> NON

-- type effectiveness chart
typetable = V.fromList
 -- NON  NOR  FIR  WAT  ELE  GRA  ICE  FIG  POI  GRO  FLY  PSY  BUG  ROC  GHO  DRA  DAR  STE  FAI
  [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 -- NON
  , 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.5, 0.0, 1.0, 1.0, 0.5, 1.0 -- NOR
  , 1.0, 1.0, 0.5, 0.5, 1.0, 2.0, 2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 0.5, 1.0, 0.5, 1.0, 2.0, 1.0 -- FIR
  , 1.0, 1.0, 2.0, 0.5, 1.0, 0.5, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 1.0, 2.0, 1.0, 0.5, 1.0, 1.0, 1.0 -- WAT
  , 1.0, 1.0, 1.0, 2.0, 0.5, 0.5, 1.0, 1.0, 1.0, 0.0, 2.0, 1.0, 1.0, 1.0, 1.0, 0.5, 1.0, 1.0, 1.0 -- ELE
  , 1.0, 1.0, 0.5, 2.0, 1.0, 0.5, 1.0, 1.0, 0.5, 2.0, 0.5, 1.0, 0.5, 2.0, 1.0, 0.5, 1.0, 0.5, 1.0 -- GRA
  , 1.0, 1.0, 0.5, 0.5, 1.0, 2.0, 0.5, 1.0, 1.0, 2.0, 2.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 0.5, 1.0 -- ICE
  , 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 0.5, 1.0, 0.5, 0.5, 0.5, 2.0, 0.0, 1.0, 2.0, 2.0, 0.5 -- FIG
  , 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, 0.5, 1.0, 1.0, 0.0, 2.0 -- POI
  , 1.0, 1.0, 2.0, 1.0, 2.0, 1.0, 1.0, 1.0, 2.0, 1.0, 0.0, 1.0, 0.5, 2.0, 1.0, 1.0, 1.0, 2.0, 1.0 -- GRO
  , 1.0, 1.0, 1.0, 1.0, 0.5, 1.0, 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 2.0, 0.5, 1.0, 1.0, 1.0, 0.5, 1.0 -- FLY
  , 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 1.0, 1.0, 0.5, 1.0, 1.0, 1.0, 1.0, 0.0, 0.5, 1.0 -- PSY
  , 1.0, 1.0, 0.5, 1.0, 1.0, 1.0, 1.0, 0.5, 0.5, 1.0, 0.5, 2.0, 1.0, 1.0, 0.5, 1.0, 2.0, 0.5, 0.5 -- BUG
  , 1.0, 1.0, 2.0, 1.0, 1.0, 2.0, 2.0, 0.5, 1.0, 0.5, 2.0, 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 0.5, 1.0 -- ROC
  , 1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 2.0, 1.0, 0.5, 1.0, 1.0 -- GHO
  , 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 0.5, 0.0 -- DRA
  , 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.5, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 2.0, 1.0, 0.5, 1.0, 0.5 -- DAR
  , 1.0, 1.0, 0.5, 0.5, 0.5, 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 1.0, 0.5, 2.0 -- STE
  , 1.0, 1.0, 0.5, 1.0, 1.0, 1.0, 1.0, 2.0, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 0.5, 1.0 -- FAI
  ]

-- The type effectiveness for an attacker against a defender.
--
effectiveness :: TYPE -> TYPE -> Float
effectiveness att def =
  typetable V.! do fromEnum att * ntypes + fromEnum def


