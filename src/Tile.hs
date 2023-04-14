module Tile where

import V2
import Picture

data Pos = Pos
   { mapID :: Int
   , pos   :: V2 Int
   }
   deriving (Show, Eq, Ord)

data Tile
   = Empty
   | Solid
   | Wall
   | Chr Char
   | Portal Char Pos
   | Grass
   | PC
   deriving Show

isSolid = \case
  Empty -> False
  Solid -> True
  Wall  -> True
  PC    -> True
  _     -> False

isOpaque = \case
  Solid -> True
  _     -> False

toChar = \case
  Empty      -> ' '
  Solid      -> '#'
  Wall       -> '#'
  PC         -> 'C'
  Chr c      -> c
  Portal c _ -> c
  Grass      -> '\''

toFILL :: Tile -> FILL
toFILL = \case
  Empty -> (None, None, None, None)
  Solid -> (Set '#', None, None, None)
  Wall  -> (Set '#', None, None, None)
  PC    -> (Set 'C', None, None, None)
  Chr c -> (Set  c, None, None, None)
  Portal c _ -> (Set c, None, None, None)
  Grass -> (Set '\'', None, Set (RGB 0 255 0), None)

