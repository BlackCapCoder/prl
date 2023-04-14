module UserInput where

import System.IO


data OverworldInput
   = Move Int Int
   | OpenMenu
   | Quit
   | Test
   deriving (Show, Eq, Ord)

getOverworldInput =
  getChar >>= \case
    'h' -> pure $ Move (-1) 0
    'j' -> pure $ Move 0 1
    'k' -> pure $ Move 0 (-1)
    'l' -> pure $ Move 1 0
    'y' -> pure $ Move (-1) (-1)
    'u' -> pure $ Move 1 (-1)
    'b' -> pure $ Move (-1) 1
    'n' -> pure $ Move 1 1
    'q' -> pure Quit
    'i' -> pure OpenMenu
    't' -> pure Test
    '\ESC' -> pure OpenMenu
    _   -> getOverworldInput

data MenuInput
   = CursorUp
   | CursorDown
   | Cancel
   | Select
   deriving (Show, Eq, Ord)

getMenuInput =
  getChar >>= \case
    'k'    -> pure CursorUp
    'j'    -> pure CursorDown
    'f'    -> pure Select
    'd'    -> pure Cancel
    '\ESC' -> pure Cancel
    _      -> getMenuInput

getMsgBoxInput =
  getChar >>= \case
    'f' -> pure ()
    'd' -> pure ()
    '\ESC' -> pure ()
    _   -> getMsgBoxInput

data NumberInput
   = NCancel
   | NSelect
   | NInc
   | NDec
   | NBigInc
   | NBigDec
   deriving (Show, Eq, Ord)

getNumberInput =
  getChar >>= \case
    'k'    -> pure NInc
    'l'    -> pure NBigInc
    'j'    -> pure NDec
    'h'    -> pure NBigDec
    'f'    -> pure NSelect
    'd'    -> pure NCancel
    '\ESC' -> pure NCancel
    _      -> getNumberInput

data BattleMenuInput
   = North
   | East
   | South
   | West
   | BSelect
   | BCancel

getBattleMenuInput =
  getChar >>= \case
    'h' -> pure West
    'l' -> pure East
    'k' -> pure North
    'j' -> pure South
    'f' -> pure BSelect
    'd' -> pure BCancel
    '\ESC' -> pure BCancel
    _ -> getBattleMenuInput

acceptInput =
  getChar >>= \case
    'f' -> pure ()
    _   -> acceptInput

