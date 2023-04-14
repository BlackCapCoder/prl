module World where

import Map
import Tile
import V2
import Render
import Picture
import Dialogue.Type
import Item (Item)
import Pokemon.PokeAPI qualified as API
import Pokemon.Pokemon
import ANSI
import UserInput

import System.Console.ANSI (getTerminalSize)
import Control.Monad.State
import Data.ByteString.Builder qualified as BS
import System.IO
import Data.Map qualified as M
import Data.IntMap qualified as IM


type WorldM m = StateT (World m) m

data World m = World
   { wm         :: Maps Tile
   , pl         :: Pos
   , menuCursor :: Int
   , twidth     :: Int
   , theight    :: Int
   , npcs       :: M.Map Pos (Dialogue (WorldM m) ())
   , money      :: Int
   , bagItems   :: IM.IntMap Int -- ItemID -> Count
   , pcItems    :: IM.IntMap Int
   , api        :: API.PokeAPI
   , party      :: [Pokemon]
   , respawnLoc :: Pos
   , encounterGraze :: Int -- turns without encounter
   , unique :: Int -- counter used to produce unique numbers
   }

----

draw pic = do
  World {..} <- get
  liftIO do
    putStr $ clearScreen <> cursorHome
    BS.hPutBuilder stdout $ drawColor 0 0 twidth theight (toF pic) <> resetStyle
    hFlush stdout

fullscreenMessage msg = do
  World {..} <- get

  draw $ Translate (fi (div (twidth - length msg) 2))
                   (fi (div theight 2))
       $ Text msg

  liftIO acceptInput

overworld = do
  World {..} <- get
  pure $ maps2pic twidth theight pl wm

healParty =
  modify \w -> w { party = heal (api w) <$> party w }

whiteOut = do
  fullscreenMessage "You whited out!"
  modify \w -> w { pl = respawnLoc w }
  healParty

