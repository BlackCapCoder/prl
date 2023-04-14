module Render where

import Tile
import V2
import Map
import Picture
import Data.Vector qualified as V
import Data.IntMap qualified as IM
import Control.Monad.State


mult s a
  | s > 0 = 1/s - (a/s)
  | let   = a/(-s) + 0.0001

ray po sx sy w h fx@(floor->x) fy@(floor->y) mi fmx@(floor->mx) fmy@(floor->my) m
  | x >= 0 && y >= 0 && x < w && y < h
  , Just t <- lookupMaps mi mx my m = do

    let i = y*w + x

    modify \v -> (i, toFILL t) : v

    let j  = 1
    let jx = sx*j
    let jy = sy*j

    case t of
      Portal _ q@(Pos mi' (V2 x y)) -> do

        if not po then do

        let ox = fi x + fmx - fi (floor fmx)
        let oy = fi y + fmy - fi (floor fmy)

        ray True sx sy w h (fx+jx) (fy+jy) mi' (ox+jx) (oy+jy) m

        else do
        ray True sx sy w h (fx+jx) (fy+jy) mi (fmx+jx) (fmy+jy) m

      _ | isOpaque t -> pure ()
        | let        ->
        ray False sx sy w h (fx+jx) (fy+jy) mi (fmx+jx) (fmy+jy) m

  | let
  = pure ()

render w h (Pos mi (V2 mx my)) m = do
  let x = div w 2
  let y = div h 2
  forM_ [0, rot .. 2*pi] \a -> do
    let sx = cos a
    let sy = sin a
    ray True sx sy w h (fi x + 0.5) (fi y + 0.5) mi (fi mx + 0.5) (fi my + 0.5) m

  modify \v -> (y*w+x, (Set '@', None, None, None)) : v

step = 0.1
rot  = 0.005

render' w h p m =
  drawColor 0 0 w h $ toF $ maps2pic w h p m

maps2pic w h p m = CharVec w v where
  v0 = execState (render w h p m) []
  v = V.update (V.replicate (w*h) (None, None, None, None)) (V.fromList (reverse v0))

