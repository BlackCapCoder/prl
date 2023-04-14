module Dialogue
  ( runDialogue
  , module Dialogue.Type
  )
  where

import World
import UserInput
import Picture
import Dialogue.Type
import Control.Monad.State


runDialogue d = do
  ow <- overworld
  go ow d
  where
    go ow = \case
      End a   -> pure a
      Act m   -> m >>= go ow
      Say nag str r -> do
        msg <- msgbox str
        draw (ow <> msg)
        when nag $ liftIO getMsgBoxInput
        go (ow <> msg) r
      DSelect os r -> do
        if null os then go ow r else do
        0 & fix \loop i -> do
          let cur = mod i (length os)
          lst <- list (map fst os) cur
          draw (ow <> lst)
          liftIO getMenuInput >>= \case
            Cancel     -> go ow r
            CursorUp   -> loop (pred cur)
            CursorDown -> loop (succ cur)
            Select     -> go ow $ snd $ os !! cur
      IntSelect lo hi f -> do
        lo & fix \loop i -> do
          let cur = mod (i - lo) (hi - lo + 1) + lo
          pic <- intSelect hi cur
          draw (ow <> pic)
          liftIO getNumberInput >>= \case
            NCancel -> go ow (f Nothing)
            NSelect -> go ow (f (Just cur))
            NInc    -> loop (succ cur)
            NDec    -> loop (pred cur)
            NBigInc -> loop (min hi (cur + 10))
            NBigDec -> loop (max lo (cur - 10))


msgbox str = do
  World {..} <- get
  let x = div (twidth - length str) 2
  pure $ Fill Unset None None (Set (RGB 32 32 32))
       $ Translate 0 (fi theight - 1)
       $ Pictures
       [ Rectangle (fi twidth) 1
       , Translate (fi x) 0 $ Text str
       ]

list os cur = do
  World {..} <- get

  let max = theight - 2

  let w = maximum (map length os) + 4
  let h = length os

  let page = div cur max

  let h' = min h max
  let os' = take h' $ drop (page*max) os

  let h'' = length os'

  let x = div (twidth - w) 2
  let y = theight - h'' - 2

  let i = mod cur max

  pure $ Fill Unset None None (Set (RGB 32 32 32))
       $ Translate (fi x) (fi y)
       $ Pictures
           [ Rectangle (fi w) (fi h'')
           , Translate 3 0 $ Text $ unlines os'
           , Translate 1 (fi i) $ Text ">"
           , Translate 3 (fi i)
           $ Fill Unset (Set Bold) Unset Unset
           $ Text (os !! cur)
           ]

intSelect max num = do
  World {..} <- get
  let x = div (twidth - w) 2
  let y = theight - h - 2
  pure $ Translate (fi x) (fi y)
       $ Pictures
    [ Fill Unset None None (Set (RGB 32 32 32)) $ Rectangle (fi w) 1
    , Translate (fi do div (w - length (show num)) 2) 0 $ Text (show num)
    ]
  where
    w = length (show max) + 2
    h = 1

