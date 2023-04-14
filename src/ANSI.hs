module ANSI where

import Data.ByteString.Builder qualified as BS


smcup = "\ESC[?1049h"
rmcup = "\ESC[?1049l"

showCursor = "\ESC[?25h"
hideCursor = "\ESC[?25l"

cursorHome  = "\ESC[H"
clearScreen = "\ESC[J"

ansiBold = "\ESC[1m"
ansiDim  = "\ESC[2m"

resetStyle = "\ESC[0m"

rgbfg r g b = mconcat
  [ "\ESC[38;2;"
  , BS.word8Dec r, ";"
  , BS.word8Dec g, ";"
  , BS.word8Dec b, "m"
  ]

rgbbg r g b = mconcat
  [ "\ESC[48;2;"
  , BS.word8Dec r, ";"
  , BS.word8Dec g, ";"
  , BS.word8Dec b, "m"
  ]

