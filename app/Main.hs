module Main where

import Play
import ANSI
import System.IO

main = do
  oldEcho   <- hGetEcho stdout
  oldBufIn  <- hGetBuffering stdin
  oldBufOut <- hGetBuffering stdout

  hSetEcho      stdout False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout (BlockBuffering Nothing)

  putStr $ smcup <> hideCursor
  play =<< initialWorld
  putStr $ rmcup <> showCursor

  hSetEcho      stdout oldEcho
  hSetBuffering stdin  oldBufIn
  hSetBuffering stdout oldBufOut

