module Main where

import Play
import ANSI

main = tuiBracket do
  play =<< initialWorld

