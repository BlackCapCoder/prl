module StyledString where

import Picture
import GHC.Exts (IsString (..))
import Data.List qualified as L


newtype StyledString = StyledString [FILL]
  deriving newtype
    (Semigroup, Monoid)

slines2pic :: [StyledString] -> Picture
slines2pic =
  coerce Chars

style' fill (StyledString xs) =
  StyledString ((<> fill) <$> xs)

style s =
  style' (Unset, Set s, Unset, Unset)

fg c =
  style' (Unset, Unset, Set c, Unset)

bg c =
  style' (Unset, Unset, Unset, Set c)

----

instance IsString StyledString where
  fromString = sstr

char2fill :: Char -> FILL
char2fill chr =
  (Set chr, Unset, Unset, Unset)

sstr :: String -> StyledString
sstr =
  StyledString #. map char2fill

fill2char (x, _, _, _) = case x of
  Set c -> c
  _     -> ' '

unsstr (StyledString s) =
  map fill2char s

sshow =
  sstr . show

----

sunwords =
  mconcat . L.intersperse " "

