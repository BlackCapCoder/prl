module Picture where

import Data.Word
import Data.ByteString.Builder qualified as BS
import Data.Vector qualified as V
import ANSI


-- Like Maybe, but `None` means literally nothing,
-- and `Unset` means "not specified". This is useful
-- for drawing things on top of each other where
-- Unset means "don't overwrite".
--
data Perhaps a
   = Unset -- Not specified
   | None  -- Nothing (literally)
   | Set a -- Just
   deriving (Functor, Show, Eq, Ord)
   deriving (Applicative, Monad) via WrappedPoint Perhaps

instance Semigroup (Perhaps a) where
  Unset <> a = a
  a <> Unset = a
  _ <> a     = a

instance Monoid (Perhaps a) where
  mempty = Unset

instance Pointed Perhaps where
  point = Set

instance Apply Perhaps where
  (<.>) = apDefault

instance Bind Perhaps where
  join = \case
    Unset -> Unset
    None  -> None
    Set a -> a

instance Alternative Perhaps where
  empty = mempty
  (<|>) = (<>)

----

data Color
   = RGB { r,g,b :: Word8 }
   deriving (Show, Eq, Ord)

data Style
   = Bold
   | Dim
   deriving (Show, Eq, Ord)

----

-- A `gloss-rendering` inspired interface for TUI graphics
--
data Picture
   = Blank
   | Filled FILL
   | Fill (Perhaps Char) (Perhaps Style) (Perhaps Color) (Perhaps Color) Picture
   | Pictures [Picture]
   | Rotate Float Picture
   | Translate Float Float Picture
   | Scale Float Float Picture
   | Circle Float
   | Rectangle Float Float
   | Text String
   | Chars [[FILL]]
   | CharVec Int (V.Vector FILL)
   | Clip Float Float Float Float Picture
   deriving (Show, Eq, Ord)

instance Semigroup Picture where
  Blank <> a = a
  a <> Blank = a
  Pictures a <> Pictures b = Pictures (a <> b)
  a <> b = Pictures [a, b]

instance Monoid Picture where
  mempty  = Blank
  mconcat = Pictures


toF :: Picture -> F FILL
toF = go mempty where
  go fill = \case
    Blank              -> empty
    Filled a           -> pure a
    Pictures ps        -> foldr (\a x -> x <> go fill a) mempty ps
    Fill chr s fg bg p -> go (fill <> (chr, s, fg, bg)) p
    Rotate deg p       -> rotateF (-deg * pi/180) $ go fill p
    Translate tx ty p  -> translateF tx ty $ go fill p
    Scale h v p        -> scaleF h v $ go fill p
    Circle r           -> fill <$ circleF r
    Rectangle w h      -> fill <$ rectF w h
    Text str           -> (fill <>) <$> stringF str
    Chars cs           -> (fill <>) <$> charsF cs
    CharVec w v        -> (fill <>) <$> charVecF w v
    Clip x y w h z     -> clipF x y w h *> go fill z

----

-- Character, Style, Foreground color, Background color
--
type FILL = (Perhaps Char, Perhaps Style, Perhaps Color, Perhaps Color)

drawFill s0 fg0 bg0 chr s fg bg = mconcat
    [ if reset then resetStyle else mempty
    , if setS  then s'  else mempty
    , if setFg then fg' else mempty
    , if setBg then bg' else mempty
    , BS.char7 chr'
    ]
  where
    chr' = case chr of
      Set a -> a
      _     -> ' '

    fg' = case fg of
      Set RGB{..} -> rgbfg r g b
      _           -> mempty

    bg' = case bg of
      Set RGB{..} -> rgbbg r g b
      _           -> mempty

    s' = case s of
      Set Bold -> ansiBold
      Set Dim  -> ansiDim
      _        -> mempty

    reset =
      (not (isSet fg) && fgChanged) ||
      (not (isSet bg) && bgChanged) ||
      (not (isSet  s) && sChanged )

    sChanged
      | Set a <- s0
      , Set b <- s = a /= b
      | otherwise  = isSet s0 /= isSet s

    fgChanged
      | Set a <- fg0
      , Set b <- fg = a /= b
      | otherwise   = isSet fg0 /= isSet fg

    bgChanged
      | Set a <- bg0
      , Set b <- bg = a /= b
      | otherwise   = isSet bg0 /= isSet bg

    setFg = fgChanged || reset
    setBg = bgChanged || reset
    setS  = sChanged  || reset

isSet = \case
  Set _ -> True
  _     -> False



----

newtype F a = F (Float -> Float -> Maybe a)
  deriving Functor
  deriving (Applicative, Monad) via WrappedPoint F

instance Pointed F where
  point a = F \_ _ -> Just a

instance Apply F where
  F f <.> F a = F \x y -> f x y <.> a x y

instance Bind F where
  join (F mma) = F \x y ->
    mma x y >>= \(F ma) ->
      ma x y

instance Alternative F where
  empty = F \_ _ -> Nothing
  F l <|> F r = F \x y -> l x y <|> r x y

instance Semigroup (F a) where
  (<>) = (<|>)

instance Monoid (F a) where
  mempty = empty


rotateF a (F f) = F \x y ->
  f (x*cos a - y*sin a) (y*cos a + x*sin a)

translateF tx ty (F f) = F \x y ->
  f (x-tx) (y-ty)

scaleF h v (F f) = F \x y ->
  f (x/h) (y/v)


predicateF f =
  F \x y -> guard (f x y)

rectF w h = predicateF \x y ->
  x >= 0 && y >= 0 && x < w && y < h
  -- abs x <= w/2 && abs y <= h/2

circleF r = predicateF \x y ->
  sqrt (x*x + y*y) <= r


safeIndex s i =
  listToMaybe $ drop i s

stringF str = F \(floor->x) (floor->y) -> do
  guard $ x >= 0 && y >= 0
  ln <- lines str `safeIndex` y
  c  <- ln `safeIndex` x
  pure (Set c, Unset, Unset, Unset)

charsF str = F \(floor->x) (floor->y) -> do
  guard $ x >= 0 && y >= 0
  ln <- str `safeIndex` y
  ln `safeIndex` x

charVecF w v = F \(floor->x) (floor->y) -> do
  let h = V.length v `div` w
  (v V.! (y*w + x)) <$ guard do x >= 0 && y >= 0 && x < w && y < h

clipF x0 y0 w h = predicateF \x y ->
  x >= x0 && y >= y0 && x < (w+x0) && y < (h+y0)

----

drawColor :: Float -> Float -> Int -> Int -> F FILL -> BS.Builder
drawColor x0 y0 w h (F f) = drawLines
  [ [ f (x0 + fi x) (y0 + fi y)
    | x <- [0..pred w]
    ]
  | y <- [0..pred h]
  ]

drawLines = go Unset Unset Unset where
  go _ _   _   []   = mempty
  go _ _   _   [[]] = mempty
  go s0 fg0 bg0 ([]:ls) = BS.char7 '\n' <> go s0 fg0 bg0 ls
  go s0 fg0 bg0 ((c:cs):ls) =
    case c of
      Just (chr,s,fg,bg) -> drawFill s0 fg0 bg0 chr s fg bg <> go s fg bg (cs:ls)
      _ -> BS.char7 ' ' <> go s0 fg0 bg0 (cs:ls)

