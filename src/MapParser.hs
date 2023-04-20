module MapParser where

import Prelude hiding (some, many)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Map
import Tile
import V2
import Data.Vector qualified as V
import Data.Map qualified as M
import Data.IntMap qualified as IM
import System.Directory


-- ! Name of map
--
-- ######
-- #    #
-- #    #
-- ######
--
-- map 1:x y -> map 2:x y

type Parser = Parsec Void Text


mapName :: Parser Text
mapName = do
  char '!'
  space
  name <- T.stripEnd <$> takeWhile1P Nothing (/= '\n')
  some newline
  pure name

mapLines :: Parser [Text]
mapLines = do
  line <- takeWhileP Nothing (/= '\n')
  if T.null line then pure [] else do
  newline
  (line :) <$> mapLines

mapP :: Parser (Text, [Text])
mapP = do
  name <- mapName
  ls   <- mapLines
  space
  pure (name, ls)


endpoint :: Parser Endpoint
endpoint = do
  name <- T.stripEnd <$> takeWhileP Nothing (/= ':')
  char ':'
  space
  x <- L.decimal
  space
  y <- L.decimal
  space
  pure (name, x, y)

portalP = do
  e1 <- endpoint
  string "<->" *> space
  e2 <- endpoint
  space
  pure (e1, e2)

----

type Endpoint = (Text, Int, Int)

data Thing
   = TMap  Text [Text]
   | TLink Endpoint Endpoint
   deriving Show

thingP = asum
  [ uncurry TMap  <$> mapP
  , uncurry TLink <$> portalP
  ]

things = some thingP


foldThings maps names links (t:ts) = case t of
  TLink e1 e2 -> foldThings maps names ((e1,e2):links) ts
  TMap n ls -> foldThings (insertMap m maps) (M.insert n (cnt maps) names) links ts
    where
      m = Map
          { content = V.fromList $ map toTile $ T.unpack $ T.concat ls'
          , stride  = maximum $ map T.length ls
          }

      w = maximum $ map T.length ls
      ls' = ls <&> \l -> l <> T.replicate (w - T.length l) " "

foldThings maps0 names ls _ = foldr go maps0 ls where
  go ((n1,x1,y1),(n2,x2,y2)) ms = fromMaybe ms do
    i1 <- M.lookup n1 names
    i2 <- M.lookup n2 names
    let p1 t = Portal (toChar t) $ Pos i2 (V2 x2 y2)
    let p2 t = Portal (toChar t) $ Pos i1 (V2 x1 y1)
    pure ms
      { maps = ms.maps
             & IM.update (Just . placeMap' x1 y1 p1) i1
             & IM.update (Just . placeMap' x2 y2 p2) i2
      }

foldThings' :: [Thing] -> Maps Tile
foldThings' =
  foldThings emptyMaps mempty []

toTile = \case
  '#'  -> Solid
  'H'  -> Wall
  'C'  -> PC
  '\'' -> Grass
  ' '  -> Empty
  '~'  -> Water
  c    -> Chr c

----

getMapsTxt = do
  pths <- listDirectory "maps"
  T.unlines <$> forM pths \pth -> T.readFile ("maps/" <> pth)

parseMaps :: IO (Maps Tile)
parseMaps = do
  f1 <- getMapsTxt
  case parse things "" f1 of
    Left  _  -> error "Parse error"
    Right ts -> pure $ foldThings' ts

parseMapFile name = do
  txt <- T.readFile ("maps/" <> name)
  case parse things "" txt of
    Left  _  -> error "Parse error"
    Right ts -> pure $ foldThings' ts

