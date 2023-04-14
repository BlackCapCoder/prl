{-# LANGUAGE OverloadedRecordDot #-}
module Item where

import Data.IntMap qualified as IM
import Data.List (find)

data Item = Item
   { name  :: String
   , price :: Int
   }

items = IM.fromList $ zip [0..]
  [ Item "Pokeball" 100
  , Item "Potion"   200
  , Item "Repel"    300
  ]

findItemByName name =
  find (\(k, item) -> item.name == name) $ IM.assocs items

findItemByID id =
  IM.lookup id items

-- Multiple of the original price that shopkeepers
-- give upon selling an item
--
storeMargin = 0.5

