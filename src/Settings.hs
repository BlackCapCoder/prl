module Settings where


-- data LevelCap
--    = Soft    -- Pokemon above the level-cap will have obedience issues
--    | Hard    -- Pokemon cannot be leveled up above the level-cap
--    | Dynamic -- Trainers scale with your highest level pokemon

data Settings = Settings
   { reusableTMs       :: Bool -- if true, TMs are not consumed upon use
   , consumables       :: Bool -- if false, consumables are not consumed on use
   , keepItems         :: Bool -- if true, held items are given back after battles
   , ppFromTMs         :: Bool -- if false, overwriting a move with a TM will not restore PP
   , noIVs             :: Bool -- if true, all pokemon have 31 IVs in every stat
   , noEVs             :: Bool -- if true, EVs are unobtainable
   , noHMs             :: Bool -- if true, HMs can be used in the overworld without having to learn them
   , instantCapture    :: Bool -- if true, pokeballs cannot fail to capture
   , sleepClause       :: Bool -- if true, at most 1 pokemon can be put to sleep in singles
   , itemClause        :: Maybe Int -- at most N items can be used in trainer battles

   -- Nothing: No poison damage in the overworld
   -- False:   Survive on 1 HP
   -- True:    Poison kills
   , poisonInOverworld :: Maybe Bool

   , noDamageRanges    :: Bool -- if true, eliminate the random damage multiplier
   , shiftMode         :: Bool -- set/shift mode

   , confusionChance :: Float -- chance of hitting yourself during confusion
   , paralysisChance :: Float -- chance of becomming fully paralyzed
   , thawChance      :: Float -- chance of thawing from a freeze
   }

defaultSettings = Settings
  { reusableTMs       = True
  , consumables       = True
  , keepItems         = True    -- implemented (iff there was an old item, overwrite new item)
  , ppFromTMs         = True
  , noIVs             = False   -- implemented
  , noEVs             = False   -- implemented
  , noHMs             = True
  , instantCapture    = False
  , sleepClause       = False
  , shiftMode         = False
  , itemClause        = Nothing
  , poisonInOverworld = Nothing -- implemented
  , noDamageRanges    = False   -- implemented
  , confusionChance   = 1/3     -- implemented
  , paralysisChance   = 1/2     -- implemented
  , thawChance        = 1/5     -- implemented
  }

