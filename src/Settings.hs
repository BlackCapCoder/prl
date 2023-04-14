module Settings where


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
   }

data LevelCap
   = Soft    -- Pokemon above the level-cap will have obedience issues
   | Hard    -- Pokemon cannot be leveled up above the level-cap
   | Dynamic -- Trainers scale with your highest level pokemon

