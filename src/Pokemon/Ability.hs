{-# LANGUAGE OverloadedRecordDot #-}
module Pokemon.Ability where

import Data.IntMap qualified as IM
import Data.List (find)


type AbilityID = IM.Key

data Ability = Ability
   { name :: String
   , desc :: String
   }
   deriving (Show, Eq, Ord)

----

findAbilityByID id =
  IM.lookup id abilities

findAbilityByName name =
  IM.assocs abilities & find \(id, ability) -> ability.name == name

----

abilities = IM.fromList $ zip [0..]
  [ Ability { name="Adaptability",     desc="Powers up moves of the same type."}
  , Ability { name="Aerilate",         desc="Turns Normal-type moves into Flying-type moves."}
  , Ability { name="Aftermath",        desc="Damages the attacker landing the finishing hit."}
  , Ability { name="Air Lock",         desc="Eliminates the effects of weather."}
  , Ability { name="Analytic",         desc="Boosts move power when the Pokémon moves last."}
  , Ability { name="Anger Point",      desc="Maxes Attack after taking a critical hit."}
  , Ability { name="Anger Shell",      desc="Lowers Defense/Sp. Def and raises Attack/Sp. Atk/Speed when HP drops below half."}
  , Ability { name="Anticipation",     desc="Senses a foe's dangerous moves."}
  , Ability { name="Arena Trap",       desc="Prevents the foe from fleeing."}
  , Ability { name="Armor Tail",       desc="Prevents opponent using priority moves."}
  , Ability { name="Aroma Veil",       desc="Protects allies from attacks that limit their move choices."}
  , Ability { name="As One",           desc="Combines Unnerve and Chilling Neigh/Grim N"}
  , Ability { name="Aura Break",       desc="Reduces power of Dark- and Fairy-type moves."}
  , Ability { name="Bad Dreams",       desc="Reduces a sleeping foe's HP."}
  , Ability { name="Ball Fetch",       desc="Retrieves a Poké Ball from a failed throw."}
  , Ability { name="Battery",          desc="Raises power of teammates' Special moves."}
  , Ability { name="Battle Armor",     desc="The Pokémon is protected against critical hits."}
  , Ability { name="Battle Bond",      desc="Transform into Ash-Greninja after causing opponent to faint."}
  , Ability { name="Beads of Ruin",    desc="Lowers Special Defense of all Pokémon except itself."}
  , Ability { name="Beast Boost",      desc="The Pokémon boosts its most proficient stat each time it knocks out a Pokémon."}
  , Ability { name="Berserk",          desc="Raises Special Attack when HP drops below half."}
  , Ability { name="Big Pecks",        desc="Protects the Pokémon from Defense-lowering attacks."}
  , Ability { name="Blaze",            desc="Powers up Fire-type moves in a pinch."}
  , Ability { name="Bulletproof",      desc="Protects the Pokémon from ball and bomb moves."}
  , Ability { name="Cheek Pouch",      desc="Restores additional HP when a Berry is consumed."}
  , Ability { name="Chilling Neigh",   desc="Boosts Attack after knocking out a Pokémon."}
  , Ability { name="Chlorophyll",      desc="Boosts the Pokémon's Speed in sunshine."}
  , Ability { name="Clear Body",       desc="Prevents other Pokémon from lowering its stats."}
  , Ability { name="Cloud Nine",       desc="Eliminates the effects of weather."}
  , Ability { name="Color Change",     desc="Changes the Pokémon's type to the foe's move."}
  , Ability { name="Comatose",         desc="The Pokémon is always asleep but can still attack."}
  , Ability { name="Commander",        desc="Goes inside the mouth of an ally Dondozo if one is on the field."}
  , Ability { name="Competitive",      desc="Sharply raises Special Attack when the Pokémon's stats are lowered."}
  , Ability { name="Compound Eyes",    desc="The Pokémon's accuracy is boosted."}
  , Ability { name="Contrary",         desc="Makes stat changes have an opposite effect."}
  , Ability { name="Corrosion",        desc="The Pokémon can poison Steel and Poison types."}
  , Ability { name="Costar",           desc="Copies ally's stat changes on entering battle."}
  , Ability { name="Cotton Down",      desc="Lowers foe's Speed when hit."}
  , Ability { name="Cud Chew",         desc="Can eat the same Berry twice."}
  , Ability { name="Curious Medicine", desc="Resets all stat changes upon entering battlefield."}
  , Ability { name="Cursed Body",      desc="May disable a move used on the Pokémon."}
  , Ability { name="Cute Charm",       desc="Contact with the Pokémon may cause infatuation."}
  , Ability { name="Damp",             desc="Prevents the use of self-destructing moves."}
  , Ability { name="Dancer",           desc="Copies the foe's Dance moves."}
  , Ability { name="Dark Aura",        desc="Raises power of Dark type moves for all Pokémon in battle."}
  , Ability { name="Dauntless Shield", desc="Boosts Defense in battle."}
  , Ability { name="Dazzling",         desc="Protects the Pokémon from high-priority moves."}
  , Ability { name="Defeatist",        desc="Lowers stats when HP drops below half."}
  , Ability { name="Defiant",          desc="Sharply raises Attack when the Pokémon's stats are lowered."}
  , Ability { name="Delta Stream",     desc="Creates strong winds when the ability activates."}
  , Ability { name="Desolate Land",    desc="Turns the sunlight extremely harsh when the ability activates."}
  , Ability { name="Disguise",         desc="Avoids damage for one turn."}
  , Ability { name="Download",         desc="Adjusts power according to a foe's defenses."}
  , Ability { name="Dragon's Maw",     desc="Powers up Dragon-type moves."}
  , Ability { name="Drizzle",          desc="The Pokémon makes it rain when it enters a battle."}
  , Ability { name="Drought",          desc="Turns the sunlight harsh when the Pokémon enters a battle."}
  , Ability { name="Dry Skin",         desc="Reduces HP if it is hot. Water restores HP."}
  , Ability { name="Early Bird",       desc="The Pokémon awakens quickly from sleep."}
  , Ability { name="Earth Eater",      desc="Restores HP when hit by a Ground-type move."}
  , Ability { name="Effect Spore",     desc="Contact may poison or cause paralysis or sleep."}
  , Ability { name="Electric Surge",   desc="The Pokémon creates an Electric Terrain when it enters a battle."}
  , Ability { name="Electromorphosis", desc="Doubles power of the next Electric-type move when hit by an attack."}
  , Ability { name="Emergency Exit",   desc="Switches out when HP falls below 50%"}
  , Ability { name="Fairy Aura",       desc="Raises power of Fairy type moves for all Pokémon in battle."}
  , Ability { name="Filter",           desc="Reduces damage from super-effective attacks."}
  , Ability { name="Flame Body",       desc="Contact with the Pokémon may burn the attacker."}
  , Ability { name="Flare Boost",      desc="Powers up special attacks when burned."}
  , Ability { name="Flash Fire",       desc="It powers up Fire-type moves if it's hit by one."}
  , Ability { name="Flower Gift",      desc="Powers up party Pokémon when it is sunny."}
  , Ability { name="Flower Veil",      desc="Prevents lowering of ally Grass-type Pokémon's stats."}
  , Ability { name="Fluffy",           desc="Halves damage from contact moves, but doubles damage from Fire-type moves."}
  , Ability { name="Forecast",         desc="Castform transforms with the weather."}
  , Ability { name="Forewarn",         desc="Determines what moves a foe has."}
  , Ability { name="Friend Guard",     desc="Reduces damage done to allies."}
  , Ability { name="Frisk",            desc="The Pokémon can check a foe's held item."}
  , Ability { name="Full Metal Body",  desc="Prevents other Pokémon from lowering its stats."}
  , Ability { name="Fur Coat",         desc="Reduces damage from physical moves."}
  , Ability { name="Gale Wings",       desc="Gives priority to Flying-type moves."}
  , Ability { name="Galvanize",        desc="Normal-type moves become Electric-type moves and their power boosted."}
  , Ability { name="Gluttony",         desc="Encourages the early use of a held Berry."}
  , Ability { name="Good as Gold",     desc="Gives immunity to status moves."}
  , Ability { name="Gooey",            desc="Contact with the Pokémon lowers the attacker's Speed stat."}
  , Ability { name="Gorilla Tactics",  desc="Boosts the Pokémon's Attack stat but only allows the use of the first selected move."}
  , Ability { name="Grass Pelt",       desc="Boosts the Defense stat in Grassy Terrain."}
  , Ability { name="Grassy Surge",     desc="The Pokémon creates a Grassy Terrain when it enters a battle."}
  , Ability { name="Grim Neigh",       desc="Boosts Special Attack after knocking out a Pokémon."}
  , Ability { name="Guard Dog",        desc="Boosts Attack if intimidated, and prevents being forced to switch out."}
  , Ability { name="Gulp Missile",     desc="Returns with a catch in its mouth after using Surf or Dive."}
  , Ability { name="Guts",             desc="Boosts Attack if there is a status problem."}
  , Ability { name="Hadron Engine",    desc="Creates an Electric Terrain when entering battle, and boosts Special Attack while active."}
  , Ability { name="Harvest",          desc="May create another Berry after one is used."}
  , Ability { name="Healer",           desc="May heal an ally's status conditions."}
  , Ability { name="Heatproof",        desc="Weakens the power of Fire-type moves."}
  , Ability { name="Heavy Metal",      desc="Doubles the Pokémon's weight."}
  , Ability { name="Honey Gather",     desc="The Pokémon may gather Honey from somewhere."}
  , Ability { name="Huge Power",       desc="Raises the Pokémon's Attack stat."}
  , Ability { name="Hunger Switch",    desc="Forms each turn."}
  , Ability { name="Hustle",           desc="Boosts the Attack stat, but lowers accuracy."}
  , Ability { name="Hydration",        desc="Heals status problems if it is raining."}
  , Ability { name="Hyper Cutter",     desc="Prevents other Pokémon from lowering Attack stat."}
  , Ability { name="Ice Body",         desc="The Pokémon gradually regains HP in a hailstorm."}
  , Ability { name="Ice Face",         desc="Avoids damage from Physical moves for one turn."}
  , Ability { name="Ice Scales",       desc="Halves damage from Special moves."}
  , Ability { name="Illuminate",       desc="Raises the likelihood of meeting wild Pokémon."}
  , Ability { name="Illusion",         desc="Enters battle disguised as the last Pokémon in the party."}
  , Ability { name="Immunity",         desc="Prevents the Pokémon from getting poisoned."}
  , Ability { name="Imposter",         desc="It transforms itself into the Pokémon it is facing."}
  , Ability { name="Infiltrator",      desc="Passes through the foe's barrier and strikes."}
  , Ability { name="Innards Out",      desc="Deals damage upon fainting."}
  , Ability { name="Inner Focus",      desc="The Pokémon is protected from flinching."}
  , Ability { name="Insomnia",         desc="Prevents the Pokémon from falling asleep."}
  , Ability { name="Intimidate",       desc="Lowers the foe's Attack stat."}
  , Ability { name="Intrepid Sword",   desc="Boosts Attack in battle."}
  , Ability { name="Iron Barbs",       desc="Inflicts damage to the Pokémon on contact."}
  , Ability { name="Iron Fist",        desc="Boosts the power of punching moves."}
  , Ability { name="Justified",        desc="Raises Attack when hit by a Dark-type move."}
  , Ability { name="Keen Eye",         desc="Prevents other Pokémon from lowering accuracy."}
  , Ability { name="Klutz",            desc="The Pokémon can't use any held items."}
  , Ability { name="Leaf Guard",       desc="Prevents problems with status in sunny weather."}
  , Ability { name="Levitate",         desc="Gives immunity to Ground type moves."}
  , Ability { name="Libero",           desc="Changes the Pokémon's type to its last used move."}
  , Ability { name="Light Metal",      desc="Halves the Pokémon's weight."}
  , Ability { name="Lightning Rod",    desc="Draws in all Electric-type moves to up Sp. Attack."}
  , Ability { name="Limber",           desc="The Pokémon is protected from paralysis."}
  , Ability { name="Lingering Aroma",  desc="Contact changes the attacker's Ability to Lingering Aroma."}
  , Ability { name="Liquid Ooze",      desc="Damages attackers using any draining move."}
  , Ability { name="Liquid Voice",     desc="All sound-based moves become Water-type moves."}
  , Ability { name="Long Reach",       desc="The Pokémon uses its moves without making contact with the target."}
  , Ability { name="Magic Bounce",     desc="Reflects status- changing moves."}
  , Ability { name="Magic Guard",      desc="Protects the Pokémon from indirect damage."}
  , Ability { name="Magician",         desc="The Pokémon steals the held item of a Pokémon it hits with a move."}
  , Ability { name="Magma Armor",      desc="Prevents the Pokémon from becoming frozen."}
  , Ability { name="Magnet Pull",      desc="Prevents Steel-type Pokémon from escaping."}
  , Ability { name="Marvel Scale",     desc="Ups Defense if there is a status problem."}
  , Ability { name="Mega Launcher",    desc="Boosts the power of aura and pulse moves."}
  , Ability { name="Merciless",        desc="The Pokémon's attacks become critical hits if the target is poisoned."}
  , Ability { name="Mimicry",          desc="Changes type depending on the terrain."}
  , Ability { name="Minus",            desc="Ups Sp. Atk if another Pokémon has Plus or Minus."}
  , Ability { name="Mirror Armor",     desc="Reflects any stat-lowering effects."}
  , Ability { name="Misty Surge",      desc="The Pokémon creates a Misty Terrain when it enters a battle."}
  , Ability { name="Mold Breaker",     desc="Moves can be used regardless of Abilities."}
  , Ability { name="Moody",            desc="Raises one stat and lowers another."}
  , Ability { name="Motor Drive",      desc="Raises Speed if hit by an Electric-type move."}
  , Ability { name="Moxie",            desc="Boosts Attack after knocking out any Pokémon."}
  , Ability { name="Multiscale",       desc="Reduces damage when HP is full."}
  , Ability { name="Multitype",        desc="Changes type to match the held Plate."}
  , Ability { name="Mummy",            desc="Contact with this Pokémon spreads this Ability."}
  , Ability { name="Mycelium Might",   desc="Status moves go last, but are not affected by the opponent's ability."}
  , Ability { name="Natural Cure",     desc="All status problems heal when it switches out."}
  , Ability { name="Neuroforce",       desc="Powers up moves that are super effective."}
  , Ability { name="Neutralizing Gas", desc="Neutralizes abilities of all Pokémon in battle."}
  , Ability { name="No Guard",         desc="Ensures attacks by or against the Pokémon land."}
  , Ability { name="Normalize",        desc="All the Pokémon's moves become the Normal type."}
  , Ability { name="Oblivious",        desc="Prevents it from becoming infatuated."}
  , Ability { name="Opportunist",      desc="Copies stat boosts by the opponent."}
  , Ability { name="Orichalcum Pulse", desc="Turns the sunlight harsh when entering battle, and boosts Attack while active."}
  , Ability { name="Overcoat",         desc="Protects the Pokémon from weather damage."}
  , Ability { name="Overgrow",         desc="Powers up Grass-type moves in a pinch."}
  , Ability { name="Own Tempo",        desc="Prevents the Pokémon from becoming confused."}
  , Ability { name="Parental Bond",    desc="Allows the Pokémon to attack twice."}
  , Ability { name="Pastel Veil",      desc="Prevents the Pokémon and its allies from being poisoned."}
  , Ability { name="Perish Body",      desc="When hit by a move that makes direct contact, the Pokémon and the attacker will faint after three turns unless they switch out of battle."}
  , Ability { name="Pickpocket",       desc="Steals an item when hit by another Pokémon."}
  , Ability { name="Pickup",           desc="The Pokémon may pick up items."}
  , Ability { name="Pixilate",         desc="Turns Normal-type moves into Fairy-type moves."}
  , Ability { name="Plus",             desc="Ups Sp. Atk if another Pokémon has Plus or Minus."}
  , Ability { name="Poison Heal",      desc="Restores HP if the Pokémon is poisoned."}
  , Ability { name="Poison Point",     desc="Contact with the Pokémon may poison the attacker."}
  , Ability { name="Poison Touch",     desc="May poison targets when a Pokémon makes contact."}
  , Ability { name="Power Construct",  desc="Changes form when HP drops below half."}
  , Ability { name="Power of Alchemy", desc="The Pokémon copies the Ability of a defeated ally."}
  , Ability { name="Power Spot",       desc="Just being next to the Pokémon powers up moves."}
  , Ability { name="Prankster",        desc="Gives priority to a status move."}
  , Ability { name="Pressure",         desc="The Pokémon raises the foe's PP usage."}
  , Ability { name="Primordial Sea",   desc="Makes it rain heavily when the ability activates."}
  , Ability { name="Prism Armor",      desc="Reduces damage from super-effective attacks."}
  , Ability { name="Propeller Tail",   desc="Ignores moves and abilities that draw in moves."}
  , Ability { name="Protean",          desc="Changes the Pokémon's type to its last used move."}
  , Ability { name="Protosynthesis",   desc="Raises highest stat in harsh sunlight, or if holding Booster Energy."}
  , Ability { name="Psychic Surge",    desc="The Pokémon creates a Psychic Terrain when it enters a battle."}
  , Ability { name="Punk Rock",        desc="Boosts sound-based moves and halves damage from the same moves."}
  , Ability { name="Pure Power",       desc="Raises the Pokémon's Attack stat."}
  , Ability { name="Purifying Salt",   desc="Protects from status conditions and halves damage from Ghost-type moves."}
  , Ability { name="Quark Drive",      desc="Raises highest stat on Electric Terrain, or if holding Booster Energy."}
  , Ability { name="Queenly Majesty",  desc="Prevents use of priority moves."}
  , Ability { name="Quick Draw",       desc="Quick Feet|Boosts Speed if there is a status problem."}
  , Ability { name="Rain Dish",        desc="The Pokémon gradually regains HP in rain."}
  , Ability { name="Rattled",          desc="Bug, Ghost or Dark type moves scare it and boost its Speed."}
  , Ability { name="Receiver",         desc="Inherits an ally's ability when it faints."}
  , Ability { name="Reckless",         desc="Powers up moves that have recoil damage."}
  , Ability { name="Refrigerate",      desc="Turns Normal-type moves into Ice-type moves."}
  , Ability { name="Regenerator",      desc="Restores a little HP when withdrawn from battle."}
  , Ability { name="Ripen",            desc="Doubles the effect of berries."}
  , Ability { name="Rivalry",          desc="Deals more damage to a Pokémon of same gender."}
  , Ability { name="RKS System",       desc="Changes type depending on held item."}
  , Ability { name="Rock Head",        desc="Protects the Pokémon from recoil damage."}
  , Ability { name="Rocky Payload",    desc="Powers up Rock-type moves."}
  , Ability { name="Rough Skin",       desc="Inflicts damage to the attacker on contact."}
  , Ability { name="Run Away",         desc="Enables a sure getaway from wild Pokémon."}
  , Ability { name="Sand Force",       desc="Boosts certain moves' power in a sandstorm."}
  , Ability { name="Sand Rush",        desc="Boosts the Pokémon's Speed in a sandstorm."}
  , Ability { name="Sand Spit",        desc="Creates a sandstorm when hit by an attack."}
  , Ability { name="Sand Stream",      desc="The Pokémon summons a sandstorm in battle."}
  , Ability { name="Sand Veil",        desc="Boosts the Pokémon's evasion in a sandstorm."}
  , Ability { name="Sap Sipper",       desc="Boosts Attack when hit by a Grass-type move."}
  , Ability { name="Schooling",        desc="Changes Wishiwashi to School Form."}
  , Ability { name="Scrappy",          desc="Enables moves to hit Ghost-type Pokémon."}
  , Ability { name="Screen Cleaner",   desc="Nullifies effects of Light Screen, Reflect, and Aurora Veil."}
  , Ability { name="Seed Sower",       desc="Turns the ground into Grassy Terrain when the Pokémon is hit by an attack."}
  , Ability { name="Serene Grace",     desc="Boosts the likelihood of added effects appearing."}
  , Ability { name="Shadow Shield",    desc="Reduces damage when HP is full."}
  , Ability { name="Shadow Tag",       desc="Prevents the foe from escaping."}
  , Ability { name="Sharpness",        desc="Powers up slicing moves."}
  , Ability { name="Shed Skin",        desc="The Pokémon may heal its own status problems."}
  , Ability { name="Sheer Force",      desc="Removes added effects to increase move damage."}
  , Ability { name="Shell Armor",      desc="The Pokémon is protected against critical hits."}
  , Ability { name="Shield Dust",      desc="Blocks the added effects of attacks taken."}
  , Ability { name="Shields Down",     desc="Changes stats when HP drops below half."}
  , Ability { name="Simple",           desc="Doubles all stat changes."}
  , Ability { name="Skill Link",       desc="Increases the frequency of multi-strike moves."}
  , Ability { name="Slow Start",       desc="Temporarily halves Attack and Speed."}
  , Ability { name="Slush Rush",       desc="Boosts the Pokémon's Speed stat in a hailstorm."}
  , Ability { name="Sniper",           desc="Powers up moves if they become critical hits."}
  , Ability { name="Snow Cloak",       desc="Raises evasion in a hailstorm."}
  , Ability { name="Snow Warning",     desc="The Pokémon summons a hailstorm in battle."}
  , Ability { name="Solar Power",      desc="In sunshine, Sp. Atk is boosted but HP decreases."}
  , Ability { name="Solid Rock",       desc="Reduces damage from super-effective attacks."}
  , Ability { name="Soul-Heart",       desc="Raises Special Attack when an ally faints."}
  , Ability { name="Soundproof",       desc="Gives immunity to sound-based moves."}
  , Ability { name="Speed Boost",      desc="Its Speed stat is gradually boosted."}
  , Ability { name="Stakeout",         desc="Deals double damage to Pokémon switching in."}
  , Ability { name="Stall",            desc="The Pokémon moves after all other Pokémon do."}
  , Ability { name="Stalwart",         desc="Ignores moves and abilities that draw in moves."}
  , Ability { name="Stamina",          desc="Raises Defense when attacked."}
  , Ability { name="Stance Change",    desc="Changes form depending on moves used."}
  , Ability { name="Static",           desc="Contact with the Pokémon may cause paralysis."}
  , Ability { name="Steadfast",        desc="Raises Speed each time the Pokémon flinches."}
  , Ability { name="Steam Engine",     desc="Drastically raises Speed when hit by a Fire- or Water-type move."}
  , Ability { name="Steelworker",      desc="Powers up Steel-type moves."}
  , Ability { name="Steely Spirit",    desc="Powers up ally Pokémon's Steel-type moves."}
  , Ability { name="Stench",           desc="The stench may cause the target to flinch."}
  , Ability { name="Sticky Hold",      desc="Protects the Pokémon from item theft."}
  , Ability { name="Storm Drain",      desc="Draws in all Water-type moves to up Sp. Attack."}
  , Ability { name="Strong Jaw",       desc="Boosts the power of biting moves."}
  , Ability { name="Sturdy",           desc="It cannot be knocked out with one hit."}
  , Ability { name="Suction Cups",     desc="Negates all moves that force switching out."}
  , Ability { name="Super Luck",       desc="Heightens the critical-hit ratios of moves."}
  , Ability { name="Supreme Overlord", desc="Attack and Special Attack are boosted for each party Pokémon that has been defeated."}
  , Ability { name="Surge Surfer",     desc="Doubles Speed during Electric Terrain."}
  , Ability { name="Swarm",            desc="Powers up Bug-type moves in a pinch."}
  , Ability { name="Sweet Veil",       desc="Prevents the Pokémon and allies from falling asleep."}
  , Ability { name="Swift Swim",       desc="Boosts the Pokémon's Speed in rain."}
  , Ability { name="Sword of Ruin",    desc="Lowers Defense of all Pokémon except itself."}
  , Ability { name="Symbiosis",        desc="The Pokémon can pass an item to an ally."}
  , Ability { name="Synchronize",      desc="Passes a burn, poison, or paralysis to the foe."}
  , Ability { name="Tablets of Ruin",  desc="Lowers Attack of all Pokémon except itself."}
  , Ability { name="Tangled Feet",     desc="Raises evasion if the Pokémon is confused."}
  , Ability { name="Tangling Hair",    desc="Contact with the Pokémon lowers the attacker's Speed stat."}
  , Ability { name="Technician",       desc="Powers up the Pokémon's weaker moves."}
  , Ability { name="Telepathy",        desc="Anticipates an ally's attack and dodges it."}
  , Ability { name="Teravolt",         desc="Moves can be used regardless of Abilities."}
  , Ability { name="Thermal Exchange", desc="Raises Attack when hit by a Fire-type move. Cannot be burned."}
  , Ability { name="Thick Fat",        desc="Ups resistance to Fire- and Ice-type moves."}
  , Ability { name="Tinted Lens",      desc="Powers up “not very effective” moves."}
  , Ability { name="Torrent",          desc="Powers up Water-type moves in a pinch."}
  , Ability { name="Tough Claws",      desc="Boosts the power of contact moves."}
  , Ability { name="Toxic Boost",      desc="Powers up physical attacks when poisoned."}
  , Ability { name="Toxic Debris",     desc="Scatters poison spikes at the feet of the opposing team when the Pokémon takes damage from physical moves."}
  , Ability { name="Trace",            desc="The Pokémon copies a foe's Ability."}
  , Ability { name="Transistor",       desc="Powers up Electric-type moves."}
  , Ability { name="Triage",           desc="Gives priority to restorative moves."}
  , Ability { name="Truant",           desc="Pokémon can't attack on consecutive turns."}
  , Ability { name="Turboblaze",       desc="Moves can be used regardless of Abilities."}
  , Ability { name="Unaware",          desc="Ignores any stat changes in the Pokémon."}
  , Ability { name="Unburden",         desc="Raises Speed if a held item is used."}
  , Ability { name="Unnerve",          desc="Makes the foe nervous and unable to eat Berries."}
  , Ability { name="Unseen Fist",      desc="Contact moves can strike through Protect/Detect."}
  , Ability { name="Vessel of Ruin",   desc="Lowers Special Attack of all Pokémon except itself."}
  , Ability { name="Victory Star",     desc="Boosts the accuracy of its allies and itself."}
  , Ability { name="Vital Spirit",     desc="Prevents the Pokémon from falling asleep."}
  , Ability { name="Volt Absorb",      desc="Restores HP if hit by an Electric-type move."}
  , Ability { name="Wandering Spirit", desc="Swaps abilities with opponents on contact."}
  , Ability { name="Water Absorb",     desc="Restores HP if hit by a Water-type move."}
  , Ability { name="Water Bubble",     desc="Halves damage from Fire-type moves, doubles power of Water-type moves used, and prevents burns."}
  , Ability { name="Water Compaction", desc="Sharply raises Defense when hit by a Water-type move."}
  , Ability { name="Water Veil",       desc="Prevents the Pokémon from getting a burn."}
  , Ability { name="Weak Armor",       desc="Physical attacks lower Defense and raise Speed."}
  , Ability { name="Well-Baked Body",  desc="Immune to Fire-type moves, and Defense is sharply boosted."}
  , Ability { name="White Smoke",      desc="Prevents other Pokémon from lowering its stats."}
  , Ability { name="Wimp Out",         desc="Switches out when HP drops below half."}
  , Ability { name="Wind Power",       desc="Doubles power of the next Electric-type move used, when hit by a wind move."}
  , Ability { name="Wind Rider",       desc="Takes no damage from wind moves, and boosts Attack if hit by one."}
  , Ability { name="Wonder Guard",     desc="Only supereffective moves will hit."}
  , Ability { name="Wonder Skin",      desc="Makes status-changing moves more likely to miss."}
  , Ability { name="Zen Mode",         desc="Changes form when HP drops below half."}
  , Ability { name="Zero to Hero",     desc="Transforms into its Hero Form when switching out."}
  ]

