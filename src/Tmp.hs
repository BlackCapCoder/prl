module Tmp where

import Pokemon.PokeAPI qualified as API
import Pokemon.Move as Pok
import Pokemon.Type as TYPE
import Data.Text qualified as T
import Data.List qualified as L
import Data.Bits ((.|.))



move2moveDesc :: API.Move -> Maybe Pok.MoveDesc
move2moveDesc move = L.find (\d -> d.name == n) Pok.moves
  where n = T.unpack $ moveName move

moveName move =
  fromMaybe move.name $ englishMoveName move

englishMoveName :: API.Move -> Maybe T.Text
englishMoveName move =
  L.find f move.names <&> \x -> x.name
  where
    f x = x.language.name == "en"

----

missingMoves = do
  api <- API.getPokeAPI

  forM_ api.moves \mov -> do
    case move2moveDesc mov of
      Nothing -> print $ englishMoveName mov
      Just _ -> pure ()

wrongPriority = do
  api <- API.getPokeAPI

  forM_ api.moves \mov -> do
    case move2moveDesc mov of
      Nothing -> pure ()
      Just md -> do
        when (mov.priority /= fi md.move.pri) do
        print (englishMoveName mov, mov.priority)

wrongPP = do
  api <- API.getPokeAPI

  forM_ api.moves \mov -> do
    case move2moveDesc mov of
      Nothing -> pure ()
      Just md -> do
        when (mov.pp /= md.pp) do
        print (englishMoveName mov, mov.pp)

wrongPwr = do
  api <- API.getPokeAPI

  forM_ api.moves \mov -> do
    case move2moveDesc mov of
      Nothing -> pure ()
      Just md -> do
        when (isJust mov.accuracy && mov.accuracy /= Just (fi md.move.acc)) do
        print (englishMoveName mov, fromJust mov.accuracy)

wrongAcc = do
  api <- API.getPokeAPI

  forM_ api.moves \mov -> do
    case move2moveDesc mov of
      Nothing -> pure ()
      Just md -> do
        when (isJust mov.power && mov.power /= Just (fi md.move.pow)) do
        print (englishMoveName mov, mov.power)

wrongType = do
  api <- API.getPokeAPI

  forM_ api.moves \mov -> do
    case move2moveDesc mov of
      Nothing -> pure ()
      Just md -> do
        let ty = maybe NON (\t -> TYPE.typeFromName t.name) mov._type
        when (ty /= md.move.ty) do
        print (englishMoveName mov, ty)

wrongCat = do
  api <- API.getPokeAPI

  forM_ api.moves \mov -> do
    case move2moveDesc mov of
      Nothing -> pure ()
      Just md -> do
        let cat = case mov.damage_class.name of
              "physical" -> Pok.Physical
              "special"  -> Pok.Special
              _          -> Pok.Status
        when (cat /= md.move.cat) do
        print (englishMoveName mov, cat)

wrongTarget = do
  api <- API.getPokeAPI

  forM_ api.moves \mov -> do
    case move2moveDesc mov of
      Nothing -> pure ()
      Just md -> do
        let targ = case mov.target.name of
              "specific-move"             -> Nothing
              "selected-pokemon-me-first" -> Nothing
              "ally"                      -> Just Pok.ALLIES
              "users-field"               -> Just Pok.SELF
              "user-or-ally"              -> Just $ Pok.ALLIES .|. Pok.SELF
              "opponents-field"           -> Just $ Pok.FOES .|. Pok.WIDE
              "user"                      -> Just Pok.SELF
              "random-opponent"           -> Just $ Pok.ADJFOES .|. Pok.RANDOM
              "all-other-pokemon"         -> Just $ Pok.ADJACENT .|. WIDE
              "selected-pokemon"          -> Just Pok.ADJACENT
              "all-opponents"             -> Just $ Pok.ADJFOES .|. WIDE
              "entire-field"              -> Just $ Pok.ALL .|. WIDE
              "all-pokemon"               -> Just $ Pok.ALL .|. WIDE
              "all-allies"                -> Just $ Pok.ALLIES .|. Pok.SELF .|. WIDE
              "user-and-allies"           -> Just $ Pok.ALLIES .|. Pok.SELF .|. WIDE
              "fainting-pokemon"          -> Nothing
              x                           -> error $ show x

        when (isJust targ && targ /= Just md.move.targ) do
        print (englishMoveName mov, mov.target.name)


