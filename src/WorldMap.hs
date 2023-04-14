module WorldMap where

import Tile
import V2
import Map
import Data.Vector qualified as V


toTile = \case
  '#'  -> Solid
  'H'  -> Wall
  'C'  -> PC
  '\'' -> Grass
  ' '  -> Empty
  c    -> Chr c

parse str = Map
  { content = V.fromList $ map toTile (concat ls)
  -- , seen    = V.replicate (w*h) False
  , stride  = w
  }
  where
    ls = lines str
    w  = maximum $ map length ls
    h  = length ls

parse' = parse . unlines

----

m0 = parse'
  [ "#########################"
  , "#                       #"
  , "# HHHHHHHHHHHHH HHHHHHH #"
  , "# HHHHHHHHHHHHH HHHHHHH #"
  , "# HHHHHH+HHHHHH HHH+HHH #"
  , "#       .          .    #"
  , "#       .          .    #"
  , "....................... #"
  , "....................... #"
  , "....................... #"
  , "#                       #"
  , "#                       #"
  , "#########################"
  ] & placeMap 0  7 (Portal '.' (Pos 1 (V2 55 3 )))
    & placeMap 0  8 (Portal '.' (Pos 1 (V2 55 4 )))
    & placeMap 0  9 (Portal '.' (Pos 1 (V2 55 5 )))
    & placeMap 19 4 (Portal '+' (Pos 2 (V2 7  6 )))
    & placeMap 8  4 (Portal '+' (Pos 3 (V2 9  11)))

m1 = parse'
  [ "########################################################"
  , "#            H   HHHHHHHHHHHHH    H                    #"
  , "#     o      H   HHHHHHHHHHHHH    H                    #"
  , "#            H   HHHHHH+HHHHHH    H      ..............."
  , "#            H                    H      ..............."
  , "#HHHHH HHHHHHH                    H      ..............."
  , "#                                 H      ....          #"
  , "#                                 H      ....          #"
  , "#HHHHHH          HHHHHHHHHHHH     H''''''''''''''''''''#"
  , " ''''''''''''''''H                H''''''''''''''''''''#"
  , " ''''''''''''''''H                H''''''''''''''''''''#"
  , " ''''''''''''''''H                H''''''''''''''''''''#"
  , "#HHHHHHHHHHHHHHHHH     HHHHHHHHHHHH''''''''''''''''''''#"
  , "#                                  ''''''''''''''''''''#"
  , "#                                  ''''''''''''''''''''#"
  , "#                                  ''''''''''''''''''''#"
  , "########################################################"
  ] & placeMap 55 3 (Portal '.' (Pos 0 (V2 0 7)))
    & placeMap 55 4 (Portal '.' (Pos 0 (V2 0 8)))
    & placeMap 55 5 (Portal '.' (Pos 0 (V2 0 9)))
    & placeMap 23 3 (Portal '+' (Pos 5 (V2 9 12)))
    & placeMap 0  9 (Portal ' ' (Pos 7 (V2 55 7)))
    & placeMap 0 10 (Portal ' ' (Pos 7 (V2 55 8)))
    & placeMap 0 11 (Portal ' ' (Pos 7 (V2 55 9)))

m5 = parse'
  [ "#########+#########"
  , "#  H     .     H  #"
  , "#  H     .     H  #"
  , "#  H     .     H  #"
  , "#  H     .     H  #"
  , "#  H     .     H  #"
  , "# @H     .     H  #"
  , "#  H     .     H  #"
  , "#  H     .     H  #"
  , "#  H     .     H  #"
  , "#  H     .     H  #"
  , "#  H     .     H  #"
  , "#########+#########"
  ] & placeMap 9 12 (Portal '+' (Pos 1 (V2 23 3 )))
    & placeMap 9 0  (Portal '+' (Pos 6 (V2 12 10)))

m6 = parse'
  [ "###############################################"
  , "#                                             #"
  , "#                                             #"
  , "#                                             #"
  , "#                           ''''''''''''      #"
  , "#                           ''''''''''''      #"
  , "#                           ''''''''''''      #"
  , "#                           ''''''''''''      #"
  , "#                           ''''''''''''      #"
  , "#                           ''''''''''''      #"
  , "#       HHHH+HHHH                             #"
  , "#       HHHHHHHHH                             #"
  , "###############################################"
  ] & placeMap 12 10 (Portal '+' (Pos 5 (V2 9 0)))

m2 = parse'
  [ "###############"
  , "#          >###"
  , "#          >###"
  , "#             #"
  , "#             #"
  , "#             #"
  , "#######+#######"
  ] & placeMap 7  6 (Portal '+' (Pos 0 (V2 19 4)))
    & placeMap 11 1 (Portal '>' (Pos 4 (V2 11 1)))
    & placeMap 11 2 (Portal '>' (Pos 4 (V2 11 2)))

m4 = parse'
  [ "###############"
  , "#  =C=   ##<  #"
  , "#        ##<  #"
  , "#             #"
  , "#             #"
  , "#             #"
  , "###############"
  ] & placeMap 11 1 (Portal '<' (Pos 2 (V2 11 1)))
    & placeMap 11 2 (Portal '<' (Pos 2 (V2 11 2)))

m3 = parse'
  [ "###################"
  , "#                 #"
  , "#        @        #"
  , "#           =ooo= #"
  , "#                 #"
  , "#                 #"
  , "#HHHHHH     HHHHHH#"
  , "#                 #"
  , "#                 #"
  , "#                 #"
  , "#                 #"
  , "#########+#########"
  ] & placeMap 9 11 (Portal '+' (Pos 0 (V2 8 4)))

m7 = parse'
  [ "########################################################"
  , "#                        HHHHHHH   HHHHHHH             #"
  , "#                        HHH+HHH   HHH+HHH             #"
  , "#                                                      #"
  , "#                                                      #"
  , "#                                                      #"
  , "#                                                      #"
  , "#                                                       "
  , "#                                                       "
  , "#                                                       "
  , "#                                                      #"
  , "#                                                      #"
  , "#                                                      #"
  , "#                                                      #"
  , "#                                                      #"
  , "#                                                      #"
  , "########################################################"
  ] & placeMap 55 7 (Portal ' ' (Pos 1 (V2 0 9 )))
    & placeMap 55 8 (Portal ' ' (Pos 1 (V2 0 10)))
    & placeMap 55 9 (Portal ' ' (Pos 1 (V2 0 11)))
    & placeMap 38 2 (Portal '+' (Pos 8 (V2 11 11)))
    & placeMap 28 2 (Portal '+' (Pos 9 (V2 8 6)))

m8 = parse'
  [ "#######################"
  , "#     H    @    H  C  #"
  , "#     HHHHHHHHHHH     #"
  , "#                     #"
  , "#                     #"
  , "#                     #"
  , "#                     #"
  , "#                     #"
  , "#                     #"
  , "#                     #"
  , "#                     #"
  , "###########+###########"
  ] & placeMap 11 11 (Portal '+' (Pos 7 (V2 38 2)))

m9 = parse'
  [ "##################"
  , "#   H            #"
  , "#  @H            #"
  , "#   H            #"
  , "#HHHH            #"
  , "#                #"
  , "########+#########"
  ] & placeMap 8 6 (Portal '+' (Pos 7 (V2 28 2)))


worldMap
  = emptyMaps
  & insertMap m0
  & insertMap m1
  & insertMap m2
  & insertMap m3
  & insertMap m4
  & insertMap m5
  & insertMap m6
  & insertMap m7
  & insertMap m8
  & insertMap m9

