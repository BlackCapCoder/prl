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
  '~'  -> Water
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
  [ "#####......#############################################"
  , "~    ......              HHHHHHH   HHHHHHH             #"
  , "~    ......              HHH+HHH   HHH+HHH             #"
  , "~    ......                                            #"
  , "~    ......                                            #"
  , "~    ......                                            #"
  , "~                                                      #"
  , "~                                                       "
  , "~                                                       "
  , "~                                                       "
  , "~                                                      #"
  , "~                                                      #"
  , "~                                                      #"
  , "~                                                      #"
  , "~                                                      #"
  , "~                                                      #"
  , "########################################################"
  ] & placeMap 55 7 (Portal ' ' (Pos 1 (V2 0 9 )))
    & placeMap 55 8 (Portal ' ' (Pos 1 (V2 0 10)))
    & placeMap 55 9 (Portal ' ' (Pos 1 (V2 0 11)))
    & placeMap 38 2 (Portal '+' (Pos 8 (V2 11 11)))
    & placeMap 28 2 (Portal '+' (Pos 9 (V2 8 6)))

    & placeMap 5  0 (Portal '.' (Pos 10 (V2 15 41)))
    & placeMap 6  0 (Portal '.' (Pos 10 (V2 16 41)))
    & placeMap 7  0 (Portal '.' (Pos 10 (V2 17 41)))
    & placeMap 8  0 (Portal '.' (Pos 10 (V2 18 41)))
    & placeMap 9  0 (Portal '.' (Pos 10 (V2 19 41)))
    & placeMap 10 0 (Portal '.' (Pos 10 (V2 20 41)))

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

m10 = parse'
  [ "#         ###############################"
  , "#         ##########                    #"
  , "#         ##########                    #"
  , "#         ##########    HHHHHHHHHHHHH   #"
  , "#        @##########    HHHHHHHHHHHHH   #"
  , "#         ##########    HHHHHH+HHHHHH   #"
  , "#                                       #"
  , "#         ##########                    #"
  , "#'''''''''##########                    #"
  , "#'''''''''##########                    #"
  , "#'''''''''##########       ########     #"
  , "#'''''''''##########       ########     #"
  , "#'''''''''########## ''''' ########'''''#"
  , "#'''''''''########## ''''' ########'''''#"
  , "#'''''''''########## ''''' ########'''''#"
  , "#         ########## ''''' ########'''''#"
  , "#         ##########       ########'''''#"
  , "#         ##########       ########'''''#"
  , "#@        ##########                    #"
  , "#         ##########                    #"
  , "#         ##########                    #"
  , "#HHHHHH   ########################      #"
  , "#         ########################      #"
  , "#        @########################      #"
  , "#         ########################      #"
  , "#   HHHHHH########################      #"
  , "#                                       #"
  , "#                                       #"
  , "#                                       #"
  , "#HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH      #"
  , "#        HHHHHHHHHHH                    #"
  , "#  o  o  HHHHHHHHHHH  o                 #"
  , "#        HHHHH+HHHHH                    #"
  , "#                                       #"
  , "#                                       #"
  , "#HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH       #"
  , "#                          '''''''''''''#"
  , "#              ......      '''''''''''''#"
  , "#              ......      '''''''''''''#"
  , "###############......####################"
  , "              #......#                   "
  , "              #......#                   "
  ] & placeMap 15 41 (Portal '.' (Pos 7  (V2 5  0 )))
    & placeMap 16 41 (Portal '.' (Pos 7  (V2 6  0 )))
    & placeMap 17 41 (Portal '.' (Pos 7  (V2 7  0 )))
    & placeMap 18 41 (Portal '.' (Pos 7  (V2 8  0 )))
    & placeMap 19 41 (Portal '.' (Pos 7  (V2 9  0 )))
    & placeMap 20 41 (Portal '.' (Pos 7  (V2 10 0 )))

    & placeMap 1  0  (Portal ' ' (Pos 11 (V2 23 13)))
    & placeMap 2  0  (Portal ' ' (Pos 11 (V2 24 13)))
    & placeMap 3  0  (Portal ' ' (Pos 11 (V2 25 13)))
    & placeMap 4  0  (Portal ' ' (Pos 11 (V2 26 13)))
    & placeMap 5  0  (Portal ' ' (Pos 11 (V2 27 13)))
    & placeMap 6  0  (Portal ' ' (Pos 11 (V2 28 13)))
    & placeMap 7  0  (Portal ' ' (Pos 11 (V2 29 13)))
    & placeMap 8  0  (Portal ' ' (Pos 11 (V2 30 13)))
    & placeMap 9  0  (Portal ' ' (Pos 11 (V2 31 13)))

m11 = parse'
  [ "##########################################################"
  , "#HH             H                          HHHHHHHHHHHHHH#"
  , "#HH             H              @o     o    HHHHHHHHHHHHHH#"
  , "#H+             H                          HHHHHHHH HHHHH#"
  , "#HH             H                                        #"
  , "#HH             H                                        #"
  , "#               H                                        #"
  , "#               H                                        #"
  , "#HHH    HHHHHHHHH     HHHHHHHHHHHHHHHHHHHH      HHHHHHHHH#"
  , "#'''''''''''''''''''''H                                  #"
  , "#'''''''''''''''''''''H                                  #"
  , "#'''''''''''''''''''''H                                  #"
  , "#'''''''''''''''''''''H                                  #"
  , "#######################         ##########################"
  ] & placeMap 23 13 (Portal ' ' (Pos 10 (V2 1 0)))
    & placeMap 24 13 (Portal ' ' (Pos 10 (V2 2 0)))
    & placeMap 25 13 (Portal ' ' (Pos 10 (V2 3 0)))
    & placeMap 26 13 (Portal ' ' (Pos 10 (V2 4 0)))
    & placeMap 27 13 (Portal ' ' (Pos 10 (V2 5 0)))
    & placeMap 28 13 (Portal ' ' (Pos 10 (V2 6 0)))
    & placeMap 29 13 (Portal ' ' (Pos 10 (V2 7 0)))
    & placeMap 30 13 (Portal ' ' (Pos 10 (V2 8 0)))
    & placeMap 31 13 (Portal ' ' (Pos 10 (V2 9 0)))

    & placeMap 2  3 (Portal '+' (Pos 12 (V2 24 4)))

m12 = parse'
  [ "#########################"
  , "#           @           #"
  , "#HHHHHHHHHHHHHHHHHHHHHHH#"
  , "#                       #"
  , "+.......................+"
  , "#                       #"
  , "#HHHHHHHHHHHHHHHHHHHHHHH#"
  , "#                       #"
  , "#########################"
  ] & placeMap 24 4 (Portal '+' (Pos 11 (V2 2  3 )))
    & placeMap 0  4 (Portal '+' (Pos 13 (V2 68 17)))

m13 = parse'
  [ "##############################  ....  #                                "
  , "#               HHHHHHHHHHH  #  ....  #                                "
  , "#    HHHHHHH    HHHHHHHHHHH  #  ....  #                                "
  , "#    HHH+HHH    HHHHH+HHHHH  #  ....  #################################"
  , "#HH     .            .          ....                                  #"
  , "#HH ................................                        HHHHHHHHH #"
  , "#H+.................................                        HHHHHHHHH #"
  , "#HH ................................                        HHHH+HHHH #"
  , "#HH                             ....                            .     #"
  , "#                               ....................................  #"
  , "#                               ....................................  #"
  , "#                               ....................................  #"
  , "#                               ....                 .....            #"
  , "#                               .... HHHHHHHHHHHHHH  .....  HHHHHHHHHH#"
  , "#                               .... H               .....            #"
  , "#                               .... H   HHHHHHH     .....          HH#"
  , "#                               .... H   HHH+HHH     .............. HH#"
  , "#                               .... H      .        ...............+H#"
  , "#                               .... H      ....................... HH#"
  , "#                               .... H                              HH#"
  , "############################### .... H#################################"
  ] & placeMap 68 17 (Portal '+' (Pos 12 (V2 0  4 )))
    & placeMap 64 7  (Portal '+' (Pos 14 (V2 11 11)))
    & placeMap 8  3  (Portal '+' (Pos 15 (V2 8  6 )))

    & placeMap 31 20 (Portal ' ' (Pos 16 (V2 31 0 )))
    & placeMap 32 20 (Portal '.' (Pos 16 (V2 32 0 )))
    & placeMap 33 20 (Portal '.' (Pos 16 (V2 33 0 )))
    & placeMap 34 20 (Portal '.' (Pos 16 (V2 34 0 )))
    & placeMap 35 20 (Portal '.' (Pos 16 (V2 35 0 )))
    & placeMap 36 20 (Portal ' ' (Pos 16 (V2 36 0 )))

m14 = parse'
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
  ] & placeMap 11 11 (Portal '+' (Pos 13 (V2 64 7)))

m15 = parse'
  [ "##################"
  , "#   H            #"
  , "#  @H            #"
  , "#   H            #"
  , "#HHHH            #"
  , "#                #"
  , "########+#########"
  ] & placeMap 8 6 (Portal '+' (Pos 13 (V2 8 3)))

m16 = parse'
  [ "                             ## .... ##"
  , "                             #  ....  #"
  , "                             #  ....  #"
  , "                             #  ....  #"
  , "                             #  ....  #"
  , "                             #  ....  #"
  , "                             #  ....  #"
  , "                             #  ....  #"
  , "                             #  ....  #"
  , "##############################  ....  #"
  , "#                               ....  #"
  , "#H .................................  #"
  , "#+..................................  #"
  , "#H .................................  #"
  , "#                               ....  #"
  , "############################### .... ##"
  , "                              #@.... # "
  , "                              # .... # "
  ] & placeMap 31 0  (Portal ' ' (Pos 13 (V2 31 20)))
    & placeMap 32 0  (Portal '.' (Pos 13 (V2 32 20)))
    & placeMap 33 0  (Portal '.' (Pos 13 (V2 33 20)))
    & placeMap 34 0  (Portal '.' (Pos 13 (V2 34 20)))
    & placeMap 35 0  (Portal '.' (Pos 13 (V2 35 20)))
    & placeMap 36 0  (Portal ' ' (Pos 13 (V2 36 20)))

    & placeMap 31 17 (Portal ' ' (Pos 17 (V2 31 0 )))
    & placeMap 32 17 (Portal '.' (Pos 17 (V2 32 0 )))
    & placeMap 33 17 (Portal '.' (Pos 17 (V2 33 0 )))
    & placeMap 34 17 (Portal '.' (Pos 17 (V2 34 0 )))
    & placeMap 35 17 (Portal '.' (Pos 17 (V2 35 0 )))
    & placeMap 36 17 (Portal ' ' (Pos 17 (V2 36 0 )))

m17 = parse'
  [ "        ####################### .... #"
  , "        #                     H .... #"
  , "        #                     H .... #"
  , "        #                     H      #"
  , "        #                     H '''''#"
  , "        #                     H '''''#"
  , "        #                     H      #"
  , "        #                     H''''' #"
  , "        #         ....        H''''' #"
  , "#########HHHHHHHH .... HHHHHHHH      #"
  , "#                 ....               #"
  , "#                 ....               #"
  , "#    ...................  '''''''''''#"
  , "#    ...................  '''''''''''#"
  , "#    ....           ....  '''''''''''#"
  , "#    ....    HHHHHH~====~HHHHHHHHHHHH#"
  , "#''''''''''''H~~~~~~====~~~~~~~~~~~~~~"
  , "#''''''''''''H~~~~~~====~~~~~~~~~~~~~~"
  , "#''''''''''''H~~~~~~===============@~~"
  , "#''''''''''''H~~~~~~================~~"
  , "#''''''''''''H~~~~~~~~~~~~~~~====~~~~~"
  , "#''''''''''''H~~~~~~~~~~~~~~~====~~~~~"
  , "#''''''''''''H~~~~~~==@==========~~~~~"
  , "#''''''''''''H~~~~~~=============~~~~~"
  , "#''''''''''''H~~~~~~~~~~~~~~~====~~~~~"
  , "#''''''''''''H~~~~~~~~~~~~~~~====~~~~~"
  , "#''''''''''''H~~~~~~~~~~~~~~~====~~~~~"
  , "#''''''''''''H~~~~~~~~~~~~~~~====~~~~~"
  , "#    ....    HHHHHHHHHHHHHHH~====~HHH#"
  , "#    ....                    ....    #"
  , "#    ............................    #"
  , "#    ............................    #"
  , "#                ....                #"
  , "#HHHHHHHHHHHHHHH .... HHHHHHHHHHHHHHH#"
  , "#HHHHHHHHHHHHHHH .... HHHHHHHHHHHHHHH#"
  , "#HHHHHHHHHHHHHHH .... HHHHHHHHHHHHHHH#"
  , "#HHHHHHHHHHHHHHH .... HHHHHHHHHHHHHHH#"
  , "#HHHHHHHHHHHHHHH .... HHHHHHHHHHHHHHH#"
  , "#HHHHHHHHHHHHHHH .... HHHHHHHHHHHHHHH#"
  , "#HHHHHHHHHHHHHHH ....                #"
  , "#HHHHHHHHHHHHHHH ....       HHHHHHH  #"
  , "#HHHHHHHHHHHHHHH ....       HHH+HHH  #"
  , "#HHHHHH+HHHHHHHH ....          .     #"
  , "#      .       H ................    #"
  , "#      .       H ................    #"
  , "#      .       H     ....            #"
  , "#      .       H     ....            #"
  , "#      .       H     ....            #"
  , "#      .             ....            #"
  , "#      ..................            #"
  , "#                                    #"
  , "######################################"
  ] & placeMap 31 0  (Portal ' ' (Pos 16 (V2 31 17)))
    & placeMap 32 0  (Portal '.' (Pos 16 (V2 32 17)))
    & placeMap 33 0  (Portal '.' (Pos 16 (V2 33 17)))
    & placeMap 34 0  (Portal '.' (Pos 16 (V2 34 17)))
    & placeMap 35 0  (Portal '.' (Pos 16 (V2 35 17)))
    & placeMap 36 0  (Portal ' ' (Pos 16 (V2 36 17)))

    & placeMap 31 41 (Portal '+' (Pos 18 (V2 11 11)))
    & placeMap 7  42 (Portal '+' (Pos 19 (V2 18 4 )))

m18 = parse'
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
  ] & placeMap 11 11 (Portal '+' (Pos 17 (V2 31 41)))

m19 = parse'
  [ "#######################"
  , "#                     #"
  , "#                     #"
  , "#                     #"
  , "#    ~~~~~~~~~~HHH+HHH#"
  , "#    ~~~~~~~~~~HHHHHHH#"
  , "#    ~~~~~~~~~~HHHHHHH#"
  , "#                @    #"
  , "#                     #"
  , "#~~~~~~~~~~~~~~~~~    #"
  , "#~~~~~~~~~~~~~~~~~    #"
  , "#~~~~~~~~~~~~~~~~~   @#"
  , "#                     #"
  , "#                     #"
  , "#HHHHHHH    HHH     HH#"
  , "#H     H    HHH     HH#"
  , "#H  >  H    HHH     HH#"
  , "#H     H    HHH     HH#"
  , "#HHH HHH    HHHHHHHHHH#"
  , "#             @       #"
  , "#                     #"
  , "#~~~~~~~~~~~~~~~~~~   #"
  , "#~~~~~~~~~~~~~~~~~~  @#"
  , "#~~~~~~~~~~~~~~~~~~   #"
  , "#~~~~~~~~~~~~~~~~~~   #"
  , "####################+##"
  ] & placeMap 18 4  (Portal '+' (Pos 17 (V2 7  42)))
    & placeMap 20 25 (Portal '+' (Pos 20 (V2 44 3)))

m20 = parse'
  [ "#################################################"
  , "#         H           H~~~~~~~~~~~H    HHHHHHHHH#"
  , "# HHHHHHH H           H~~HHHHHHH~~H    HHHHHHHHH#"
  , "# HHH+HHH H  HHHHHHH  H~~HHH>HHH~~H    HHHHH+HHH#"
  , "#    .    H  HHH+HHH  H~~HHH.HHH~~H         .   #"
  , "#HHHH.HHHHH     .     H~~~~~.~~~~~H      ....   #"
  , "#    .          .           .     H      ...    #"
  , "#HH  .          ................  HHHHHH ... HHH#"
  , "#H+..............   .       ....         ...    #"
  , "#HH                 .    H  ................    #"
  , "#     HHHHHHHHHH    .    H  ................    #"
  , "#     HHHHHHHHHH    . HH H                      #"
  , "#     HHHH+HHHHH    ..+H H                      #"
  , "#         .         . HH H  ''''''''''''''''''' #"
  , "#         ...........    H  ''''''''''''''''''' #"
  , "#                        ########################"
  , "##########################                       "
  ] & placeMap 44 3  (Portal '+' (Pos 19 (V2 20 25)))


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
  & insertMap m10
  & insertMap m11
  & insertMap m12
  & insertMap m13
  & insertMap m14
  & insertMap m15
  & insertMap m16
  & insertMap m17
  & insertMap m18
  & insertMap m19
  & insertMap m20

