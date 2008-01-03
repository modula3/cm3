(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

INTERFACE HumanPlayer;

IMPORT MinimaxAlgClass, GamePlay;

TYPE InteractivePlayer <: GamePlay.Player;

PROCEDURE DoHumanSelection (alg           : MinimaxAlgClass.T;
                            xCoord, yCoord: INTEGER);

PROCEDURE InitModule(alg:MinimaxAlgClass.T);


END HumanPlayer.
