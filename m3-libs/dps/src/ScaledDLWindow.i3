(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Mon Feb 10 17:52:19 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)



INTERFACE ScaledDLWindow;

IMPORT DLWindow, DPS;

TYPE T = DLWindow.T OBJECT 
  dx, dy: REAL := 0.0;
  scale: REAL := 1.0;
  rotation: REAL := 0.0; 
  OVERRIDES 
  Paint := Paint; 
  END; 

PROCEDURE Paint (t: T; box: DPS.Box; only: REFANY := NIL);

PROCEDURE InstallButtons (t: T);

  END ScaledDLWindow.



