(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "Focus" interface describes the positioning of a window's focus
   on the board.  
*)

INTERFACE Focus;

IMPORT PointR;

TYPE T = REF RECORD
    offset: PointR.T;
    scale: REAL;
  END;

END Focus.
