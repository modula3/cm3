(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun May  1 16:58:44 PDT 1994 by heydon                   *)

INTERFACE JunoRect;

(* A dummy interface for defining the type "JunoRect.T". *)

IMPORT JunoValue;

TYPE T = RECORD west, east, north, south: JunoValue.Real END;

END JunoRect.
