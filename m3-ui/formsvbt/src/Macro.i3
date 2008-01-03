(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun  3 18:35:41 PDT 1993 by meehan                   *)
(*      modified on Tue Jun 16 21:55:37 PDT 1992 by muller                   *)

INTERFACE Macro;

IMPORT FormsVBT, RefList;

TYPE
  T <: Public;
  Public = OBJECT
           METHODS
             apply (actuals: RefList.T): REFANY RAISES {FormsVBT.Error}
           END;

PROCEDURE Parse (exp: RefList.T): T RAISES {FormsVBT.Error};

PROCEDURE Init ();

END Macro.
