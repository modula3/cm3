(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jun 16 17:07:20 PDT 1997 by heydon                   *)
<* PRAGMA LL *>

INTERFACE JunoWM;

(* Procedures for handling unsafe window management actions. *)

IMPORT VBT, TrestleComm;

EXCEPTION Error(TEXT);

PROCEDURE Install(w: VBT.T; disp, geom: TEXT; applName: TEXT)
  RAISES {Error, TrestleComm.Failure};
  <* LL.sup < VBT.mu *>
(* Install the window "w" according on the display "disp", with geometry
   "geom". Either "disp" or "geom" may be NIL to indicate that the
   corresponding command-line option was not supplied. The window is decorated
   with the application name "applName". Raises "Error" if there is an error
   parsing either "disp" or "geom"; raises "TrestleComm.Failure" if there is
   an error installing the window "w". *)

END JunoWM.
