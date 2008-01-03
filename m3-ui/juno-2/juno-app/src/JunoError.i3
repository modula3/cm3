(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon May 30 20:55:34 PDT 1994 by heydon                   *)
(*      modified on Sat Oct 17 14:23:06 PST 1992 by gnelson                  *)
<* PRAGMA LL *>

INTERFACE JunoError;

IMPORT VBT, TextPort, Wr;

PROCEDURE P(
    tp: TextPort.T;
    errmsg: TEXT;
    start, finish: INTEGER := -1;
    time: VBT.TimeStamp := 0);
<* LL.sup = VBT.mu *>
(* Pop up an error window in "tp" containing the message "errmsg". If "start #
   -1 AND finish # -1", then select the text between "start" and "finish" in
   "tp" (in pending-replace mode), and scroll the window so this selection is
   visible; in this case, "time" is the timestamp of the event causing the
   error. Otherwise, "time" is ignored. *)

PROCEDURE Display(v: VBT.T; errmsg: TEXT); <* LL.sup < v *>
(* Display the error message to the user in a pop-up window above "v". *)

PROCEDURE DisplayPS(wr: Wr.T; errmsg: TEXT) RAISES {Wr.Failure};
(* Write the appropriate PostScript code to "wr" to display the error message
   "errmsg" in a box centered in the lower half of the output page. *)

END JunoError.
