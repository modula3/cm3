(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Mon Oct 26 16:55:02 PST 1992 by msm *)
(* modified on Mon Feb 24 13:59:46 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE INTERFACE XInput;

(* Module which accepts input from X, and filters the result onto a local
   event queue *)

IMPORT XClient;

PROCEDURE Start (trsl: XClient.T; stackSize := 20000);
(* for the given XClient, fork a thread which checks when input from X has
   arrived.  Then fork another thread which accepts the input, and filters
   the result onto the input queue in the XClient *)

END XInput.

