(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Wed Oct 21 16:46:17 PDT 1992 by msm *)
(* modified on Mon Feb 24 13:59:46 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE INTERFACE XMessenger;

IMPORT XClient;

PROCEDURE Start (trsl: XClient.T; stackSize := 20000);

END XMessenger.

