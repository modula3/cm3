(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Sat Feb 25 23:24:02 PST 1995 by msm *)
(* modified on Mon Feb 24 13:59:46 PST 1992 by muller *)
<*PRAGMA LL*>

INTERFACE XConfCtl;

(* Module which parses conference control messages, and acts on them. *)

IMPORT VBT;

PROCEDURE Process(v: VBT.T);
  (* Fetch the conference control property on v's X window, and set v's app
     state accordingly. *)
  
END XConfCtl. 

