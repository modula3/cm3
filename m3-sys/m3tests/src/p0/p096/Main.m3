(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Wed Nov  2 13:45:54 PST 1994 by kalsow    *)

(*---------------------------------------------------------------------------
Return-Path: <mjordan@src.dec.com>
Received: by jumbo.pa.dec.com; id AA14111; Tue, 29 Oct 91 11:19:30 -0800
From: mjordan (Mick Jordan)
Message-Id: <9110291919.AA14111@jumbo.pa.dec.com>
Date: Tue, 29 Oct 91 11:19:13 PST
To: kalsow
Subject: m3xx errors

---------------------------------------------------------------------------*)
MODULE Main;

CONST K = T.m; L = Q;
TYPE TProc = PROCEDURE(s: T);
CONST A = ARRAY OF TProc{Q};

TYPE T = OBJECT METHODS m() := L; END;
  ST1 = T OBJECT OVERRIDES m := K END;
  (*** ILLEGAL *** ST2 = T OBJECT OVERRIDES m := TProc END; ***)
  ST3 = T OBJECT OVERRIDES m := A[0]; END;

PROCEDURE Q(s: T) = BEGIN EVAL s END Q;

VAR t: T;  st1: ST1;  st3: ST3;
BEGIN
  EVAL t;  EVAL st1;  EVAL st3;
END Main.

(*---------------------------------------------------------------------------
"O7w.i3", line 7: default is not a procedure (m)
"O7w.i3", line 7: default is not a procedure (m)
"O7w.i3", line 8: default is not a procedure (m)
"O7w.i3", line 8: default is not a procedure (m)
"O7w.i3", line 9: default is not a procedure (m)
"O7w.i3", line 9: default is not a procedure (m)
"O7w.i3", line 10: default is not a procedure (m)
7 errors encountered

I think this is the full horror allowed by the report. Only ST2 is
illegal in the above, I beleve.

Mick
----------------------------------------------------------------------------*)

