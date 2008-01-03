(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(***************************************
Return-Path: <mjordan@src.dec.com>
Received: by jumbo.pa.dec.com; id AA08219; Tue, 19 Nov 91 17:22:03 -0800
From: mjordan (Mick Jordan)
Message-Id: <9111200122.AA08219@jumbo.pa.dec.com>
Date: Tue, 19 Nov 91 17:21:55 PST
To: kalsow
Subject: m3xx bug


In Main m3xx complains about the supertype (I.T) not being an object type.

INTERFACE I;

TYPE T <: REFANY;

END I.

***********************************************)

MODULE Main EXPORTS Main, I;

REVEAL T <: ROOT;

TYPE S = T OBJECT END;

REVEAL T = BRANDED OBJECT END;

BEGIN
  EVAL BITSIZE (S);
END Main.


