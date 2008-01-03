(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*****
Return-Path: <ramsey@parc.xerox.com>
Received: by jumbo.pa.dec.com; id AA26413; Tue, 6 Aug 91 18:41:18 -0700
Received: by enet-gw.pa.dec.com; id AA26529; Tue, 6 Aug 91 18:41:09 -0700
Received: from worsel.parc.xerox.com ([13.2.116.58]) by alpha.xerox.com with SMTP id <12204>; Tue, 6 Aug 1991 18:41:06 PDT
Received: by worsel.parc.xerox.com id <35>; Tue, 6 Aug 1991 18:40:57 -0700
From: Norman Ramsey <ramsey@parc.xerox.com>
To: m3-request
Subject: Simpler demonstration of compiler bug
Message-Id: <91Aug6.184057pdt.35@worsel.parc.xerox.com>
Date: 	Tue, 6 Aug 1991 18:40:51 PDT

Here's a demonstration of what's wrong in a MUCH smaller program.
*********)

MODULE Main;

TYPE
  Proc = RECORD do_it: PROCEDURE(); END;

CONST
  procs = ARRAY OF Proc { Proc { Fish }, Proc { Fowl } };

PROCEDURE Fish() = BEGIN Fowl() END Fish;

PROCEDURE Fowl() = BEGIN END Fowl;

BEGIN
  Fish();
  procs[0].do_it ();
  procs[1].do_it ();
END Main.

(*************

And the compiler hits:

ramsey@worsel (702) % m3 -c -g Bug23.mc
"Bug23.m3", line 1: warning: undeclared initializer name _Bug23__Fish
"Bug23.m3", line 1: warning: illegal combination of pointer and integer, op =
"Bug23.m3", line 1: illegal initialization
"Bug23.m3", line 1: warning: undeclared initializer name _Bug23__Fowl
"Bug23.m3", line 1: warning: illegal combination of pointer and integer, op =
"Bug23.m3", line 1: illegal initialization
"Bug23.m3", line 1: redeclaration of _Bug23__Fish
"Bug23.m3", line 1: redeclaration of _Bug23__Fowl
"Bug23.m3", line 9: redeclaration of _Bug23__Fish
"Bug23.m3", line 9: illegal function
"Bug23.m3", line 9: illegal function
"Bug23.m3", line 11: redeclaration of _Bug23__Fowl
"Bug23.m3", line 14: illegal function
"Bug23.m3", line 14: illegal function

***********)
