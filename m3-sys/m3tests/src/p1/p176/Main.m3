(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(****************************
Return-Path: Mike_Spreitzer.PARC@xerox.com
Received: by jumbo.pa.dec.com; id AA02578; Tue, 22 Sep 92 12:37:45 -0700
Received: by inet-gw-1.pa.dec.com; id AA15708; Tue, 22 Sep 92 12:37:43 -0700
Received: from Cipher.Parc.Xerox.xns by alpha.xerox.com via XNS id <11681>; Tue, 22 Sep 1992 12:35:03 PDT
X-Ns-Transport-Id: 0000AA005A585FA82E73
Date: Tue, 22 Sep 1992 12:34:29 PDT
From: Mike_Spreitzer.PARC@xerox.com
Subject: Assignability bugs
To: m3-request
Cc: Mike_Spreitzer.PARC@xerox.com, David_Goldberg.PARC@xerox.com
Message-Id: <92Sep22.123503pdt.11681@alpha.xerox.com>

Consider the following M3 program:
*****************************)

MODULE Main;

TYPE Obj = OBJECT f1: ARRAY BOOLEAN OF INTEGER END;
TYPE Rec = RECORD f2: ARRAY [1 .. 2] OF INTEGER END;

PROCEDURE P1 (a1: INTEGER;
   <*NOWARN*> a2: ARRAY OF INTEGER;
              a3: ARRAY BOOLEAN OF INTEGER):
  ARRAY BOOLEAN OF INTEGER RAISES {} =
  VAR
    o1                           := NEW(Obj, f1 := a2);
    r1                           := Rec{f2 := o1.f1};
    o2                           := NEW(Obj, f1 := r1.f2);
    r2                           := Rec{f2 := a2};
    la: ARRAY BOOLEAN OF INTEGER := a2;
  BEGIN
    EVAL r2;
    EVAL la;
    r1.f2 := o1.f1;
    o2.f1 := r1.f2;
    IF a1 = 2 THEN a2 := P1(3, a3, a2) END;
    IF a1 = 1 THEN a2 := a3 ELSE a3 := a2 END;
    IF a1 = 0 THEN RETURN a2
              ELSE RETURN r1.f2 END;
  END P1;

BEGIN
END Main.

(********************************
When I compile this with m3, I get the following messages:

augustus % m3 Bugs.m3
"Bugs.m3", line 11: assignment of different structures
"Bugs.m3", line 12: CAST is not a permitted struct/union operation
"Bugs.m3", line 12: assignment of different structures
"Bugs.m3", line 13: CAST is not a permitted struct/union operation
"Bugs.m3", line 13: assignment of different structures
"Bugs.m3", line 14: assignment of different structures
"Bugs.m3", line 21: assignment of different structures
"Bugs.m3", line 22: CAST is not a permitted struct/union operation
"Bugs.m3", line 22: assignment of different structures
augustus %

(where line 1 is the one that says "MODULE Bugs EXPORTS Main;")

These look to me like C compiler error messages, which I understand you
consider indication of bugs in the M3 compiler.

The RETURN statements look clearly valid to me.  Earlier parts of the program
establish, by lack of error message, the fact that "a2" and "r1.f2" are
assignable to variables of the return type of the procedure.

When I read SPwM3, I find the language concerning bindings in record
constructors and NEW calls ambiguous.  The first paragraph of page 53, and the
first of page 54, don't seem to me to say explicitly which expressions are
allowed for initial values.  My intuition and taste suggest the
expression-to-variable assignability condition is what is/should be applicable,
but the above experiment suggests that might not be so.  What is the rule?

****************************************)
