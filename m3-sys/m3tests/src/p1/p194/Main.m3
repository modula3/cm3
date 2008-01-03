(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(******************************
Return-Path: fn00@gte.com
Received: by jumbo.pa.dec.com; id AA26626; Thu, 28 May 92 06:44:25 -0700
Received: by mts-gw.pa.dec.com; id AA11227; Thu, 28 May 92 06:44:21 -0700
Received: from tahoe by bunny.gte.com (5.61/GTEL2.19)id AA19383; Thu, 28 May 92 09:44:18 -0400
Received: by tahoe.gtel.com (4.1/SMI-4.1)id AA29604; Thu, 28 May 92 09:44:32 EDT
Date: Thu, 28 May 92 09:44:32 EDT
From: fn00@gte.com (Farshad Nayeri)
Message-Id: <9205281344.AA29604@tahoe.gtel.com>
To: m3-request
Subject: ARRAY Constructors


I am not sure if the following is really a bug, but I thought I should
let you know anyway. 

The following compiles with no Modula-3 syntax errors, but the C
compiler chokes on the initialization of the a variable.  The program
compiles and runs fine if I use ARRAY [1..5] OF INTEGER instead of
ARRAY OF INTEGER. I came around this when trying to answer someone's
question. This is not an urgent issue at all.

We run Modula-3 version 2.06 on SPARCstations.

-- the source --
*****************************)

MODULE Main;

VAR
  a := ARRAY [1..5],[1..5] OF INTEGER 
      { ARRAY OF INTEGER { 1, 2, 3, 4, 5 },
        ARRAY OF INTEGER { 6, 7, 8, 9, 10 },
        ARRAY OF INTEGER { 11, 12, 13, 14, 15 },
        ARRAY OF INTEGER { 16, 17, 18, 19, 20 },
        ARRAY OF INTEGER { 21, 22, 23, 24, 25 }};

BEGIN
  EVAL a;
END Main.

(*******************************************
-- the error messages --
"test_m.c", line 46: warning: undeclared initializer name _openConst1
"test_m.c", line 46: illegal initialization
"test_m.c", line 47: warning: undeclared initializer name _openConst2
"test_m.c", line 47: } expected
"test_m.c", line 47: } expected
"test_m.c", line 49: warning: undeclared initializer name _openConst3
"test_m.c", line 49: illegal initialization
"test_m.c", line 50: warning: undeclared initializer name _openConst4
"test_m.c", line 50: } expected
"test_m.c", line 50: } expected
"test_m.c", line 52: warning: undeclared initializer name _openConst5
"test_m.c", line 52: illegal initialization

**************************************)
