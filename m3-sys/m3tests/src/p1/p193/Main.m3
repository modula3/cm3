(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(**************************
Return-Path: gnelson@src.dec.com
Received: by jumbo.pa.dec.com; id AA17204; Sun, 24 May 92 15:51:22 -0700
Received: by nemesia.pa.dec.com; id AA07895; Sun, 24 May 92 15:51:20 -0700
Date: Sun, 24 May 92 15:51:20 -0700
From: gnelson (Greg Nelson)
Message-Id: <9205242251.AA07895@nemesia.pa.dec.com>
To: kalsow
Subject: Bug in m3
Cc: gnelson

The following program:
***************************)

MODULE Main;

CONST a: REFANY = "a";

VAR ab := a & "b";

BEGIN
  EVAL ab;
END Main.

(*****************************
leads to the error

"Bug.m3", line 5: illegal operands for '&'

But REFANY is assignable to TEXT, so this should be
allowed, with a runtime check compiled.

Greg
********************************)
