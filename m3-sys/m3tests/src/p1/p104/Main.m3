(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Wed Nov  2 13:49:31 PST 1994 by kalsow    *)

(*********************
Return-Path: <goldberg@parc.xerox.com>
Received: by jumbo.pa.dec.com; id AA14412; Tue, 21 Jan 92 18:49:17 -0800
Received: by inet-gw-1.pa.dec.com; id AA01783; Tue, 21 Jan 92 18:49:06 -0800
Received: from arcturia.parc.xerox.com ([13.2.116.23]) by alpha.xerox.com with SMTP id <11797>; Tue, 21 Jan 1992 18:48:14 PST
Received: by arcturia.parc.xerox.com id <3172>; Tue, 21 Jan 1992 18:46:56 PST
From: David Goldberg <goldberg@parc.xerox.com>
To: m3-request
Subject: OBSOLETE pragma
Message-Id: <92Jan21.184656pst.3172@arcturia.parc.xerox.com>
Date: 	Tue, 21 Jan 1992 18:46:56 PST

The OBSOLETE program doesn't seem to work.  The program below 
references Thread.DefaultStackSize, which is declared obsolete,
but I get no warning when I compile it.

**********************)

MODULE Main;

IMPORT Foo;
FROM Foo IMPORT x;

VAR
  y: INTEGER;
BEGIN
  y := Foo.x;
END Main.

(*************************
IMPORT Thread, ThreadF;
FROM Thread IMPORT Closure;

VAR
  x: INTEGER;
BEGIN
  x := Thread.DefaultStackSize;
END t1.
************************)

