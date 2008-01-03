(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(**********************************
Return-Path: ryan@ponder.csci.unt.edu
Received: by jumbo.pa.dec.com; id AA16039; Thu, 12 Nov 92 07:53:27 -0800
Received: by inet-gw-2.pa.dec.com; id AA09379; Thu, 12 Nov 92 07:53:25 -0800
Received: from ponder.csci.unt.edu by ponder (5.61/1.36)id AA29213; Thu, 12 Nov 92 09:35:39 -0600
Message-Id: <9211121535.AA29213@ponder>
To: m3-request
Subject: possible bug in passing arrays
Date: Thu, 12 Nov 92 09:35:38 CST
From: Ryan Stansifer <ryan@ponder.csci.unt.edu>

The following program looks to me like it exhibits a bug.
I am running m3 on a SPARC, I think the version is 2.07.

sparc> cat bug.m3
****************************)

MODULE Main;

(*  Works without READONLY  *)
PROCEDURE D (<*UNUSED*> READONLY A: ARRAY [0..3] OF CHAR) =
    BEGIN
    END D;

VAR h : ARRAY [0..25] OF CHAR;

BEGIN
    D (SUBARRAY (h,0,4));
END Main.

(******************************
sparc> m3 bug.m3
bug.m3: In function `_init_':
bug.m3:12: incompatible types in assignment
******************************)
