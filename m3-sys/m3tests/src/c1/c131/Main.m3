(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(**********************************
Return-Path: <harbison@bert.pinecreek.com>
Received: by jumbo.pa.dec.com; id AA06985; Mon, 19 Aug 91 08:07:58 -0700
Received: by inet-gw-1.pa.dec.com; id AA02969; Mon, 19 Aug 91 08:07:48 -0700
Received: by bert.pinecreek.com (5.57/Ultrix3.0-C)
	id AA11446; Mon, 19 Aug 91 08:06:03 -0700
Message-Id: <9108191506.AA11446@bert.pinecreek.com>
To: m3-request
Cc: harbison@bert.pinecreek.com
Subject: M3 1.6 bug report
Date: Mon, 19 Aug 91 08:06:01 PDT
From: harbison@bert.pinecreek.com
******************************)

MODULE Main;

(* This program provokes an internal "initialization alignment error"
  from ccom using SRC M3 1.6.  The error is actually caused by the
  presence of 4 expressions in an initialization of a 3-expression
  structure, which is apparently a constant dope vector. 
	Reproduce: m3 -c <thisfile>
*)

TYPE 
	Stat = {Runs, Hits, Errors};
	Stats = ARRAY Stat OF CARDINAL;
	StatArray = ARRAY OF ARRAY OF Stats;


(****
CONST EmptyArray = 
	ARRAY OF ARRAY OF Stats{ARRAY OF Stats{Stats{0,0,0},..},..};
******)

CONST EmptyArray = StatArray{ARRAY OF Stats{Stats{0,0,0},..},..};

BEGIN
  EVAL EmptyArray[0];
END Main.

(*******************************************
Sam Harbison 
Pine Creek Software; 305 S. Craig St., Suite 300; Pittsburgh, PA  15213
+1 412 681 9811; harbison@bert.pinecreek.com
***********************************************)


