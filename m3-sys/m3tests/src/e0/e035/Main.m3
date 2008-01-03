(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*********************************************************
Return-Path: <harbison@bert.pinecreek.com>
Received: by jumbo.pa.dec.com; id AA19224; Sat, 9 Nov 91 10:57:11 -0800
Received: by inet-gw-1.pa.dec.com; id AA08659; Sat, 9 Nov 91 10:57:17 -0800
Received: by bert.pinecreek.com (5.57/Ultrix3.0-C)
	id AA15399; Sat, 9 Nov 91 10:55:35 -0800
Message-Id: <9111091855.AA15399@bert.pinecreek.com>
To: m3-request
Cc: harbison@bert.pinecreek.com
Subject: For the M3 bug log: recursive type decl
Date: Sat, 09 Nov 91 10:55:34 PST
From: harbison@bert.pinecreek.com

The following small Modula-3 program contains an illegal recursive
type declaration (because I accidentally left out a "REF").  It causes
SRC Modula-3 1.6 on my DECStation 3100 to die, printing:

	sendsig: can't grow stack, pid 15323, proc m3compiler
	Program /usr/local/lib/m3/m3compiler got fatal signal 4.

**************************************************************)

MODULE Main;

TYPE
  Cache = RECORD  
    next: (*REF*) Cache := NIL;
  END;

BEGIN
END Main.

(**************************************************************
Sam Harbison 
Pine Creek Software; Suite 300; 305 South Craig Street; Pittsburgh, PA 15213;
USA. Phone&FAX: +1 412 681 9811. E-mail: harbison@bert.pinecreek.com.
****************************************************************)

