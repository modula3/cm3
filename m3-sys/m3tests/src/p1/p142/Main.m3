(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(****************
Return-Path: nichols@parc.xerox.com
Received: by jumbo.pa.dec.com; id AA07284; Mon, 26 Oct 92 11:31:20 -0800
Received: by inet-gw-1.pa.dec.com; id AA16403; Mon, 26 Oct 92 11:29:02 -0800
Received: from osprey.parc.xerox.com ([13.2.116.9]) by alpha.xerox.com with SMTP id <11659>; Mon, 26 Oct 1992 11:26:02 PST
Received: by osprey.parc.xerox.com id <6121>; Mon, 26 Oct 1992 11:25:59 -0800
From: David Nichols <nichols@parc.xerox.com>
To: m3-request
Cc: nichols@parc.xerox.com
Subject: default procedure-typed formals
Message-Id: <92Oct26.112559pst.6121@osprey.parc.xerox.com>
Date: Mon, 26 Oct 1992 11:25:51 PST

If I try to compile the following modules, then I get the error message:
"test.m3", line 6: List__EqualQ undefined
================================================================
================================================================
================================================================
******************)

MODULE Main;

IMPORT A;

BEGIN
  A.P();
END Main.

(******************
================================================================

If I add List to the imports for test.m3, then it works.  However, in a
larger program that I'm working on, I get an unused warning for List even
though it's needed for the compilation.

	David

****************)
