(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(********
Return-Path: <harrison@kodak.pa.dec.com>
Received: by jumbo.pa.dec.com; id AA16822; Thu, 5 Dec 91 11:53:51 -0800
Received: by kodak.pa.dec.com; id AA01402; Thu, 5 Dec 91 11:53:49 -0800
Received: by gossamer.pa.dec.com; id AA05547; Thu, 5 Dec 91 11:53:47 -0800
Message-Id: <9112051953.AA05547@gossamer.pa.dec.com>
To: kalsow
Subject: This program chokes the C compiler
Date: Thu, 05 Dec 91 11:53:46 -0800
From: "Stephen Harrison" <harrison>
X-Mts: smtp


Bill,

LAST(Real.T) seems to be the culprit.  The FIRST is ok.

Cheers,

/Stephen

----------------
***************)
MODULE Main;

IMPORT Wr, Stdio, Fmt, Real;
<*FATAL ANY*>

BEGIN
  Wr.PutText(Stdio.stdout, "\tFIRST(Real.T) = " & Fmt.Real(FIRST(Real.T)) & "\n");
  Wr.PutText(Stdio.stdout, "\tLAST(Real.T) = " & Fmt.Real(LAST(Real.T)) & "\n");
END Main.









