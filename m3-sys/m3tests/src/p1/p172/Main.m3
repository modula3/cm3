(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*************************
Return-Path: goldberg@parc.xerox.com
Received: by jumbo.pa.dec.com; id AA11324; Thu, 27 Aug 92 11:29:48 -0700
Received: by inet-gw-1.pa.dec.com; id AA23056; Thu, 27 Aug 92 11:29:09 -0700
Received: from arcturia.parc.xerox.com ([13.2.116.23]) by alpha.xerox.com with SMTP id <11612>; Thu, 27 Aug 1992 11:28:46 PDT
Received: by arcturia.parc.xerox.com id <40744>; Thu, 27 Aug 1992 11:28:40 -0700
From: David Goldberg <goldberg@parc.xerox.com>
To: m3-request
Subject: problem with REAL in 2.07
X-Face: "8f"c;6Z}TbIq0Io35sH8vqkT(rdm-ofN~D%vb&ABJ;{Ar/{aIx?\}{Z!fa!-Ray\I~WzB3 6I,*eA_3Z|Qzk&RBEs:}Y`&f[L)V$aeg9%@8sLZo;'4?y"tkKd4x$BydZ16RA%PZMk`bavMk(/z#gD <X(:[i<b9O~g[Hku0jW3vQz8vQwq47G,+7Q,>U@{"
Message-Id: <92Aug27.112840pdt.40744@arcturia.parc.xerox.com>
Date: Thu, 27 Aug 1992 11:28:26 PDT

In the following program, z and z1 should have the same value, but
they don't.  THe problem is that C is promoting float to double.

Possible solutions:
(1) when generating code for expressions like (x+y)-z,
turn it into tmp=x+y, tmp-z.  Hopefully the C optimizer make make
this generate efficient C code
(2) For compilers like SunOS that have an -fsingle flag that turns off
automatic promotion, have this be a default flag passed to cc?
(3) other?

	-david
	
============

**************************)

MODULE Main;

IMPORT Test, Wr, Stdio, Fmt;
<*FATAL ANY*>

VAR
  x, y, z, z1, tmp: REAL;
BEGIN
  x := 23.4;
  y := 8388608.0;

  z := (x + y) - y;

  tmp := (x + y);
  z1 := tmp - y;

  Wr.PutText(Stdio.stdout, Fmt.F("%s %s\n", Fmt.Real(z), Fmt.Real(z1)));
  Test.checkR (z, z1);
  Test.done ();
END Main.
