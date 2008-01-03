(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(**********************************************
Return-Path: David.Evers@cl.cam.ac.uk
Received: by jumbo.pa.dec.com; id AA08467; Tue, 3 Mar 92 10:23:36 -0800
Received: by enet-gw.pa.dec.com; id AA09565; Tue, 3 Mar 92 10:22:49 -0800
Received: from ely.cl.cam.ac.uk by swan.cl.cam.ac.uk with SMTP (PP-6.0) to cl 
          id <02095-0@swan.cl.cam.ac.uk>; Tue, 3 Mar 1992 18:22:20 +0000
To: m3-request
Cc: Peter.Robinson@cl.cam.ac.uk
Subject: 2.03-SPARC RTTypeFP assertion failure.
Date: Tue, 03 Mar 92 18:22:15 +0000
From: David.Evers@cl.cam.ac.uk
Message-Id: <"swan.cl.ca.101:03.02.92.18.22.27"@cl.cam.ac.uk>


In SRC-M3 version 2.03 built for a SPARC (Sun4), the following program
exits with an assertion failure:

********************************************************)

MODULE Main;

IMPORT RTTypeFP;

VAR fp := RTTypeFP.ToFingerprint(TYPECODE(REF INTEGER));
BEGIN
  EVAL fp;
END Main.

(*****************************************************

gdb sez:

*** runtime error: ASSERT failed ***
...
(gdb) bt
#0  0xf77a3638 in kill ()
#1  0x1050c in RTMisc__Crash () at RTMisc.m3:115
#2  0x1041c in RTMisc__FatalError (msgA=0x45064 "", msgB=0x0, msgC=0x0)
    at RTMisc.m3:95
#3  0x10564 in RTMisc__AssertFault () at RTMisc.m3:121
#4  0x16500 in RTTypeFP__Init () at RTTypeFP.m3:176
#5  0x1558c in RTTypeFP__ToFingerprint (tc=85, _return=0xf7fff9a4)
    at RTTypeFP.m3:55
#6  0x232c in main ()
#7  0x16ac0 in RTMain__Run () at RTMain.m3:60
#8  0x22f8 in main ()

(gdb) frame 4
#4  0x16500 in RTTypeFP__Init () at RTTypeFP.m3:176

(gdb) print *x
$1 = {id = 1099421963, fp = {elts = {0, 0}}, data = 0x4b04c, d_len = 0, 
  seq = 0, class = 0 '\000'}

(gdb) list
171	          y := Locate (t^.selfID);
172	          t.fpInfo := x; (* remember this lookup *)
173	          x.data := y.data;
174	          x.d_len := y.d_len;
175	        END;
176	        <* ASSERT x.d_len >= NUMBER (Fingerprint) *>
177	      END;
178	    END;
179	  END Init;
180	
(gdb) 



So it looks like the compiler has generated a RT0.TypeInfo record for a non-
opaque type (x->class = '0' ?=? RT0.TypeClass.None above) which has a zero
d_len field??

Does this work on DS3100s?

Regards,						---- Dave.

****************************************************************)
