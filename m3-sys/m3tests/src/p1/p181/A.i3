(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(********
Return-Path: nr@Princeton.EDU
Received: by jumbo.pa.dec.com; id AA28618; Tue, 28 Apr 92 01:04:44 -0700
Received: by mts-gw.pa.dec.com; id AA19390; Tue, 28 Apr 92 01:04:29 -0700
Received: from fs.Princeton.EDU by Princeton.EDU (5.65b/2.89/princeton)id AA02098; Mon, 27 Apr 92 18:06:39 -0400
Received: from cs.Princeton.EDU (elan.Princeton.EDU) by fs.Princeton.EDU (4.0/1.105)id AA07672; Mon, 27 Apr 92 18:06:38 EDT
Received: by cs.Princeton.EDU (5.57/1.105)id AA14430; Mon, 27 Apr 92 18:06:37 -0400
Date: Mon, 27 Apr 92 18:06:37 -0400
From: Norman Ramsey <nr@Princeton.EDU>
Message-Id: <9204272206.AA14430@cs.Princeton.EDU>
To: m3-request
Subject: bug report --- <* NOWARN *> doesn't work


nr@elan (116) % cat Bug28.i3
***************)

INTERFACE A;
TYPE T = RECORD do: INTEGER; END; <* NOWARN *>
END A.

(**************************
nr@elan (117) % m3 -C -g -w1 Bug28.i3
"Bug28.i3", line 2: warning: C reserved word, appending underscore (do)
1 warning encountered

SRC M3 v2.04

DS3100


Norman
**************************)
