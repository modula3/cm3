(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(***************
Return-Path: <harrison@kodak.pa.dec.com>
Received: by jumbo.pa.dec.com; id AA18853; Thu, 27 Jun 91 22:08:52 -0700
Received: by kodak.pa.dec.com; id AA05928; Thu, 27 Jun 91 22:08:50 -0700
Received: by darwin.pa.dec.com; id AA04822; Thu, 27 Jun 91 22:08:48 -0700
Message-Id: <9106280508.AA04822@darwin.pa.dec.com>
To: kalsow, muller
Subject: Problem with large sets
Date: Thu, 27 Jun 91 22:08:47 PDT
From: "Stephen Harrison" <harrison>


Hello,

1.6 generates a reference to the non-existant C macro INCL for the
following program.  The problem disappears if lo and hi are constants.

Cheers,

/Stephen

-------------------------
**************************)
MODULE Main;

FROM Test IMPORT checkB, done;

TYPE
  X = [0 .. 10000];

VAR
  lo := 100;
  hi := 200;
  big := SET OF X {lo .. hi};

BEGIN
  FOR i := FIRST (X) TO LAST (X) DO
    checkB ((lo <= i) AND (i <= hi), (i IN big));
  END;
  done ();
END Main.


