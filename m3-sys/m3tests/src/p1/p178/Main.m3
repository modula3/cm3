(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*********
Return-Path: muller@src.dec.com
Received: by jumbo.pa.dec.com; id AA02293; Tue, 4 Aug 92 16:06:02 -0700
Received: by procope.pa.dec.com; id AA08471; Tue, 4 Aug 92 16:06:00 -0700
Resent-Message-Id: <9208042306.AA08471@procope.pa.dec.com>
Return-Path: ramshaw@src.dec.com 
Delivery-Date: Tue, 04 Aug 92 16:04:15 PDT
Return-Path: ramshaw@src.dec.com
Received: by flimflam.pa.dec.com; id AA06048; Tue, 4 Aug 92 16:04:13 -0700
Received: by jumbo.pa.dec.com; id AA02105; Tue, 4 Aug 92 16:04:04 -0700
From: ramshaw (Lyle Ramshaw)
Message-Id: <9208042304.AA02105@jumbo.pa.dec.com>
Date: Tue,  4 Aug 92 16:03:47 PDT
To: muller
Cc: ramshaw
Subject: Re: bug with packing
Resent-To: kalsow
Resent-Date: Tue, 04 Aug 92 16:06:00 PDT
Resent-From: Eric Muller <muller>

[Eric, I am retransmitting this message because I'm not sure whether 
the first copy got through.]

Eric, here's a short demo of the compiler bug with packed records:

-----------
****************************)

MODULE Main;

IMPORT Test, Fmt, Wr, Stdio;

TYPE
  CList = REF RECORD
                flink: CList;
                row: BITS 18 FOR [0..16_3FFFF];
                val: BITS 14 FOR [0..16_3FFF];
              END;
  DList = REF RECORD
                flink: DList;
                row: INTEGER;
                val: INTEGER;
              END;

VAR c: CList;
    d: DList;
    k: INTEGER;

<* FATAL ANY *>

PROCEDURE Out (name: TEXT;   value: INTEGER) =
  BEGIN
    Wr.PutText (Stdio.stderr, "The value of ");
    Wr.PutText (Stdio.stderr, name);
    Wr.PutText (Stdio.stderr, " is: ");
    Wr.PutText (Stdio.stderr, Fmt.Int (value));
    Wr.PutText (Stdio.stderr, ".\n");
    Wr.Flush (Stdio.stderr);
  END Out;

BEGIN
c := NEW(CList, row := 27, val := 16380);
Out ("c.val", c.val);
Test.checkI (c.val, 16380);

k := (c.val * (-1)) MOD 16381;
Out ("k", k);
Test.checkI (k, 1);

(** c.val := 16380;**)

d := NEW(DList, row := 27, val := 16380);
Out ("d.val", d.val);
Test.checkI (d.val, 16380);

k := (d.val * (-1)) MOD 16381;
Out ("k", k);
Test.checkI (k, 1);

Test.done ();
END Main.

(****************8
--------------

In the packed case, the value of k is the mysterious number 145;  
in the unpacked case, the value of k is 1, which is correct.

Lyle

*************************)
