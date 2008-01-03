(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*******************************
Return-Path: <goldberg@parc.xerox.com>
Received: by jumbo.pa.dec.com; id AA04134; Mon, 21 Oct 91 13:33:36 -0700
Received: by enet-gw.pa.dec.com; id AA07557; Mon, 21 Oct 91 13:33:29 -0700
Received: from arcturia.parc.xerox.com ([13.2.116.23]) by alpha.xerox.com with SMTP id <11526>; Mon, 21 Oct 1991 13:33:11 -0700
Received: by arcturia.parc.xerox.com id <3145>; Mon, 21 Oct 1991 13:33:51 PDT
From: David Goldberg <goldberg@parc.xerox.com>
To: m3-request
Subject: compiler bug
Message-Id: <91Oct21.133351pdt.3145@arcturia.parc.xerox.com>
Date: 	Mon, 21 Oct 1991 13:33:39 -0700


I think this is a compiler bug:  It rejects the following program:
***********************************)

MODULE Main;

FROM Test IMPORT checkB, done;

TYPE
  Proc = PROCEDURE(arg: INTEGER);
  Ref = REFANY;

VAR
  ref: Ref := NIL;
  arrRef := ARRAY[0..0] OF Ref {NIL};

  proc: Proc := NIL;
  arrProc := ARRAY[0..0] OF Proc {NIL}; (* this line gives compiler error *)

BEGIN
  checkB (ref = NIL, TRUE);
  checkB (arrRef[0] = NIL,  TRUE);

  checkB (proc = NIL, TRUE);
  checkB (arrProc[0] = NIL, TRUE);

  done ();
END Main.


