(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*************************************************************************
Return-Path: <sharon@proton.LCS.MIT.EDU>
Received: by jumbo.pa.dec.com; id AA18479; Tue, 3 Sep 91 12:43:25 -0700
Received: by uucp-gw-1.pa.dec.com; id AA16599; Tue, 3 Sep 91 12:42:54 -0700
Received: from PROTON.LCS.MIT.EDU by proton.LCS.MIT.EDU via TCP with SMTP
	id AA05500; Tue, 3 Sep 91 15:42:46 EDT
Message-Id: <9109031942.AA05500@proton.LCS.MIT.EDU>
To: kalsow, muller
Subject: Modula-3 compiler bug?
Date: Tue, 03 Sep 91 15:42:42 -0400
From: Sharon E. Perl <sharon@proton.LCS.MIT.EDU>

Bill & Eric,

Maybe you've heard about this one already. In the following program,
*************************************************************************)

    MODULE Main;
    TYPE Rec = RECORD a: INTEGER END;
    VAR  r := Rec;
    BEGIN
      INC(r.a)
    END Main.

(*************************************************************************
I get the error message:

    "bug.m3", line 5: unknown qualification '.' (a)"

rather than a complaint about the variable initialization for r.  The error
message had me mystified for a while until I noticed my typo. Seems like the
compiler should catch the error when it tries to assign the type Rec to a
variable.

Sharon

**************************************************************************)

