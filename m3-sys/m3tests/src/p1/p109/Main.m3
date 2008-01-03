(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(**********************************************************************
Return-Path: <harbison@bert.pinecreek.com>Received: by jumbo.pa.dec.com; id AA27351; Sun, 17 Nov 91 13:11:33 -0800
Received: by inet-gw-1.pa.dec.com; id AA01307; Sun, 17 Nov 91 13:11:29 -0800
Received: by bert.pinecreek.com (5.57/Ultrix3.0-C)
	id AA12702; Sun, 17 Nov 91 13:09:44 -0800
Message-Id: <9111172109.AA12702@bert.pinecreek.com>
To: m3-request
Cc: harbison@bert.pinecreek.com
Subject: FIRST and LAST disallowed where they should be
Date: Sun, 17 Nov 91 13:09:43 PST
From: harbison@bert.pinecreek.com

This program exhibits a problem with the use of FIRST and LAST in a
record constructor for a CONST declaration.

The definition of the constant 'Error' get an error, but the constant
'OK' is accepted. Not the (apparently accepted) use of FIRST and LAST
in the record declaration. This is a cut-down example from a larger
program.
**********************************************************************)

    MODULE Main;
    CONST 
      OK    = T{ Rank.Joker };	(* acceptable *)
      Error = T{ FIRST(Rank)};	(* causes an error *)
    TYPE
      Rank = {Joker, Ace, Deuce, Trey};
      T = RECORD 
	rank: Rank := FIRST(Rank);
      END;	
    BEGIN
      EVAL OK.rank = Error.rank;
    END Main.

(**********************************************************************

Compilation messages are:

"bug.m3", line 4: expression is not assignable to field (rank)
"bug.m3", line 3: warning: not used (OK)
"bug.m3", line 4: warning: not used (Error)
1 error and 2 warnings encountered

Sam

**********************************************************************)

