(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(**********************************************************************
Return-Path: <harbison@bert.pinecreek.com>
Received: by jumbo.pa.dec.com; id AA28536; Sun, 17 Nov 91 14:02:43 -0800
Received: by inet-gw-1.pa.dec.com; id AA05693; Sun, 17 Nov 91 14:02:43 -0800
Received: by bert.pinecreek.com (5.57/Ultrix3.0-C)
	id AA12907; Sun, 17 Nov 91 14:00:56 -0800
Message-Id: <9111172200.AA12907@bert.pinecreek.com>
To: m3-request
Cc: harbison@bert.pinecreek.com
Subject: More trouble with FIRST
Date: Sun, 17 Nov 91 14:00:55 PST
From: harbison@bert.pinecreek.com

This may be the same problem I mentioned in my previous message, but in
a different guise. Or not...

All three FOR statements below should be OK, but the 2nd is rejected.
The same error happens then the TO limit is LAST(Rank) and the FROM
limit is Rank.Joker.
**********************************************************************)

    MODULE Main;
    TYPE
      Rank = {Joker, Ace, Deuce, Trey};
    VAR
      aRank: Rank;
    BEGIN
      FOR r := Rank.Joker  TO Rank.Trey  DO aRank := r; END; (* OK *)
      FOR r := FIRST(Rank) TO Rank.Trey  DO aRank := r; END; (* Error *)
      FOR r := FIRST(Rank) TO LAST(Rank) DO aRank := r; END; (* OK *)
    END Main.

(**********************************************************************
Messages:

"bug.m3", line 8: 'from' and 'to' expressions must be compatible ordinals
1 error encountered

Sam 



**********************************************************************)
