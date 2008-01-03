(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(**********************
Return-Path: rustan@src.dec.com
Received: by jumbo.pa.dec.com; id AA02437; Fri, 28 Aug 92 10:54:16 -0700
Received: by barnum.pa.dec.com; id AA17477; Fri, 28 Aug 92 10:54:27 -0700
Date: Fri, 28 Aug 92 10:54:27 -0700
From: rustan (K. Rustan M. Leino)
Message-Id: <9208281754.AA17477@barnum.pa.dec.com>
To: kalsow
Subject: never-ending compile

Bill,
  I have found that trying to compile the following program
results in RTHeap.GrowHeap failing to extend the traced heap
(and for this one has to wait quite a while).  Perhaps the
compiler attempts to map all mentioned "CASE"'d values in an
array?
************************)

    MODULE Main;
    VAR c: INTEGER;  d: BOOLEAN;
    BEGIN
      CASE c OF
        0..LAST(INTEGER) =>  d := TRUE
      | -1 =>                d := FALSE
      ELSE                   (* skip *)
      END
    END Main.

(***************

  Rustan

**************)
