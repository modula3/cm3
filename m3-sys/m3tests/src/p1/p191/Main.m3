(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(**********************
Return-Path: stolfi@src.dec.com
Received: by jumbo.pa.dec.com; id AA17107; Thu, 11 Jun 92 00:49:54 -0700
Received: by bullwinkle.pa.dec.com; id AA25149; Thu, 11 Jun 92 00:49:52 -0700
Message-Id: <9206110749.AA25149@bullwinkle.pa.dec.com>
To: kalsow, muller
Subject: Compiler bug: passing SUBARRAY(2Dim?) to READONLY
Date: Thu, 11 Jun 92 00:49:52 -0700
From: stolfi
X-Mts: smtp


Bill and Eric,

One more for the collection.

--jorge

PS. By the way, why is the test "if (_z4.size[0] != 3) ..." needed there?
Doesn't the compiler know the size of all arrays in this program?

  bullwinkle.pa.dec.com 1> m3 -o Subarray2D Subarray2D.m3
  ccom: Error: Subarray2D.m3, line 16: assignment of different structures
         if (_z4.size[0] != 3)     
           RTMisc__NarrowFault (&__file.str, 16); 
         _z1 = _z4; Subarray2D__P (&_z1); }
      
      ------------^
  bullwinkle.pa.dec.com 2> more Subarray2D.m3
***************************************)

    MODULE Main;

    TYPE
      V3 = ARRAY [0..3] OF REAL;
      M3x3 = ARRAY [0..3] OF V3;
      M2x3 = ARRAY [0..2] OF V3;

    VAR m3x3: M3x3;

    PROCEDURE P(<*UNUSED*> READONLY m2x3: M2x3) =
      BEGIN
        (* ...*)
      END P;

    BEGIN
      P(SUBARRAY(m3x3, 1, 3))
    END Main.

