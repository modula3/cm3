(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*****************
Return-Path: guarino@src.dec.com
Received: by jumbo.pa.dec.com; id AA27404; Thu, 17 Sep 92 16:43:17 -0700
From: guarino (Loretta Guarino Reid)
Message-Id: <9209172343.AA27404@jumbo.pa.dec.com>
Date: Thu, 17 Sep 92 16:43:05 PDT
To: kalsow
Subject: deadly case statement
*******************)

MODULE Main;
IMPORT Test;

PROCEDURE X (i: INTEGER): INTEGER =
  BEGIN
    CASE i OF
    | 2147483647 => RETURN 1;
    | 2147483646 => RETURN 2;
    | 2147483645 => RETURN 3;
    ELSE            RETURN 4;
    END;
  END X;

BEGIN
  Test.checkI (X (2147483647), 1);
  Test.checkI (X (2147483646), 2);
  Test.checkI (X (2147483645), 3);
  Test.checkI (X (0), 4);
  Test.checkI (X (10102938), 4);
  Test.checkI (X (FIRST (INTEGER)), 4);
  Test.done ();
END Main.
