(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*********
Return-Path: najork@src.dec.com
Received: by jumbo.pa.dec.com; id AA01498; Mon, 8 Jun 92 19:36:38 -0700
Received: by inet-gw-2.pa.dec.com; id AA00768; Mon, 8 Jun 92 18:44:54 -0700
Received: by oak.pa.dec.com; id AA26521; Mon, 8 Jun 92 18:43:36 -0700
Date: Mon, 8 Jun 92 18:43:36 -0700
From: najork (Marc Najork)
Message-Id: <9206090143.AA26521@oak.pa.dec.com>
To: mhb
Subject: Modula-3 compiler bug ...
Cc: najork

I believe I found a bug in m3. Where should I send a bug report?

-- Marc

------------------

Here is the problem:

m3 accepts the type definitions

  Aref = REF A;
  A = RECORD f : PROCEDURE (a : Aref); END;

However, if we reverse the order, i.e. write

  A = RECORD f : PROCEDURE (a : Aref); END;
  Aref = REF A;

it barfs. Also, it chokes on

  A = RECORD f : PROCEDURE (a : A); END;


------- End of Forwarded Message
*****************)

MODULE Main;
TYPE
  Aref = REF A;
  A = RECORD a : PROCEDURE (a : Aref); END;

  B = RECORD b : PROCEDURE (b : Bref); END;
  Bref = REF B;

  C = RECORD c : PROCEDURE (c : C); END;
BEGIN
END Main.
