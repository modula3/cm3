(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*---------------------------------------------------------------------------
Return-Path: <mjordan@src.dec.com>
Received: by jumbo.pa.dec.com; id AA18848; Tue, 29 Oct 91 14:00:30 -0800
From: mjordan (Mick Jordan)
Message-Id: <9110292200.AA18848@jumbo.pa.dec.com>
Date: Tue, 29 Oct 91 14:00:27 PST
To: kalsow
Subject: m3xx error


You get this half right. The compiler complains about P being incompatible
wtih 'm' on the NEW call. However, if 'P' is changed to take type T
it compiles and the exception doesnt get raised. I rather thought
it might.
----------------------------------------------------------------------------*)

MODULE Main;

TYPE T = OBJECT METHODS m() END;
     TS = T OBJECT OVERRIDES m := P END;

PROCEDURE P (<*UNUSED*> t: TS) RAISES {}=
  BEGIN
  END P;

EXCEPTION Error;
<*FATAL Error*>

VAR t: T;
BEGIN
  t := NEW(T, m := P);
  IF NOT ISTYPE(t, TS) THEN RAISE Error END;
END Main.


