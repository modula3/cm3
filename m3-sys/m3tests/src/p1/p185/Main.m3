(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(**************
Return-Path: stolfi@src.dec.com
Received: by jumbo.pa.dec.com; id AA25446; Thu, 20 Aug 92 02:44:07 -0700
Received: by bullwinkle.pa.dec.com; id AA12916; Thu, 20 Aug 92 02:44:06 -0700
Message-Id: <9208200944.AA12916@bullwinkle.pa.dec.com>
To: muller, kalsow
Cc: harrison
Subject: One last compiler bug (?)...
Date: Thu, 20 Aug 92 02:44:05 -0700
From: stolfi
X-Mts: smtp


I believe that the expression "(1.0 + s) - 1.0" in the code below 
is being evaluated in double precision, even though the variable 
"s" and the literals are all single precision.

The evidence is that SignificandBits comes out as 53, instead of 
the 23 I was expecting. 

I conjecture that the whole expression is being translated into
a single C expression, and cc is promoting floats to doubles
automatically in this case.

If that is what is happening, isn't it a violation of the M3 language 
definition? 

*********************)

MODULE Main;
IMPORT Test;

VAR s := 0.5;
    SignificandBits: CARDINAL;
BEGIN
  SignificandBits := 1;
  LOOP
    IF (1.0 + s) - 1.0 # s THEN EXIT END;
    INC(SignificandBits);
    s := s / 2.0
  END;
  Test.checkI (SignificandBits, 24);
  Test.done ();
END Main.

(*****************************

--jorge
******************************)
