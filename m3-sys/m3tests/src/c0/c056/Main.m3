(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: twisted, mutually recursive types *)

MODULE Main;

TYPE  (* T1 == T2 == T3 == T4 == U4 *)
   T1 = REF T1;
   T2 = REF REF T2;
   T3 = REF REF REF T3;
   T4 = REF U4;   U4 = REF T4;

TYPE  (* R1 == R2 *)
   R1 = RECORD  i: INTEGER;  j: T3;  k: X2  END;
   R2 = RECORD  i: INTEGER;  j: T4;  k: X1  END;

TYPE  (* X1 == X2 *)
   X1 = REF R1;
   X2 = REF R2;


PROCEDURE P (<*UNUSED*>VAR x: X1) =
  BEGIN
  END P;

(* the examples of the report *)

TYPE
   List = REF RECORD x: REAL; link: List END;
   T = PROCEDURE (n: INTEGER; p: T);
   X = OBJECT END;
   XList = X OBJECT link: XList; END;
CONST 
  N = BYTESIZE (REF ARRAY [0..N] OF REAL);
PROCEDURE R (b: BOOLEAN) = 
  BEGIN IF b THEN R (NOT b); END; END R;
EXCEPTION 
  E (PROCEDURE () RAISES {E});
VAR
  v : REF ARRAY [0..BYTESIZE(v)] OF INTEGER;
  w := BITSIZE (w);
  y := ARRAY [0..5] OF INTEGER { BITSIZE (y) , ..};
  z := LAST ([0..BITSIZE (z)]);
  
VAR x2: X2; 

BEGIN
  P (x2);
END Main.
