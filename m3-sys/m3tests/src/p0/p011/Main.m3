(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: Type operations: BITSIZE, BYTESIZE, ADRSIZE *)

MODULE Main;


FROM Wr IMPORT PutText, Close;
FROM Stdio IMPORT stdout;
FROM Fmt IMPORT Int;
IMPORT Text;
<*FATAL ANY*>

PROCEDURE msg (t: Text.T) = BEGIN PutText (stdout, t & "\n"); END msg;

TYPE
  T1 = INTEGER;
  T2 = [23..35];
  T3 = {a, b, c, d, e, f};
  T4 = [T3.b .. T3.d];
  T5 = REAL;
  T6 = LONGREAL;
  T7 = CHAR;
  T8 = BOOLEAN;

  T10 = SET OF T2;
  T11 = SET OF T3;
  T12 = SET OF T4;

  T20 = ARRAY T2 OF T1;
  T21 = ARRAY T2 OF T2;
  T22 = ARRAY T2 OF T5;
  T23 = ARRAY T2 OF T6;
  T24 = ARRAY T2 OF T10;
  T25 = ARRAY T4 OF T20;

  T30 = RECORD 
	  t1: T1;
  	  t2: T2;
	  t3: T3; END;

  T31 = RECORD
	  t20: T20;
	  t7:  T7;
	  t12: T12; END;

  T40 = BITS 4 FOR T3;
  T41 = BITS BITSIZE (T40) FOR T40;

  T50 = OBJECT
	  t1: T1;
	  t2: T2;
	  t3: T3;
	METHODS
	  m1 (t1: T1): T2;
	  m2 (t2: T2): T3; END;
  T51 = T50 OBJECT
	  t4: T4;
	  t5: T5;
	METHODS
	  m3 (t3: T3): T4; END;
  
BEGIN

msg ("--- Bitsize ---\n");

msg ("BITSIZE (T1) = " & Int (BITSIZE (T1)));
msg ("BITSIZE (T2) = " & Int (BITSIZE (T2)));
msg ("BITSIZE (T3) = " & Int (BITSIZE (T3)));
msg ("BITSIZE (T4) = " & Int (BITSIZE (T4)));
msg ("BITSIZE (T5) = " & Int (BITSIZE (T5)));
msg ("BITSIZE (T6) = " & Int (BITSIZE (T6)));
msg ("BITSIZE (T7) = " & Int (BITSIZE (T7)));
msg ("BITSIZE (T8) = " & Int (BITSIZE (T8)));

msg ("BITSIZE (T10) = " & Int (BITSIZE (T10)));
msg ("BITSIZE (T11) = " & Int (BITSIZE (T11)));
msg ("BITSIZE (T12) = " & Int (BITSIZE (T12)));

msg ("BITSIZE (T20) = " & Int (BITSIZE (T20)));
msg ("BITSIZE (T21) = " & Int (BITSIZE (T21)));
msg ("BITSIZE (T22) = " & Int (BITSIZE (T22)));
msg ("BITSIZE (T23) = " & Int (BITSIZE (T23)));
msg ("BITSIZE (T24) = " & Int (BITSIZE (T24)));
msg ("BITSIZE (T25) = " & Int (BITSIZE (T25)));

msg ("BITSIZE (T30) = " & Int (BITSIZE (T30)));
msg ("BITSIZE (T31) = " & Int (BITSIZE (T31)));

msg ("BITSIZE (T40) = " & Int (BITSIZE (T40)));
msg ("BITSIZE (T41) = " & Int (BITSIZE (T41)));

msg ("BITSIZE (T50) = " & Int (BITSIZE (T50)));
msg ("BITSIZE (T51) = " & Int (BITSIZE (T51)));

msg ("\n--- Bytesize ---\n");

msg ("BYTESIZE (T1) = " & Int (BYTESIZE (T1)));
msg ("BYTESIZE (T2) = " & Int (BYTESIZE (T2)));
msg ("BYTESIZE (T3) = " & Int (BYTESIZE (T3)));
msg ("BYTESIZE (T4) = " & Int (BYTESIZE (T4)));
msg ("BYTESIZE (T5) = " & Int (BYTESIZE (T5)));
msg ("BYTESIZE (T6) = " & Int (BYTESIZE (T6)));
msg ("BYTESIZE (T7) = " & Int (BYTESIZE (T7)));
msg ("BYTESIZE (T8) = " & Int (BYTESIZE (T8)));

msg ("BYTESIZE (T10) = " & Int (BYTESIZE (T10)));
msg ("BYTESIZE (T11) = " & Int (BYTESIZE (T11)));
msg ("BYTESIZE (T12) = " & Int (BYTESIZE (T12)));

msg ("BYTESIZE (T20) = " & Int (BYTESIZE (T20)));
msg ("BYTESIZE (T21) = " & Int (BYTESIZE (T21)));
msg ("BYTESIZE (T22) = " & Int (BYTESIZE (T22)));
msg ("BYTESIZE (T23) = " & Int (BYTESIZE (T23)));
msg ("BYTESIZE (T24) = " & Int (BYTESIZE (T24)));
msg ("BYTESIZE (T25) = " & Int (BYTESIZE (T25)));

msg ("BYTESIZE (T30) = " & Int (BYTESIZE (T30)));
msg ("BYTESIZE (T31) = " & Int (BYTESIZE (T31)));

msg ("BYTESIZE (T40) = " & Int (BYTESIZE (T40)));
msg ("BYTESIZE (T41) = " & Int (BYTESIZE (T41)));

msg ("BYTESIZE (T50) = " & Int (BYTESIZE (T50)));
msg ("BYTESIZE (T51) = " & Int (BYTESIZE (T51)));

msg ("\n--- Adrsize ---\n");

msg ("ADRSIZE (T1) = " & Int (ADRSIZE (T1)));
msg ("ADRSIZE (T2) = " & Int (ADRSIZE (T2)));
msg ("ADRSIZE (T3) = " & Int (ADRSIZE (T3)));
msg ("ADRSIZE (T4) = " & Int (ADRSIZE (T4)));
msg ("ADRSIZE (T5) = " & Int (ADRSIZE (T5)));
msg ("ADRSIZE (T6) = " & Int (ADRSIZE (T6)));
msg ("ADRSIZE (T7) = " & Int (ADRSIZE (T7)));
msg ("ADRSIZE (T8) = " & Int (ADRSIZE (T8)));

msg ("ADRSIZE (T10) = " & Int (ADRSIZE (T10)));
msg ("ADRSIZE (T11) = " & Int (ADRSIZE (T11)));
msg ("ADRSIZE (T12) = " & Int (ADRSIZE (T12)));

msg ("ADRSIZE (T20) = " & Int (ADRSIZE (T20)));
msg ("ADRSIZE (T21) = " & Int (ADRSIZE (T21)));
msg ("ADRSIZE (T22) = " & Int (ADRSIZE (T22)));
msg ("ADRSIZE (T23) = " & Int (ADRSIZE (T23)));
msg ("ADRSIZE (T24) = " & Int (ADRSIZE (T24)));
msg ("ADRSIZE (T25) = " & Int (ADRSIZE (T25)));

msg ("ADRSIZE (T30) = " & Int (ADRSIZE (T30)));
msg ("ADRSIZE (T31) = " & Int (ADRSIZE (T31)));

msg ("ADRSIZE (T40) = " & Int (ADRSIZE (T40)));
msg ("ADRSIZE (T41) = " & Int (ADRSIZE (T41)));

msg ("ADRSIZE (T50) = " & Int (ADRSIZE (T50)));
msg ("ADRSIZE (T51) = " & Int (ADRSIZE (T51)));

msg ("\ndone.");
Close (stdout);

END Main.
