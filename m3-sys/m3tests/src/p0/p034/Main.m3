(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: equality of open arrays and records *)

MODULE Main;

FROM Test IMPORT msg, check, done;


IMPORT Fmt, Text;

PROCEDURE dummy (<*UNUSED*>m: Text.T) =
  BEGIN
  END dummy;

BEGIN

  msg ("records");
  VAR
    r1, r2: RECORD x: INTEGER; y: [3..4]; END;
  BEGIN
    r1.x := 17; r1.y := 4;
    r2.x := 17; r2.y := 4;
    msg (Fmt.Bool (r1=r2) & " [TRUE]");		check (r1 = r2);
    msg (Fmt.Bool (r1#r2) & " [FALSE]");	check (NOT (r1 # r2));
    r2.y := 3;
    msg (Fmt.Bool (r1=r2) & " [FALSE]"); 	check (NOT (r1 = r2));
    msg (Fmt.Bool (r1#r2) & " [TRUE]\n"); 	check (r1 # r2); END;

  msg ("fixed arrays");
  VAR
    w1, w2: ARRAY [1..3] OF ARRAY [1..5] OF INTEGER;
  BEGIN
    FOR i := 1 TO 3 DO 
      FOR j := 1 TO 5 DO 
        w1 [i,j] := 4; w2 [i,j] := 4; END; END;

    msg (Fmt.Bool (w1 = w2) & " [TRUE]");	check (w1 = w2);
    msg (Fmt.Bool (w1 # w2) & " [FALSE]");	check (NOT (w1 # w2));
    w2 [3, 4] := 3;
    msg (Fmt.Bool (w1 = w2) & " [FALSE]"); 	check (NOT (w1 = w2));
    msg (Fmt.Bool (w1 # w2) & " [TRUE]\n"); 	check (w1 # w2); END;

  msg ("ARRAY [0..2]? OF INTEGER");
  VAR
    x1: REF ARRAY        OF INTEGER;
    w1:     ARRAY [0..2] OF INTEGER;
  BEGIN
    x1 := NEW (REF ARRAY OF INTEGER, 3);
    FOR i := 0 TO 2 DO
      w1 [i] := 3;
      x1 [i] := 3; END;
    msg (Fmt.Bool (w1 = x1^) & " [TRUE]");	check (w1 = x1^);
    msg (Fmt.Bool (w1 # x1^) & " [FALSE]");	check (NOT (w1 # x1^));
    w1 [2] := 7;
    msg (Fmt.Bool (w1 = x1^) & " [FALSE]"); 	check (NOT (w1 = x1^));
    msg (Fmt.Bool (w1 # x1^) & " [TRUE]\n");    check (w1 # x1^); END;

  msg ("ARRAY [0..2]?  [0..5] OF INTEGER");
  VAR
    x1: REF ARRAY        OF ARRAY [0..5] OF INTEGER;
    w1:     ARRAY [0..2] OF ARRAY [0..5] OF INTEGER;
  BEGIN
    x1 := NEW (REF ARRAY OF ARRAY [0..5] OF INTEGER, 3);
    FOR i := 0 TO 2 DO
      FOR j := 0 TO 5 DO
        w1 [i, j] := 3;
        x1 [i, j] := 3; END; END;
    msg (Fmt.Bool (w1 = x1^) & " [TRUE]");	check (w1 = x1^);
    msg (Fmt.Bool (w1 # x1^) & " [FALSE]");	check (NOT (w1 # x1^));
    w1 [2, 4] := 7;
    msg (Fmt.Bool (w1 = x1^) & " [FALSE]");	check (NOT (w1 = x1^)); 
    msg (Fmt.Bool (w1 # x1^) & " [TRUE]\n");	check (w1 # x1^); END;


  msg ("ARRAY [0..2]? [0..3]? [0..5] OF INTEGER");
  VAR
    x1: REF ARRAY        OF ARRAY        OF ARRAY [0..5] OF INTEGER;
    w1:     ARRAY [0..2] OF ARRAY [0..3] OF ARRAY [0..5] OF INTEGER;
  BEGIN
    x1 := NEW (REF ARRAY OF ARRAY OF ARRAY [0..5] OF INTEGER, 3, 4);
    FOR i := 0 TO 2 DO
      FOR j := 0 TO 3 DO
        FOR k := 0 TO 5 DO
          w1 [i, j, k] := 3;
          x1 [i, j, k] := 3; END; END; END;
    msg (Fmt.Bool (w1 = x1^) & " [TRUE]");	check (w1 = x1^);
    msg (Fmt.Bool (w1 # x1^) & " [FALSE]");	check (NOT (w1 # x1^));
    w1 [2, 1, 3] := 7;
    msg (Fmt.Bool (w1 = x1^) & " [FALSE]");	check (NOT (w1 = x1^));
    msg (Fmt.Bool (w1 # x1^) & " [TRUE]\n"); 	check (w1 # x1^);END;


  msg ("ARRAY [0..2]? [0..3]? [0..5] OF record");
  TYPE
    R = RECORD i: INTEGER; a: CHAR; END;
  VAR
    x1: REF ARRAY        OF ARRAY        OF ARRAY [0..5] OF R;
    w1:     ARRAY [0..2] OF ARRAY [0..3] OF ARRAY [0..5] OF R;
  BEGIN
    x1 := NEW ( REF ARRAY OF ARRAY OF ARRAY [0..5] OF R, 3, 4);
    FOR i := 0 TO 2 DO
      FOR j := 0 TO 3 DO
        FOR k := 0 TO 5 DO
          w1 [i, j, k] := R {7, 'a'};
          x1 [i, j, k] := R {7, 'a'}; END; END; END;
    msg (Fmt.Bool (w1 = x1^) & " [TRUE]");	check (w1 = x1^);
    msg (Fmt.Bool (w1 # x1^) & " [FALSE]");	check (NOT (w1 # x1^));
    w1 [2, 1, 3] := R {7, 'b'};
    msg (Fmt.Bool (w1 = x1^) & " [FALSE]"); 	check (NOT (w1 = x1^));
    msg (Fmt.Bool (w1 # x1^) & " [TRUE]"); 	check (w1 # x1^);
    w1 [2, 1, 3] := R {5, 'a'};
    msg (Fmt.Bool (w1 = x1^) & " [FALSE]");	check (NOT (w1 = x1^));
    msg (Fmt.Bool (w1 # x1^) & " [TRUE]\n"); 	check (w1 # x1^);END;

  msg ("ARRAY [0..2]? [0..3]? [0..5] OF complex record");
  TYPE
    A = ARRAY [0..1] OF B;
    B = ARRAY [0..4] OF INTEGER;
    R = RECORD i: INTEGER; c: CHAR; a: A; END;
  VAR
    x1: REF ARRAY        OF ARRAY        OF ARRAY [0..5] OF R;
    w1:     ARRAY [0..2] OF ARRAY [0..3] OF ARRAY [0..5] OF R;
  BEGIN
    x1 := NEW (REF ARRAY OF ARRAY OF ARRAY [0..5] OF R, 3, 4);
    FOR i := 0 TO 2 DO
      FOR j := 0 TO 3 DO
        FOR k := 0 TO 5 DO
          w1 [i, j, k] := R {7, 'a', A{ B{1, 2, 3, 4, 5}, B{6, 7, 8, 9, 10}}};
          x1 [i, j, k] := R {7, 'a', A{ B{1, 2, 3, 4, 5}, B{6, 7, 8, 9, 10}}};
                END; END; END;
    msg (Fmt.Bool (w1 = x1^) & " [TRUE]");	check (w1 = x1^);
    msg (Fmt.Bool (w1 # x1^) & " [FALSE]");	check (NOT (w1 # x1^));
    w1 [2, 1, 3] := R {7, 'b', A{ B{1, 2, 3, 4, 5}, B{6, 7, 8, 9, 10}}};
    msg (Fmt.Bool (w1 = x1^) & " [FALSE]");	check (NOT (w1 = x1^)); 
    msg (Fmt.Bool (w1 # x1^) & " [TRUE]"); 	check (w1 # x1^);
    w1 [2, 1, 3] := R {7, 'b', A{ B{1, 2, 3, 4, 5}, B{6, 7, 8, 9, 10}}};
    msg (Fmt.Bool (w1 = x1^) & " [FALSE]");	check (NOT (w1 = x1^)); 
    msg (Fmt.Bool (w1 # x1^) & " [TRUE]"); 	check (w1 # x1^);
    w1 [2, 1, 3] := R {7, 'a', A{ B{1, 2, 3, 4, 5}, B{6, 7, 8, 9, 11}}};
    msg (Fmt.Bool (w1 = x1^) & " [FALSE]");	check (NOT (w1 = x1^));
    msg (Fmt.Bool (w1 # x1^) & " [TRUE]");	check (w1 # x1^);
    w1 [2, 1, 3].a[1,4] := 10;
    msg (Fmt.Bool (w1 = x1^) & " [TRUE]");	check (w1 = x1^);
    msg (Fmt.Bool (w1 # x1^) & " [FALSE]\n"); 	check (NOT (w1 # x1^)); END;


  msg ("ARRAY [0..5] OF record with global proc");
  TYPE
    P = PROCEDURE (m: Text.T);
    R = RECORD i: INTEGER; p: P; END;
    A = ARRAY [0..5] OF R;
  VAR
    x, y: A;
  BEGIN
    FOR i := 0 TO 5 DO
      x[i] := R {6, msg};
      y[i] := R {6, msg}; END;
    msg (Fmt.Bool (x = y) & " [TRUE]");		check (x = y);
    msg (Fmt.Bool (x # y) & " [FALSE]");	check (NOT (x # y));
    y[5].p := dummy;
    msg (Fmt.Bool (x = y) & " [FALSE]");	check (NOT (x = y)); 
    msg (Fmt.Bool (x # y) & " [TRUE]\n");	check (x # y); END;

  done ();
END Main.
