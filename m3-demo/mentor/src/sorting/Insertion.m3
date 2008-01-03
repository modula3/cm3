(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

@Insertion
PROCEDURE InsertionSort (a: ARRAY OF INTEGER) @=
  VAR j: CARDINAL; v: INTEGER;
  BEGIN
    @1 FOR i := 2 TO LAST(a) DO@
      @2 v := a[i];@
      @3 j := i;@
      @4 WHILE a[j - 1] > v DO@ 
        @5 a[j] := a[j - 1];@
        @6 DEC (j);@
      END;
      @7 a[j] := v;@
    END
  END InsertionRun;
@Insertion
