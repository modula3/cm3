(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Thu Jul  1 14:59:55 PDT 1993 by kalsow    *)

MODULE Main;

IMPORT RefList, RefListSort;

FROM Test IMPORT done;

TYPE T= REF INTEGER;

PROCEDURE CompareInt (a, b: REFANY): [-1..+1] =
  VAR ai, bi: REF INTEGER;
  BEGIN
    ai := NARROW(a, REF INTEGER);
    bi := NARROW(b, REF INTEGER);
    IF ai^ < bi^ THEN RETURN -1
    ELSIF ai^ > bi^ THEN RETURN +1
    ELSE RETURN 0
    END
  END CompareInt;

VAR s: RefList.T; 
    v1, v2, v3, v4: T;

BEGIN
  v1 := NEW (T);  v1^ := 12;
  v2 := NEW (T);  v2^ := 61;
  v3 := NEW (T);  v3^ := 22;
  v4 := NEW (T);  v4^ := 10;
  s := RefList.Cons (v1, RefList.List3 (v2, v3, v4));
  s := RefListSort.SortD (s, CompareInt);

  done ();
END Main.
