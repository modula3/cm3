(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

MODULE M3ArrVal;

IMPORT M3Const;

REVEAL
  T = BRANDED "M3ArrVal.T" REF ARRAY OF M3Const.T;

PROCEDURE NewEmpty (n_elts: CARDINAL): T =
  VAR t := NEW (T, n_elts);
  BEGIN
    FOR i := 0 TO LAST (t^) DO
      WITH z = t[i] DO
        z.class := M3Const.Class.Exception; (* unlikely *)
        z.info  := -1;  (* impossible *)
        z.ref   := NIL; (* impossible *)
      END;
    END;
    RETURN t;
  END NewEmpty;

PROCEDURE Set (t: T;  index: CARDINAL;  READONLY val: M3Const.T): BOOLEAN =
  BEGIN
    IF (t = NIL) OR (index >= NUMBER (t^)) THEN RETURN FALSE; END;
    t[index] := val;
    RETURN TRUE;
  END Set;

PROCEDURE Index (t: T;  index: CARDINAL;  VAR(*OUT*) val: M3Const.T): BOOLEAN =
  BEGIN
    IF (t = NIL) OR (index >= NUMBER (t^)) THEN RETURN FALSE; END;
    val := t[index];
    RETURN TRUE;
  END Index;

PROCEDURE Compare (a, b: T): INTEGER =
  BEGIN
    IF (a = NIL) OR (b = NIL) OR (NUMBER (a^) # NUMBER (b^)) THEN RETURN -99; END;
    FOR i := 0 TO LAST (a^) DO
      IF NOT M3Const.IsEQ (a[i], b[i]) THEN RETURN -99; END;
    END;
    RETURN 0;
  END Compare;

BEGIN
END M3ArrVal.

