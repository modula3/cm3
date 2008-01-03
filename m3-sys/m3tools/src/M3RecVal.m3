(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE M3RecVal;

IMPORT M3ID, M3Const;

REVEAL
  T = BRANDED "M3RecVal.T" REF RECORD
    next  : T;
    name  : M3ID.T;
    value : M3Const.T;
  END;

PROCEDURE NewEmpty (): T =
  BEGIN
    RETURN NIL;
  END NewEmpty;

PROCEDURE SetField (t: T;  id: M3ID.T;  READONLY val: M3Const.T): T =
  BEGIN    RETURN NEW (T, name := id, value := val, next := t);
  END SetField;

PROCEDURE Qualify (t: T;  id: M3ID.T;  VAR(*OUT*) val: M3Const.T): BOOLEAN =
  BEGIN
    WHILE (t # NIL) AND (t.name # id) DO t := t.next; END;
    IF (t # NIL) THEN val := t.value; RETURN TRUE; END;
    RETURN FALSE;
  END Qualify;

PROCEDURE Compare (a, b: T): INTEGER =
  VAR b_val: M3Const.T;
  BEGIN
    IF Len (a) # Len (b) THEN RETURN -99; END;
    WHILE (a # NIL) DO
      IF NOT Qualify (b, a.name, b_val) THEN RETURN -99;  END;
      IF NOT M3Const.IsEQ (a.value, b_val) THEN RETURN -99; END;
      a := a.next;
    END;
    RETURN 0;
  END Compare;

PROCEDURE Len (t: T): CARDINAL =
  VAR x := 0;
  BEGIN
    WHILE (t # NIL) DO INC (x);  t := t.next; END;
    RETURN x;
  END Len;

BEGIN
END M3RecVal.

