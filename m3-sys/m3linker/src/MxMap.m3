(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxMap.m3                                              *)
(* Last Modified On Tue Mar 15 15:33:19 PST 1994 By kalsow     *)

MODULE MxMap;

REVEAL
  T = BRANDED "MxMap.T" REF RECORD
          n_used : INTEGER := 0;
          data   : Contents;
        END;

PROCEDURE New (initialSize: CARDINAL): T =
  VAR t := NEW (T);
  BEGIN
    t.data := NEW (Contents, MAX (16, initialSize));
    RETURN t;
  END New;

PROCEDURE Get (t: T;  k: Key): Value =
  VAR x0 := k MOD NUMBER (t.data^);
  VAR x := x0;
  BEGIN
    LOOP
      WITH z = t.data[x] DO
        IF (z.key = k) THEN RETURN z.value END;
        IF (z.value = NIL) THEN RETURN NIL END;
      END;
      INC (x);
      IF (x > LAST (t.data^)) THEN x := 0 END;
      IF (x = x0) THEN RETURN NIL END;
    END;
  END Get;

PROCEDURE GetDirect (t: T;  index: INTEGER): Value =
  BEGIN
    IF (0 <= index) AND (index <= LAST (t.data^))
      THEN RETURN t.data[index].value;
      ELSE RETURN NIL;
    END;
  END GetDirect;

PROCEDURE GetIndex (t: T;  k: Key): INTEGER =
  VAR x0 := k MOD NUMBER (t.data^);
  VAR x := x0;
  BEGIN
    LOOP
      WITH z = t.data [x] DO
        IF (z.key = k) THEN RETURN x END;
        IF (z.value = NIL) THEN RETURN MISSING END;
      END;
      INC (x);
      IF (x > LAST (t.data^)) THEN x := 0 END;
      IF (x = x0) THEN RETURN MISSING END;
    END;
  END GetIndex;

PROCEDURE Insert (t: T;  k: Key;  v: Value) =
  VAR x0 := k MOD NUMBER (t.data^);
  VAR x := x0;
  BEGIN
    LOOP
      WITH z = t.data [x] DO
        IF (z.key = k) THEN (* a new value for an existing key *)
          z.value := v;
          RETURN;
        ELSIF (z.value = NIL) THEN (* an empty hole => insert it here *)
          z.key   := k;
          z.value := v;
          INC (t.n_used);
          IF (2 * t.n_used > NUMBER (t.data^)) THEN Expand (t) END;
          RETURN;
        END;
      END;
      INC (x);
      IF (x > LAST (t.data^)) THEN x := 0 END;
      IF (x = x0) THEN (* no free slots *) Expand (t) END;
    END;
  END Insert;

PROCEDURE Expand (t: T) =
  VAR old := t.data;
  VAR n   := NUMBER (old^);
  BEGIN
    t.n_used := 0;
    t.data   := NEW (Contents, 2 * n);
    FOR i := 0 TO n-1 DO
      WITH z = old[i] DO
        IF (z.value # NIL) THEN Insert (t, z.key, z.value) END;
      END;
    END;
  END Expand;

PROCEDURE Delete (t: T;  k: Key) =
  VAR x0 := k MOD NUMBER (t.data^);
  VAR x := x0;  v: Value;
  BEGIN
    LOOP
      WITH z = t.data [x] DO
        IF (z.key = k) THEN (* a new value for an existing key *)
          (* this is the value to delete *)
          z.value := NIL;  DEC (t.n_used);
          EXIT;
        ELSIF (z.value = NIL) THEN
          (* an empty hole => nobody to delete *)
          RETURN;
        END;
      END;
      INC (x);
      IF (x > LAST (t.data^)) THEN x := 0 END;
      IF (x = x0) THEN (* no match *) RETURN; END;
    END;

    (* reinsert the elements that may be rehashes of the target *)
    LOOP
      INC (x);
      IF (x > LAST (t.data^)) THEN x := 0; END;
      WITH z = t.data[x] DO
        v := z.value;
        IF (v = NIL) THEN (* the end of the rehash chain *) RETURN; END;
        z.value := NIL;  DEC (t.n_used);
        Insert (t, z.key, v);
      END;
    END;

  END Delete;

PROCEDURE GetData (t: T): Contents =
  BEGIN
    IF (t = NIL)
      THEN RETURN NIL
      ELSE RETURN t.data;
    END;
  END GetData;

BEGIN
END MxMap.
