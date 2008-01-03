(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TypeTbl.i3                                            *)
(* Last modified on Fri Jul 29 13:51:30 PDT 1994 by kalsow     *)

MODULE TypeTbl;

IMPORT Type, TypeRep, Word;

REVEAL
  T = BRANDED "TypeTbl.T" REF RECORD
    cnt   : INTEGER := 0;
    mask  : INTEGER := 0;
    shift : INTEGER := 0;
    map   : Map     := NIL;
  END;

TYPE
  Map = REF ARRAY OF RECORD key: Type.T;  value: REFANY;  END;

PROCEDURE Put (VAR t: T;  key: Type.T;  value: REFANY): REFANY =
  VAR
    uid  := Type.GlobalUID (key);
    hash := uid;
    old  : REFANY;
    x, shift, mask, cnt: INTEGER;
  BEGIN
    <*ASSERT uid = key.uid*>
    IF (t = NIL) THEN
      t := NEW (T, mask := 255, shift := 24, map := NEW (Map, 256));
    END;
    LOOP
      shift := Word.RightShift (t.shift, 1);
      mask  := t.mask;
      cnt   := NUMBER (t.map^);
      REPEAT
        x := Word.And (Word.RightShift (hash, shift), mask);
        WITH z = t.map[x] DO
          IF (z.key = NIL) THEN
            z.key := key;
            z.value := value;
            INC (t.cnt);
            IF (2 * t.cnt > NUMBER (t.map^)) THEN Expand (t) END;
            RETURN NIL;
          ELSIF (z.key.uid = uid) THEN
            old := z.value;
            z.value := value;
            RETURN old;
          END;
        END;
        hash := Word.Times (hash, 16_9e3779b9 (*== 2^32/phi*)) + 1;
        DEC (cnt);
      UNTIL (cnt <= 0);
      Expand (t);
    END;
  END Put;

PROCEDURE Get (t: T;  key: Type.T): REFANY =
  VAR
    uid  := Type.GlobalUID (key);
    hash := uid;
    x, shift, mask, cnt: INTEGER;
  BEGIN
    <*ASSERT uid = key.uid*>
    IF (t = NIL) THEN RETURN NIL END;

      shift := Word.RightShift (t.shift, 1);
      mask  := t.mask;
      cnt   := NUMBER (t.map^);
      REPEAT
        x := Word.And (Word.RightShift (hash, shift), mask);
        WITH z = t.map[x] DO
          IF (z.key = NIL) THEN
            RETURN NIL;
          ELSIF (z.key.uid = uid) THEN
            RETURN z.value;
          END;
        END;
        hash := Word.Times (hash, 16_9e3779b9 (*== 2^32/phi*)) + 1;
        DEC (cnt);
      UNTIL (cnt <= 0);

    RETURN NIL;
  END Get;

PROCEDURE Reset (t: T) =
  BEGIN
    IF (t = NIL) THEN RETURN; END;
    t.cnt := 0;
    FOR i := 0 TO LAST (t.map^) DO
      WITH z = t.map[i] DO  z.key := NIL;  z.value := NIL;  END;
    END;
  END Reset;

PROCEDURE Expand (t: T) =
  VAR
    n   := NUMBER (t.map^);
    old := t.map;
  BEGIN
    t.cnt   := 0;
    t.map   := NEW (Map, n + n);
    t.mask  := t.mask + t.mask + 1;
    t.shift := t.shift - 1;
    FOR i := 0 TO n-1 DO
      WITH z = old[i] DO
        IF z.key # NIL THEN EVAL Put (t, z.key, z.value); END;
      END;
    END;
  END Expand;

BEGIN
END TypeTbl.
