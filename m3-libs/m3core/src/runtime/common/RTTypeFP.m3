(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Jul 14 15:35:04 PDT 1994 by kalsow     *)
(*      modified on Wed Jun  2 15:35:18 PDT 1993 by muller     *)

UNSAFE MODULE RTTypeFP;

IMPORT Word, Fingerprint;
IMPORT RT0, RT0u, RTType;
FROM RTType IMPORT Typecode;

VAR map : UNTRACED REF ARRAY OF INTEGER := NIL;

PROCEDURE ToFingerprint (tc: Typecode): Fingerprint.T =
  VAR t := RTType.Get (tc);  fp: Fingerprint.T;
  BEGIN
    LOOPHOLE (fp, RT0.Fingerprint) := t.fp;
    RETURN fp;
  END ToFingerprint;

PROCEDURE FromFingerprint (READONLY fp_in: Fingerprint.T): Typecode =
  VAR n, x, y : INTEGER;  t: RT0.TypeDefn;  fp: RT0.Fingerprint;
  BEGIN
    fp := LOOPHOLE (fp_in, RT0.Fingerprint);
    IF (map = NIL) THEN BuildFPMap () END;
    n := NUMBER (map^);
    x := FPHash (fp, n);
    LOOP
      y := map[x];
      IF (y = RTType.NoSuchType) THEN  RETURN RTType.NoSuchType;  END;
      t := RTType.Get (y);
      IF (t.fp = fp) THEN  RETURN t.typecode;  END;
      INC (x);  IF (x >= n) THEN x := 0 END;
    END;
  END FromFingerprint;

PROCEDURE BuildFPMap () =
  VAR
    n   := RT0u.nTypes;
    n_m := 3 * n;
    m   := NEW (UNTRACED REF ARRAY OF INTEGER, n_m);
    t   : RT0.TypeDefn;
    x   : INTEGER;
  BEGIN
    FOR i := FIRST (m^) TO LAST (m^) DO
      m[i] := RTType.NoSuchType;
    END;
    FOR i := 0 TO n-1 DO
      t := RTType.Get (i);
      IF (t.traced # 0) OR (t.typecode = RT0.NilTypecode) THEN
        x := FPHash (t.fp, n_m);
        WHILE (m[x] # RTType.NoSuchType) DO
          INC (x);
          IF (x >= n_m) THEN x := 0 END;
        END;
        m[x] := t.typecode;
      END;
    END;
    map := m;
  END BuildFPMap;

PROCEDURE FPHash (READONLY fp: RT0.Fingerprint;  x: INTEGER): INTEGER =
  BEGIN
    RETURN Word.Xor (fp[0], fp[1]) MOD x;
  END FPHash;

BEGIN
END RTTypeFP.

