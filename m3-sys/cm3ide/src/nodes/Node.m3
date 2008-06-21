(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

MODULE Node;

(** IMPORT Text; **)
IMPORT ID, OS, PkgRoot, RegExpr, Roots, Type;

PROCEDURE DefaultName (t: Named_T): TEXT =
  BEGIN
    RETURN ID.ToText (t.name);
  END DefaultName;

PROCEDURE DefaultArcName (t: Named_T): ID.T =
  BEGIN
    RETURN t.name;
  END DefaultArcName;

PROCEDURE MatchName (t: T;  re: RegExpr.T): BOOLEAN =
  BEGIN
    RETURN RegExpr.Match (re, ID.ToText (t.arcname()));
  END MatchName;

PROCEDURE Append (VAR s: Set;  t: T) =
  BEGIN
    IF (s.elts = NIL) THEN s.elts := NEW (Array, 30);  END;
    IF (s.cnt >= NUMBER (s.elts^)) THEN Expand (s); END;
    s.elts [s.cnt] := t;  INC (s.cnt);
  END Append;

PROCEDURE Expand (VAR s: Set) =
  VAR n := NUMBER (s.elts^);  new := NEW (Array, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := s.elts^;
    s.elts := new;
  END Expand;

PROCEDURE Squash (VAR s: Set) =
  VAR n_unique: INTEGER;  a, b: T;
  BEGIN
    IF (s.cnt < 2) THEN RETURN END;
    Sort (s);

    (* remove duplicates *)
    a := s.elts[0];
    n_unique := 1;
    FOR i := 1 TO s.cnt-1 DO
      b := s.elts[i];
      IF (a # b) AND Cmp (a, b) # 0 THEN
        (* they're different => preserve this one *)
        s.elts[n_unique] := b;  INC (n_unique);
        a := b;
      END;
    END;
    s.cnt := n_unique;
  END Squash;

PROCEDURE Sort (VAR s: Set) =
  BEGIN
    IF (s.cnt < 2) THEN RETURN END;
    QuickSort (s.elts^, 0, s.cnt);
    InsertionSort (s.elts^, 0, s.cnt);
  END Sort;

(*------------------------------------------------------------ sorting ---*)

TYPE Elem_T = T;

PROCEDURE Cmp (a, b: Elem_T): INTEGER =
  VAR ca, cb: Class;  cmp: INTEGER;
  BEGIN
    IF (a = b) THEN RETURN 0; END;

    ca := a.class ();
    cb := b.class ();
    IF (ca # cb) THEN RETURN ORD (ca) - ORD (cb); END;

    cmp := CompareArcName (a, b);
    IF (cmp # 0) THEN RETURN cmp; END;

    RETURN CompareFullName (a, b);
  END Cmp;

PROCEDURE QuickSort (VAR a: ARRAY OF Elem_T;  lo, hi: INTEGER) =
  CONST CutOff = 5;
  VAR i, j: INTEGER;  key, tmp: Elem_T;
  BEGIN
    WHILE (hi - lo > CutOff) DO (* sort a[lo..hi) *)

      (* use median-of-3 to select a key *)
      i := (hi + lo) DIV 2;
      IF Cmp (a[lo], a[i]) < 0 THEN
        IF Cmp (a[i], a[hi-1]) < 0 THEN
          key := a[i];
        ELSIF Cmp (a[lo], a[hi-1]) < 0 THEN
          key := a[hi-1];  a[hi-1] := a[i];  a[i] := key;
        ELSE
          key := a[lo];  a[lo] := a[hi-1];  a[hi-1] := a[i];  a[i] := key;
        END;
      ELSE (* a[lo] >= a[i] *)
        IF Cmp (a[hi-1], a[i]) < 0 THEN
          key := a[i];  tmp := a[hi-1];  a[hi-1] := a[lo];  a[lo] := tmp;
        ELSIF Cmp (a[lo], a[hi-1]) < 0 THEN
          key := a[lo];  a[lo] := a[i];  a[i] := key;
        ELSE
          key := a[hi-1];  a[hi-1] := a[lo];  a[lo] := a[i];  a[i] := key;
        END;
      END;

      (* partition the array *)
      i := lo+1;  j := hi-2;

      (* find the first hole *)
      WHILE Cmp (a[j], key) > 0 DO DEC (j) END;
      tmp := a[j];
      DEC (j);

      LOOP
        IF (i > j) THEN EXIT END;

        WHILE Cmp (a[i], key) < 0 DO INC (i) END;
        IF (i > j) THEN EXIT END;
        a[j+1] := a[i];
        INC (i);

        WHILE Cmp (a[j], key) > 0 DO DEC (j) END;
        IF (i > j) THEN  IF (j = i-1) THEN  DEC (j)  END;  EXIT  END;
        a[i-1] := a[j];
        DEC (j);
      END;

      (* fill in the last hole *)
      a[j+1] := tmp;
      i := j+2;

      (* then, recursively sort the smaller subfile *)
      IF (i - lo < hi - i)
        THEN  QuickSort (a, lo, i-1);   lo := i;
        ELSE  QuickSort (a, i, hi);     hi := i-1;
      END;

    END; (* WHILE (hi-lo > CutOff) *)
  END QuickSort;

PROCEDURE InsertionSort (VAR a: ARRAY OF Elem_T;  lo, hi: INTEGER) =
  VAR j: INTEGER;  key: Elem_T;
  BEGIN
    FOR i := lo+1 TO hi-1 DO
      key := a[i];
      j := i-1;
      WHILE (j >= lo) AND Cmp (key, a[j]) < 0 DO
        a[j+1] := a[j];
        DEC (j);
      END;
      a[j+1] := key;
    END;
  END InsertionSort;

(*----------------------------------------------------------- names ---*)

PROCEDURE FullPath (t: T): TEXT =
  VAR
    path := "";
    arcs : ARRAY [0..19] OF T;
    len  := FindArcs (t, arcs);
  BEGIN
    IF (len > 0) THEN
      path := arcs[0].filename ();
      FOR i := 1 TO len-1 DO
        path := OS.MakePath (path, arcs[i].filename ());
      END;
    END;
    RETURN path;
  END FullPath;

PROCEDURE CompareArcName (a, b: T): INTEGER =
  VAR
    a_nm := a.arcname ();
    b_nm := b.arcname ();
  BEGIN
    IF    (a_nm = b_nm)        THEN  RETURN 0;
    ELSIF ID.IsLT (a_nm, b_nm) THEN  RETURN -1;
    ELSE                             RETURN +1;
(***
    ELSIF (a_nm = NIL)  THEN  RETURN -1;
    ELSIF (b_nm = NIL)  THEN  RETURN + 1;
    ELSE                      RETURN Text.Compare (a_nm, b_nm);
***)
    END;
  END CompareArcName;

PROCEDURE CompareFullName (a, b: T): INTEGER =
  VAR
    a_arcs, b_arcs: ARRAY [0..19] OF T;
    a_len := FindArcs (a, a_arcs);
    b_len := FindArcs (b, b_arcs);
    cmp: INTEGER;
  BEGIN
    FOR i := 0 TO MIN (a_len, b_len) - 1 DO
      IF (a_arcs[i] # b_arcs[i]) THEN
        cmp := CompareArcName (a_arcs[i], b_arcs[i]);
        IF (cmp # 0) THEN RETURN cmp; END;
      END;
    END;
    IF    (a_len = b_len) THEN RETURN 0;
    ELSIF (a_len < b_len) THEN RETURN -1;
    ELSE (*a_len > b_len*)     RETURN +1;
    END;
  END CompareFullName;

PROCEDURE FindArcs (t: T;  VAR x: ARRAY OF T): CARDINAL =
  VAR n: CARDINAL := LAST (x);  cnt: CARDINAL := 0;
  BEGIN
    LOOP
      TYPECASE t OF
      | NULL =>
          EXIT;  (* skip *)

      | PkgRoot.T (p) =>
          (* package roots are all registered roots => cut off the search here *)
          x[n] := p;  DEC (n); INC (cnt);
          EXIT;

      | Named_T (tt) =>
          x[n] := tt;  DEC (n);  INC (cnt);
          t := tt.parent;

      | Type.T (tx) =>
          x[n] := tx;              DEC (n);  INC (cnt);
          x[n] := Roots.TypeRoot;  DEC (n);  INC (cnt);
          EXIT;

      ELSE <*ASSERT FALSE*>
      END;
    END;

    FOR i := 0 TO cnt-1 DO
      INC (n);
      x[i] := x[n];
    END;
    RETURN cnt;
  END FindArcs;

PROCEDURE Init () =
  BEGIN
    FOR c := FIRST (ClassID) TO LAST (ClassID) DO
      IF (ClassTag [c] = NIL)
        THEN ClassID [c] := ID.NoID;
        ELSE ClassID [c] := ID.Add (ClassTag [c]);
      END;
    END;
  END Init;

BEGIN
END Node.
