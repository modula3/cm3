(* Copyright (C) 1994 Digital Equipment Corporation.           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Aug 23 08:09:06 PDT 1994 by kalsow     *)
(*      modified on Wed Oct  7 16:01:43 PDT 1992 by johnh      *)
(*      modified on Tue Feb  6 10:02:10 PST 1990 by mcjones    *)

MODULE Test1 EXPORTS Main;

IMPORT Fmt, IntIntTbl, IO, Random, Text, TextTextTbl;
IMPORT SortedIntIntTbl, SortedTextTextTbl;
<*FATAL ANY*>

PROCEDURE TestInts (N: CARDINAL) =
  VAR
    key, k0: INTEGER;
    value  : INTEGER;
    t0     := NEW(IntIntTbl.Default).init(N DIV 2);
    t      := NEW(SortedIntIntTbl.Default).init();
    r      := NEW (Random.Default);
    b1, b2 : BOOLEAN;
    v1, v2 : INTEGER;
    s      : SortedIntIntTbl.Iterator;
    s0     : IntIntTbl.Iterator;
    ct     : INTEGER;
  BEGIN
    IO.Put("\nInteger  N = " & Fmt.Int(N) & "\n");
    SortedIntIntTbl.Validate(t);

    r := r.init(fixed := TRUE);
    FOR i := 1 TO N DO
      key := r.integer(0, 100000);
      value := key;
      <*ASSERT t0.put(key, value) = t.put(key, value) *>
      <*ASSERT t0.size() = t.size() *>
      IF i MOD 100 = 0 THEN SortedIntIntTbl.Validate(t) END
    END;
    SortedIntIntTbl.Validate(t);

    key := 12345678;
    value := key;
    <*ASSERT t0.put(key, value) = t.put(key, value) *>
    <*ASSERT t0.size() = t.size() *>
    <*ASSERT t0.get(key, v1) AND t.get(key, v2) *>
    <*ASSERT t0.size() = t.size() *>
    <*ASSERT (v1 = v2) AND (v1 = value) *>
    <*ASSERT t0.delete(key, v1) AND t.delete(key, v2) *>
    <*ASSERT t0.size() = t.size() *>
    <*ASSERT (v1 = v2) AND (v1 = value) *>
    <*ASSERT NOT t0.delete(key, v1) *>
    <*ASSERT NOT t.delete(key, v2) *>
    <*ASSERT t0.size() = t.size() *>
    SortedIntIntTbl.Validate(t);

    r := r.init(fixed := TRUE);
    FOR i := 1 TO N DO
      key := r.integer(0, 100000);
      <*ASSERT t0.get(key, v1) = t.get(key, v2) AND (v1 = v2) *>
    END;
    SortedIntIntTbl.Validate(t);
    (* <* ASSERT List.Equal (List.ReverseD (List.Sort (IntIntTbl.ToValuesList
       (t0))), SortedIntIntTbl.ToValuesList (t, FALSE)) *> *)

    r := r.init(fixed := TRUE);
    FOR i := 1 TO N DO
      key := r.integer(0, 100000);
      b1 := t0.delete(key, v1);
      b2 := t.delete(key, v2);
      <*ASSERT t0.size() = t.size() *>
      <*ASSERT (b1 = b2) AND (NOT b1 OR (v1 = v2)) *>
      IF (i < 10) OR (i MOD 100 = 0) THEN SortedIntIntTbl.Validate(t) END
    END;
    SortedIntIntTbl.Validate(t);
    <*ASSERT t.size() = 0 *>

    r := r.init(fixed := TRUE);
    FOR i := 1 TO N DIV 2 * 2 DO
      <*ASSERT NOT t.put(2*i, 2*i) *>
      IF i MOD 100 = 0 THEN SortedIntIntTbl.Validate(t) END
    END;
    <*ASSERT t.size() = N DIV 2 * 2 *>

    s := t.iterateOrdered(TRUE);
    s.seek(N DIV 2 * 2);
    FOR i := N DIV 2 TO N DIV 2 * 2 DO
      <*ASSERT s.next(key, v2) AND (key = 2*i) *>
      <*ASSERT v2 = key *>
    END;

    s := t.iterateOrdered(FALSE);
    s.seek(N DIV 2 * 4);
    FOR i := N DIV 2 * 2 TO N DIV 2 BY -1 DO
      <*ASSERT s.next(key, v2) AND (key = 2*i) *>
      <*ASSERT v2 = key *>
    END;

    <*ASSERT t.size() = N DIV 2 * 2 *>
    SortedIntIntTbl.Validate(t);

    (* test random access *)
    s := t.iterateOrdered();
    FOR i := 1 TO N DIV 2 DO
      k0 := r.integer(1, N DIV 2 * 4);
      s.seek(k0);
      <*ASSERT s.next(key, v2) AND (key = ((k0+1)  DIV 2 * 2)) *>
      <*ASSERT v2 = key *>
      s.seek(2*i);
      <*ASSERT s.next(key, v2) AND (key = 2*i) *>
      <*ASSERT v2 = key *>
    END;
    <*ASSERT t.size() = N DIV 2 * 2 *>
    SortedIntIntTbl.Validate(t);

    s.seek(4*N);
    <*ASSERT NOT s.next(key, v2) *>
    SortedIntIntTbl.Validate(t);

    s0 := t.iterate();
    ct := 0;
    WHILE s0.next(key, v2) DO INC(ct) END;
    <*ASSERT ct = N DIV 2 * 2 *>

    IO.Put ("  done\n");
  END TestInts;

PROCEDURE IntToText(n: INTEGER): TEXT =
  BEGIN
    RETURN Fmt.Pad(Fmt.Int(n), 6)
  END IntToText;

PROCEDURE TestTexts (N: CARDINAL) =
  VAR
    key, k0, k1: TEXT;
    j      : INTEGER;
    t0     := NEW(TextTextTbl.Default).init (N DIV 2);
    t      := NEW(SortedTextTextTbl.Default).init ();
    r      := NEW(Random.Default);
    b1, b2 : BOOLEAN;
    v1, v2 : TEXT;
    s      : SortedTextTextTbl.Iterator;
    s0     : TextTextTbl.Iterator;
    ct     : INTEGER;
  BEGIN
    IO.Put("\nText     N = " & Fmt.Int(N) & "\n");

    SortedTextTextTbl.Validate(t);

    r := r.init(fixed := TRUE);
    FOR i := 1 TO N DO
      key := IntToText(r.integer(0, 100000));
      <*ASSERT t0.put(key, key) = t.put(key, key) *>
      <*ASSERT t0.size() = t.size() *>
      IF i MOD 100 = 0 THEN SortedTextTextTbl.Validate(t) END
    END;
    SortedTextTextTbl.Validate(t);

    key := "12345678";
    <*ASSERT t0.put(key, key) = t.put(key, key) *>
    <*ASSERT t0.size() = t.size() *>
    <*ASSERT t0.get(key, v1) AND t.get(key, v2) *>
    <*ASSERT t0.size() = t.size() *>
    <*ASSERT v1 = v2 *>
    <*ASSERT t0.delete(key, v1) AND t.delete(key, v2) *>
    <*ASSERT t0.size() = t.size() *>
    <*ASSERT v1 = v2 *>
    <*ASSERT NOT t0.delete(key, v1) *>
    <*ASSERT NOT t.delete(key, v2) *>
    <*ASSERT t0.size() = t.size() *>
    SortedTextTextTbl.Validate(t);

    r := r.init(fixed := TRUE);
    FOR i := 1 TO N DO
      key := IntToText(r.integer(0, 100000));
      <*ASSERT t0.get(key, v1) = t.get(key, v2) AND (v1 = v2) *>
    END;
    SortedTextTextTbl.Validate(t);
    (*
    ASSERT (List.Equal (List.ReverseD (List.Sort (TextTextTbl.ToValuesList (t0))),
                        SortedTextTextTbl.ToValuesList (t, FALSE)));
    *)

    r := r.init(fixed := TRUE);
    FOR i := 1 TO N DO
      key := IntToText(r.integer(0, 100000));
      b1 := t0.delete(key, v1);
      b2 := t.delete(key, v2);
      <*ASSERT t0.size() = t.size() *>
      <*ASSERT (b1 = b2) AND (NOT b1 OR (v1 = v2)) *>
      IF (i < 10) OR (i MOD 100 = 0) THEN SortedTextTextTbl.Validate(t) END
    END;
    SortedTextTextTbl.Validate(t);
    <*ASSERT t.size() = 0 *>

    r := r.init(fixed := TRUE);
    FOR i := 1 TO N DIV 2 * 2 DO
      key := IntToText(2*i);
      <*ASSERT NOT t.put(key, key) *>
      IF i MOD 100 = 0 THEN SortedTextTextTbl.Validate(t) END
    END;
    <*ASSERT t.size() = N DIV 2 * 2 *>

    s := t.iterateOrdered(TRUE);
    s.seek(IntToText(N DIV 2 * 2));
    FOR i := N DIV 2 TO N DIV 2 * 2 DO
      <*ASSERT s.next(key, v2)
               AND (Text.Compare(key, IntToText(2*i)) = 0)
               AND (v2 = key) *>
    END;

    s := t.iterateOrdered(FALSE);
    s.seek(IntToText(N DIV 2 * 4));
    FOR i := N DIV 2 * 2 TO N DIV 2 BY -1 DO
      <*ASSERT s.next(key, v2)
               AND (Text.Compare(key, IntToText(2*i)) = 0)
               AND (v2 = key) *>
    END;
    <*ASSERT t.size() = N DIV 2 * 2 *>
    SortedTextTextTbl.Validate(t);

    (* test random access *)
    s := t.iterateOrdered();
    FOR i := 1 TO N DIV 2 DO
      j := r.integer(1, N DIV 2 * 4);
      k0 := IntToText(j);
      s.seek(k0);
      k1 := IntToText((j+1) DIV 2 * 2);
      <*ASSERT s.next(key, v2)
               AND (Text.Compare(key, k1) = 0)
               AND (v2 = key) *>
      s.seek(IntToText(2*i));
      <*ASSERT s.next(key, v2)
               AND (Text.Compare(key, IntToText(2*i)) = 0)
               AND (v2 = key) *>
    END;
    <*ASSERT t.size() = N DIV 2 * 2 *>
    SortedTextTextTbl.Validate(t);

    s.seek(IntToText(4*N));
    <*ASSERT NOT s.next(key, v2) *>
    SortedTextTextTbl.Validate(t);

    s0 := t.iterate();
    ct := 0;
    WHILE s0.next(key, v2) DO INC(ct) END;
    <*ASSERT ct = N DIV 2 * 2 *>

    IO.Put ("  done\n");
  END TestTexts;

BEGIN
  TestInts (1000);
  TestTexts (1000);
  (* FOR i := 1 TO 10000 DO TestInts(i) END *)
END Test1.
