(* WITH over designators that do not start on a byte: sub-byte fields,
   bit-packed record fields and packed array elements, at static and dynamic
   positions.  (Dynamic indexes use element sizes that divide a byte: see the
   separate bug with elements that cross one.)  The alias reads and writes the field or element, its base is
   evaluated once, and neighbouring bits are preserved. *)

MODULE Main;
IMPORT IO, Fmt;

TYPE
  R   = RECORD a: BITS 3 FOR [0..7];  b: BITS 5 FOR [0..31];  c: BITS 9 FOR [0..511] END;
  In  = BITS 13 FOR RECORD a: BITS 4 FOR [0..15];  b: BITS 9 FOR [-256..255] END;
  Out = RECORD x: BITS 3 FOR [0..7];  f: In;  y: BITS 6 FOR [0..63] END;
  O   = OBJECT k: BITS 3 FOR [0..7];  m: BITS 5 FOR [-16..15] END;

VAR
  r    : R;
  outs : ARRAY [0..3] OF Out;
  p2   : ARRAY [0..9] OF BITS 2 FOR [0..3];
  p4   : ARRAY [0..9] OF BITS 4 FOR [0..15];
  p3   : ARRAY [0..9] OF BITS 3 FOR [0..7];
  o    := NEW (O);
  n    := 0;
  bad  := 0;

PROCEDURE Two (): INTEGER = BEGIN INC (n);  RETURN 2 END Two;

PROCEDURE Value (what: TEXT;  got, want: INTEGER) =
  BEGIN
    IF got # want THEN
      IO.Put ("FAIL " & what & " = " & Fmt.Int (got) & ", want " & Fmt.Int (want) & "\n");
      INC (bad);
    END;
  END Value;

PROCEDURE Nested () =
  (* the alias is used from a nested procedure *)
  BEGIN
    WITH w = outs[1].f DO
      PROCEDURE Set () = BEGIN w.a := 13;  w.b := -77 END Set;
      BEGIN
        Set ();
      END;
    END;
  END Nested;

BEGIN
  (* sub-byte scalar fields at static bit offsets *)
  r.a := 5;  r.c := 405;   (* low bits of c set: a wider store would clear them *)
  WITH x = r.b DO x := 21;  INC (x, 3) END;
  Value ("r.b", r.b, 24);  Value ("r.a", r.a, 5);  Value ("r.c", r.c, 405);
  WITH x = r.c DO DEC (x, 100) END;
  Value ("r.c", r.c, 305);  Value ("r.b", r.b, 24);  Value ("r.a", r.a, 5);

  (* a bit-packed record field, at a dynamic index evaluated once *)
  outs[2].x := 7;  outs[2].y := 63;
  WITH w = outs[Two ()].f DO w.b := 77;  w.a := 9;  INC (w.a) END;
  Value ("calls", n, 1);
  Value ("outs[2].f.b", outs[2].f.b, 77);
  Value ("outs[2].f.a", outs[2].f.a, 10);
  Value ("outs[2].x", outs[2].x, 7);
  Value ("outs[2].y", outs[2].y, 63);

  (* packed elements of 2 and 4 bits at every (dynamic) index *)
  FOR i := 0 TO 9 DO
    WITH e = p2[i] DO e := i MOD 4 END;
    WITH e = p4[i] DO e := i + 4;  INC (e) END;
  END;
  FOR i := 0 TO 9 DO
    Value ("p2[" & Fmt.Int (i) & "]", p2[i], i MOD 4);
    Value ("p4[" & Fmt.Int (i) & "]", p4[i], i + 5);
  END;

  (* a 4-bit element at a static index that starts on a byte: reading the
     alias must not pick up its neighbour *)
  p4[2] := 3;  p4[3] := 12;
  WITH e = p4[2] DO Value ("p4[2] via alias", e, 3);  INC (e) END;
  Value ("p4[2]", p4[2], 4);  Value ("p4[3]", p4[3], 12);

  (* 3-bit elements at static indexes, one of them (5) crossing a byte *)
  p3[4] := 7;  p3[6] := 7;
  WITH e = p3[5] DO e := 2;  INC (e, 3) END;
  WITH e = p3[1] DO e := 6 END;
  Value ("p3[5]", p3[5], 5);  Value ("p3[1]", p3[1], 6);
  Value ("p3[4]", p3[4], 7);  Value ("p3[6]", p3[6], 7);

  (* sub-byte object fields *)
  WITH k = o.k, m = o.m DO k := 6;  m := -9;  INC (m, 3) END;
  Value ("o.k", o.k, 6);  Value ("o.m", o.m, -6);

  Nested ();
  Value ("outs[1].f.a", outs[1].f.a, 13);
  Value ("outs[1].f.b", outs[1].f.b, -77);

  IF bad = 0 THEN IO.Put ("ok\n") END;
END Main.
