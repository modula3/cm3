(* Packed array elements of 1..7 bits written and read through a
   non-constant subscript, including elements that cross a byte boundary,
   in global, local and record-field arrays (the latter two through a VAR
   parameter) and a heap open array, each within a 32-bit word so that the
   test runs on 32-bit targets too (2.2.5); and small arrays that are only
   byte-aligned, whose elements cross a byte wherever they start. *)

MODULE Main;
IMPORT IO, Fmt;

VAR bad := 0;

PROCEDURE Value (what: TEXT;  i, got, want: INTEGER) =
  BEGIN
    IF got # want THEN
      IO.Put ("FAIL " & what & "[" & Fmt.Int (i) & "] = " & Fmt.Int (got)
              & ", want " & Fmt.Int (want) & "\n");
      INC (bad);
    END;
  END Value;

TYPE A1 = ARRAY [0..31] OF BITS 1 FOR [0..1];
VAR g1: A1;
TYPE R1 = RECORD pad: BITS 5 FOR [0..31];  a: A1 END;
VAR r1: R1;

PROCEDURE Fill1 (VAR a: A1;  what: TEXT) =
  VAR l: A1;
  BEGIN
    FOR i := 0 TO 31 DO a[i] := (i * 5 + 3) MOD 2;  l[i] := (i * 7 + 1) MOD 2 END;
    FOR i := 0 TO 31 DO
      Value (what, i, a[i], (i * 5 + 3) MOD 2);
      Value ("local 1", i, l[i], (i * 7 + 1) MOD 2);
    END;
    Value (what & " static", 0, a[0], (0 * 5 + 3) MOD 2);
    Value (what & " static", 1, a[1], (1 * 5 + 3) MOD 2);
    Value (what & " static", 16, a[16], (16 * 5 + 3) MOD 2);
    Value (what & " static", 31, a[31], (31 * 5 + 3) MOD 2);
  END Fill1;

TYPE A2 = ARRAY [0..31] OF BITS 2 FOR [0..3];
VAR g2: A2;
TYPE R2 = RECORD pad: BITS 5 FOR [0..31];  a: A2 END;
VAR r2: R2;

PROCEDURE Fill2 (VAR a: A2;  what: TEXT) =
  VAR l: A2;
  BEGIN
    FOR i := 0 TO 31 DO a[i] := (i * 5 + 3) MOD 4;  l[i] := (i * 7 + 1) MOD 4 END;
    FOR i := 0 TO 31 DO
      Value (what, i, a[i], (i * 5 + 3) MOD 4);
      Value ("local 2", i, l[i], (i * 7 + 1) MOD 4);
    END;
    Value (what & " static", 0, a[0], (0 * 5 + 3) MOD 4);
    Value (what & " static", 1, a[1], (1 * 5 + 3) MOD 4);
    Value (what & " static", 16, a[16], (16 * 5 + 3) MOD 4);
    Value (what & " static", 31, a[31], (31 * 5 + 3) MOD 4);
  END Fill2;

TYPE A3 = ARRAY [0..9] OF BITS 3 FOR [0..7];
VAR g3: A3;
TYPE R3 = RECORD pad: BITS 5 FOR [0..31];  a: A3 END;
VAR r3: R3;

PROCEDURE Fill3 (VAR a: A3;  what: TEXT) =
  VAR l: A3;
  BEGIN
    FOR i := 0 TO 9 DO a[i] := (i * 5 + 3) MOD 8;  l[i] := (i * 7 + 1) MOD 8 END;
    FOR i := 0 TO 9 DO
      Value (what, i, a[i], (i * 5 + 3) MOD 8);
      Value ("local 3", i, l[i], (i * 7 + 1) MOD 8);
    END;
    Value (what & " static", 0, a[0], (0 * 5 + 3) MOD 8);
    Value (what & " static", 1, a[1], (1 * 5 + 3) MOD 8);
    Value (what & " static", 5, a[5], (5 * 5 + 3) MOD 8);
    Value (what & " static", 9, a[9], (9 * 5 + 3) MOD 8);
  END Fill3;

TYPE A4 = ARRAY [0..15] OF BITS 4 FOR [0..15];
VAR g4: A4;
TYPE R4 = RECORD pad: BITS 5 FOR [0..31];  a: A4 END;
VAR r4: R4;

PROCEDURE Fill4 (VAR a: A4;  what: TEXT) =
  VAR l: A4;
  BEGIN
    FOR i := 0 TO 15 DO a[i] := (i * 5 + 3) MOD 16;  l[i] := (i * 7 + 1) MOD 16 END;
    FOR i := 0 TO 15 DO
      Value (what, i, a[i], (i * 5 + 3) MOD 16);
      Value ("local 4", i, l[i], (i * 7 + 1) MOD 16);
    END;
    Value (what & " static", 0, a[0], (0 * 5 + 3) MOD 16);
    Value (what & " static", 1, a[1], (1 * 5 + 3) MOD 16);
    Value (what & " static", 8, a[8], (8 * 5 + 3) MOD 16);
    Value (what & " static", 15, a[15], (15 * 5 + 3) MOD 16);
  END Fill4;

TYPE A5 = ARRAY [0..5] OF BITS 5 FOR [0..31];
VAR g5: A5;
TYPE R5 = RECORD pad: BITS 5 FOR [0..31];  a: A5 END;
VAR r5: R5;

PROCEDURE Fill5 (VAR a: A5;  what: TEXT) =
  VAR l: A5;
  BEGIN
    FOR i := 0 TO 5 DO a[i] := (i * 5 + 3) MOD 32;  l[i] := (i * 7 + 1) MOD 32 END;
    FOR i := 0 TO 5 DO
      Value (what, i, a[i], (i * 5 + 3) MOD 32);
      Value ("local 5", i, l[i], (i * 7 + 1) MOD 32);
    END;
    Value (what & " static", 0, a[0], (0 * 5 + 3) MOD 32);
    Value (what & " static", 1, a[1], (1 * 5 + 3) MOD 32);
    Value (what & " static", 3, a[3], (3 * 5 + 3) MOD 32);
    Value (what & " static", 5, a[5], (5 * 5 + 3) MOD 32);
  END Fill5;

TYPE A6 = ARRAY [0..4] OF BITS 6 FOR [0..63];
VAR g6: A6;
TYPE R6 = RECORD pad: BITS 5 FOR [0..31];  a: A6 END;
VAR r6: R6;

PROCEDURE Fill6 (VAR a: A6;  what: TEXT) =
  VAR l: A6;
  BEGIN
    FOR i := 0 TO 4 DO a[i] := (i * 5 + 3) MOD 64;  l[i] := (i * 7 + 1) MOD 64 END;
    FOR i := 0 TO 4 DO
      Value (what, i, a[i], (i * 5 + 3) MOD 64);
      Value ("local 6", i, l[i], (i * 7 + 1) MOD 64);
    END;
    Value (what & " static", 0, a[0], (0 * 5 + 3) MOD 64);
    Value (what & " static", 1, a[1], (1 * 5 + 3) MOD 64);
    Value (what & " static", 2, a[2], (2 * 5 + 3) MOD 64);
    Value (what & " static", 4, a[4], (4 * 5 + 3) MOD 64);
  END Fill6;

TYPE A7 = ARRAY [0..3] OF BITS 7 FOR [0..127];
VAR g7: A7;
TYPE R7 = RECORD pad: BITS 5 FOR [0..31];  a: A7 END;
VAR r7: R7;

PROCEDURE Fill7 (VAR a: A7;  what: TEXT) =
  VAR l: A7;
  BEGIN
    FOR i := 0 TO 3 DO a[i] := (i * 5 + 3) MOD 128;  l[i] := (i * 7 + 1) MOD 128 END;
    FOR i := 0 TO 3 DO
      Value (what, i, a[i], (i * 5 + 3) MOD 128);
      Value ("local 7", i, l[i], (i * 7 + 1) MOD 128);
    END;
    Value (what & " static", 0, a[0], (0 * 5 + 3) MOD 128);
    Value (what & " static", 1, a[1], (1 * 5 + 3) MOD 128);
    Value (what & " static", 2, a[2], (2 * 5 + 3) MOD 128);
    Value (what & " static", 3, a[3], (3 * 5 + 3) MOD 128);
  END Fill7;

TYPE
  S6 = ARRAY [0..1] OF BITS 6 FOR [0..63];    (* 12 bits *)
  S5 = ARRAY [0..2] OF BITS 5 FOR [0..31];    (* 15 bits *)
  S7 = ARRAY [0..2] OF BITS 7 FOR [0..127];   (* 21 bits *)
  RS = RECORD c: CHAR;  a6: S6;  b: BOOLEAN;  a5: S5;  a7: S7 END;
  PS = RECORD pad: BITS 3 FOR [0..7];  a6: S6;  a5: S5;  a7: S7 END;
VAR
  gs6: S6;  gc: CHAR;  gs5: S5;  gs7: S7;
  rs: RS;  ps: PS;
  vs: ARRAY [0..2] OF S6;

PROCEDURE Small (VAR a6: S6;  VAR a5: S5;  VAR a7: S7;  what: TEXT) =
  VAR l6: S6;  l5: S5;  l7: S7;
  BEGIN
    FOR i := 0 TO 1 DO a6[i] := 27 + i;  l6[i] := 60 - i END;
    FOR i := 0 TO 2 DO
      a5[i] := 29 - i;  l5[i] := 3 + 13 * i;
      a7[i] := 125 - 7 * i;  l7[i] := 66 + 21 * i;
    END;
    FOR i := 0 TO 1 DO
      Value (what & " 6", i, a6[i], 27 + i);
      Value ("small local 6", i, l6[i], 60 - i);
    END;
    FOR i := 0 TO 2 DO
      Value (what & " 5", i, a5[i], 29 - i);
      Value ("small local 5", i, l5[i], 3 + 13 * i);
      Value (what & " 7", i, a7[i], 125 - 7 * i);
      Value ("small local 7", i, l7[i], 66 + 21 * i);
    END;
    Value (what & " 6 static", 1, a6[1], 28);
    Value (what & " 5 static", 1, a5[1], 28);
    Value (what & " 7 static", 2, a7[2], 111);
  END Small;

PROCEDURE SmallDirect () =
  (* Indexed in place, where the static offset of the array is known. *)
  VAR lr: RS;
  BEGIN
    FOR i := 0 TO 1 DO rs.a6[i] := 17 + i;  ps.a6[i] := 40 + 3 * i;  lr.a6[i] := 9 + i END;
    FOR i := 0 TO 2 DO
      rs.a5[i] := 1 + 7 * i;  ps.a5[i] := 30 - 6 * i;  lr.a5[i] := 11 + i;
      rs.a7[i] := 100 + i;  ps.a7[i] := 5 + 40 * i;  lr.a7[i] := 127 - 9 * i;
    END;
    FOR i := 0 TO 1 DO
      Value ("direct field 6", i, rs.a6[i], 17 + i);
      Value ("direct packed 6", i, ps.a6[i], 40 + 3 * i);
      Value ("direct local 6", i, lr.a6[i], 9 + i);
    END;
    FOR i := 0 TO 2 DO
      Value ("direct field 5", i, rs.a5[i], 1 + 7 * i);
      Value ("direct packed 5", i, ps.a5[i], 30 - 6 * i);
      Value ("direct local 5", i, lr.a5[i], 11 + i);
      Value ("direct field 7", i, rs.a7[i], 100 + i);
      Value ("direct packed 7", i, ps.a7[i], 5 + 40 * i);
      Value ("direct local 7", i, lr.a7[i], 127 - 9 * i);
    END;
  END SmallDirect;

PROCEDURE SmallNested () =
  BEGIN
    FOR i := 0 TO 2 DO
      FOR j := 0 TO 1 DO vs[i][j] := (i * 2 + j) * 9 + 1 END;
    END;
    FOR i := 0 TO 2 DO
      FOR j := 0 TO 1 DO Value ("nested 6", i * 2 + j, vs[i][j], (i * 2 + j) * 9 + 1) END;
    END;
  END SmallNested;

PROCEDURE Open () =
  VAR o := NEW (REF ARRAY OF BITS 4 FOR [0..15], 37);
  BEGIN
    FOR i := 0 TO LAST (o^) DO o[i] := (i * 3) MOD 16 END;
    FOR i := 0 TO LAST (o^) DO Value ("open 4", i, o[i], (i * 3) MOD 16) END;
  END Open;

BEGIN
  Fill1 (g1, "global 1");
  r1.pad := 31;  Fill1 (r1.a, "field 1");  Value ("pad 1", 0, r1.pad, 31);
  Fill2 (g2, "global 2");
  r2.pad := 31;  Fill2 (r2.a, "field 2");  Value ("pad 2", 0, r2.pad, 31);
  Fill3 (g3, "global 3");
  r3.pad := 31;  Fill3 (r3.a, "field 3");  Value ("pad 3", 0, r3.pad, 31);
  Fill4 (g4, "global 4");
  r4.pad := 31;  Fill4 (r4.a, "field 4");  Value ("pad 4", 0, r4.pad, 31);
  Fill5 (g5, "global 5");
  r5.pad := 31;  Fill5 (r5.a, "field 5");  Value ("pad 5", 0, r5.pad, 31);
  Fill6 (g6, "global 6");
  r6.pad := 31;  Fill6 (r6.a, "field 6");  Value ("pad 6", 0, r6.pad, 31);
  Fill7 (g7, "global 7");
  r7.pad := 31;  Fill7 (r7.a, "field 7");  Value ("pad 7", 0, r7.pad, 31);
  Open ();
  gc := 'x';  Small (gs6, gs5, gs7, "small global");  Value ("gc", 0, ORD (gc), ORD ('x'));
  rs.c := 'y';  rs.b := TRUE;
  Small (rs.a6, rs.a5, rs.a7, "small field");
  Value ("rs.c", 0, ORD (rs.c), ORD ('y'));  Value ("rs.b", 0, ORD (rs.b), 1);
  ps.pad := 5;  Small (ps.a6, ps.a5, ps.a7, "small packed");  Value ("ps.pad", 0, ps.pad, 5);
  SmallNested ();
  rs.c := 'z';  ps.pad := 6;
  SmallDirect ();
  Value ("rs.c", 1, ORD (rs.c), ORD ('z'));  Value ("ps.pad", 1, ps.pad, 6);
  IF bad = 0 THEN IO.Put ("ok\n") END;
END Main.
