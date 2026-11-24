(* Packed array elements of 1..7 bits written and read through a
   non-constant subscript, including elements that cross a byte boundary,
   in global, local and record-field arrays (the latter two through a VAR
   parameter) and a heap open array. *)

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

TYPE A3 = ARRAY [0..20] OF BITS 3 FOR [0..7];
VAR g3: A3;
TYPE R3 = RECORD pad: BITS 5 FOR [0..31];  a: A3 END;
VAR r3: R3;

PROCEDURE Fill3 (VAR a: A3;  what: TEXT) =
  VAR l: A3;
  BEGIN
    FOR i := 0 TO 20 DO a[i] := (i * 5 + 3) MOD 8;  l[i] := (i * 7 + 1) MOD 8 END;
    FOR i := 0 TO 20 DO
      Value (what, i, a[i], (i * 5 + 3) MOD 8);
      Value ("local 3", i, l[i], (i * 7 + 1) MOD 8);
    END;
    Value (what & " static", 0, a[0], (0 * 5 + 3) MOD 8);
    Value (what & " static", 1, a[1], (1 * 5 + 3) MOD 8);
    Value (what & " static", 10, a[10], (10 * 5 + 3) MOD 8);
    Value (what & " static", 20, a[20], (20 * 5 + 3) MOD 8);
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

TYPE A5 = ARRAY [0..11] OF BITS 5 FOR [0..31];
VAR g5: A5;
TYPE R5 = RECORD pad: BITS 5 FOR [0..31];  a: A5 END;
VAR r5: R5;

PROCEDURE Fill5 (VAR a: A5;  what: TEXT) =
  VAR l: A5;
  BEGIN
    FOR i := 0 TO 11 DO a[i] := (i * 5 + 3) MOD 32;  l[i] := (i * 7 + 1) MOD 32 END;
    FOR i := 0 TO 11 DO
      Value (what, i, a[i], (i * 5 + 3) MOD 32);
      Value ("local 5", i, l[i], (i * 7 + 1) MOD 32);
    END;
    Value (what & " static", 0, a[0], (0 * 5 + 3) MOD 32);
    Value (what & " static", 1, a[1], (1 * 5 + 3) MOD 32);
    Value (what & " static", 6, a[6], (6 * 5 + 3) MOD 32);
    Value (what & " static", 11, a[11], (11 * 5 + 3) MOD 32);
  END Fill5;

TYPE A6 = ARRAY [0..9] OF BITS 6 FOR [0..63];
VAR g6: A6;
TYPE R6 = RECORD pad: BITS 5 FOR [0..31];  a: A6 END;
VAR r6: R6;

PROCEDURE Fill6 (VAR a: A6;  what: TEXT) =
  VAR l: A6;
  BEGIN
    FOR i := 0 TO 9 DO a[i] := (i * 5 + 3) MOD 64;  l[i] := (i * 7 + 1) MOD 64 END;
    FOR i := 0 TO 9 DO
      Value (what, i, a[i], (i * 5 + 3) MOD 64);
      Value ("local 6", i, l[i], (i * 7 + 1) MOD 64);
    END;
    Value (what & " static", 0, a[0], (0 * 5 + 3) MOD 64);
    Value (what & " static", 1, a[1], (1 * 5 + 3) MOD 64);
    Value (what & " static", 5, a[5], (5 * 5 + 3) MOD 64);
    Value (what & " static", 9, a[9], (9 * 5 + 3) MOD 64);
  END Fill6;

TYPE A7 = ARRAY [0..8] OF BITS 7 FOR [0..127];
VAR g7: A7;
TYPE R7 = RECORD pad: BITS 5 FOR [0..31];  a: A7 END;
VAR r7: R7;

PROCEDURE Fill7 (VAR a: A7;  what: TEXT) =
  VAR l: A7;
  BEGIN
    FOR i := 0 TO 8 DO a[i] := (i * 5 + 3) MOD 128;  l[i] := (i * 7 + 1) MOD 128 END;
    FOR i := 0 TO 8 DO
      Value (what, i, a[i], (i * 5 + 3) MOD 128);
      Value ("local 7", i, l[i], (i * 7 + 1) MOD 128);
    END;
    Value (what & " static", 0, a[0], (0 * 5 + 3) MOD 128);
    Value (what & " static", 1, a[1], (1 * 5 + 3) MOD 128);
    Value (what & " static", 4, a[4], (4 * 5 + 3) MOD 128);
    Value (what & " static", 8, a[8], (8 * 5 + 3) MOD 128);
  END Fill7;

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
  IF bad = 0 THEN IO.Put ("ok\n") END;
END Main.
