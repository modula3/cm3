(* Packed designators (sub-byte fields of records and objects, elements of
   bit-packed arrays, fields of a bit-packed record field): the base of the
   designator is evaluated once by assignment, INC, DEC and WITH, and
   neighbouring bits are preserved. *)

MODULE Main;
IMPORT IO, Fmt;

TYPE
  R   = RECORD a: BITS 3 FOR [0..7]; b: BITS 5 FOR [0..31]; c: BITS 4 FOR [0..15] END;
  In  = BITS 13 FOR RECORD a: BITS 4 FOR [0..15]; b: BITS 9 FOR [-256..255] END;
  Out = RECORD x: BITS 3 FOR [0..7]; f: In; y: BITS 6 FOR [0..63] END;
  O   = OBJECT k: BITS 3 FOR [0..7]; m: BITS 5 FOR [-16..15] END;

VAR
  arr : ARRAY [0..3] OF R;
  pa  : ARRAY [0..7] OF BITS 4 FOR [0..15];
  outs: ARRAY [0..3] OF Out;
  o   := NEW (O);
  n   := 0;

PROCEDURE One (): INTEGER = BEGIN INC (n); RETURN 1 END One;
PROCEDURE Two (): INTEGER = BEGIN INC (n); RETURN 2 END Two;
PROCEDURE GetO (): O = BEGIN INC (n); RETURN o END GetO;
PROCEDURE MkOut (): Out =
  VAR r: Out;
  BEGIN
    INC (n);  r.x := 5;  r.f.a := 9;  r.f.b := -100;  r.y := 33;
    RETURN r;
  END MkOut;

PROCEDURE Calls (what: TEXT;  want: INTEGER) =
  BEGIN
    IF n # want
      THEN IO.Put ("FAIL " & what & ": " & Fmt.Int (n) & " calls\n");
      ELSE IO.Put ("ok " & what & "\n");
    END;
    n := 0;
  END Calls;

PROCEDURE Value (what: TEXT;  got, want: INTEGER) =
  BEGIN
    IF got # want
      THEN IO.Put ("FAIL " & what & " = " & Fmt.Int (got) & ", want " & Fmt.Int (want) & "\n");
      ELSE IO.Put ("ok " & what & "\n");
    END;
  END Value;

BEGIN
  (* sub-byte record fields and packed array elements *)
  arr[1].c := 5;
  arr[One ()].b := 17;            Calls ("assign field", 1);
  INC (arr[One ()].b);            Calls ("INC field", 1);
  INC (arr[One ()].b, 3);         Calls ("INC field by", 1);
  DEC (arr[One ()].c);            Calls ("DEC field", 1);
  pa[Two ()] := 9;                Calls ("assign packed element", 1);
  INC (pa[Two ()]);               Calls ("INC packed element", 1);
  WITH x = pa[Two ()] DO x := 3;  INC (x) END;
                                  Calls ("WITH packed element", 1);
  Value ("WITH packed element", pa[2], 4);
  Value ("field b", arr[1].b, 21);
  Value ("field c", arr[1].c, 4);

  (* fields of a bit-packed record field *)
  outs[2].x := 7;  outs[2].y := 63;
  outs[Two ()].f.a := 11;         Calls ("nested assign a", 1);
  outs[Two ()].f.b := -200;       Calls ("nested assign b", 1);
  INC (outs[Two ()].f.b, 50);     Calls ("nested INC b", 1);
  DEC (outs[Two ()].f.a);         Calls ("nested DEC a", 1);
  Value ("nested a", outs[2].f.a, 10);
  Value ("nested b", outs[2].f.b, -150);
  Value ("neighbour x", outs[2].x, 7);
  Value ("neighbour y", outs[2].y, 63);

  (* sub-byte object fields *)
  GetO ().k := 6;                 Calls ("object assign k", 1);
  GetO ().m := -9;                Calls ("object assign m", 1);
  INC (GetO ().m, 3);             Calls ("object INC m", 1);
  Value ("object k", o.k, 6);
  Value ("object m", o.m, -6);

  (* fields of a function result *)
  Value ("result a", MkOut ().f.a, 9);   Calls ("result read", 1);
  Value ("result b", MkOut ().f.b, -100);
  Value ("result y", MkOut ().y, 33);
END Main.
