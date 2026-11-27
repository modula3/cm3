(* Variables of a record type are aligned as the front end laid the
   record out.  A record with a LONGREAL is 8-aligned for the C backend
   even on 32-bit targets, but M3C's C struct for it only pads the field
   offsets, and the i386 System V ABI aligns doubles in structures to 4:
   a local of such a record could start at a 4-byte boundary.  The
   alignment each check uses is the one the front end chose, the offset
   of the field after a CHAR, so the expected output is the same for
   every target and backend. *)

UNSAFE MODULE Main;

IMPORT IO, Fmt, Word;

TYPE
  RL = RECORD c: CHAR;  d: LONGREAL;  i: INTEGER END;
  RX = RECORD c: CHAR;  x: EXTENDED;  i: INTEGER END;
  RI = RECORD c: CHAR;  l: LONGINT END;

VAR
  gl: RL;  gx: RX;  gi: RI;
  ga: ARRAY [0..2] OF RL;
  hx := NEW (REF RX);
  alignL, alignX, alignI: INTEGER;

PROCEDURE Check (name: TEXT;  a: ADDRESS;  n: INTEGER) =
  BEGIN
    IO.Put (name & " " & Fmt.Bool (Word.Mod (LOOPHOLE (a, Word.T), n) = 0) & "\n");
  END Check;

PROCEDURE Locals () =
  VAR c1: CHAR;  l: RL;  c2: CHAR;  x: RX;  c3: CHAR;  i: RI;
      a: ARRAY [0..2] OF RL;
  BEGIN
    c1 := 'a';  c2 := 'b';  c3 := 'c';
    Check ("local RL", ADR (l.d), alignL);
    Check ("local RX", ADR (x.x), alignX);
    Check ("local RI", ADR (i.l), alignI);
    FOR k := 0 TO 2 DO Check ("local array RL", ADR (a[k].d), alignL) END;
    EVAL c1;  EVAL c2;  EVAL c3;
  END Locals;

PROCEDURE UpLevel () =
  VAR c: CHAR;  u: RL;  v: RX;
  PROCEDURE Inner () =
    BEGIN
      Check ("up-level RL", ADR (u.d), alignL);
      Check ("up-level RX", ADR (v.x), alignX);
    END Inner;
  BEGIN
    c := 'z';  EVAL c;
    Inner ();
  END UpLevel;

PROCEDURE Aligns () =
  VAR l: RL;  x: RX;  i: RI;
  BEGIN
    alignL := ADR (l.d) - ADR (l);
    alignX := ADR (x.x) - ADR (x);
    alignI := ADR (i.l) - ADR (i);
  END Aligns;

BEGIN
  Aligns ();
  Check ("global RL", ADR (gl.d), alignL);
  Check ("global RX", ADR (gx.x), alignX);
  Check ("global RI", ADR (gi.l), alignI);
  FOR k := 0 TO 2 DO Check ("global array RL", ADR (ga[k].d), alignL) END;
  Check ("heap RX", ADR (hx.x), alignX);
  Locals ();
  UpLevel ();
END Main.
