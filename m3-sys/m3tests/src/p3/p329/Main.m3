(* Word.Extract and Word.Insert (and Long) of the full word width, and at
   offset BITSIZE with count 0.  The C backend built the mask with a shift
   by the full width, which C leaves undefined.  W is the word size; the
   32-bit results are in stdout.pgm32. *)

MODULE Main;

IMPORT IO, Fmt, Word, Long;

CONST W = BITSIZE (Word.T);

VAR x: Word.T := 16_FF;  y: Word.T := 16_5A;  lx: LONGINT := 16_FFL;  ly: LONGINT := 16_5AL;
    i, n: CARDINAL;

PROCEDURE P(t: TEXT; v: Word.T) = BEGIN IO.Put(t & " = 16_" & Fmt.Unsigned(v) & "\n") END P;
PROCEDURE L(t: TEXT; v: LONGINT) = BEGIN IO.Put(t & " = 16_" & Fmt.LongUnsigned(v) & "\n") END L;

BEGIN
  i := 0;  n := W;
  P("Extract(16_FF, 0, W)", Word.Extract(x, i, n));
  P("Extract(-1, 0, W)", Word.Extract(Word.Not(0), i, n));
  P("Insert(16_FF, 16_5A, 0, W)", Word.Insert(x, y, i, n));
  i := W;  n := 0;
  P("Extract(16_FF, W, 0)", Word.Extract(x, i, n));
  P("Insert(16_FF, 16_5A, W, 0)", Word.Insert(x, y, i, n));
  i := W - 4;  n := 4;
  P("Extract(-1, W-4, 4)", Word.Extract(Word.Not(0), i, n));
  P("Insert(0, 16_5A, W-4, 4)", Word.Insert(0, y, i, n));
  i := 4;  n := 8;
  P("Extract(16_FF, 4, 8)", Word.Extract(x, i, n));
  P("Insert(16_FF, 16_5A, 4, 8)", Word.Insert(x, y, i, n));
  i := 0;  n := 64;
  L("Long.Extract(16_FF, 0, 64)", Long.Extract(lx, i, n));
  L("Long.Insert(16_FF, 16_5A, 0, 64)", Long.Insert(lx, ly, i, n));
  i := 64;  n := 0;
  L("Long.Insert(16_FF, 16_5A, 64, 0)", Long.Insert(lx, ly, i, n));
END Main.
