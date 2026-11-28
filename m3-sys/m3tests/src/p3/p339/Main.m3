(* FIRST DIV -1 wraps around to FIRST, and FIRST MOD -1 is 0, for INTEGER
   and LONGINT, at run time and in constant expressions, with no trap.
   Hardware division traps there (x86) and C leaves it undefined, so the
   front end divides by 1 instead when the divisor is -1.  The output is
   the same for 32- and 64-bit INTEGER. *)

MODULE Main;

IMPORT IO;

CONST
  CD  = FIRST (INTEGER) DIV -1;   (* warns: the result wraps around *)
  CM  = FIRST (INTEGER) MOD -1;
  CLD = FIRST (LONGINT) DIV -1L;  (* warns *)
  CLM = FIRST (LONGINT) MOD -1L;

TYPE Small = [-5 .. 5];

PROCEDURE Check (name: TEXT;  ok: BOOLEAN) =
  BEGIN
    IF ok
      THEN IO.Put (name & ": pass\n");
      ELSE IO.Put (name & ": FAIL\n");
    END;
  END Check;

PROCEDURE Int (a, b: INTEGER;  q, r: INTEGER;  name: TEXT) =
  BEGIN
    Check (name & " DIV", a DIV b = q);
    Check (name & " MOD", a MOD b = r);
  END Int;

PROCEDURE Long (a, b: LONGINT;  q, r: LONGINT;  name: TEXT) =
  BEGIN
    Check (name & " DIV", a DIV b = q);
    Check (name & " MOD", a MOD b = r);
  END Long;

VAR
  F := FIRST (INTEGER);  L := LAST (INTEGER);
  FL := FIRST (LONGINT);  LL := LAST (LONGINT);
  s: Small := -1;  t: Small := 5;
BEGIN
  Check ("constant INTEGER DIV", CD = FIRST (INTEGER));
  Check ("constant INTEGER MOD", CM = 0);
  Check ("constant LONGINT DIV", CLD = FIRST (LONGINT));
  Check ("constant LONGINT MOD", CLM = 0L);

  Int (F, -1, F, 0, "INTEGER FIRST, -1");
  Int (F + 1, -1, L, 0, "INTEGER FIRST+1, -1");
  Int (L, -1, F + 1, 0, "INTEGER LAST, -1");
  Int (-7, -1, 7, 0, "INTEGER -7, -1");
  Int (7, -2, -4, -1, "INTEGER 7, -2");
  Int (-7, 2, -4, 1, "INTEGER -7, 2");
  Int (F, 1, F, 0, "INTEGER FIRST, 1");
  Int (F, -2, -(F DIV 2), 0, "INTEGER FIRST, -2");

  Long (FL, -1L, FL, 0L, "LONGINT FIRST, -1");
  Long (FL + 1L, -1L, LL, 0L, "LONGINT FIRST+1, -1");
  Long (-7L, -1L, 7L, 0L, "LONGINT -7, -1");
  Long (7L, -2L, -4L, -1L, "LONGINT 7, -2");

  (* subranges: no FIRST dividend, so no wrapping code *)
  Check ("subrange DIV", t DIV s = -5);
  Check ("subrange MOD", t MOD s = 0);
END Main.
