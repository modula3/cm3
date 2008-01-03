(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Test;

IMPORT Wr, Stdio, Fmt, Text, Cstdlib;
<*FATAL ANY*>

PROCEDURE msg (t: Text.T) =
  BEGIN
    Out (t);
  END msg;

PROCEDURE msgB (b: BOOLEAN) =
  BEGIN
    Out (Fmt.Bool (b));
  END msgB;

PROCEDURE msgI (i: INTEGER) =
  BEGIN
    Out (Fmt.Int (i));
  END msgI;

PROCEDURE msgC (c: CHAR) =
  BEGIN
    Out (Fmt.Char (c));
  END msgC;

PROCEDURE msgR (c: REAL) =
  BEGIN
    Out (Fmt.Real (c));
  END msgR;

PROCEDURE warn (b: BOOLEAN) =
  BEGIN
    IF NOT b THEN
      Out ("------------------------ WARNING");
      INC (warnings);
    END;
  END warn;

PROCEDURE check (b: BOOLEAN) =
  BEGIN
    IF NOT b THEN Err() END;
  END check;

PROCEDURE checkB (b: BOOLEAN; shouldBe: BOOLEAN) =
  BEGIN
    IF NOT b = shouldBe THEN
      Err (Fmt.Bool (b), " instead of ", Fmt.Bool (shouldBe));
    END;
  END checkB;

PROCEDURE checkI (i: INTEGER; shouldBe: INTEGER) =
  BEGIN
    IF NOT i = shouldBe THEN
      Err (Fmt.Int (i), " instead of ", Fmt.Int (shouldBe));
    END;
  END checkI;

PROCEDURE checkC (c, shouldBe: CHAR) =
  BEGIN
    IF NOT c = shouldBe THEN
      Err (Fmt.Char (c), " instead of ", Fmt.Char (shouldBe));
    END;
  END checkC;

PROCEDURE checkR (c, shouldBe: REAL) =
  BEGIN
    IF NOT c = shouldBe THEN
      Err (Fmt.Real (c), " instead of ", Fmt.Real (shouldBe));
    END;
  END checkR;

PROCEDURE checkL (c, shouldBe: LONGREAL) =
  BEGIN
    IF NOT c = shouldBe THEN
      Err (Fmt.LongReal (c), " instead of ", Fmt.LongReal (shouldBe));
    END;
  END checkL;

PROCEDURE checkX (c, shouldBe: EXTENDED) =
  BEGIN
    IF NOT c = shouldBe THEN
      Err (Fmt.LongReal (FLOAT (c, LONGREAL)), " instead of ",
           Fmt.LongReal (FLOAT (shouldBe, LONGREAL)));
    END;
  END checkX;

PROCEDURE done () =
  BEGIN
    Out ("\n\n", Fmt.Int (errors), " error(s) and ",
                              Fmt.Int (warnings), " warning(s) detected");
    Wr.Flush (Stdio.stderr);
    IF errors # 0 THEN Cstdlib.exit (1); END;
  END done;

PROCEDURE Err (a, b, c, d: TEXT := NIL) =
  BEGIN
    Out ("************************ ERROR: ", a, b, c, d);
    INC (errors);
  END Err;

PROCEDURE Out (a, b, c, d, e: TEXT := NIL) =
  BEGIN
    IF (a # NIL) THEN Wr.PutText (Stdio.stderr, a) END;
    IF (b # NIL) THEN Wr.PutText (Stdio.stderr, b) END;
    IF (c # NIL) THEN Wr.PutText (Stdio.stderr, c) END;
    IF (d # NIL) THEN Wr.PutText (Stdio.stderr, d) END;
    IF (e # NIL) THEN Wr.PutText (Stdio.stderr, e) END;
    Wr.PutChar (Stdio.stderr, '\n');
  END Out;

BEGIN
END Test.
