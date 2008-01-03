(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Jul  5 09:21:58 PDT 1995 by kalsow    *)
(*      modified on Fri Jan  5 07:15:46 1990 by muller        *)

MODULE Main;

IMPORT Wr, Convert, Fmt;
FROM Stdio IMPORT stdout;
<*FATAL ANY*>


TYPE Buffer = ARRAY [0..255] OF CHAR;

PROCEDURE FromInt (VAR buf: Buffer;  VAR len: INTEGER;
                    value: INTEGER;  base: INTEGER;  prefix: BOOLEAN) =
  BEGIN
    Wr.PutText (stdout, "  FromInt (");
    Wr.PutText (stdout, Fmt.Int (value));
    Wr.PutText (stdout, ", base = ");
    Wr.PutText (stdout, Fmt.Int (base));
    IF (prefix)
      THEN Wr.PutText (stdout, ", prefix = TRUE) => ");
      ELSE Wr.PutText (stdout, ", prefix = FALSE) => ");
    END;
    TRY
      len := Convert.FromInt (buf, value, base, prefix);
      Wr.PutText (stdout, "\"");
      Wr.PutString (stdout, SUBARRAY (buf, 0, len));
      Wr.PutText (stdout, "\"\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "FAILED\n");
    END;
  END FromInt;

PROCEDURE ToInt (READONLY buf: Buffer;  len, base: INTEGER): INTEGER =
  VAR value, used: INTEGER;
  BEGIN
    Wr.PutText (stdout, "  ToInt (\"");
    Wr.PutString (stdout, SUBARRAY (buf, 0, len));
    Wr.PutText (stdout, "\", base = ");
    Wr.PutText (stdout, Fmt.Int (base));
    Wr.PutText (stdout, ") => ");
    value := Convert.ToInt (SUBARRAY (buf, 0, len), used, base);
    Wr.PutText (stdout, "  ");
    Wr.PutText (stdout, Fmt.Int (value));
    Wr.PutText (stdout, "   used = ");
    Wr.PutText (stdout, Fmt.Int (used));
    Wr.PutText (stdout, "\n");
    IF (used # len) THEN
      Wr.PutText (stdout, "**** previous conversion failed ****\n");
    END;
    RETURN value;
  END ToInt;

PROCEDURE DoInt (i: INTEGER) =
  VAR  buf: Buffer;  len, j: INTEGER;
  BEGIN

    Wr.PutText (stdout, "\n\n");
    Wr.PutText (stdout, Fmt.Int (i, 10));
    Wr.PutText (stdout, "   8_");
    Wr.PutText (stdout, Fmt.Int (i, 8));
    Wr.PutText (stdout, "   16_");
    Wr.PutText (stdout, Fmt.Int (i, 16));
    Wr.PutText (stdout, "\n\n");

    FOR base := 2 TO 16 DO
      FromInt (buf, len, i, base, FALSE);
      j := ToInt (buf, len, base);
      IF (i # j) THEN
        Wr.PutText (stdout, "**** previous conversion failed ****\n");
      END;
      FromInt (buf, len, i, base, TRUE);
      j := ToInt (buf, len, 10);
      IF (i # j) THEN
        Wr.PutText (stdout, "**** previous conversion failed ****\n");
      END;
    END;

  END DoInt;

PROCEDURE FromUnsigned (VAR buf: Buffer;  VAR len: INTEGER;
                    value: INTEGER;  base: INTEGER;  prefix: BOOLEAN) =
  BEGIN
    Wr.PutText (stdout, "  FromUnsigned (16_");
    Wr.PutText (stdout, Fmt.Unsigned (value, 16));
    Wr.PutText (stdout, ", base = ");
    Wr.PutText (stdout, Fmt.Int (base));
    IF (prefix)
      THEN Wr.PutText (stdout, ", prefix = TRUE) => ");
      ELSE Wr.PutText (stdout, ", prefix = FALSE) => ");
    END;
    TRY
      len := Convert.FromUnsigned (buf, value, base, prefix);
      Wr.PutText (stdout, "\"");
      Wr.PutString (stdout, SUBARRAY (buf, 0, len));
      Wr.PutText (stdout, "\"\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "FAILED\n");
    END;
  END FromUnsigned;

PROCEDURE ToUnsigned (READONLY buf: Buffer;  len, base: INTEGER): INTEGER =
  VAR value, used: INTEGER;
  BEGIN
    Wr.PutText (stdout, "  ToUnsigned (\"");
    Wr.PutString (stdout, SUBARRAY (buf, 0, len));
    Wr.PutText (stdout, "\", base = ");
    Wr.PutText (stdout, Fmt.Int (base));
    Wr.PutText (stdout, ") => ");
    value := Convert.ToUnsigned (SUBARRAY (buf, 0, len), used, base);
    Wr.PutText (stdout, "  16_");
    Wr.PutText (stdout, Fmt.Unsigned (value, 16));
    Wr.PutText (stdout, "   used = ");
    Wr.PutText (stdout, Fmt.Int (used));
    Wr.PutText (stdout, "\n");
    IF (used # len) THEN
      Wr.PutText (stdout, "**** previous conversion failed ****\n");
    END;
    RETURN value;
  END ToUnsigned;

PROCEDURE DoUnsigned (i: INTEGER) =
  VAR  buf: Buffer;  len, j: INTEGER;
  BEGIN

    Wr.PutText (stdout, "\n\n");
    Wr.PutText (stdout, "8_");
    Wr.PutText (stdout, Fmt.Unsigned (i, 8));
    Wr.PutText (stdout, "   16_");
    Wr.PutText (stdout, Fmt.Unsigned (i, 16));
    Wr.PutText (stdout, "\n\n");

    FOR base := 2 TO 16 DO
      FromUnsigned (buf, len, i, base, FALSE);
      j := ToUnsigned (buf, len, base);
      IF (i # j) THEN
        Wr.PutText (stdout, "**** previous conversion failed ****\n");
      END;
      FromUnsigned (buf, len, i, base, TRUE);
      j := ToUnsigned (buf, len, 10);
      IF (i # j) THEN
        Wr.PutText (stdout, "**** previous conversion failed ****\n");
      END;
    END;

  END DoUnsigned;


PROCEDURE DoFloat (r: REAL) =
  VAR
    buf: ARRAY [0..255] OF CHAR;
    len: INTEGER;
    s  : REAL;
    used: INTEGER;
  BEGIN

    Wr.PutText (stdout, "\n\n");
    Wr.PutText (stdout, Fmt.Real (r, Fmt.Style.Auto, prec := 15));
    Wr.PutText (stdout, "\n\n");

    TRY
      len := Convert.FromFloat (buf, r, 15, Convert.Style.Flo);
      Wr.PutText (stdout, "  FromFloat [Flo]    => ");
      Wr.PutString (stdout, SUBARRAY (buf, 0, len));
      Wr.PutText (stdout, "\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.FromFloat failed\n");
    END;

    TRY
      s := Convert.ToFloat (SUBARRAY (buf, 0, len), used);
      Wr.PutText (stdout, "  ToFloat => ");
      Wr.PutText (stdout, Fmt.Real (s, Fmt.Style.Auto, 15));
      Wr.PutText (stdout, "   / " & Fmt.Int (used) &"\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.ToFloat failed\n");
    END;

    TRY
      len := Convert.FromFloat (buf, r, 15, Convert.Style.AltFlo);
      Wr.PutText (stdout, "  FromFloat [AltFlo] => ");
      Wr.PutString (stdout, SUBARRAY (buf, 0, len));
      Wr.PutText (stdout, "\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.FromFloat failed\n");
    END;

    TRY
      s := Convert.ToFloat (SUBARRAY (buf, 0, len), used);
      Wr.PutText (stdout, "  ToFloat => ");
      Wr.PutText (stdout, Fmt.Real (s, Fmt.Style.Auto, 15));
      Wr.PutText (stdout, "   / " & Fmt.Int (used) &"\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.ToFloat failed\n");
    END;

    TRY
      len := Convert.FromFloat (buf, r, 15, Convert.Style.Sci);
      Wr.PutText (stdout, "  FromFloat [Sci]    => ");
      Wr.PutString (stdout, SUBARRAY (buf, 0, len));
      Wr.PutText (stdout, "\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.FromFloat failed\n");
    END;

    TRY
      s := Convert.ToFloat (SUBARRAY (buf, 0, len), used);
      Wr.PutText (stdout, "  ToFloat => ");
      Wr.PutText (stdout, Fmt.Real (s, Fmt.Style.Auto, 15));
      Wr.PutText (stdout, "   / " & Fmt.Int (used) &"\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.ToFloat failed\n");
    END;

    TRY
      len := Convert.FromFloat (buf, r, 15, Convert.Style.AltSci);
      Wr.PutText (stdout, "  FromFloat [AltSci] => ");
      Wr.PutString (stdout, SUBARRAY (buf, 0, len));
      Wr.PutText (stdout, "\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.FromFloat failed\n");
    END;

    TRY
      s := Convert.ToFloat (SUBARRAY (buf, 0, len), used);
      Wr.PutText (stdout, "  ToFloat => ");
      Wr.PutText (stdout, Fmt.Real (s, Fmt.Style.Auto, 15));
      Wr.PutText (stdout, "   / " & Fmt.Int (used) &"\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.ToFloat failed\n");
    END;

    TRY
      len := Convert.FromFloat (buf, r, 15, Convert.Style.Mix);
      Wr.PutText (stdout, "  FromFloat [Mix]    => ");
      Wr.PutString (stdout, SUBARRAY (buf, 0, len));
      Wr.PutText (stdout, "\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.FromFloat failed\n");
    END;

    TRY
      s := Convert.ToFloat (SUBARRAY (buf, 0, len), used);
      Wr.PutText (stdout, "  ToFloat => ");
      Wr.PutText (stdout, Fmt.Real (s, Fmt.Style.Auto, 15));
      Wr.PutText (stdout, "   / " & Fmt.Int (used) &"\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.ToFloat failed\n");
    END;
  END DoFloat;

PROCEDURE DoLongFloat (r: LONGREAL) =
  VAR
    buf: ARRAY [0..255] OF CHAR;
    len: INTEGER;
    s  : LONGREAL;
    used: INTEGER;
  BEGIN

    Wr.PutText (stdout, "\n\n");
    Wr.PutText (stdout, Fmt.LongReal (r, Fmt.Style.Auto, 15));
    Wr.PutText (stdout, "\n\n");

    TRY
      len := Convert.FromLongFloat (buf, r, 15, Convert.Style.Flo);
      Wr.PutText (stdout, "  FromLongFloat [Flo]    => ");
      Wr.PutString (stdout, SUBARRAY (buf, 0, len));
      Wr.PutText (stdout, "\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.FromLongFloat failed\n");
    END;

    TRY
      s := Convert.ToLongFloat (SUBARRAY (buf, 0, len), used);
      Wr.PutText (stdout, "  ToLongFloat => ");
      Wr.PutText (stdout, Fmt.LongReal (s, Fmt.Style.Auto, 15));
      Wr.PutText (stdout, "   / " & Fmt.Int (used) &"\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.ToLongFloat failed\n");
    END;

    TRY
      len := Convert.FromLongFloat (buf, r, 15, Convert.Style.AltFlo);
      Wr.PutText (stdout, "  FromLongFloat [AltFlo] => ");
      Wr.PutString (stdout, SUBARRAY (buf, 0, len));
      Wr.PutText (stdout, "\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.FromLongFloat failed\n");
    END;

    TRY
      s := Convert.ToLongFloat (SUBARRAY (buf, 0, len), used);
      Wr.PutText (stdout, "  ToLongFloat => ");
      Wr.PutText (stdout, Fmt.LongReal (s, Fmt.Style.Auto, 15));
      Wr.PutText (stdout, "   / " & Fmt.Int (used) &"\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.ToLongFloat failed\n");
    END;

    TRY
      len := Convert.FromLongFloat (buf, r, 15, Convert.Style.Sci);
      Wr.PutText (stdout, "  FromLongFloat [Sci]    => ");
      Wr.PutString (stdout, SUBARRAY (buf, 0, len));
      Wr.PutText (stdout, "\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.FromLongFloat failed\n");
    END;

    TRY
      s := Convert.ToLongFloat (SUBARRAY (buf, 0, len), used);
      Wr.PutText (stdout, "  ToLongFloat => ");
      Wr.PutText (stdout, Fmt.LongReal (s, Fmt.Style.Auto, 15));
      Wr.PutText (stdout, "   / " & Fmt.Int (used) &"\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.ToLongFloat failed\n");
    END;

    TRY
      len := Convert.FromLongFloat (buf, r, 15, Convert.Style.AltSci);
      Wr.PutText (stdout, "  FromLongFloat [AltSci] => ");
      Wr.PutString (stdout, SUBARRAY (buf, 0, len));
      Wr.PutText (stdout, "\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.FromLongFloat failed\n");
    END;

    TRY
      s := Convert.ToLongFloat (SUBARRAY (buf, 0, len), used);
      Wr.PutText (stdout, "  ToLongFloat => ");
      Wr.PutText (stdout, Fmt.LongReal (s, Fmt.Style.Auto, 15));
      Wr.PutText (stdout, "   / " & Fmt.Int (used) &"\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.ToLongFloat failed\n");
    END;

    TRY
      len := Convert.FromLongFloat (buf, r, 15, Convert.Style.Mix);
      Wr.PutText (stdout, "  FromLongFloat [Mix]    => ");
      Wr.PutString (stdout, SUBARRAY (buf, 0, len));
      Wr.PutText (stdout, "\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.FromLongFloat failed\n");
    END;

    TRY
      s := Convert.ToLongFloat (SUBARRAY (buf, 0, len), used);
      Wr.PutText (stdout, "  ToLongFloat => ");
      Wr.PutText (stdout, Fmt.LongReal (s, Fmt.Style.Auto, 15));
      Wr.PutText (stdout, "   / " & Fmt.Int (used) &"\n");
    EXCEPT Convert.Failed =>
      Wr.PutText (stdout, "Convert.ToLongFloat failed\n");
    END;
  END DoLongFloat;

BEGIN
  DoInt (0);
  DoInt (1);
  DoInt (12345);
  DoInt (-12345);
  DoInt (LAST (INTEGER));
  DoInt (FIRST (INTEGER));

  DoUnsigned (0);
  DoUnsigned (1);
  DoUnsigned (12345);
  DoUnsigned (-12345);
  DoUnsigned (LAST (INTEGER));
  DoUnsigned (FIRST (INTEGER));

  DoFloat (0.0e+0);
  DoFloat (1.0e+0);
  DoFloat (0.5e+0);
  DoFloat (0.1e+0);
  DoFloat (1.234567890123456e-3);
  DoFloat (1.234567890123456e-33);
  DoFloat (1.234567890123456e-38);
  DoFloat (1.234567890123456e-29);
  DoFloat (1.234567890123456e+3);
  DoFloat (1.234567890123456e+27);

  DoLongFloat (0.0d+0);
  DoLongFloat (1.0d+0);
  DoLongFloat (0.5d+0);
  DoLongFloat (0.1d+0);
  DoLongFloat (1.234567890123456d-3);
  DoLongFloat (1.234567890123456d-33);
  DoLongFloat (1.234567890123456d-38);
  DoLongFloat (1.234567890123456d-29);
  DoLongFloat (1.234567890123456d+3);
  DoLongFloat (1.234567890123456d+27);

  Wr.Close (stdout);
END Main.
