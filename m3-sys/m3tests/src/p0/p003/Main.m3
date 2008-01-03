(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: the Fmt interface *)

MODULE Main;

IMPORT Fmt, Wr, Stdio;

PROCEDURE Out (a, b, c, d, e: TEXT := NIL) =
  <*FATAL ANY*>
  BEGIN
    IF (a # NIL) THEN Wr.PutText (Stdio.stdout, a); END;
    IF (b # NIL) THEN Wr.PutText (Stdio.stdout, b); END;
    IF (c # NIL) THEN Wr.PutText (Stdio.stdout, c); END;
    IF (d # NIL) THEN Wr.PutText (Stdio.stdout, d); END;
    IF (e # NIL) THEN Wr.PutText (Stdio.stdout, e); END;
    Wr.PutText (Stdio.stdout, Wr.EOL);
  END Out;

PROCEDURE DoReal (name: TEXT;  x: REAL;  prec: INTEGER) =
  TYPE S = Fmt.Style;
  BEGIN
    Out (name, ", ", Fmt.Int (prec), ", Sci           = ",
          Fmt.Real (x, S.Sci, prec := prec));
    Out (name, ", ", Fmt.Int (prec), ", Fix           = ",
          Fmt.Real (x, S.Fix, prec := prec));
    Out (name, ", ", Fmt.Int (prec), ", Auto          = ",
          Fmt.Real (x, S.Auto, prec := prec));
    Out (name, ", ", Fmt.Int (prec), ", Sci,  Literal = ",
          Fmt.Real (x, S.Sci, prec := prec));
    Out (name, ", ", Fmt.Int (prec), ", Fix,  Literal = ",
          Fmt.Real (x, S.Fix, prec := prec));
    Out (name, ", ", Fmt.Int (prec), ", Auto, Literal = ",
          Fmt.Real (x, S.Auto, prec := prec));
    Out ();
  END DoReal;

BEGIN
  Out ("true  = ", Fmt.Bool (TRUE));
  Out ("false = ", Fmt.Bool (FALSE));
  Out ();

  Out ("0    = ", Fmt.Int (0));
  Out ("179  = ", Fmt.Int (179));
  Out ("-83  = ", Fmt.Int (-83));
  Out ("1000 = ", Fmt.Int (1000));
  Out ();

  Out ("(5) 179  = ", Fmt.Int (179, 5));
  Out ("(5) -83  = ", Fmt.Int (-83, 5));
  Out ("(5) 1000 = ", Fmt.Int (1000, 5));
  Out ();

  DoReal ("12.34", 12.34, 6);
  DoReal ("12.34", 12.34, 1);
  DoReal ("178.3456e-23", 178.345e-23, 6);
  DoReal ("178.3456e-23", 178.345e-23, 1);

  Out ("z = ", Fmt.Char ('z'));
  Out ();

  Out ("++++zzzz = ", Fmt.Pad ("zzzz", 8, '+', Fmt.Align.Right));
  Out ("zzzz++++ = ", Fmt.Pad ("zzzz", 8, '+', Fmt.Align.Left));
  Out ("+aaaaaaa = ", Fmt.Pad ("aaaaaaa", 8, '+', Fmt.Align.Right));
  Out ("aaaaaaaa = ", Fmt.Pad ("aaaaaaaa", 8, '+', Fmt.Align.Right));
  Out ("aaaaaaaa = ", Fmt.Pad ("aaaaaaaa", 3, '+', Fmt.Align.Right));
  Out ();

  Out ("done.");
END Main.
