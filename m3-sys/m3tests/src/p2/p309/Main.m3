MODULE Main;

(* Exception arguments of type REAL and LONGREAL, passed by value in the
   activation's argument word.  The C backend cast a REAL to and from an
   ADDRESS (invalid C) instead of carrying its bits. *)
IMPORT IO, Fmt, Real AS R, LongReal AS LR, RealFloat, LongFloat;
EXCEPTION ER (REAL);  EL (LONGREAL);
PROCEDURE RaiseR (x: REAL) RAISES {ER} = BEGIN RAISE ER (x) END RaiseR;
PROCEDURE RaiseL (x: LONGREAL) RAISES {EL} = BEGIN RAISE EL (x) END RaiseL;
PROCEDURE TryR (x: REAL): TEXT =
  BEGIN
    TRY RaiseR (x) EXCEPT ER (v) =>
      IF RealFloat.IsNaN (v) THEN RETURN "nan" END;
      RETURN Fmt.Real (v, prec := 9)
    END;
    RETURN "none";
  END TryR;
PROCEDURE TryL (x: LONGREAL): TEXT =
  BEGIN
    TRY RaiseL (x) EXCEPT EL (v) =>
      IF LongFloat.IsNaN (v) THEN RETURN "nan" END;
      RETURN Fmt.LongReal (v, prec := 17)
    END;
    RETURN "none";
  END TryL;
VAR zr := 0.0;  zl := 0.0d0;
BEGIN
  IO.Put ("REAL     " & TryR (1.5) & " " & TryR (-2.25) & " " & TryR (R.MaxFinite)
          & " " & TryR (zr / zr) & "\n");
  IO.Put ("LONGREAL " & TryL (1.5d0) & " " & TryL (-2.25d0) & " " & TryL (LR.MaxFinite)
          & " " & TryL (zl / zl) & "\n");
END Main.
