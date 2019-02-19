(* $Id$ *)

MODULE Fortran;
FROM Math IMPORT sqrt;

PROCEDURE Sign(a, b : LONGREAL) : LONGREAL =
  BEGIN
    IF b > 0.0d0 THEN RETURN ABS(a) ELSE RETURN -ABS(a) END
  END Sign;

PROCEDURE pythag(a, b : LONGREAL) : LONGREAL =
  VAR
    absa := ABS(a);
    absb := ABS(b);
  BEGIN
    IF absa > absb THEN
      RETURN absa * sqrt(1.0d0 + (absb/absa)*(absb/absa))
    ELSE
      IF absb = 0.0d0 THEN
        RETURN 0.0d0
      ELSE
        RETURN absb * sqrt(1.0d0 + (absa/absb)*(absa/absb))
      END
    END
  END pythag;

BEGIN END Fortran.
