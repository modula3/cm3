(* $Id$ *)

MODULE DerivRidders; (* num. rec. /c 2nd pp. 188--189 *)
IMPORT LRFunction AS Function;

CONST
  Con   = 1.4d0;
  ConSq = Con * Con;
  Big   = LAST(LONGREAL);
  Ntab  = 10;
  Safe  = 2.0d0;

PROCEDURE Deriv(func : Function.T; 
                x : LONGREAL;      
                h : LONGREAL;      
                VAR err : LONGREAL 
               ) : LONGREAL =

  PROCEDURE MidPoint(x, hh : LONGREAL) : LONGREAL =
    BEGIN RETURN (func.eval(x+hh) - func.eval(x-hh))/(2.0d0*hh) END MidPoint;

  VAR
    a : ARRAY [0..Ntab-1] OF ARRAY [0..Ntab-1] OF LONGREAL;
    hh, ans, errt, fac : LONGREAL;
  BEGIN
    <* ASSERT h # 0.0d0 *>
    hh := h;
    a[0,0] := MidPoint(x,hh);
    err := Big;
    FOR i := 1 TO LAST(a) DO
      hh := hh/Con;
      a[0,i] := MidPoint(x,hh);
      fac := ConSq;
      FOR j := 1 TO i DO
        a[j,i] := (a[j-1,i]*fac - a[j-1,i-1])/(fac-1.0d0);
        fac := ConSq * fac;
        errt := MAX(ABS(a[j,i]-a[j-1,i]),ABS(a[j,i]-a[j-1,i-1]));
        IF errt <= err THEN
          err := errt;
          ans := a[j,i]
        END
      END;
      IF ABS(a[i,i]-a[i-1,i-1]) >= Safe * err THEN EXIT END
    END;
    RETURN ans
  END Deriv;

BEGIN END DerivRidders.
