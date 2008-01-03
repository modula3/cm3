MODULE Math;

(* I have tested these computations in Haskell,
   however I did not check the port to Modula-3.
   The values and number of terms are chosen with respect to IEEE double precision. *)

CONST
  d1=1.0D0;
  d2=2.0D0;
  r2=d1/2.0D0;
  r3=d1/3.0D0;
  r4=d1/4.0D0;
  r5=d1/5.0D0;
  r6=d1/6.0D0;
  r7=d1/7.0D0;
  r8=d1/8.0D0;
  r9=d1/9.0D0;
  r11=d1/11.0D0;
  r13=d1/13.0D0;
  r15=d1/15.0D0;

(* untested *)
PROCEDURE expm1 (x: LONGREAL): LONGREAL =
  BEGIN
    IF ABS(x)<0.04D0 THEN
      (* Taylor expansion of exp(x)-1 *)
      (* Maybe we could make use of exp(x) = cosh(x) + sinh(x) *)
      RETURN x*(d1+x*r2*(d1+x*r3*(d1+x*r4*(d1+x*r5*(d1+x*r6*(d1+x*r7*(d1+x*r8)))))));
    ELSE
      RETURN exp(x)-1.0D0;
    END;
  END expm1;

(* untested *)
PROCEDURE log1p (x: LONGREAL): LONGREAL =
  BEGIN
    IF ABS(x)<0.04D0 THEN
      RETURN d2*atanh_small(x/(x+d2));
    ELSE
      RETURN log(1.0D0+x);
    END;
  END log1p;


(* http://darcs.haskell.org/haskore/src/Haskore/Interface/CSound/Orchestra.lhs *)

(* untested *)
PROCEDURE asinh (x: LONGREAL): LONGREAL =
  BEGIN
    RETURN log (sqrt (x*x+d1) + x);
  END asinh;

(* untested *)
PROCEDURE acosh (x: LONGREAL): LONGREAL =
  BEGIN
    RETURN log (sqrt (x*x-d1) + x);
  END acosh;

(* untested *)
PROCEDURE atanh (x: LONGREAL): LONGREAL =
  BEGIN
    IF ABS(x)<0.1D0 THEN
      RETURN atanh_small(x);
    ELSE
      RETURN (log(d1+x) - log(d1-x)) * r2;
    END;
  END atanh;

(* untested *)
PROCEDURE atanh_small (x: LONGREAL): LONGREAL =
  BEGIN
    (* Taylor expansion of ln(1+x):
          (\x -> x*(1+x*(-1/2+x*(1/3+x*(-1/4+x*(1/5+x*(-1/6+x*(1/7-x/8)))))))) *)
    (* Taylor expansion of ln((1+x)/(1-x))/2, which is atanh(x) *)
    (* The values are chosen with respect to IEEE double precision *)
    WITH x2 = x*x DO
      RETURN x*(d1+x2*(r3+x2*(r5+x2*(r7+x2*(r9+x2*(r11+x2*(r13+x2*r15)))))));
    END;
  END atanh_small;



BEGIN
END Math.
