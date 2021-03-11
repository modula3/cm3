(* $Id$ *)

MODULE CubicSpline;

REVEAL
  T = Public BRANDED Brand OBJECT
    coords : REF ARRAY OF Coord;
    secondDerivs : REF ARRAY OF LONGREAL;
  OVERRIDES
    init := Init;
    eval := Eval;
  END;


PROCEDURE Init(self : T; 
               READONLY coords : ARRAY OF Coord;
               firstDeriv,lastDeriv := NaturalSpline) : T =
  VAR
    n := NUMBER(coords);
    u := NEW(REF ARRAY OF LONGREAL, n);
  BEGIN
    FOR i := 0 TO n - 2 DO
      <* ASSERT coords[i].x < coords[i+1].x *>
    END;

    self.coords := NEW(REF ARRAY OF Coord, n);
    self.coords^ := coords;
    self.secondDerivs := NEW(REF ARRAY OF LONGREAL, n);
    
    WITH y2 = self.secondDerivs DO
      IF firstDeriv >= NaturalSpline THEN
        u[0] := 0.0d0;
        y2[0] := 0.0d0;
      ELSE
        y2[0] := -0.5d0;
        u[0] := (3.0d0/(coords[1].x - coords[0].x))*
                ((coords[1].y - coords[0].y)/
                 (coords[1].x - coords[0].x) - firstDeriv)
      END;

      FOR i := 1 TO n - 2 DO 
        VAR 
          sig := (coords[i].x-coords[i-1].x)/(coords[i+1].x-coords[i-1].x);
          p := sig*y2[i-1]+2.0d0;
        BEGIN
          y2[i] := (sig-1.0d0)/p;

          u[i] := (coords[i+1].y-coords[i].y)/(coords[i+1].x-coords[i].x) - 
                  (coords[i].y-coords[i-1].y)/(coords[i].x-coords[i-1].x);

          u[i] := (6.0d0*u[i]/(coords[i+1].x-coords[i-1].x)-sig*u[i-1])/p
        END
      END;

      VAR
        qn, un : LONGREAL;
      BEGIN
        IF lastDeriv >= NaturalSpline THEN
          qn := 0.0d0;
          un := 0.0d0;
        ELSE
          qn := 0.5d0;
          un := (3.0d0/(coords[n-1].x - coords[n-2].x))*
          (lastDeriv - (coords[n-1].y - coords[n-2].y)/
          (coords[n-1].x - coords[n-2].x))
        END;
      
        y2[(n-1)] := (un-qn*u[n-2])/
                     (qn*y2[n-2]+1.0d0)
      END;

      FOR k := n - 2 TO 0 BY -1 DO
        y2[k] := y2[k] * y2[k+1]+u[k]
      END
    END; (* WITH y2a = ... *)
    RETURN self
  END Init;


PROCEDURE Eval(self : T; at : LONGREAL) : LONGREAL =
  VAR
    klo := 0;
    khi := LAST(self.coords^);
    a, b, h : LONGREAL;
    k : INTEGER;
  BEGIN
    WHILE khi - klo > 1 DO
      k := (khi + klo) DIV 2;
      IF self.coords[k].x > at THEN khi := k ELSE klo := k END
    END;

    h := self.coords[khi].x - self.coords[klo].x;
    <* ASSERT h # 0.0d0 *>
    a := (self.coords[khi].x-at)/h;
    b := (at-self.coords[klo].x)/h;
    
    RETURN a * self.coords[klo].y + b * self.coords[khi].y + 
           ((a*a*a - a) * self.secondDerivs[klo] +
            (b*b*b - b) * self.secondDerivs[khi])*(h*h)/6.0d0
  END Eval;

BEGIN END CubicSpline.
