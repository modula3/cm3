(* $Id$ *)

MODULE Bracket;
IMPORT Debug;
IMPORT Fmt;
IMPORT LongReal AS LR;
IMPORT LRFunction AS Function;

CONST Gold = 1.618034d0;
CONST GLimit = 100.0d0;
CONST CGold = 0.3819660d0;
CONST ZEps = 1.0d-10;
CONST Tiny = 1.0d-20;
CONST ItMax = 100;

PROCEDURE Sign(READONLY a, b : LONGREAL) : LONGREAL =
  BEGIN
    IF b > 0.0d0 THEN RETURN ABS(a) ELSE RETURN -ABS(a) END
  END Sign;

PROCEDURE Shft(VAR a, b, c : LONGREAL; READONLY d : LONGREAL) =
  BEGIN a := b; b := c; c := d END Shft;
        
PROCEDURE Initial(VAR bracket : Trio;
                  func : Function.T) : Trio = 
  BEGIN
    WITH ax = bracket.a, bx = bracket.b, cx = bracket.c DO

      VAR
        ulim, u, r, q, fu: LONGREAL;
        fa, fb, fc : LONGREAL;
        
      VAR
        dum : LONGREAL;

      PROCEDURE Swap(VAR a, b : LONGREAL) =
        BEGIN dum := a; a := b; b := dum END Swap;
        
      BEGIN
        fa := func.eval(ax);
        fb := func.eval(bx);
        IF fb > fa THEN Swap(ax,bx); Swap(fb,fa) END;
        cx := bx + Gold * (bx-ax);
        fc := func.eval(cx);
        
        WHILE fb > fc DO
          r := (bx-ax) * (fb-fc);
          q := (bx-cx) * (fb-fa);
          u := bx - ((bx-cx)*q - (bx-ax)*r)/(2.0d0*Sign(MAX(ABS(q-r),Tiny),q-r));
          ulim := bx+GLimit*(cx-bx);
          
          IF((bx-u)*(u-cx)>0.0d0) THEN
            fu := func.eval(u);
            IF fu < fc THEN 
              ax := bx; bx := u; fa := fb; fb := fu; RETURN Trio { fa,fb,fc }
            ELSIF fu > fb THEN
              cx := u; fc := fu; RETURN Trio { fa,fb,fc }
            END;
            u := cx + Gold * (cx-bx);
            fu := func.eval(u);
          ELSIF (cx-u)*(u-ulim) > 0.0d0 THEN
            fu := func.eval(u);
            IF fu < fc THEN
              Shft(bx,cx,u,cx+Gold*(cx-bx));
              Shft(fb,fc,fu,func.eval(u));
            END
          ELSIF (u-ulim)*(ulim-cx) >= 0.0d0 THEN
            u := ulim; fu := func.eval(u)
          ELSE
            u := cx + Gold*(cx-bx); fu := func.eval(u)
          END;
          Shft(ax,bx,cx,u);
          Shft(fa,fb,fc,fu)
        END;
        RETURN Trio { fa,fb,fc }
      END
    END
  END Initial;

PROCEDURE Brent(bracket : Trio; f : Function.T; tol : LONGREAL; 
                VAR xmin : LONGREAL) : LONGREAL =

  BEGIN
    WITH ax = bracket.a, bx = bracket.b, cx = bracket.c DO
      VAR
        a, b, d, etemp, fu, fv, fw, fx : LONGREAL;
        p, q, r, tol1, tol2, u, v, w, x, xm : LONGREAL;
        e := 0.0d0;
      BEGIN
        IF ax < cx THEN a := ax ELSE a := cx END;
        IF ax > cx THEN b := ax ELSE b := cx END;
        v := bx; w := bx; x := bx;
        fx := f.eval(x); fv := fx; fw := fx;
        
        FOR iter := 1 TO ItMax DO
          xm := 0.5d0*(a+b);
          tol1 := tol * ABS(x) + ZEps;
          tol2 := 2.0d0 * tol1;

          IF ABS(x-xm) <= tol2 - 0.5d0*(b-a) THEN
            xmin := x; RETURN fx
          END;
          
          IF ABS(e) > tol1 THEN
            r := (x-w)*(fx-fv);
            q := (x-v)*(fx-fw);
            p := (x-v)*q-(x-w)*r;
            q := 2.0d0*(q-r);
            IF q > 0.0d0 THEN p := -p END;
            q := ABS(q);
            etemp := e;
            e := d;
            IF ABS(p) >= ABS(0.5d0*q*etemp) OR 
                p <= q*(a-x) OR 
                p >= q*(b-x) THEN
              IF x >= xm THEN e := a-x ELSE e := b-x END;
              d := CGold*e;
            ELSE
              d := p/q; u := x+d;
              IF u-a < tol2 OR b-u < tol2 THEN
                d := Sign(tol1,xm-x)
              END
            END
          ELSE
            IF x >= xm THEN e := a-x ELSE e:= b-x END;
            d := CGold * e;
          END;
          IF ABS(d) >= tol1 THEN u := x+d ELSE u := x + Sign(tol1,d) END;
          fu := f.eval(u);
          IF fu <= fx THEN
            IF u >= x THEN a := x ELSE b := x END;
            Shft(v,w,x,u); Shft(fv,fw,fx,fu)
          ELSE
            IF u < x THEN a := u ELSE b := u END;
            IF fu <= fw OR w = x THEN
              v := w; w:= u; fv := fw; fw := fu
            ELSIF fu <= fv OR v = x OR v = w THEN
              v := u; fv := fu
            END;
          END
        END;
        Debug.Warning("Too many iterations in Bracket.Brent.");
        xmin := x; RETURN fx
      END
    END (* WITH ... *)
  END Brent;

PROCEDURE Format(bracket : Trio ; style := Fmt.Style.Auto;
                 prec: CARDINAL := LR.MaxSignifDigits - 1;
                 literal := FALSE) : TEXT =
  BEGIN
    RETURN "{ " & Fmt.LongReal(bracket.a,style,prec,literal) & ", " &
                  Fmt.LongReal(bracket.b,style,prec,literal) & ", " &
                  Fmt.LongReal(bracket.c,style,prec,literal) & " " &
           " }"
  END Format;

BEGIN END Bracket.
