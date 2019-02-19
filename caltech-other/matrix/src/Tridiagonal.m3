(* $Id$ *)

MODULE Tridiagonal;
IMPORT Matrix, LRVector, Fortran, Process;
IMPORT Math;

PROCEDURE Reduce(VAR a : Matrix.S; 
                 VAR d : LRVector.S; VAR e : LRVector.S) =
  VAR
    n := LAST(a);
    l : [-1..LAST(CARDINAL)];
    scale, h, hh, f, g : LONGREAL;
  BEGIN
    FOR i := n TO 1 BY -1 DO
      l := i-1;
      h := 0.0d0;
      scale := 0.0d0;
      IF l > 0 THEN
        FOR k := 0 TO l DO
          scale := scale + ABS(a[i,k])
        END;
        IF scale = 0.0d0 THEN
          e[i] := a[i,l]
        ELSE
          FOR k := 0 TO l DO
            a[i,k] := a[i,k]/scale;
            h := h + a[i,k]*a[i,k]
          END;
          f := a[i,l]; g := -Fortran.Sign(Math.sqrt(h),f);

          e[i] := scale*g;
          h := h-f*g;
          a[i,l] := f-g;
          f := 0.0d0;

          FOR j := 0 TO l DO
            a[j,i] := a[i,j]/h;
            g := 0.0d0;
            FOR k := 0 TO j DO g := g+ a[j,k]*a[i,k] END; (* 13 *)
            FOR k := j+1 TO l DO g := g+ a[k,j]*a[i,k] END; (* 14 *)
            e[j] := g/h;
            f := f + e[j]*a[i,j]
          END;

          hh := f/(h+h);

          FOR j := 0 TO l DO
            f := a[i,j];
            g := e[j]-hh*f;
            e[j] := g;
            FOR k := 0 TO j DO
              a[j,k] := a[j,k]-f*e[k]-g*a[i,k]
            END
          END
        END
      ELSE
        e[i] := a[i,l]
      END;
      d[i] := h
    END; (* 18 *)

    d[0] := 0.0d0; e[0] := 0.0d0;

    FOR i := 0 TO n DO
      l := i-1;
      IF d[i] # 0.0d0 THEN
        FOR j := 0 TO l DO
          g := 0.0d0;
          FOR k := 0 TO l DO g := g + a[i,k]*a[k,j] END;
          FOR k := 0 TO l DO a[k,j] := a[k,j]-g*a[k,i] END
        END
      END;
      d[i] := a[i,i];
      a[i,i] := 1.0d0;
      FOR j := 0 TO l DO
        a[i,j] := 0.0d0; a[j,i] := 0.0d0
      END (* 23 *)
    END; (* 24 *)
    RETURN
  END Reduce;

PROCEDURE QLi(VAR d, e : LRVector.S;
              VAR z : Matrix.S) =
  VAR 
    n := LAST(e);
    underflow : BOOLEAN;
    iter, m : CARDINAL;
    f, g, p, r, s, dd, b, c : LONGREAL;
  BEGIN
    FOR i := 1 TO n DO e[i-1] := e[i] END;
    e[n] := 0.0d0;

    FOR l := 0 TO n DO
      iter := 0;

      (* 1 *)
      LOOP
        underflow := FALSE;
        m := l;
        WHILE m <= n-1 DO
          dd := ABS(d[m])+ABS(d[m+1]);
          IF ABS(e[m])+dd = dd THEN
            EXIT
          END;
          INC(m)
        END; (* 12 *)
        
        (*2*) 
        IF m # l THEN
          IF iter=30 THEN Process.Crash("Too many iterations in QLi.") END;
          iter := iter+1;
          g := (d[l+1]-d[l])/(2.0d0*e[l]);
          r := Fortran.pythag(g,1.0d0);
          g := d[m]-d[l]+e[l]/(g+Fortran.Sign(r,g));
          s := 1.0d0; c := 1.0d0; p := 0.0d0;
          FOR i := m-1 TO l BY -1 DO
            f := s*e[i]; b := c*e[i];
            r := Fortran.pythag(f,g);
            e[i+1] := r;
            
            IF r = 0.0d0 THEN
              (* underflow... *)
              d[i+1] := d[i+1]-p;
              e[m] := 0.0d0;
              underflow := TRUE;
              EXIT
            END;

            s := f/r; c := g/r; g := d[i+1]-p;

            r := (d[i]-g)*s + 2.0d0*c*b;
            p := s*r;
            d[i+1] := g+p;
            g := c*r-b;
            FOR k := 0 TO n DO
              f := z[k,i+1];
              z[k,i+1] := s*z[k,i] + c*f;
              z[k,i] := c*z[k,i] - s*f
            END (* 13 *)
          END; (* 14 *)

          IF NOT underflow THEN
            d[l] := d[l]-p;
            e[l] := g;
            e[m] := 0.0d0
          END
        ELSE
          EXIT (* exit loop if m=l *)
        END
      END (* LOOP *)

    END; (* 15 *)
    RETURN
  END QLi;

BEGIN END Tridiagonal.
