(* -*- Modula-3 -*-

   Routines to solve eigenvalue problems.  Unoptimised translations from
   Wilkinson+Reinsch, Linear Algebra, Grundlehren der mathematischen
   Wissenschaften in Einzeldarstellungen, Band 186, Springer Verlag

   The re-implementation of the original algorythms tries to follow the
   original Algol sources as close as possible.

   First Version Thomas Brupbacher (thomas@chem.ubc.ca)

   *)

GENERIC MODULE EigenSystem(R,RT,V,M);
(*
IMPORT Wr, Stdio, IO, Fmt,
       LongRealVectorFmtLex AS VF,
       LongRealFmtLex AS RF;
*)
FROM NADefinitions IMPORT Error, Err;

EXCEPTION NormalTermination;

PROCEDURE PowerMethod (A: M.T; VAR v: V.T; tol: R.T; maxiter: CARDINAL; ):
  R.T RAISES {Error} =
  VAR
    x, dx : V.T;
    err   : R.T;
    tol2        := tol * tol;
    x2, vx: R.T;
  BEGIN
    x := V.New(NUMBER(A^));
    x^ := A[0];                  (*is this initialization random enough?*)
    x2 := V.Inner(x, x);
    REPEAT
      IF maxiter = 0 THEN RAISE Error(Err.not_converging); END;
      DEC(maxiter);

      (*turn new into old*)
      v := x;

      x := M.MulV(A, v);
      (* this error estimation sucks because of cancellations
      v2 := V.Inner(v, v);
      x2 := V.Inner(x, x);
      vx := V.Inner(v, x);
      err := (x2*v2-vx*vx)/(v2*v2);
      *)
      (*
      x2 := V.Inner(x, x);
      vx := V.Inner(v, x);
      (*the error cannot become negative mathematically,
        but numerically it is sometimes*)
      err := ABS(x2-vx*vx);
    UNTIL err <= tol2*vx*vx;
      IO.Put(Fmt.FN("err %s, tol %s, x2 %s, vx %s, x %s\n",ARRAY OF TEXT{
        RF.Fmt(err), RF.Fmt(tol), RF.Fmt(x2), RF.Fmt(vx), VF.Fmt(x)}));
      *)

      (*compute the minimum possible Euclidean distance from lambda*v to
         x*)
      x2 := V.Inner(x, x);
      vx := V.Inner(v, x);  (*approximation for largest eigenvalue lambda*)
      dx := V.Sub(v,V.Scale(x,R.Rec(vx)));
      err := V.Inner(dx,dx);
(*
IO.Put(Fmt.FN("err %s, tol %s, dx %s, v %s, x %s\n",ARRAY OF TEXT{
  RF.Fmt(err), RF.Fmt(tol), VF.Fmt(dx), VF.Fmt(v), VF.Fmt(x)}));
*)
      x := V.Scale(x, R.Rec(RT.SqRt(x2)));
    UNTIL err <= tol2;
    (*calculate the lambda for which x is optimally approximated by
       lambda*v with respect to the Euclidean norm*)
    (*RETURN R.Div(vx,v2);*)
    RETURN vx;
  END PowerMethod;


PROCEDURE MaxColumn(x:M.T;VAR max:R.T):CARDINAL=
  VAR
    sum:R.T;
    maxcol:CARDINAL:=0;
  BEGIN
    max:=R.Zero;
    FOR j:=FIRST(x[0]) TO LAST(x[0]) DO
      sum:=R.Zero;
      FOR i:=FIRST(x^) TO LAST(x^) DO
        sum:=R.Add(sum,RT.Abs(x[i,j]));
      END;
      IF max<sum THEN
        maxcol:=j;
        max:=sum;
      END;
    END;
    RETURN maxcol;
  END MaxColumn;

(*The same idea as for PowerMethod,
  iterated squaring of A instead of taking successive powers;
  needs less but more expensive iterations.
  One have to compare whether computing the whole eigenvalue spectrum
  using other methods is faster. *)
PROCEDURE SquareMethod (A: M.T; VAR v: V.T; tol: R.T; maxiter: CARDINAL; ):
  R.T RAISES {Error} =
  VAR
    B,C: M.T;
    x, dx : V.T;
    norm1,
    err   : R.T;
    tol2        := tol * tol;
    v2, x2, vx: R.T;
    j : CARDINAL;
  BEGIN
    C := A;
    REPEAT
      IF maxiter = 0 THEN RAISE Error(Err.not_converging); END;
      DEC(maxiter);

      (*turn new into old*)
      B := C;

      B := M.Mul(B,B);
      C := M.Mul(A,B);

      j  := MaxColumn(B,norm1);
      v  := M.GetColumn(B,j);
      x  := M.GetColumn(C,j);
      (*compute the minimum possible Euclidean distance from lambda*v to
         x*)
      v2 := V.Inner(v, v);
      x2 := V.Inner(x, x);
      vx := V.Inner(v, x);  (*approximation for largest eigenvalue*)
      dx := V.Sub(V.Scale(v,vx),V.Scale(x,v2));
      err := V.Inner(dx,dx);
(*
IO.Put(Fmt.FN("err %s, tol %s, maxcol %s, v2 %s, x2 %s, vx %s,\ndx %s, v %s, x %s\n",ARRAY OF TEXT{
  RF.Fmt(err), RF.Fmt(tol), Fmt.Int(j),
  RF.Fmt(v2), RF.Fmt(x2), RF.Fmt(vx),
  VF.Fmt(dx), VF.Fmt(v), VF.Fmt(x)}));
*)
      C := M.Scale(C, R.Rec(norm1));
    UNTIL err <= tol2*v2*v2*x2;
    (*calculate the lambda for which x is optimally approximated by
       lambda*v with respect to the Euclidean norm*)
    RETURN R.Div(vx,v2);
  END SquareMethod;


CONST tol = RT.MinPos / RT.MinPosNormal;

(* Solve the real symmetric eigenvalue problem by the algorithm of Jacobi.
   The routine has been tested against the first example given in
   Wilkinson/Reinsch and gives the same results. *)

PROCEDURE Jacobi (VAR a        : M.T;
                      n        : INTEGER;
                  VAR d        : V.T;
                  VAR v        : M.T;
                  VAR nrot     : INTEGER;
                      eigenVect            := FALSE)
  RAISES {ArrayTooSmall} =
  VAR
    tresh, theta, tau, t, sm, s, h, g, c: R.T;
    b                                         := NEW(V.T, n);
    z                                         := NEW(V.T, n);
  BEGIN
    IF NUMBER(a^) < n OR NUMBER(a[0]) < n THEN RAISE ArrayTooSmall END;
    IF NUMBER(d^) < n THEN RAISE ArrayTooSmall END;
    IF NUMBER(v^) < n OR NUMBER(v[0]) < n THEN RAISE ArrayTooSmall END;
    IF eigenVect THEN
      FOR p := 0 TO n - 1 DO
        FOR q := 0 TO n - 1 DO v[p, q] := R.Zero; END;
        v[p, p] := R.One;
      END;
    END;
    FOR p := 0 TO n - 1 DO
      b[p] := a[p, p];
      d[p] := b[p];
      z[p] := R.Zero;
    END;
    nrot := 0;
    TRY

      FOR i := 1 TO 50 DO
        sm := R.Zero;
        FOR p := 0 TO n - 2 DO
          FOR q := p + 1 TO n - 1 DO sm := sm + ABS(a[p, q]); END;
        END;
        IF sm = R.Zero THEN RAISE NormalTermination; END;
        IF i < 4 THEN
          tresh := FLOAT(0.2D0, R.T) * sm / FLOAT(n * n, R.T);
        ELSE
          tresh := R.Zero;
        END;
        FOR p := 0 TO n - 2 DO
          FOR q := p + 1 TO n - 1 DO
            g := FLOAT(100.0D0, R.T) * ABS(a[p, q]);
            IF (i > 4) AND (ABS(d[p]) + g = ABS(d[p]))
                 AND (ABS(d[q]) + g = ABS(d[q])) THEN
              a[p, q] := R.Zero;
            ELSE
              IF ABS(a[p, q]) > tresh THEN
                h := d[q] - d[p];
                IF ABS(h) + g = ABS(h) THEN
                  t := a[p, q] / h
                ELSE
                  theta := RT.Half * h / a[p, q];
                  t :=
                    R.One / (ABS(theta) + RT.SqRt(R.One + theta * theta));
                  IF theta < R.Zero THEN t := -t; END;
                END;
                c := R.One / RT.SqRt(R.One + t * t);
                s := t * c;
                tau := s / (R.One + c);
                h := t * a[p, q];
                z[p] := z[p] - h;
                z[q] := z[q] + h;
                d[p] := d[p] - h;
                d[q] := d[q] + h;
                a[p, q] := R.Zero;
                FOR j := 0 TO p - 1 DO
                  g := a[j, p];
                  h := a[j, q];
                  a[j, p] := g - s * (h + g * tau);
                  a[j, q] := h + s * (g - h * tau);
                END;
                FOR j := p + 1 TO q - 1 DO
                  g := a[p, j];
                  h := a[j, q];
                  a[p, j] := g - s * (h + g * tau);
                  a[j, q] := h + s * (g - h * tau)
                END;
                FOR j := q + 1 TO n - 1 DO
                  g := a[p, j];
                  h := a[q, j];
                  a[p, j] := g - s * (h + g * tau);
                  a[q, j] := h + s * (g - h * tau)
                END;
                IF eigenVect THEN
                  FOR j := 0 TO n - 1 DO
                    g := v[j, p];
                    h := v[j, q];
                    v[j, p] := g - s * (h + g * tau);
                    v[j, q] := h + s * (g - h * tau)
                  END;           (* for *)
                END;
                nrot := nrot + 1
              END;
            END;
          END;
        END;
        FOR p := 0 TO n - 1 DO
          b[p] := b[p] + z[p];
          d[p] := b[p];
          z[p] := R.Zero;
        END;
      END;
    EXCEPT
    | NormalTermination => RETURN;
    END;

  END Jacobi;

(*
(* Just a support routine to mimick FORTRANs SIGN *)

PROCEDURE sign(a,b: R.T): R.T =
BEGIN
   IF b < 0 THEN sign := -ABS(a) ELSE sign := ABS(a); END;
END sign;



(* Calculation of the eigenvalues of a tridiagonal matrix by the QL
alorithm. Check Wilkinson/Reinsch for the description. *)
PROCEDURE Tqli(VAR d,e: ARRAY OF R.T;
                     n: INTEGER;
                 VAR z: ARRAY OF ARRAY OF R.T; ) =
LABEL 10,20;
VAR
   m,l,iter,i,k: INTEGER;
   s,r,p,g,f,dd,c,b: R.T;

BEGIN
   FOR i := 1 TO n-1 DO e[i-1] := e[i];
   e[n] := R.Zero;
   FOR l := 0 TO n-1 DO BEGIN
      iter := 0;
(*10:   FOR m := l TO n-2 DO
         dd := abs(d[m])+abs(d[m+1]);
         IF abs(e[m])+dd = dd THEN GOTO 20
      END;
      m := n-1;*)
20:   IF m <> l THEN BEGIN
         IF iter = 30 THEN BEGIN
            writeln('pause in routine TQLI');
            writeln('too many iterations');
            readln
         END;
         iter := iter+1;
         g := (d[l+1]-d[l])/(2.0*e[l]);
         r := sqrt(sqr(g)+1.0);
         g := d[m]-d[l]+e[l]/(g+sign(r,g));
         s := 1.0;
         c := 1.0;
         p := 0.0;
         FOR i := m-1 DOWNTO l DO BEGIN
            f := s*e[i];
            b := c*e[i];
            IF abs(f) >= abs(g) THEN BEGIN
               c := g/f;
               r := sqrt(sqr(c)+1.0);
               e[i+1] := f*r;
               s := 1.0/r;
               c := c*s
            END
            ELSE BEGIN
               s := f/g;
               r := sqrt(sqr(s)+1.0);
               e[i+1] := g*r;
               c := 1.0/r;
               s := s*c
            END;
            g := d[i+1]-p;
            r := (d[i]-g)*s+2.0*c*b;
            p := s*r;
            d[i+1] := g+p;
            g := c*r-b;
            FOR k := 1 TO n DO BEGIN
               f := z[k,i+1];
               z[k,i+1] := s*z[k,i]+c*f;
               z[k,i] := c*z[k,i]-s*f
            END
         END;
         d[l] := d[l]-p;
         e[l] := g;
         e[m] := 0.0;
         GOTO 10
      END
   END
END;
*)

PROCEDURE EigenSort (VAR vects: M.T; VAR vals: V.T)
  RAISES {ArraySizesDontMatch} =
  VAR p, q: R.T;
  BEGIN
    IF NUMBER(vals^) # NUMBER(vects[0]) THEN
      RAISE ArraySizesDontMatch;
    END;
    FOR i := FIRST(vals^) TO LAST(vals^) - 1 DO
      p := vals[i];
      FOR j := i + 1 TO LAST(vals^) DO
        IF vals[j] > p THEN
          p := vals[j];
          vals[j] := vals[i];
          vals[i] := p;
          FOR k := FIRST(vects^) TO LAST(vects^) DO
            q := vects[k, i];
            vects[k, i] := vects[k, j];
            vects[k, j] := q;
          END;                   (* for *)
        END;                     (* if *)
      END;                       (* for *)
    END;                         (* for *)
  END EigenSort;


(*
 Implementation of the tred[1234] Householder reductions of a real
 symmetric matrix. Translation of the original ALGOL procedures.
*)
PROCEDURE Tred1 (n: CARDINAL; VAR a: M.T; VAR d, e, e2: V.T; )
  RAISES {ArraySizesDontMatch} =
  VAR
    l      : INTEGER;
    f, g, h: R.T;
  BEGIN
    IF NUMBER(a[0]) < n OR NUMBER(a^) < n OR NUMBER(d^) < n
         OR NUMBER(e^) < n OR NUMBER(e2^) < n THEN
      RAISE ArraySizesDontMatch;
    END;                         (* if *)
    FOR i := FIRST(d^) TO LAST(d^) DO d[i] := a[i, i]; END; (* for *)
    FOR i := LAST(d^) TO FIRST(d^) BY -1 DO
      l := i - 1;
      h := R.Zero;
      FOR k := FIRST(a[0]) TO l DO
        h := h + a[i, k] * a[i, k];
      END;                       (* for *)
      (* If h is too small for orthogonality to be guaranteed, skip
         transformation *)
      IF h <= tol THEN
        e[i] := R.Zero;
        e2[i] := R.Zero;
      ELSE
        e2[i] := h;
        f := a[i, i - 1];
        IF f >= R.Zero THEN
          g := -RT.SqRt(h);
        ELSE
          g := RT.SqRt(h);
        END;                     (* if *)
        e[i] := g;
        h := h - f * g;
        a[i, i - 1] := f - g;
        f := R.Zero;
        FOR j := FIRST(a[0]) TO l DO
          (* form element of A x u *)
          g := R.Zero;
          FOR k := FIRST(a[0]) TO j DO
            g := g + a[j, k] * a[i, k];
          END;                   (* for *)
          FOR k := j + 1 TO l DO g := g + a[k, j] * a[i, k]; END; (* for *)
          (* form element of p *)
          e[j] := g / h;
          g := e[j];
          f := f + g * a[i, j];
        END;                     (* for *)
        (* form K *)
        h := f / (h + h);
        (* form reduced A *)
        FOR j := FIRST(a[0]) TO l DO
          f := a[i, j];
          e[j] := e[j] - h * f;
          g := e[j];
          FOR k := FIRST(a[0]) TO j DO
            a[j, k] := a[j, k] - f * e[k] - g * a[i, k];
          END;                   (* for *)
        END;                     (* for *)
      END;                       (* if *)
      (* now for all cases of h *)
      h := d[i];
      d[i] := a[i, i];
      a[i, i] := h;
    END;                         (* for *)
  END Tred1;

PROCEDURE Tred2 (n: CARDINAL; VAR a: M.T; VAR d, e: V.T)
  RAISES {ArraySizesDontMatch} =
  VAR
    l          : INTEGER;
    firstD               := FIRST(d^);
    f, g, h, hh: R.T;
  BEGIN
    (* Test for array sizes. *)
    IF NUMBER(a[0]) # n OR NUMBER(a^) # n OR NUMBER(d^) # n
         OR NUMBER(e^) # n THEN
      RAISE ArraySizesDontMatch;
    END;                         (* if *)

    FOR i := n - 1 TO 1 BY -1 DO
      l := i - 2;
      f := a[i, i - 1];
      g := R.Zero;
      FOR k := 0 TO l DO
        g := g + a[i, k] * a[i, k];
        h := g + f * f;
      END;                       (* for *)
      (* If h is too small for orthogonality to be guaranteed, skip
         transformation *)
      IF g <= tol THEN
        e[i] := f;
        h := R.Zero;
      ELSE
        l := l + 1;
        f := a[i, i - 1];
        IF f >= R.Zero THEN
          g := -RT.SqRt(h);
        ELSE
          g := RT.SqRt(h);
        END;                     (* if *)
        e[i] := g;
        h := h - f * g;
        a[i, i - 1] := f - g;
        f := R.Zero;
        FOR j := firstD TO l DO
          (* form element of A x u *)
          a[j, i] := a[i, j] / h;
          g := R.Zero;
          FOR k := firstD TO j DO
            g := g + a[j, k] * a[i, k];
          END;                   (* for *)
          FOR k := j + 1 TO l DO g := g + a[k, j] * a[i, k]; END; (* for *)
          (* form element of p *)
          e[j] := g / h;
          f := f + g * a[j, i];
        END;                     (* for *)
        (* form K *)
        hh := f / (h + h);
        (* form reduced A *)
        FOR j := firstD TO l DO
          f := a[i, j];
          e[j] := e[j] - hh * f;
          g := e[j];
          FOR k := firstD TO j DO
            a[j, k] := a[j, k] - f * e[k] - g * a[i, k];
          END;                   (* for *)
        END;                     (* for *)
      END;                       (* if *)
      (* now for all cases of h *)
      d[i] := h;
    END;                         (* for *)
    d[0] := R.Zero;
    e[0] := R.Zero;
    (* Accumulation of transformation matrices *)
    FOR i := 0 TO n - 1 DO
      l := i - 1;
      IF d[i] # R.Zero THEN
        FOR j := 0 TO l DO
          g := R.Zero;
          FOR k := 0 TO l DO g := g + a[i, k] * a[k, j]; END; (* for *)
          FOR k := firstD TO l DO
            a[k, j] := a[k, j] - g * a[k, i];
          END;                   (* for *)
        END;                     (* for *)
      END;                       (* if *)
      d[i] := a[i, i];
      a[i, i] := R.One;
      FOR j := firstD TO l DO
        a[i, j] := R.Zero;
        a[j, i] := R.Zero;
      END;                       (* for *)
    END;                         (* for *)
  END Tred2;

PROCEDURE Trbak1 (    n     : CARDINAL;
                      a     : M.T;
                      d, e  : V.T;
                  VAR z     : M.T;
                      m1, m2: CARDINAL  ) RAISES {ArraySizesDontMatch} =
  VAR
    l   : INTEGER;
    h, s: R.T;
  BEGIN
    (* Test for array sizes. *)
    IF NUMBER(a[0]) # n OR NUMBER(a^) # n OR NUMBER(d^) # n
         OR NUMBER(e^) # n OR NUMBER(z[0]) # n OR NUMBER(z^) # n OR m1 > n
         OR m2 > n THEN
      RAISE ArraySizesDontMatch;
    END;                         (* if *)

    FOR i := FIRST(e^) + 1 TO LAST(e^) DO
      IF e[i] # R.Zero THEN
        l := i - 1;
        h := e[i] * a[i, i - 1];
        FOR j := m1 + 1 TO m2 + 1 DO
          s := R.Zero;
          FOR k := FIRST(a^) TO l DO
            s := s + a[i, k] * z[k, j];
          END;                   (* for *)
          s := s / h;
          FOR k := 1 TO l DO
            z[k, j] := z[k, j] + s * a[i, k];
          END;                   (* for *)
        END;                     (* for *)
      END;                       (* if *)
    END;                         (* for *)
  END Trbak1;

PROCEDURE Trbak3 (    n     : CARDINAL;
                      a     : V.T;
                      d, e  : V.T;
                  VAR z     : M.T;
                      m1, m2: CARDINAL  ) RAISES {ArraySizesDontMatch} =
  VAR
    l, iz: INTEGER;
    h, s : R.T;
  BEGIN
    (* Test for array sizes. *)
    IF NUMBER(a^) # (n * (n + 1) DIV 2) OR NUMBER(d^) # n OR NUMBER(e^) # n
         OR NUMBER(z[0]) # n OR NUMBER(z^) # n OR m1 > n OR m2 > n THEN
      RAISE ArraySizesDontMatch;
    END;                         (* if *)

    FOR i := FIRST(e^) + 1 TO LAST(e^) DO
      l := i - 1;
      iz := i * l DIV 2;
      h := a[iz + i];
      IF h # R.Zero THEN
        FOR j := m1 + 1 TO m2 + 1 DO
          s := R.Zero;
          FOR k := FIRST(a^) TO l DO
            s := s + a[iz + k] * z[k, j];
          END;                   (* for *)
          s := s / h;
          FOR k := 1 TO l DO
            z[k, j] := z[k, j] + s * a[iz + k];
          END;                   (* for *)
        END;                     (* for *)
      END;                       (* if *)
    END;                         (* for *)
  END Trbak3;

PROCEDURE Tql1 (VAR d, e: V.T)
  RAISES {ArraySizesDontMatch, NoConvergence} =
  VAR
    iter, m               : INTEGER;
    b, c, f, g, h, p, r, s: R.T;
  BEGIN
    IF NUMBER(d^) # NUMBER(e^) THEN
      RAISE ArraySizesDontMatch;
    END;                         (* if *)
    FOR i := FIRST(e^) + 1 TO LAST(e^) DO e[i - 1] := e[i]; END; (* for *)
    f := R.Zero;
    b := R.Zero;
    e[LAST(e^)] := R.Zero;

    FOR l := FIRST(d^) TO LAST(d^) DO
      h := RT.MinPosNormal * (ABS(d[l]) + ABS(e[l]));
      IF b < h THEN b := h; END; (* if *)
      (* look for small subdiagonal element *)
      m := l;
      WHILE m <= LAST(e^) AND ABS(e[m]) > b DO INC(m); END; (* while *)
      IF m # l AND l < LAST(d^) THEN
        iter := 0;
        REPEAT
          IF iter > 30 THEN RAISE NoConvergence; END; (* if *)
          INC(iter);
          (* form shift *)
          g := d[l];
          p := (d[l + 1] - g) / (R.Two * e[l]);
          r := RT.SqRt(p * p + R.One);
          IF p < r THEN
            d[l] := e[l] / (p - r);
          ELSE
            d[l] := e[l] / (p + r);
          END;                   (* IF *)
          h := g - d[l];
          FOR i := l + 1 TO LAST(e^) DO d[i] := d[i] - h; END; (* for *)
          f := f + h;
          (* QL transformation *)
          p := d[m];
          c := R.One;
          s := R.Zero;
          FOR i := m - 1 TO l BY -1 DO
            g := c * e[i];
            h := c * p;
            IF ABS(p) >= ABS(e[i]) THEN
              c := e[i] / p;
              r := RT.SqRt(c * c + R.One);
              e[i + 1] := s * p * r;
              s := c / r;
              c := R.One / r;
            ELSE
              c := p / e[i];
              r := RT.SqRt(c * c + R.One);
              e[i + 1] := s * e[i] * r;
              s := R.One / r;
              c := c / r;
            END;                 (* if *)
            p := c * d[i] - s * g;
            d[i + 1] := h + s * (c * g + s * d[i]);
          END;                   (* for *)
          e[l] := s * p;
          d[l] := c * p;
        UNTIL ABS(e[l]) <= b;
      END;                       (* if *)
      (* original <root> label *)
      p := d[l] + f;
      (* order eigenvalue *)
      m := l;
      WHILE m > FIRST(d^) AND p < d[m - 1] DO
        d[m] := d[m - 1];
        DEC(m);
      END;                       (* while *)
      d[m] := p;
    END;                         (* for *)
  END Tql1;

PROCEDURE Tql2 (VAR d, e: V.T; VAR z: M.T)
  RAISES {ArraySizesDontMatch, NoConvergence} =
  VAR
    k, m, iter            : INTEGER;
    b, c, f, g, h, r, s, p: R.T;
  BEGIN
    IF NUMBER(d^) # NUMBER(e^) OR NUMBER(d^) # NUMBER(z^)
         OR NUMBER(d^) # NUMBER(z[0]) THEN
      RAISE ArraySizesDontMatch;
    END;                         (* if *)

    FOR i := FIRST(e^) + 1 TO LAST(e^) DO e[i - 1] := e[i]; END; (* for *)
    f := R.Zero;
    b := R.Zero;
    e[LAST(e^)] := R.Zero;

    FOR l := FIRST(d^) TO LAST(d^) DO
      h := RT.MinPosNormal * (ABS(d[l]) + ABS(e[l]));
      IF b < h THEN b := h; END; (* if *)
      (* look for small subdiagonal element *)
      m := l;
      WHILE m <= LAST(e^) AND ABS(e[m]) > b DO INC(m); END; (* while *)
      IF m # l AND l < LAST(d^) THEN
        iter := 0;
        REPEAT
          IF iter > 30 THEN RAISE NoConvergence; END; (* if *)
          INC(iter);
          (* form shift *)
          g := d[l];
          p := (d[l + 1] - g) / (R.Two * e[l]);
          r := RT.SqRt(p * p + R.One);
          IF p < r THEN
            d[l] := e[l] / (p - r);
          ELSE
            d[l] := e[l] / (p + r);
          END;                   (* IF *)
          h := g - d[l];
          FOR i := l + 1 TO LAST(d^) DO d[i] := d[i] - h; END; (* for *)
          f := f + h;
          (* QL transformation *)
          p := d[m];
          c := R.One;
          s := R.Zero;
          FOR i := m - 1 TO l BY -1 DO
            g := c * e[i];
            h := c * p;
            IF ABS(p) >= ABS(e[i]) THEN
              c := e[i] / p;
              r := RT.SqRt(c * c + R.One);
              e[i + 1] := s * p * r;
              s := c / r;
              c := R.One / r;
            ELSE
              c := p / e[i];
              r := RT.SqRt(c * c + R.One);
              e[i + 1] := s * e[i] * r;
              s := R.One / r;
              c := c / r;
            END;                 (* if *)
            p := c * d[i] - s * g;
            d[i + 1] := h + s * (c * g + s * d[i]);
            (* form vector *)
            FOR k := FIRST(d^) TO LAST(d^) DO
              h := z[k, i + 1];
              z[k, i + 1] := s * z[k, i] + c * h;
              z[k, i] := c * z[k, i] - s * h;
            END;                 (* for *)
          END;                   (* for *)
          e[l] := s * p;
          d[l] := c * p;
        UNTIL ABS(e[l]) <= b;
      END;                       (* if *)
      (* original <root> label *)
      d[l] := d[l] + f;
    END;                         (* for l *)
    (* order eigenvalues and eigenvectors *)
    FOR i := FIRST(d^) TO LAST(d^) DO
      k := i;
      p := d[i];
      FOR j := i + 1 TO LAST(d^) DO
        IF d[j] < p THEN k := j; p := d[j]; END; (* if *)
      END;                       (* for *)
      IF k # i THEN
        d[k] := d[i];
        d[i] := p;
        FOR j := FIRST(z^) TO LAST(z^) DO
          p := z[j, i];
          z[j, i] := z[j, k];
          z[j, k] := p;
        END;                     (* for *)
      END;                       (* if *)
    END;                         (* for *)
  END Tql2;


BEGIN
END EigenSystem.
