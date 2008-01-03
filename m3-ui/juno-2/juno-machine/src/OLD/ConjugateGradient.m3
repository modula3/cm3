(* ------------------ Begin Conjugate Gradient Code ---------------------------

CONST
  MaxConjIter = 20;
  ConjEps1 = 0.1;
  ConjEps2 = 0.001;

PROCEDURE ConjugateGradient(
    nn: CARDINAL;
    VAR p: LinearSolve.Vector;
    READONLY c: ARRAY OF Constraint) =
(* Conjugate gradient method from "Numerical Recipes in Pascal" by Press et.
   al., Cambridge University Press, 1989, Section 10.6.

   Q0: "p" is the current point, "fp" is the value of the function at "p", "g"
   is the negative of the gradient of the function at "p", and "h" is the
   direction in which to do line minimization of the function.
*)
  VAR
    fp := Error(p, c);
    grad, g, h := NEW(REF LinearSolve.Vector, nn);
    zero_grad := TRUE;
  BEGIN
    (* Establish Q0 *)
    Gradient(p, c, grad^);
    FOR i := FIRST(g^) TO LAST(g^) DO
      g[i] := - grad[i];
      h[i] := g[i];
      zero_grad := zero_grad AND g[i] = 0.0
    END;
    IF NOT zero_grad THEN
      VAR iter := 0; BEGIN
        WHILE iter < MaxConjIter DO
          (* Q0 *)
          INC(iter);
          IF debug >= 2 THEN
            Wr.PutText(stderr, "Hint Iteration " & Fmt.Int(iter) & ":\n");
            ShowVector("g_i", g^, nn);
            ShowVector("h_i", h^, nn)
          END;
          VAR new_fp := LineMinimize(p, (* h^ *) g^, c); BEGIN
            IF debug >= 2 THEN
              ShowVector("Values", p, nn);
              Wr.PutText(stderr, "   Error: " &
                Fmt.Pad(Fmt.Real(new_fp, 3, Fmt.Style.Sci), 9) & "\n");
            END;
            IF ABS(new_fp - fp) <=
              ConjEps1 * (ABS(new_fp) + ABS(fp) + ConjEps2) THEN
              EXIT
            END;
            fp := new_fp
          END;
          Gradient(p, c, grad^);
          VAR gg,dgg := 0.0; gamma: T; BEGIN
            FOR i := FIRST(g^) TO LAST(g^) DO
              gg := gg + g[i] * g[i];
              dgg := dgg + (grad[i] + g[i]) * grad[i]
            END;
            IF gg = 0.0 THEN EXIT END;
            gamma := dgg / gg;
            FOR i := FIRST(g^) TO LAST(g^) DO
              g[i] := - grad[i];
              h[i] := g[i] + gamma * h[i]
            END
          END
        END
      END
    END
  END ConjugateGradient;

PROCEDURE Error(
    READONLY p: LinearSolve.Vector;
    READONLY c: ARRAY OF Constraint): T =
(* Return the value of the error function determined by the constraints "c" at
   the point "p".
*)
  VAR res := 0.0; diff: T; BEGIN
    FOR i := FIRST(c) TO LAST(c) DO
      TYPECASE c[i] OF <* NOWARN *>
        Plus(con) =>  diff := p[con.arg[0]] - (p[con.arg[1]] + p[con.arg[2]])
      | Times(con) => diff := p[con.arg[0]] - (p[con.arg[1]] * p[con.arg[2]])
      | Sin(con) =>   diff := p[con.arg[0]] - JunoValue.Sin(p[con.arg[1]])
      | Cos(con) =>   diff := p[con.arg[0]] - JunoValue.Cos(p[con.arg[1]])
      | Atan(con) =>  diff := p[con.arg[0]] - JunoValue.Atan(p[con.arg[1]])
      | Exp(con) =>   diff := p[con.arg[0]] - JunoValue.Exp(p[con.arg[1]])
      END;
      res := res + (diff * diff)
    END;
    RETURN res
  END Error;

PROCEDURE Gradient(
    READONLY p: LinearSolve.Vector;
    READONLY c: ARRAY OF Constraint;
    VAR (* OUT *) grad: LinearSolve.Vector) =
(* Set "grad" to the gradient of the error function determined from the
   constraints "c" at the point "p".
*)
  VAR nn := NUMBER(grad); BEGIN
    FOR i := FIRST(grad) TO LAST(grad) DO grad[i] := 0.0 END;
    FOR i := FIRST(c) TO LAST(c) DO
      VAR
        con := c[i]; a0 := con.arg[0]; a1 := con.arg[1]; a2 := con.arg[2];
        scaler: T;
      BEGIN
        TYPECASE con OF <* NOWARN *>
          Plus =>
            scaler := 2.0 * (p[a0] - (p[a1] + p[a2]));
            IF a0 < nn THEN grad[a0] := grad[a0] + scaler END;
            IF a1 < nn THEN grad[a1] := grad[a1] - scaler END;
            IF a2 < nn THEN grad[a2] := grad[a2] - scaler END;
        | Times =>
            scaler := 2.0 * (p[a0] - (p[a1] * p[a2]));
            IF a0 < nn THEN grad[a0] := grad[a0] + scaler END;
            IF a1 < nn THEN grad[a1] := grad[a1] - (scaler * p[a2]) END;
            IF a2 < nn THEN grad[a2] := grad[a2] - (scaler * p[a1]) END;
        | Sin =>
            scaler := 2.0 * (p[a0] - JunoValue.Sin(p[a1]));
            IF a0 < nn THEN grad[a0] := grad[a0] + scaler END;
            IF a1 < nn THEN
              grad[a1] := grad[a1] - (scaler * JunoValue.Cos(p[a1]))
            END
        | Cos =>
            scaler := 2.0 * (p[a0] - JunoValue.Cos(p[a1]));
            IF a0 < nn THEN grad[a0] := grad[a0] + scaler END;
            IF a1 < nn THEN
              grad[a1] := grad[a1] + (scaler * JunoValue.Sin(p[a1]))
            END
        | Atan =>
            scaler := 2.0 * (p[a0] - JunoValue.Atan(p[a1]));
            IF a0 < nn THEN grad[a0] := grad[a0] + scaler END;
            IF a1 < nn THEN
              grad[a1] := grad[a1] - (scaler / (1.0 + (p[a1] * p[a1])))
            END
        | Exp =>
            VAR exp := JunoValue.Exp(p[a1]); BEGIN
              scaler := 2.0 * (p[a0] - exp);
              IF a0 < nn THEN grad[a0] := grad[a0] + scaler END;
              IF a1 < nn THEN grad[a1] := grad[a1] + (scaler * exp) END
            END
        END
      END
    END
  END Gradient;

CONST
  NearZero = 1.0E-4;			 (* starting "fa" and "fb" values *)
  MinEps = 0.001;			 (* epsilon value for "LineMinimize" *)

PROCEDURE LineMinimize(
    VAR p: LinearSolve.Vector;
    READONLY dir: LinearSolve.Vector;
    READONLY c: ARRAY OF Constraint): T =
(* Change "p" so that its new value lies along the line through "p" in the
   direction "dir" and so that it is a local minimum of the error function
   determined by the constraints "c". Return the value of the error function
   at the new point "p".
*)
  CONST Phi = 0.61803399;
  VAR temp := NEW(REF LinearSolve.Vector, NUMBER(p));

  PROCEDURE Error1(t: T): T =
    BEGIN
      FOR i := FIRST(dir) TO LAST(dir) DO
        temp[i] := p[i] + t * dir[i]
      END;
      <* ASSERT t # 0.0 OR temp^ = p *>
      RETURN Error(temp^, c)
    END Error1;

  VAR ta, tb, tc, fa, fb, fc: T; BEGIN
    VAR knowns := NUMBER(p) - NUMBER(dir); BEGIN
      SUBARRAY(temp^, NUMBER(dir), knowns) := SUBARRAY(p, NUMBER(dir), knowns)
    END;
    ta := -NearZero; fa := Error1(ta);
    tb :=  NearZero; fb := Error1(tb);
    IF fb > fa THEN
      VAR temp := ta; BEGIN ta := tb; tb := temp END;
      VAR temp := fa; BEGIN fa := fb; fb := temp END
    END;
    (* fb < fa AND fa = Error1(ta) AND fb = Error1(tb) *)
    LOOP
      tc := tb + 2.0 * (tb - ta);
      fc := Error1(tc);
      IF fc > fb THEN EXIT END;
      ta := tb; fa := fb;
      tb := tc; fb := fc
    END;
    (* Q1: "fa = Error1(ta)"; similarly for "b" and "c".
       Q2: "(ta, tb, tc)" increasing or decreasing.
       Q3: "fb < MIN(fa, fc)".
    *)
    WHILE ABS(tc-ta) > MinEps * (ABS(tc) + ABS(ta)) DO
      VAR tx, fx: T; BEGIN
        IF ABS(tb-ta) > ABS(tc-tb) THEN
          tx := ta + Phi * (tb - ta);
          fx := Error1(tx);
          (* monotonic (ta, tx, tb, tc) *)
          IF fb > fx THEN
            tc := tb; fc := fb;
            tb := tx; fb := fx;
          ELSE
            ta := tx; fa := fx;
          END;
        ELSE
          tx := tc - Phi * (tc - tb);
          fx := Error1(tx);
          (* monotonic (ta, tb, tx, tc) *)
          IF fb > fx THEN
            ta := tb; fa := fb;
            tb := tx; fb := fx;
          ELSE
            tc := tx; fc := fx;
          END;
        END;
      END
    END;
    IF debug >= 3 THEN
      Wr.PutText(stderr, "  t vals: ");
      Wr.PutText(stderr, Fmt.Pad(Fmt.Real(ta, 3, Fmt.Style.Sci), 9) & " ");
      Wr.PutText(stderr, Fmt.Pad(Fmt.Real(tb, 3, Fmt.Style.Sci), 9) & " ");
      Wr.PutText(stderr, Fmt.Pad(Fmt.Real(tc, 3, Fmt.Style.Sci), 9) & " ");
      Wr.PutChar(stderr, '\n')
    END;
    FOR i := FIRST(dir) TO LAST(dir) DO
      p[i] := p[i] + tb * dir[i]
    END;
    RETURN fb
  END LineMinimize;

  ------------------ End Conjugate Gradient Code --------------------------- *)
