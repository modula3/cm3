GENERIC MODULE Interpolation(R, RT, V);
(* Arithmetic for Modula-3, see doc for details *)
IMPORT Arithmetic AS Arith;

CONST Module = "Interpolation.";


PROCEDURE CheckSizes
  (READONLY xa: ARRAY OF R.T; READONLY ya: ARRAY OF V.T; ) =
  BEGIN
    <* ASSERT NUMBER(xa) = NUMBER(ya),
                "The number of interpolation nodes and the number of node values must match." *>
  END CheckSizes;


PROCEDURE Linear (READONLY xa: ARRAY OF R.T;  (* interpolation nodes *)
                  READONLY ya: ARRAY OF V.T;  (* interpolation values *)
                           x : R.T;                                     ):
  V.T =
  (* Given an interpolation table with xa input and ya output, do linear
     interpolation for x. *)
  VAR
    n                         := NUMBER(xa);
    n1                        := 0;
    nn                        := n - 1;
    diffbest, diff : R.T;
    ndx, ndx1, ndx2: CARDINAL;
    x1, x2, x12    : R.T;
    y1, y2         : V.T;

  BEGIN
    CheckSizes(xa, ya);

    (*---find the best start point---*)
    ndx := n1;                   (* this is arbitrary, but fix the FOR loop
                                    if you change *)
    diffbest := ABS(x - xa[ndx]);
    FOR i := n1 + 1 TO nn DO
      diff := ABS(x - xa[i]);
      IF diff < RT.Tiny THEN
        (* quick victory *)
        RETURN ya[i];
      ELSIF diff < diffbest THEN
        ndx := i;
        diffbest := diff;
      END;
    END;

    (*---find the best partner---*)
    IF ndx = n1 THEN
      ndx1 := n1;
      ndx2 := n1 + 1;
    ELSIF ndx = nn THEN
      ndx1 := nn - 1;
      ndx2 := nn;
    ELSIF ABS(x - xa[ndx - 1]) < ABS(x - xa[ndx + 1]) THEN
      ndx1 := ndx - 1;
      ndx2 := ndx;
    ELSE
      ndx1 := ndx;
      ndx2 := ndx + 1;
    END;

    (*---compute the y value---*)
    x1 := xa[ndx1];
    y1 := ya[ndx1];
    x2 := xa[ndx2];
    y2 := ya[ndx2];
    x12 := x1 - x2;
    RETURN V.Add(V.Scale(y1, (x - x2) / x12), V.Scale(y2, (x1 - x) / x12));
  END Linear;


PROCEDURE Newton (READONLY xa: ARRAY OF R.T;  (* interpolation nodes *)
                  READONLY ya: ARRAY OF V.T;  (* interpolation values *)
                           x : R.T;
                  VAR      dy: V.T;                                     ):
  V.T RAISES {Arith.Error} =
  (* Given an interpolation table with xa input and ya output, do Newton
     polynomial interpolation for x.  Report error estimate as dy.  Partial
     access: Give the starting index and the length to be used. *)
  <* UNUSED *>
  CONST
    ftn = Module & "Newton";
  VAR
    col_n: CARDINAL;
    c               := NEW(REF ARRAY OF V.T, NUMBER(xa));
    d               := NEW(REF ARRAY OF V.T, NUMBER(xa));
    ndx             := LAST(xa); (* get a starter x *)
    y    : V.T;

  BEGIN
    CheckSizes(xa, ya);

    VAR
      difftmp: R.T;
      (*---find starting y---*)
      diff := ABS(x - xa[ndx]);  (* and its difference from true x *)
    BEGIN
      FOR i := 0 TO LAST(xa) DO
        difftmp := ABS(x - xa[i]);
        IF difftmp < RT.Tiny THEN
          y := ya[i];
          dy := V.Sub(y, y);
          (*dy:=V.NewCompliantZero(y);*)
          RETURN y;
        ELSIF difftmp < diff THEN (* found a better one *)
          ndx := i;
          diff := difftmp;
        END;
        c[i] := ya[i];           (* c and d are 1..NUMBER(xa) *)
      END;
    END;
    d^ := c^;                    (* load d from c, thus from ya *)

    y := ya[ndx];                (* use the best ndx to get starting y *)

    (*---compute and use c and d---*)
    col_n := NUMBER(xa);         (* originally there are n in the col *)
    FOR m := 1 TO LAST(xa) DO
      DEC(col_n);                (* each col recalc loses 1 cell *)
      FOR i := 0 TO col_n - 1 DO
        VAR
          xi    := xa[i];
          xim   := xa[i + m];
          den   := xi - xim;
          delta := V.Sub(c[i + 1], d[i]);
        BEGIN
          IF ABS(den) < RT.Tiny THEN
            RAISE Arith.Error(NEW(Arith.ErrorDivisionByZero).init());
          END;
          d[i] := V.Scale(delta, (xim - x) / den);
          c[i] := V.Scale(delta, (xi - x) / den);
        END;
      END;
      (*---which correction to use?---*)
      IF ndx * 2 >= col_n THEN
        (* we are at or below the center, need to move up *)
        DEC(ndx);
        dy := d[ndx];
      ELSE
        (* we are above the center, need to move down *)
        dy := c[ndx];
        (* don't need to adjust ndx, because it is effectively moved down
           when we slide the next col up *)
      END;
      (*---update y---*)
      y := V.Add(y, dy);
    END;
    RETURN y;
  END Newton;


PROCEDURE CubicHermite
  (READONLY xa: ARRAY OF R.T;    (* interpolation nodes *)
   READONLY ya: ARRAY OF V.T;    (* interpolation values *)
            x : R.T;             (* the function argument *)
  ): V.T =

  PROCEDURE InterpolateQuadratic (READONLY xb: ARRAY [0 .. 2] OF R.T;
                                  READONLY yb: ARRAY [0 .. 2] OF V.T  ):
    V.T =
    (* for some datatypes no Error can occur *)
    VAR
      x01 := xb[0] - xb[1];
      x12 := xb[1] - xb[2];
      x02 := xb[0] - xb[2];
      xx0 := x - xb[0];
      xx1 := x - xb[1];
      xx2 := x - xb[2];
      sum := V.Scale(yb[0], xx1 * xx2 / (x01 * x02));
    BEGIN
      sum := V.Sub(sum, V.Scale(yb[1], xx0 * xx2 / (x01 * x12)));
      sum := V.Add(sum, V.Scale(yb[2], xx0 * xx1 / (x12 * x02)));
      RETURN sum;
    END InterpolateQuadratic;

  (* probably not very efficient *)
  PROCEDURE InterpolateHalf (READONLY xb: ARRAY [0 .. 2] OF R.T;
                             READONLY yb: ARRAY [0 .. 2] OF V.T  ): V.T =
    (* for some datatypes no Error can occur *)
    CONST Three = FLOAT(3, R.T);
    VAR
      x01    := xb[0] - xb[1];
      x12    := xb[1] - xb[2];
      x02    := xb[0] - xb[2];
      xin12  := (x - xb[2]) / x12;
      hermy1 := xin12 * xin12 * (Three - R.Two * xin12);
      (* p(x[1])=1, p(x'[1])=0, p(x[2])=0, p(x'[2])=0 *)
      hermdy1 := xin12 * xin12 * (x - xb[1]);
      (* p(x[1])=0, p(x'[1])=1, p(x[2])=0, p(x'[2])=0 *)
      sum := V.Scale(yb[1], hermdy1 * (x01 - x12) / (x01 * x12) + hermy1);
    BEGIN
      sum := V.Add(sum, V.Scale(yb[0], hermdy1 * x12 / (x01 * x02)));
      sum := V.Sub(sum, V.Scale(yb[2], hermdy1 * x01 / (x12 * x02)));
      RETURN sum;
    END InterpolateHalf;

  PROCEDURE InterpolatePiece (READONLY xb: ARRAY [0 .. 3] OF R.T;
                              READONLY yb: ARRAY [0 .. 3] OF V.T  ): V.T =
    BEGIN
      RETURN V.Add(InterpolateHalf(SUBARRAY(xb, 0, 3), SUBARRAY(yb, 0, 3)),
                   InterpolateHalf(ARRAY OF R.T{xb[3], xb[2], xb[1]},
                                   ARRAY OF V.T{yb[3], yb[2], yb[1]}));
    END InterpolatePiece;

  BEGIN
    CheckSizes(xa, ya);

    IF x <= xa[1] THEN
      RETURN InterpolateQuadratic(SUBARRAY(xa, 0, 3), SUBARRAY(ya, 0, 3));
    ELSE
      FOR j := 2 TO LAST(xa) - 1 DO
        IF (* xa[j-1]<x AND *) x <= xa[j] THEN
          RETURN InterpolatePiece(
                   SUBARRAY(xa, j - 2, 4), SUBARRAY(ya, j - 2, 4));
        END;
      END;
      RETURN InterpolateQuadratic(SUBARRAY(xa, LAST(xa) - 2, 3),
                                  SUBARRAY(ya, LAST(xa) - 2, 3));
    END;
  END CubicHermite;


BEGIN
END Interpolation.
