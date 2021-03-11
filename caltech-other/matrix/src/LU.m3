(* Copyright (c) 2000 Mika Nystrom.  All Rights Reserved. *)
(* $Id$ *)
MODULE LU;
IMPORT Matrix;

(* there has got to be a better way to code these things *)

PROCEDURE Decompose(m : Matrix.T; indx : REF ARRAY OF INTEGER; VAR d : LONGREAL) RAISES { Matrix.Singular } =
  VAR
    vv := NEW(Matrix.Vector, NUMBER(m^));
  BEGIN
    DecomposeR(m,vv,indx,d)
  END Decompose;

PROCEDURE DecomposeR(m : Matrix.T; vv : Matrix.Vector;
                    indx : REF ARRAY OF INTEGER; 
                    VAR d : LONGREAL) RAISES { Matrix.Singular } =
  (* LU decomposition of a square matrix by Crout's method *)
  (* Numerical Recipes in FORTRAN 2nd ed. pp. 38--39 *)
  (* numbers in comments refer to ENDDO statements in the book *)

  (* MODIFIES m , indx , d *)
  (* ENSURES m' contains LU decomposition of m *)

  CONST
    Tiny = 1.0d-20;
  VAR
    imax :=  -1; (* make it something that will crash if wrong *) 
    last := LAST(m^);
  BEGIN
    d := 1.0d0;

    <* ASSERT FIRST(m^) = 0 AND 
              FIRST(m^) = FIRST(m[0]) AND 
              LAST(m^) = LAST(m[0]) *>

    FOR row := 0 TO last DO VAR aamax := 0.0d0; BEGIN
      (* find largest element on row *)
      FOR col := 0 TO last DO aamax := MAX(aamax, ABS(m[row,col])) END;
      IF aamax = 0.0d0 THEN RAISE Matrix.Singular(row) END;
      vv[row] := 1.0d0 / aamax
    END END; (* FOR row *)

    (* loop over cols *)
    FOR col := 0 TO last DO

      FOR row := 0 TO col - 1 DO VAR sum := m[row,col]; BEGIN
        FOR k:= 0 TO row - 1 DO sum := sum - m[row,k] * m[k,col] END;
        m[row,col] := sum
      END END; (* 14 *)

      VAR aamax := 0.0d0; BEGIN
        FOR row := col TO last DO
          VAR
            sum := m[row,col];
          BEGIN
            FOR k:= 0 TO col - 1 DO sum := sum - m[row,k] * m[k,col] END;
            m[row,col] := sum;
            (*<* ASSERT vv[row] >= 0.0d0 *>*)
            WITH dum = vv[row] * ABS(sum) DO IF dum >= aamax THEN
              imax := row;
              aamax := dum
            END END
          END
        END (* 16 *)
      END;

      IF col # imax THEN
        FOR k:= 0 TO last DO VAR dum := m[imax,k]; BEGIN
          m[imax,k] := m[col,k]; m[col,k] := dum 
        END END; (* 17 *)
        d := -d;
        vv[imax] := vv[col]
      END; (* IF *)
      indx[col] := imax;
      IF m[col,col] = 0.0d0 THEN m[col,col] := Tiny END; 
      IF col # last THEN VAR dum := 1.0d0 / m[col,col]; BEGIN
          FOR row:=col + 1 TO last DO m[row,col] := m[row,col] * dum END
      END END
    END (* FOR col *) (* 19 *)
  END DecomposeR;
  
PROCEDURE BackSubstitute(READONLY m : Matrix.T; 
                           READONLY indx : REF ARRAY OF INTEGER; 
                           b : Matrix.Vector) =
  (* LU back-substitution to solve system of linear equations *)
  (* m should be the LU decomposition of a matrix M established by *)
  (* LUdecompose, above *)
  (* Num. Rec. FORTRAN 2nd ed. p. 39 *)

  (* MODIFIES b *)
  VAR
    ii := -1;
    last := LAST(m^);
  BEGIN
    <* ASSERT FIRST(m^) = 0 AND 
              FIRST(m^) = FIRST(m[0]) AND 
              LAST(m^) = LAST(m[0]) *>
    FOR row := 0 TO last DO
      VAR
        ll := indx[row];
        sum := b[ll];
      BEGIN
        b[ll] := b[row];
        IF ii # -1 THEN 
          FOR col := ii TO row - 1 DO
            sum := sum - m[row,col] * b[col];
          END;
        ELSIF sum # 0.0d0 THEN
          ii := row;
        END; (* IF *)
        b[row] := sum;
      END;
    END; (* FOR row *)

    FOR row := last TO 0 BY -1 DO VAR sum := b[row]; BEGIN
      FOR col := row + 1 TO last DO sum := sum - m[row,col] * b[col] END;
      b[row] := sum / m[row,row];
    END END; (* FOR row *)

  END BackSubstitute;

PROCEDURE BackSubstituteArray(READONLY m : Matrix.T; 
                           READONLY indx : REF ARRAY OF INTEGER; 
                           VAR b : ARRAY OF LONGREAL) =
  (* LU back-substitution to solve system of linear equations *)
  (* m should be the LU decomposition of a matrix M established by *)
  (* LUdecompose, above *)
  (* Num. Rec. FORTRAN 2nd ed. p. 39 *)

  (* MODIFIES b *)
  VAR
    ii := -1;
    last := LAST(m^);
  BEGIN
    <* ASSERT FIRST(m^) = 0 AND 
              FIRST(m^) = FIRST(m[0]) AND 
              LAST(m^) = LAST(m[0]) *>
    FOR row := 0 TO last DO
      VAR
        ll := indx[row];
        sum := b[ll];
      BEGIN
        b[ll] := b[row];
        IF ii # -1 THEN 
          FOR col := ii TO row - 1 DO
            sum := sum - m[row,col] * b[col];
          END;
        ELSIF sum # 0.0d0 THEN
          ii := row;
        END; (* IF *)
        b[row] := sum;
      END;
    END; (* FOR row *)

    FOR row := last TO 0 BY -1 DO VAR sum := b[row]; BEGIN
      FOR col := row + 1 TO last DO sum := sum - m[row,col] * b[col] END;
      b[row] := sum / m[row,row];
    END END; (* FOR row *)

  END BackSubstituteArray;

BEGIN END LU.

