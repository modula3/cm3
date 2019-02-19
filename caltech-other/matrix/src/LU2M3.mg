GENERIC MODULE LU2M3(M);

PROCEDURE BackSubstitute(READONLY m : M.M; 
                         READONLY indx : REF ARRAY OF INTEGER; 
                         VAR b : M.V) =
  (* LU back-substitution to solve system of linear equations *)
  (* m should be the LU decomposition of a matrix M established by *)
  (* LUdecompose, above *)
  (* Num. Rec. FORTRAN 2nd ed. p. 39 *)

  (* MODIFIES b *)
  VAR
    ii := -1;
    last := LAST(m);
  BEGIN
    <* ASSERT FIRST(m) = 0 AND 
              FIRST(m) = FIRST(m[0]) AND 
              LAST(m) = LAST(m[0]) *>
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
        ELSIF sum # FLOAT(0,M.Base) THEN
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

BEGIN END LU2M3.



