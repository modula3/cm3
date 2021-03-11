(* $Id$ *)

UNSAFE MODULE LU2_F_dp;
IMPORT LRMatrix2 AS M;
IMPORT MatrixF;

PROCEDURE BackSubstitute(READONLY m : M.M; 
                         READONLY indx : REF ARRAY OF INTEGER; 
                         VAR b : M.V) =
  VAR n := NUMBER(m);
  BEGIN
    <* ASSERT FIRST(m) = 0 AND 
              FIRST(m) = FIRST(m[0]) AND 
              LAST(m) = LAST(m[0]) *>
    MatrixF.lu2_backsubstitute_dp_(ADR(m[0,0]),ADR(indx[0]),ADR(b[0]),ADR(n))
  END BackSubstitute;

BEGIN END LU2_F_dp.
