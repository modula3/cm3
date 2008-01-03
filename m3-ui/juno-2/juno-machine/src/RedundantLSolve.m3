(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Feb  4 10:40:47 PST 2000 by gnelson   *)
(*      modified on Thu Apr 10 09:47:22 PDT 1997 by heydon    *)
(*      modified on Thu Nov  3 17:56:57 PST 1994 by isard     *)

MODULE RedundantLSolve;

FROM JunoValue IMPORT Real, Sqrt;
(* IMPORT RowOp; *)
IMPORT IO;
IMPORT Wr, Fmt, Text;
FROM Thread IMPORT Alerted;
<* FATAL Wr.Failure, Alerted *>

VAR debug := 0;
(* debug >= 1 => show initial and scaled matrices
   debug >= 2 => show row-echelon matrix
   debug >= 3 => show column permutation, row-echelon matrix, original &
                 orthonormal basis, and minimal solution
*)

VAR UseGramSchmidt := TRUE;
VAR UseCompletePivoting := TRUE;

PROCEDURE SetGramSchmidt(flag: BOOLEAN) =
  BEGIN UseGramSchmidt := flag END SetGramSchmidt;

TYPE
  T = Real;
  CardArray = REF ARRAY OF CARDINAL;

CONST
  InitRows = 50;
  InitCols = 50;

VAR
  Indent := 9;
  Prec := 3;
  FieldWidth := Prec + 8;

VAR 
  c1 := 0.66; 
  c2 := 0.5;

(* The constants "c1" and "c2" are used in the Veach Heuristic. *)

PROCEDURE LogT(t: TEXT) =
  BEGIN IO.Put(t, logWr) END LogT;

PROCEDURE EtpLogSolveRow(<*UNUSED*> ops: INTEGER) =
  BEGIN END EtpLogSolveRow;

PROCEDURE ShowMatrix(m, n: CARDINAL; READONLY a: Matrix; READONLY t: TEXT) =
  BEGIN
    Wr.PutText(logWr, "  " & t & ":\n");
    FOR i := 0 TO m - 1 DO
      ShowVector(n, a[i]);
    END;
    Wr.Flush(logWr)
  END ShowMatrix;

PROCEDURE ShowVector(n: CARDINAL; READONLY v: Vector) =
  BEGIN
    Wr.PutText(logWr, Fmt.Pad("", Indent));
    FOR j := 0 TO n - 1 DO
      Wr.PutText(logWr,
        Fmt.Pad(Fmt.Real(v[j], Fmt.Style.Sci, prec := Prec), FieldWidth))
    END;
    Wr.PutChar(logWr, '\n');
  END ShowVector;

VAR
  p := NEW(REF Vector, InitCols);     (* solution vector (not permuted) *)
  temp := NEW(REF Vector, InitCols);  (* temporary vector for "SwapRows" *)
  basis := NEW(REF Matrix, InitCols, InitCols);
  colPerm := NEW(CardArray, InitCols);
  rowMax := NEW(CardArray, InitRows);

  (* "colPerm[c]" (0 <= c < n) is the index in "x" of the unknown represented
     by column "c" of matrix "a".

     "rowMax[i]" (0 <= i < m) is the index of the column of row "i" of "A"
     with largest absolute value (note that "A" does not include column "n" of
     the matrix "a"). *)

PROCEDURE P(
    m, n: CARDINAL;
    VAR (*INOUT*) a: Matrix;
    VAR (*OUT*) x: Vector) =
  (* Matrix "A" is stored in "a[0..m-1, 0..n-1]".
     Vector "b" is stored in "a[0..m-1, n]".

     IMPLEMENTATION: The current implementation reduces "A" to a row-echelon
     form in which all of the pivot elements are along "A"'s diagonal. At each
     stage, it chooses the element with largest magnitude in the remaining
     sub-matrix and swaps rows and columns as necessary to make that element
     the next pivot. Once the matrix has been reduced to this row-echelon
     form, it may be under-constrained. In that case, the implementation
     chooses for the solution the point on the solution space with smallest L2
     norm (i.e., the point closest to the origin in Euclidean space).
  *)

  VAR rc: INTEGER := 0;
  (* We only consider pivots along the diagonal of "A", so the current row and
     the current column are always the same value, called "rc". *)

  PROCEDURE InitColPerm() =
  (* Make sure "NUMBER(colPerm^) >= n", and initialize "colPerm[i] = i"
     for "i <= 0 < n". *)
    BEGIN
      (* make sure "colPerm" is large enough *)
      IF NUMBER(colPerm^) < n THEN
	colPerm := NEW(CardArray, MAX(n, 2 * NUMBER(colPerm^)))
      END;
      (* initialize to identity permutation *)
      FOR i := 0 TO n - 1 DO
        colPerm[i] := i
      END
    END InitColPerm;

  PROCEDURE InitRowMax() =
  (* Initialize "rowMax" so it satisfies its invariant.  Also,
     scale the matrix so that the uniform norm of each row
     is unity. *)
  BEGIN
    (* make sure "rowMax" is large enough *)
    IF NUMBER(rowMax^) < m THEN
      rowMax := NEW(CardArray, MAX(m, 2 * NUMBER(rowMax^)))
    END;
    (* initialize maximum index for each row *)
    FOR i := 0 TO m - 1 DO
      WITH currRow = a[i] DO
        VAR maxVal: T := ABS(currRow[0]); maxCol := 0; BEGIN
          FOR j := 1 TO n - 1 DO
            VAR abs := ABS(currRow[j]); BEGIN
              IF abs > maxVal THEN
                maxVal := abs;
                maxCol := j
              END
            END
          END;
          rowMax[i] := maxCol;
          IF maxVal # 0.0 THEN
            (* scale the row so that its uniform norm is unity. *)
            FOR i := 0 TO n DO
              currRow[i] := currRow[i] / maxVal
            END
          END
        END
      END
    END
  END InitRowMax;

  PROCEDURE MaxEntry(r: CARDINAL): CARDINAL =
  (* Return the index "x" of "rowMax" that maximizes the value
     "ABS(a[x, rowMax[x]])" for "r <= x < m". Requires "r < m". *)
    VAR maxVal: T := ABS(a[r, rowMax[r]]); maxR := r; BEGIN
      FOR i := r + 1 TO m - 1 DO
        VAR val := ABS(a[i, rowMax[i]]); BEGIN
          IF val > maxVal THEN
            maxR := i;
            maxVal := val
          END
        END
      END;
      RETURN maxR
    END MaxEntry;

  PROCEDURE MaxEntry2(r: CARDINAL): CARDINAL =
  (* Return the index "x" of "rowMax" that maximizes the value
     "ABS(a[x,r)" for "x: r <= x < m". Requires "r < m". *)
    VAR maxVal: T := ABS(a[r, r]); maxR := r; BEGIN
      FOR i := r + 1 TO m - 1 DO
        VAR val := ABS(a[i, r]); BEGIN
          IF val > maxVal THEN
            maxR := i;
            maxVal := val
          END
        END
      END;
      RETURN maxR
    END MaxEntry2;


  PROCEDURE SwapCols(c1, c2: CARDINAL) =
  (* Swap columns "c1" and "c2" of the matrix "a", adjusting "colPerm" and
     "rowMax" to reflect the swap. *)
    BEGIN
      (* swap column values *)
      FOR i := 0 TO m - 1 DO
        WITH currRow = a[i], v1 = currRow[c1], v2 = currRow[c2] DO
          VAR t := v1; BEGIN v1 := v2; v2 := t END
        END;
        WITH currMax = rowMax[i] DO
          IF currMax = c1 THEN currMax := c2
          ELSIF currMax = c2 THEN currMax := c1
          END
        END
      END;
      (* reflect swap in "colPerm" *)
      WITH v1 = colPerm[c1], v2 = colPerm[c2] DO
        VAR t := v1; BEGIN v1 := v2; v2 := t END
      END
    END SwapCols;

  PROCEDURE SwapRows(r1, r2, c: CARDINAL) =
  (* Swap rows "r1" and "r2" of the matrix "a" from column "c" through
     column "n", adjusting "rowMax" to reflect the swap. *)
    BEGIN
      (* swap rows *)
      WITH row1 = a[r1], row2 = a[r2], cnt = (n - c) + 1 DO
        SUBARRAY(temp^, c, cnt) := SUBARRAY(row1,  c, cnt);
        SUBARRAY(row1,  c, cnt) := SUBARRAY(row2,  c, cnt);
        SUBARRAY(row2,  c, cnt) := SUBARRAY(temp^, c, cnt)
      END;
      (* swap "rowMax" entries *)
      VAR t := rowMax[r1]; BEGIN
        rowMax[r1] := rowMax[r2]; rowMax[r2] := t
      END
    END SwapRows;

  PROCEDURE Pivot(r, c: CARDINAL) =
  (* Pivots matrix "a" about location "(r, c)". This sets all entries in
     column "c" of "a" below row "r" to "0.0" without changing the set of
     solutions to the equations represented by "a". It also maintain the
     invariant on "rowMax". *)
    VAR pivot, factor: T; BEGIN
      WITH pivotRow = a[r] DO
        pivot := pivotRow[c];
      	FOR i := r + 1 TO m - 1 DO
          WITH currRow = a[i] DO
      	    (* adjust row "i" *)
      	    IF currRow[c] # 0.0 THEN
      	      factor := currRow[c] / pivot;
      	      currRow[c] := 0.0;
      	      EtpLogSolveRow(n-c);
      	      VAR maxCol := c; maxAbs := 0.0; BEGIN
      	        FOR j := c+1 TO n-1 DO
                  WITH currRowJ = currRow[j] DO
                    currRowJ := currRowJ - (factor * pivotRow[j]);
                    WITH abs = ABS(currRowJ) DO
                      IF abs > maxAbs THEN
                        maxAbs := abs;
                        maxCol := j
                      END
                    END
                  END
      	        END;
      	        rowMax[i] := maxCol
      	      END;
      	      currRow[n] := currRow[n] - (factor * pivotRow[n])
      	    END
          END
      	END
      END
    END Pivot;

  PROCEDURE SolveRow(
      r: CARDINAL;
      c: INTEGER;
      READONLY v: Vector;
      homog: BOOLEAN): T =
  (* Let "a" be matrix in row-echelon form. The entry "a[r, c]" is assumed to
     be a pivot element of "a". "SolveRow" returns the value for "v[c]" using
     the previously computed values of "v[c+1..n-1]". The value returned for
     "v[c]" satisfies:

|      (+ i: c <= i < n: a[r, i] * v[i]) = x

     where "x = 0" if "homog" is "TRUE" and "x = a[r, n]" if "homog" is
     "FALSE". *)
    VAR sum: T; BEGIN
      WITH rowR = a[r] DO
        (* incorporate known values to the right of "c" *)
        IF homog
          THEN sum := 0.0
          ELSE sum := rowR[n]
        END;
        FOR i := c + 1 TO n - 1 DO
          sum := sum - (rowR[i] * v[i])
        END;
        RETURN sum / rowR[c]
      END
    END SolveRow;

  PROCEDURE BackProp(rc: CARDINAL; VAR (*INOUT*) res: Vector; homog := FALSE) =
  (* Sets the values of the solution vector entries "res[0..rc-1]" so as to
     satisfy "A * res = b". Assumes that the values of "res[rc..n-1]" are
     valid. If "homog" is "TRUE", then solves the system "A * res = 0". *)
    BEGIN
      FOR i := rc - 1 TO 0 BY -1 DO
        res[i] := SolveRow(i, i, res, homog);
      END
    END BackProp;

  PROCEDURE MakeBases(dim: CARDINAL) =
  (* Forms "dim" dimensional basis vectors in "basis[0..dim-1]". *)
    VAR ndim := n - dim; BEGIN
      (* Zero last "dim" entries of each basis vector *)
      FOR i := 0 TO dim - 1 DO
        WITH currRow = basis[i] DO
          FOR j := ndim TO n - 1 DO
            currRow[j] := 0.0
          END
        END
      END;
      FOR i := 0 TO dim - 1 DO
        basis[i, rc + i] := 1.0;	      (* form i'th basis vector *)
        BackProp(rc, basis[i], TRUE);
      END
    END MakeBases;

  PROCEDURE GramSchmidt(dim: CARDINAL; VAR (*INOUT*) b: Matrix) =
  (* Convert the "dim" x "n" matrix of "dim" basis vectors "b" into an
     orthonormal basis using the Gram-Schmidt algorithm. The basis vectors are
     stored in "b[0..dim-1]"; they are assumed to span a space of dimension
     "dim". *)
    BEGIN
      FOR i := 0 TO dim - 1 DO
        WITH rowI = b[i] DO
          (* orthogonalize "b[i]"; "b[0..i-1]" are orthonormal *)
          FOR j := 0 TO i - 1 DO
            WITH rowJ = b[j] DO
              VAR dot := Dot(rowI, rowJ); BEGIN
            	FOR k := 0 TO n - 1 DO
        	  rowI[k] := rowI[k] - (dot * rowJ[k])
            	END
              END
            END
          END;
          (* normalize "b[i]" *)
          VAR len := L2Norm(rowI); BEGIN
            <* ASSERT len > 0.0 *>
            FOR k := 0 TO n - 1 DO
              rowI[k] := rowI[k] / len
            END
          END
        END
      END
    END GramSchmidt;

  PROCEDURE L2Norm(READONLY v: Vector): T =
    VAR sum := Dot(v, v); BEGIN
      RETURN Sqrt(sum)
    END L2Norm;

  PROCEDURE Dot(READONLY v1, v2: Vector): T =
    VAR res := 0.0; BEGIN
      FOR i := 0 TO n - 1 DO
        res := res + (v1[i] * v2[i])
      END;
      RETURN res
    END Dot;

  PROCEDURE VeachHeuristic(): CARDINAL =
  (* Use Eric Veach's heuristic to compute and return how many rows to use.
     On entry, "c1" is the fraction of the L1 norm of the residual
     vector that must be accounted for, and "c2" is the additional
     shrinkage that will be allowed in the pivot elements of rows
     that will be added on for free (relative to the pivot element
     of the row that is required in order to get "c1" of the L1 norm
     of the residual. The procedure returns the number of rows to
     ignore. *)
   VAR 
     residnorm := 0.0; 
     goal: REAL;
     numToUse: CARDINAL;
     pivtotal := 0.0;
   CONST
     Epsilon = 1.0E-6;
   BEGIN
     FOR j := 0 TO m - 1 DO residnorm := residnorm + ABS(a[j, n]) END;
     IF residnorm = 0.0 THEN RETURN 0 END;
     goal := residnorm * c1;
     (* use at least enough rows to reduce the residual norm by "goal". *)
     residnorm := 0.0;
     numToUse := 0;
     WHILE residnorm < goal AND numToUse < MIN(n,m) AND ABS(a[numToUse,numToUse]) > Epsilon DO
       residnorm := residnorm + ABS(a[numToUse, n]);
       pivtotal := pivtotal + ABS(a[numToUse, numToUse]);
       INC(numToUse)
     END;
     (* Now the sum of the residuals in the first "numToUse" rows is
        at least "goal"; or else "numToUse=m".  Use any additional rows 
        whose pivots are not too small; that is, not smaller than "c2" times
        the average magnitude of one of the pivots in use: *)
     IF numToUse > 0 THEN
       VAR limit := c2 * (pivtotal / FLOAT(numToUse)); BEGIN
         WHILE numToUse < MIN(m,n) AND ABS(a[numToUse, numToUse]) >= limit DO
           INC(numToUse)
         END
       END
     END;
     RETURN numToUse;
   END VeachHeuristic;

  (* PROCEDURE P *)
  BEGIN
    (* Check for buggy call *)
    IF NUMBER(a) < m OR (m > 0 AND NUMBER(a[0]) < n + 1) OR NUMBER(x) < n THEN
      <* ASSERT FALSE *>
    END;
    (* make sure "temp" is large enough for "SwapRows" *)
    IF NUMBER(temp^) < n + 1 THEN
      temp := NEW(REF Vector, MAX(n + 1, 2 * NUMBER(temp^)))
    END;
    IF debug >= 1 THEN ShowMatrix(m, n + 1, a, "Matrix before scaling") END;

    (* initialize bookkeeping arrays *)
    InitRowMax();
    IF debug >= 1 THEN ShowMatrix(m, n+1, a, "Matrix after scaling") END;
    InitColPerm();

    (* Put Matrix A in Row-Echelon form *)
    VAR
      maxRow, maxCol: CARDINAL;
    CONST
      Epsilon = 1.0E-6;
      (* Pivots must have magnitude at least "Epsilon". *) 
    BEGIN
      WHILE rc < MIN(m, n) DO
        IF UseCompletePivoting THEN
          maxRow := MaxEntry(rc);
          maxCol := rowMax[maxRow]
        ELSE
          maxRow := MaxEntry2(rc);
          maxCol := rc
        END;
        IF ABS(a[maxRow, maxCol]) > Epsilon THEN
          IF rc # maxCol THEN SwapCols(rc, maxCol) END;
          IF rc # maxRow THEN SwapRows(rc, maxRow, rc) END;
          Pivot(rc, rc);
        END;
        INC(rc)
      END
    END;
    IF debug >= 2 THEN
      ShowMatrix(m, n+1, a, "After pivoting")
    END;
    IF debug >= 3 THEN
      Wr.PutText(logWr, "  Column Permutation:\n");
      Wr.PutText(logWr, Fmt.Pad("", Indent));
      FOR i := 0 TO n - 1 DO
        VAR c := Text.FromChar(VAL(colPerm[i] + ORD('a'), CHAR)); BEGIN
          Wr.PutText(logWr, Fmt.Pad(c, FieldWidth))
        END
      END;
      Wr.PutChar(logWr, '\n');
      ShowMatrix(m, n + 1, a, "Row-Echelon Matrix");
    END;
    rc := VeachHeuristic();   
    IF debug >= 2 THEN 
      LogT("number of rows to use: ");
      IO.PutInt(rc, logWr);
      LogT("\n")
    END;
    IF rc = 0 THEN 
      FOR i := 0 TO n - 1 DO x[i] := 0.0 END
    END;
    (* Back-propagate solution values *)
    IF n > NUMBER(p^) THEN
      p := NEW(REF Vector, MAX(n, 2 * NUMBER(p^)))
    END;
    FOR i := rc TO n - 1 DO p[i] := 0.0 END;
    BackProp(rc, p^);
    (* Check if system is under-constrained *)
    IF rc < n AND UseGramSchmidt THEN
      (* Use Gram-Schmidt to find the solution with smallest L2 norm. *)
      IF n > NUMBER(basis^) THEN
        VAR new_size := MAX(n, 2 * NUMBER(basis^)); BEGIN
          basis := NEW(REF Matrix, new_size, new_size)
        END
      END;
      VAR dim := n - rc; BEGIN
        (* compute the basis vectors *)
        MakeBases(dim);
        IF debug >= 3 THEN
          Wr.PutText(logWr, "  Particular Solution:\n");
          ShowVector(n, p^);
          ShowMatrix(dim, n, basis^, "Original Basis")
        END;
        (* make the basis orthonormal *)
        GramSchmidt(dim, basis^);
        IF debug >= 3 THEN
          ShowMatrix(dim, n, basis^, "Orthonormal Basis")
        END;
        (* compute the minimal solution in "p" *)
        FOR i := 0 TO dim - 1 DO
          WITH basisI = basis[i] DO
            VAR alpha := Dot(p^, basisI); BEGIN
              FOR k := 0 TO n - 1 DO
          	p[k] := p[k] - (alpha * basisI[k])
              END
            END
          END
        END;
        IF debug >= 3 THEN
          Wr.PutText(logWr, "  Minimal Solution:\n");
          ShowVector(n, p^)
        END
      END
    END;
    (* Permute solution "p" into result "x" *)
    FOR i := 0 TO n - 1 DO
      x[colPerm[i]] := p[i]
    END
  END P;

BEGIN END RedundantLSolve.
