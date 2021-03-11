(* $Id$ *)

GENERIC MODULE M2M3(M2);

PROCEDURE MulMV(READONLY a : M2.M; READONLY b : M2.V; VAR prod : M2.V) =
  BEGIN
    WITH aRows = NUMBER(a),
         aCols = NUMBER(a[0]) DO
      FOR row:= 0 TO aRows - 1 DO
        VAR
          element := FLOAT(0,M2.Base);
        BEGIN
          FOR term := 0 TO aCols - 1 DO
            element := element + a[row,term] * b[term]
          END;
          prod[row] := element
        END
      END
    END
  END MulMV;

PROCEDURE MulMC(READONLY a : M2.M; READONLY b : M2.M; VAR prod : M2.V) =
  BEGIN
    WITH aRows = NUMBER(a),
         aCols = NUMBER(a[0]) DO
      FOR row:= 0 TO aRows - 1 DO
        VAR
          element := FLOAT(0,M2.Base);
        BEGIN
          FOR term := 0 TO aCols - 1 DO
            element := element + a[row,term] * b[term,0]
          END;
          prod[row] := element
        END
      END
    END
  END MulMC;

PROCEDURE MulMVC(READONLY a : M2.M; READONLY b : M2.V; VAR prod : M2.M) =
  BEGIN
    WITH aRows = NUMBER(a),
         aCols = NUMBER(a[0]) DO
      FOR row:= 0 TO aRows - 1 DO
        VAR
          element:= FLOAT(0,M2.Base);
        BEGIN
          FOR term := 0 TO aCols - 1 DO
            element := element + a[row,term] * b[term]
          END;
          prod[row,0] := element
        END
      END
    END
  END MulMVC;

PROCEDURE IndexedDot(READONLY v : M2.V; 
                     READONLY idx : ARRAY OF CARDINAL;
                     READONLY w : M2.V) : M2.Base =
  VAR sum := FLOAT(0,M2.Base);
  BEGIN
    FOR i := FIRST(idx) TO LAST(idx) DO
      sum := sum + v[idx[i]]*w[i]
    END;
    RETURN sum
  END IndexedDot;

PROCEDURE Delta(READONLY v : M2.V; VAR d : M2.V) =
  BEGIN
    FOR i := FIRST(d) TO LAST(d) DO
      d[i] := v[i+1]-v[i]
    END
  END Delta;

PROCEDURE MulTransposeMM(READONLY a,b : M2.M; VAR prod : M2.M) =
  VAR
    aDim := M2.GetDim(a);
    bDim := M2.GetDim(b);
  BEGIN
    FOR row:= 0 TO aDim.cols - 1 DO
      FOR col:= 0 TO bDim.cols - 1 DO
        VAR
          element := FLOAT(0,M2.Base);
        BEGIN
          FOR term := 0 TO aDim.rows - 1 DO
            element := element + a[term,row] * b[term,col];
          END;
          prod[row,col] := element
        END;
      END;
    END
  END MulTransposeMM;


BEGIN END M2M3.
