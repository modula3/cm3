GENERIC MODULE Matrix2(Elem, M3, F);
IMPORT Env;

PROCEDURE MulTransposeMM(READONLY a,b : M; VAR prod : M) =
  BEGIN
    IF UseFortran THEN
      F.MulTransposeMM(a,b,prod)
    ELSE
      M3.MulTransposeMM(a,b,prod)
    END
  END MulTransposeMM;

PROCEDURE Delta(READONLY v : V; VAR d : V) =
  BEGIN
    IF UseFortran THEN
      F.Delta(v,d)
    ELSE
      M3.Delta(v,d)
    END
  END Delta;

PROCEDURE IndexedDot(READONLY v : V; 
                     READONLY idx : ARRAY OF CARDINAL;
                     READONLY w : V) : Elem.T =
  BEGIN
    IF UseFortran THEN
      RETURN F.IndexedDot(v,idx,w)
    ELSE
      RETURN M3.IndexedDot(v,idx,w)
    END
  END IndexedDot;


PROCEDURE Copy(READONLY a : M; VAR b : M) =
  BEGIN
    FOR r := FIRST(a) TO LAST(a) DO
      FOR c := FIRST(a[0]) TO LAST(a[0]) DO
        b[r,c] := a[r,c]
      END
    END
  END Copy;

PROCEDURE Zero(VAR a : M) =
  BEGIN
    FOR r := FIRST(a) TO LAST(a) DO
      FOR c := FIRST(a[0]) TO LAST(a[0]) DO
        a[r,0] := FLOAT(0,Base)
      END
    END
  END Zero;

PROCEDURE MakeUnit(VAR a : M) =
  BEGIN
    Zero(a);
    FOR r := FIRST(a) TO LAST(a) DO
      a[r,r] := FLOAT(1,Base)
    END
  END MakeUnit;

PROCEDURE MulMM(READONLY a, b : M; VAR prod : M) =
  BEGIN
    WITH aRows = NUMBER(a),
         aCols = NUMBER(a[0]),
         bCols = NUMBER(b[0]) DO
      FOR row:= 0 TO aRows - 1 DO
        FOR col:= 0 TO bCols - 1 DO
          VAR
            element := FLOAT(0,Base);
          BEGIN
            FOR term := 0 TO aCols - 1 DO
              element := element + a[row,term] * b[term,col];
            END;
            prod[row,col] := element;
          END
        END
      END
    END
  END MulMM;

VAR UseFortran := Env.Get("FORTRANMATH") # NIL;

PROCEDURE MulMV(READONLY a : M; READONLY b : V; VAR prod : V) =
  BEGIN
    IF UseFortran THEN
      F.MulMV(a,b,prod)
    ELSE
      M3.MulMV(a,b,prod)
    END
  END MulMV;

PROCEDURE MulMC(READONLY a : M; READONLY b : M; VAR res : V) =
  BEGIN
    IF UseFortran THEN
      F.MulMC(a,b,res)
    ELSE
      M3.MulMC(a,b,res)
    END
  END MulMC;

PROCEDURE MulMC2(READONLY a : M; READONLY b : M; VAR prod, psq : V) =
  BEGIN
    WITH aRows = NUMBER(a),
         aCols = NUMBER(a[0]) DO
      FOR row:= 0 TO aRows - 1 DO
        VAR
          element, e2 := FLOAT(0,Base);
        BEGIN
          FOR term := 0 TO aCols - 1 DO
            VAR part := a[row,term] * b[term,0]; BEGIN
              element := element + part;
              e2 := e2 + part * part
            END
          END;
          prod[row] := element; psq[row] := e2
        END
      END
    END
  END MulMC2;

PROCEDURE MulMVC(READONLY a : M; READONLY b : V; VAR res : M) =
  BEGIN
    IF UseFortran THEN
      F.MulMVC(a,b,res)
    ELSE
      M3.MulMVC(a,b,res)
    END
  END MulMVC;

(**********************************************************************)

PROCEDURE MeanM(READONLY m : M) : Elem.T =
  VAR
    res := FLOAT(0,Elem.T);
  BEGIN
    FOR r := 0 TO NUMBER(m)-1 DO
      FOR c := 0 TO NUMBER(m[0])-1 DO
        res := res + m[r,c]
      END
    END;
    RETURN res / FLOAT(NUMBER(m), Elem.T)
  END MeanM;

PROCEDURE MeanSqM(READONLY m : M) : Elem.T =
  VAR
    res := FLOAT(0,Elem.T);
  BEGIN
    FOR r := 0 TO NUMBER(m)-1 DO
      FOR c := 0 TO NUMBER(m[0])-1 DO
        res := res + m[r,c] * m[r,c]
      END
    END;
    RETURN res / FLOAT(NUMBER(m), Elem.T)
  END MeanSqM;

PROCEDURE SumSqM(READONLY m : M) : Elem.T =
  VAR
    res := FLOAT(0,Elem.T);
  BEGIN
    FOR r := 0 TO NUMBER(m)-1 DO
      FOR c := 0 TO NUMBER(m[0])-1 DO
        res := res + m[r,c] * m[r,c]
      END
    END;
    RETURN res 
  END SumSqM;

PROCEDURE SumM(READONLY m : M) : Elem.T =
  VAR
    res := FLOAT(0,Elem.T);
  BEGIN
    FOR r := 0 TO NUMBER(m)-1 DO
      FOR c := 0 TO NUMBER(m[0])-1 DO
        res := res + m[r,c]
      END
    END;
    RETURN res 
  END SumM;

PROCEDURE DevSqM(READONLY m : M) : Elem.T =
  VAR
    mm := FLOAT(0,Elem.T);
    msq := FLOAT(0,Elem.T);
    rows := NUMBER(m);
    cols := NUMBER(m[0]);
    n := FLOAT(rows * cols, Elem.T);
  BEGIN
    FOR r := 0 TO rows - 1 DO
      FOR c := 0 TO cols - 1 DO
        mm := mm + m[r,c];
        msq := msq + m[r,c] * m[r,c]
      END
    END;
    
    RETURN msq - mm * mm/n
  END DevSqM;

PROCEDURE SumDiffSqM(READONLY m,n : M) : Elem.T =
  VAR
    msq := FLOAT(0,Elem.T);
    rows := GetDim(m).rows;
    cols := GetDim(m).cols;
  BEGIN
    WITH ndim = GetDim(n) DO
      <* ASSERT ndim.rows = rows AND ndim.cols = cols *>
    END;

    FOR r := 0 TO rows - 1 DO
      FOR c := 0 TO cols - 1 DO
        WITH diff = m[r,c]-n[r,c] DO
          msq := msq + diff*diff
        END
      END
    END;
    
    RETURN msq 
  END SumDiffSqM;

(**********************************************************************)

PROCEDURE MeanV(READONLY m : V) : Elem.T =
  VAR
    res := FLOAT(0,Elem.T);
  BEGIN
    FOR r := 0 TO NUMBER(m)-1 DO
      res := res + m[r]
    END;
    RETURN res / FLOAT(NUMBER(m), Elem.T)
  END MeanV;

PROCEDURE MeanSqV(READONLY m : V) : Elem.T =
  VAR
    res := FLOAT(0,Elem.T);
  BEGIN
    FOR r := 0 TO NUMBER(m)-1 DO
      res := res + m[r] * m[r]
    END;
    RETURN res / FLOAT(NUMBER(m), Elem.T)
  END MeanSqV;

PROCEDURE SumSqV(READONLY m : V) : Elem.T =
  VAR
    res := FLOAT(0,Elem.T);
  BEGIN
    FOR r := 0 TO NUMBER(m)-1 DO
      res := res + m[r] * m[r]
    END;
    RETURN res 
  END SumSqV;

PROCEDURE SumV(READONLY m : V) : Elem.T =
  VAR
    res := FLOAT(0,Elem.T);
  BEGIN
    FOR r := 0 TO NUMBER(m)-1 DO
      res := res + m[r]
    END;
    RETURN res 
  END SumV;

PROCEDURE DevSqV(READONLY m : V) : Elem.T =
  VAR
    mm := FLOAT(0,Elem.T);
    msq := FLOAT(0,Elem.T);
    rows := NUMBER(m);
    n := FLOAT(rows, Elem.T);
  BEGIN
    FOR r := 0 TO rows - 1 DO
      mm := mm + m[r];
      msq := msq + m[r] * m[r]
    END;
    
    RETURN msq - mm * mm/n
  END DevSqV;

PROCEDURE SumDiffSqV(READONLY m,n : V) : Elem.T =
  VAR
    msq := FLOAT(0,Elem.T);
    rows := NUMBER(m);
  BEGIN
    FOR r := 0 TO rows - 1 DO
      WITH diff = m[r]-n[r] DO
        msq := msq + diff*diff
      END
    END;
    
    RETURN msq 
  END SumDiffSqV;

(**********************************************************************)

PROCEDURE GetDim(READONLY m : M) : Dim =
  VAR
    rows : INTEGER;
    cols := 0;
  BEGIN
    rows := NUMBER(m);
    IF rows # 0 THEN
      cols := NUMBER(m[0]);
    END;
    RETURN Dim{ rows, cols };
  END GetDim;

PROCEDURE NewM(dims : Dim) : REF M =
  BEGIN RETURN NEW(REF M, dims.rows, dims.cols) END NewM;

PROCEDURE NewV(s : CARDINAL) : REF V =
  BEGIN RETURN NEW(REF V, s) END NewV;

PROCEDURE FormatM(READONLY m : M) : TEXT =
  VAR
    str := "";
  BEGIN
    FOR row := 0 TO GetDim(m).rows - 1 DO
      FOR col := 0 TO GetDim(m).cols - 1 DO
        str := str & Elem.Format(m[row,col]) & " ";
      END;
      str := str & "\n";
    END;
    RETURN str;
  END FormatM;

PROCEDURE FormatV(READONLY m : V) : TEXT =
  VAR
    str := "";
  BEGIN
    FOR row := 0 TO NUMBER(m) - 1 DO
      str := str & Elem.Format(m[row]) & " "
    END;
    RETURN str;
  END FormatV;


PROCEDURE AddToDiagonal(VAR m : M; a : Base) =
  BEGIN
    FOR i := 0 TO GetDim(m).rows - 1 DO
      m[i,i] := a + m[i,i]
    END
  END AddToDiagonal;

PROCEDURE Det(READONLY m : M) : Base =

  PROCEDURE Small(READONLY b : M; skipCol : INTEGER) : REF M =
    VAR
      s : REF M := NewM(Dim{ GetDim(b).rows - 1, GetDim(b).cols - 1 });
    BEGIN
      <* ASSERT skipCol >= 0 AND skipCol <= GetDim(b).cols - 1 *>
      <* ASSERT GetDim(s^).cols = NUMBER(s[0]) *>
      FOR row := 0 TO GetDim(s^).rows - 1 DO VAR corr := 0; BEGIN
          FOR col := 0 TO GetDim(b).cols - 1 DO
            IF col = skipCol THEN 
              corr := -1
            ELSE
              (* copy element *) 
              s[row,col + corr] := b[row + 1,col];
            END 
          END (* FOR col *)
        END END;
      RETURN s
    END Small;

  BEGIN
    IF GetDim(m).cols = 1 THEN
      RETURN m[0,0]
    ELSE
      VAR
        det := FLOAT(0,Base);
        mult : Base;
      BEGIN
        FOR col := 0 TO GetDim(m).cols - 1 DO
          IF col MOD 2 = 0 THEN mult := FLOAT(1,Base) ELSE mult := FLOAT(-1,Base) END;
          det := det + mult * m[0,col] * Det(Small(m,col)^);
        END;
        RETURN det;
      END;
    END;
  END Det;

PROCEDURE SetCol(VAR m : M; c : CARDINAL; READONLY col : V) =
  BEGIN
    <* ASSERT NUMBER(col) = GetDim(m).rows *>
    FOR r := 0 TO NUMBER(col) - 1 DO
      m[r, c] := col[r]
    END
  END SetCol;

PROCEDURE ExtractRowAsVector(READONLY m : M; r : CARDINAL; VAR res : V) =
  BEGIN res := m[r] END ExtractRowAsVector;

PROCEDURE ExtractColAsVector(READONLY m : M; c : CARDINAL; VAR res : V) =
  BEGIN
    FOR r := FIRST(m) TO LAST(m) DO
      res[r] := m[r,c]
    END
  END ExtractColAsVector;

PROCEDURE SubV(READONLY a, b : V; VAR c : V) =
  BEGIN 
    FOR i := FIRST(c) TO LAST(c) DO
      c[i] := a[i] - b[i] 
    END
  END SubV;

PROCEDURE AddV(READONLY a, b : V; VAR c : V) =
  BEGIN 
    FOR i := FIRST(c) TO LAST(c) DO
      c[i] := a[i] + b[i] 
    END
  END AddV;

PROCEDURE SubM(READONLY a, b : M; VAR c : M) =
  BEGIN 
    FOR i := FIRST(c) TO LAST(c) DO
      FOR j := FIRST(c[0]) TO LAST(c[0]) DO
        c[i,j] := a[i,j] - b[i,j] 
      END
    END
  END SubM;

PROCEDURE AddM(READONLY a, b : M; VAR c : M) =
  BEGIN 
    FOR i := FIRST(c) TO LAST(c) DO
      FOR j := FIRST(c[0]) TO LAST(c[0]) DO
        c[i,j] := a[i,j] + b[i,j] 
      END
    END
  END AddM;

PROCEDURE MulSM(s : Base; READONLY a : M; VAR c : M) =
  BEGIN 
    FOR i := FIRST(c) TO LAST(c) DO
      FOR j := FIRST(c[0]) TO LAST(c[0]) DO
        c[i,j] := s * a[i,j]
      END
    END
  END MulSM;

PROCEDURE LinearCombination(aw : Elem.T; READONLY a : M;
                            bw : Elem.T; READONLY b : M;
                            VAR c : M) =
  BEGIN
    FOR i := FIRST(c) TO LAST(c) DO
      FOR j := FIRST(c[0]) TO LAST(c[0]) DO
        c[i,j] := aw*a[i,j] + bw*b[i,j] 
      END
    END
  END LinearCombination;

BEGIN END Matrix2.

