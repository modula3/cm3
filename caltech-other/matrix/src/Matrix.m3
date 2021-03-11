(* Copyright (c) 2000 Mika Nystrom.  All Rights Reserved. *)
(* $Id$ *)
(* an inefficient matrix library *)
(* it is inefficient because things are done with newly allocated matrices *)
(* rather than in-place *)

MODULE Matrix;
IMPORT Fmt;

PROCEDURE ColVector(vector : REF ARRAY OF LONGREAL) : T =
  VAR
    m := New(Dim{NUMBER(vector^),1});
  BEGIN
    FOR row:= 0 TO NUMBER(vector^) - 1 DO
      m[row,0] := vector[row];
    END;
    RETURN m;
  END ColVector;

PROCEDURE RowVector(vector : REF ARRAY OF LONGREAL) : T =
  VAR
    m := New(Dim{1,NUMBER(vector^)});
  BEGIN
    FOR col := 0 TO NUMBER(vector^) - 1 DO
      m[0,col] := vector[col];
    END;
    RETURN m;
  END RowVector;

PROCEDURE GetDim(m : T) : Dim =
  VAR
    rows : INTEGER;
    cols := 0;
  BEGIN
    rows := NUMBER(m^);
    IF rows # 0 THEN
      cols := NUMBER(m[0]);
    END;
    RETURN Dim{ rows, cols };
  END GetDim;

PROCEDURE Add(a,b : T): T            RAISES { DimensionMismatch } =
  VAR
    sum : T;
  BEGIN
    IF GetDim(a) # GetDim(b) THEN RAISE DimensionMismatch END;
    sum := New(GetDim(a));
    FOR row:= 0 TO GetDim(a).rows - 1 DO
      FOR col:= 0 TO GetDim(a).cols - 1 DO
        sum[row,col] := a[row,col] + b[row,col];
      END;
    END;
    RETURN sum;
  END Add;

PROCEDURE Sub(a,b : T): T            RAISES { DimensionMismatch } =
  VAR
    diff : T;
  BEGIN
    IF GetDim(a) # GetDim(b) THEN RAISE DimensionMismatch END;
    diff := New(GetDim(a));
    FOR row:= 0 TO GetDim(diff).rows - 1 DO
      FOR col:= 0 TO GetDim(diff).cols - 1 DO
        diff[row,col] := a[row,col] - b[row,col];
      END;
    END;
    RETURN diff;
  END Sub;

PROCEDURE Mul(a,b : T): T            RAISES { DimensionMismatch } =
  VAR
    prod : T;
    aDim := GetDim(a);
    bDim := GetDim(b);
  BEGIN
    IF GetDim(a).cols # GetDim(b).rows THEN RAISE DimensionMismatch END;
    prod := New(Dim{ aDim.rows, bDim.cols });
    FOR row:= 0 TO aDim.rows - 1 DO
      FOR col:= 0 TO bDim.cols - 1 DO
        VAR
          element := 0.0d0;
        BEGIN
          FOR term := 0 TO aDim.cols - 1 DO
            element := element + a[row,term] * b[term,col];
          END;
          prod[row,col] := element;
        END;
      END;
    END;
    RETURN prod;
  END Mul;

PROCEDURE MulTranspose(a,b : T): T            RAISES { DimensionMismatch } =
  VAR
    prod : T;
    aDim := GetDim(a);
    bDim := GetDim(b);
  BEGIN
    IF GetDim(a).rows # GetDim(b).rows THEN RAISE DimensionMismatch END;
    prod := New(Dim{ aDim.cols, bDim.cols });
    FOR row:= 0 TO aDim.cols - 1 DO
      FOR col:= 0 TO bDim.cols - 1 DO
        VAR
          element := 0.0d0;
        BEGIN
          FOR term := 0 TO aDim.rows - 1 DO
            element := element + a[term,row] * b[term,col];
          END;
          prod[row,col] := element;
        END;
      END;
    END;
    RETURN prod;
  END MulTranspose;

PROCEDURE MulTransposeD(a,b,prod : T)            RAISES { DimensionMismatch } =
  VAR
    aDim := GetDim(a);
    bDim := GetDim(b);
  BEGIN
    IF GetDim(a).rows # GetDim(b).rows THEN RAISE DimensionMismatch END;

    IF GetDim(prod).rows # aDim.cols OR GetDim(prod).cols # bDim.cols THEN
      RAISE DimensionMismatch 
    END;
      
    FOR row:= 0 TO aDim.cols - 1 DO
      FOR col:= 0 TO bDim.cols - 1 DO
        VAR
          element := 0.0d0;
        BEGIN
          FOR term := 0 TO aDim.rows - 1 DO
            element := element + a[term,row] * b[term,col];
          END;
          prod[row,col] := element;
        END;
      END;
    END
  END MulTransposeD;

PROCEDURE AddToDiagonal(m : T; a : LONGREAL) RAISES { NotSquare } =
  BEGIN
    IF GetDim(m).rows # GetDim(m).cols THEN
      RAISE NotSquare
    END;

    FOR i := 0 TO GetDim(m).rows - 1 DO
      m[i,i] := a + m[i,i]
    END
  END AddToDiagonal;

PROCEDURE Trace(m : T) : LONGREAL RAISES { NotSquare } =
  VAR
    res := 0.0d0;
  BEGIN
    IF GetDim(m).rows # GetDim(m).cols THEN
      RAISE NotSquare
    END;

    FOR i := 0 TO GetDim(m).rows - 1 DO
      res := res + m[i,i]
    END;
    RETURN res
  END Trace;

PROCEDURE Mean(m : T) : LONGREAL =
  VAR
    res := 0.0d0;
    rows := GetDim(m).rows;
    cols := GetDim(m).cols;
  BEGIN
    FOR r := 0 TO rows - 1 DO
      FOR c := 0 TO cols - 1 DO
        res := res + m[r,c]
      END
    END;
    RETURN res / FLOAT(rows, LONGREAL)
  END Mean;

PROCEDURE MeanSq(m : T) : LONGREAL =
  VAR
    res := 0.0d0;
    rows := GetDim(m).rows;
    cols := GetDim(m).cols;
  BEGIN
    FOR r := 0 TO rows - 1 DO
      FOR c := 0 TO cols - 1 DO
        res := res + m[r,c] * m[r,c]
      END
    END;
    RETURN res / FLOAT(rows, LONGREAL)
  END MeanSq;

PROCEDURE DevSq(m : T) : LONGREAL =
  VAR
    mm := 0.0d0;
    msq := 0.0d0;
    rows := GetDim(m).rows;
    cols := GetDim(m).cols;
    n := FLOAT(rows * cols, LONGREAL);
  BEGIN
    FOR r := 0 TO rows - 1 DO
      FOR c := 0 TO cols - 1 DO
        mm := mm + m[r,c];
        msq := msq + m[r,c] * m[r,c]
      END
    END;
    
    RETURN msq - mm * mm/n
  END DevSq;
    
PROCEDURE SumSq(m : T) : LONGREAL =
  VAR
    msq := 0.0d0;
    rows := GetDim(m).rows;
    cols := GetDim(m).cols;
  BEGIN
    FOR r := 0 TO rows - 1 DO
      FOR c := 0 TO cols - 1 DO
        msq := msq + m[r,c] * m[r,c]
      END
    END;
    
    RETURN msq 
  END SumSq;
    
PROCEDURE SumDiffSq(m,n : T) : LONGREAL =
  VAR
    msq := 0.0d0;
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
  END SumDiffSq;
    
PROCEDURE Det(m : T): LONGREAL       RAISES { NotSquare } =

  PROCEDURE Small(b : T; skipCol : INTEGER) : T =
    VAR
      s : T := New(Dim{ GetDim(b).rows - 1, GetDim(b).cols - 1 });
    BEGIN
      <* ASSERT skipCol >= 0 AND skipCol <= GetDim(b).cols - 1 *>
      <* ASSERT GetDim(s).cols = NUMBER(s[0]) *>
      FOR row := 0 TO GetDim(s).rows - 1 DO VAR corr := 0; BEGIN
          FOR col := 0 TO GetDim(b).cols - 1 DO
            IF col = skipCol THEN 
              corr := -1
            ELSE
              (* copy element *) 
              s[row,col + corr] := b[row + 1,col];
            END 
          END (* FOR col *)
        END END;
      (*Debug.Out("Small on: \n" & Format(b) & "Returning: \n" & Format(s));*)
      RETURN s
    END Small;

  BEGIN
    IF GetDim(m).cols # GetDim(m).rows THEN RAISE NotSquare END;
    IF GetDim(m).cols = 1 THEN
      RETURN m[0,0];
    ELSE
      VAR
        det := 0.0d0;
        mult : LONGREAL;
      BEGIN
        FOR col := 0 TO GetDim(m).cols - 1 DO
          IF col MOD 2 = 0 THEN mult := 1.0d0 ELSE mult := -1.0d0 END;
          det := det + mult * m[0,col] * Det(Small(m,col));
        END;
        RETURN det;
      END;
    END;
  END Det;

PROCEDURE New(dim : Dim) : T =
  BEGIN
    RETURN NEW(T,dim.rows,dim.cols);
  END New;

PROCEDURE Unit(dim : Dim) : T RAISES { NotSquare } =
  VAR 
    new := Zero(dim);
  BEGIN
    IF dim.rows # dim.cols THEN RAISE NotSquare END;
    FOR i := 0 TO dim.rows - 1 DO
      new[i,i] := 1.0d0;
    END;
    RETURN new;
  END Unit;

PROCEDURE Zero(dim : Dim) : T =
  VAR
    new := New(dim);
  BEGIN
    FOR row := 0 TO dim.rows - 1 DO
      FOR col := 0 TO dim.cols - 1 DO
        new[row,col] := 0.0d0;
      END;
    END;
    RETURN new;
  END Zero;

(* format a matrix for printing *)
PROCEDURE Format(m : T) : TEXT =
  VAR
    str := "";
  BEGIN
    FOR row := 0 TO GetDim(m).rows - 1 DO
      FOR col := 0 TO GetDim(m).cols - 1 DO
        str := str & Fmt.LongReal(m[row,col],style := Fmt.Style.Sci, 
                                  prec := 3 ) & " ";
      END;
      str := str & "\n";
    END;
    RETURN str;
  END Format;

PROCEDURE FormatVector(v : Vector) : TEXT =
  VAR
    str := "";
  BEGIN
    FOR i := FIRST(v^) TO LAST(v^) DO
      str := str & Fmt.LongReal(v[i],style := Fmt.Style.Sci, 
                                prec := 3 ) & " ";
    END;
    RETURN str;
  END FormatVector;

PROCEDURE Transpose(a : T): T =
  VAR
    new := New(Dim { GetDim(a).cols, GetDim(a).rows });
  BEGIN
    FOR row := 0 TO GetDim(new).rows - 1 DO
      FOR col := 0 TO GetDim(new).cols - 1 DO
        WITH oldcol = row, oldrow = col DO
          new[row,col] := a[oldrow,oldcol];
        END;
      END;
    END;
    RETURN new;
  END Transpose;
  
PROCEDURE Scale(a : LONGREAL; m : T) : T =
  VAR
    new := New(GetDim(m));
  BEGIN
    FOR row := 0 TO GetDim(new).rows - 1 DO
      FOR col := 0 TO GetDim(new).cols - 1 DO
        new[row,col] := a * m[row,col];
      END;
    END;
    RETURN new;
  END Scale;

PROCEDURE Zap(m : T; threshold : LONGREAL ) : T = (* Zap small entries *)
  VAR
    res := New(GetDim(m));
  BEGIN
    FOR row := 0 TO GetDim(res).rows - 1 DO
      FOR col := 0 TO GetDim(res).cols - 1 DO
        IF ABS(m[row,col]) > threshold THEN res[row,col] := m[row,col]
        ELSE res[row,col] := 0.0d0
        END
      END
    END;
    RETURN res
  END Zap;

PROCEDURE L(m : T) : T = (* get L part of LU matrix *)
  VAR
    res := New(GetDim(m));
  BEGIN
    FOR row := 0 TO GetDim(res).rows - 1 DO
      FOR col := 0 TO GetDim(res).cols - 1 DO
        IF row > col THEN res[row,col] := m[row,col]
        ELSIF row = col THEN res[row,col] := 1.0d0
        ELSE res[row,col] := 0.0d0 END
      END
    END;
    RETURN res
  END L;

PROCEDURE U(m : T) : T = (* get U part of LU matrix *)
  VAR
    res := New(GetDim(m));
  BEGIN
    FOR row := 0 TO GetDim(res).rows - 1 DO
      FOR col := 0 TO GetDim(res).cols - 1 DO
        IF row <= col THEN res[row,col] := m[row,col]
        ELSE res[row,col] := 0.0d0 END
      END
    END;
    RETURN res
  END U;

PROCEDURE ExtractRowAsVector(m : T; r : CARDINAL) : Vector =
  VAR
    cols := GetDim(m).cols;
    res := NEW(Vector, cols);
  BEGIN
    FOR c := 0 TO cols-1 DO
      res[c] := m[r,c]
    END;
    RETURN res
  END ExtractRowAsVector;

PROCEDURE ExtractRowAsVectorD(m : T; r : CARDINAL; res : Vector) =
  VAR
    cols := GetDim(m).cols;
  BEGIN
    FOR c := 0 TO cols-1 DO res[c] := m[r,c] END
  END ExtractRowAsVectorD;

PROCEDURE ExtractColAsVector(m : T; c : CARDINAL) : Vector =
  VAR
    rows := GetDim(m).rows;
    res := NEW(Vector, rows);
  BEGIN
    FOR r := 0 TO rows-1 DO
      res[r] := m[r,c]
    END;
    RETURN res
  END ExtractColAsVector;

PROCEDURE ExtractRow(m : T; r : CARDINAL) : T =
  VAR
    cols := GetDim(m).cols;
    res := NEW(T, 1, cols);
  BEGIN
    FOR c := 0 TO cols-1 DO
      res[0,c] := m[r,c]
    END;
    RETURN res
  END ExtractRow;

PROCEDURE ExtractCol(m : T; c : CARDINAL) : T=
  VAR
    rows := GetDim(m).rows;
    res := NEW(T, rows, 1);
  BEGIN
    FOR r := 0 TO rows-1 DO
      res[r, 0] := m[r,c]
    END;
    RETURN res
  END ExtractCol;

PROCEDURE SetCol(m : T; c : CARDINAL; col : Vector) =
  BEGIN
    <* ASSERT NUMBER(col^) = GetDim(m).rows *>
    FOR r := 0 TO NUMBER(col^) - 1 DO
      m[r, c] := col[r]
    END
  END SetCol;

PROCEDURE CopyIn(from, to : T) =
  BEGIN
    FOR r := 0 TO LAST(from^) DO
      FOR c := 0 TO LAST(from[0]) DO
        to[r,c] := from[r,c]
      END
    END
  END CopyIn;

PROCEDURE Measure(colVector, squareMatrix : T) : LONGREAL RAISES { DimensionMismatch }=
  VAR
    temp := Mul(squareMatrix,colVector);
    sum := 0.0d0;
  BEGIN
    FOR r := 0 TO LAST(colVector^) DO
      sum := sum + colVector[r,0] * temp[r,0]
    END;
    RETURN sum
  END Measure;

BEGIN

END Matrix.
