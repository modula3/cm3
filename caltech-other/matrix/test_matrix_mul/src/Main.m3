(* $Id$ *)

MODULE Main;
IMPORT Matrix, MatrixM3;
IMPORT Random;
IMPORT RefSeq;
IMPORT XTime AS Time;
IMPORT IO, Fmt;

IMPORT RMatrix2 AS M2, RM2M3 AS M2M3;

VAR r := NEW(Random.Default).init();
VAR s := NEW(RefSeq.T).init();

PROCEDURE RandomMatrix() : Matrix.T =
  VAR 
    res := NEW(Matrix.T, 10, 10);
  BEGIN
    FOR i := FIRST(res^) TO LAST(res^) DO
      FOR j := FIRST(res[0]) TO LAST(res[0]) DO
        res[i,j] := r.longreal()
      END
    END;
    RETURN res
  END RandomMatrix;

PROCEDURE RandomRMatrix() : REF M2.M =
  VAR 
    res := NEW(REF M2.M, 10, 10);
  BEGIN
    FOR i := FIRST(res^) TO LAST(res^) DO
      FOR j := FIRST(res[0]) TO LAST(res[0]) DO
        res[i,j] := M2.Rand(r)
      END
    END;
    RETURN res
  END RandomRMatrix;

CONST NumMats = 100;

BEGIN
  FOR i := 0 TO NumMats-1 DO
    s.addhi(RandomMatrix())
  END;

  VAR
    a, b, c := RandomMatrix();
    m3Sum, fSum := 0.0d0;
    m3Start, m3Stop, fStart, fStop : Time.T;
  BEGIN
    Matrix.MulD(a,b,c);

    m3Start := Time.Now();
    FOR i := 0 TO s.size()-1 DO
      FOR j := 0 TO i DO
        MatrixM3.MulD(s.get(i),s.get(j),c);

        m3Sum := m3Sum + Matrix.Mean(c)
      END
    END;
    m3Stop := Time.Now();

    IO.Put("m3Sum = " & Fmt.LongReal(m3Sum) & 
      " m3Time = " & Fmt.LongReal(m3Stop - m3Start) & "\n");


    fStart := Time.Now();
    FOR i := 0 TO s.size()-1 DO
      FOR j := 0 TO i DO
        Matrix.MulD(s.get(i),s.get(j),c);
        
        fSum := fSum + Matrix.Mean(c) 
      END
    END;
    fStop := Time.Now();

    IO.Put("fSum  = " & Fmt.LongReal(fSum) & 
      " fTime  = " & Fmt.LongReal(fStop - fStart) & "\n");
  END;

  IO.Put(
   "**********************************************************************\n");

  s := NEW(RefSeq.T).init();

  FOR i := 0 TO NumMats-1 DO
    s.addhi(RandomRMatrix())
  END;

  PROCEDURE Get(whch : CARDINAL) : REF M2.M = 
    BEGIN RETURN s.get(whch) END Get;

  VAR
    c := NEW(REF M2.M, 10, 10);
    m3Sum, fSum := FLOAT(0, M2.Base);
    m3Start, m3Stop, fStart, fStop : Time.T;
  BEGIN

    m3Start := Time.Now();
    FOR i := 0 TO s.size()-1 DO
      FOR j := 0 TO i DO
        M2M3.MulTransposeMM(Get(i)^,Get(j)^,c^);

        m3Sum := m3Sum + M2.MeanM(c^)
      END
    END;
    m3Stop := Time.Now();

    IO.Put("m3Sum = " & M2.Format(m3Sum) & 
      " m3Time = " & Fmt.LongReal(m3Stop - m3Start) & "\n");


    fStart := Time.Now();
    FOR i := 0 TO s.size()-1 DO
      FOR j := 0 TO i DO
        M2.MulTransposeMM(Get(i)^,Get(j)^,c^);
        
        fSum := fSum + M2.MeanM(c^) 
      END
    END;
    fStop := Time.Now();

    IO.Put("fSum  = " & M2.Format(fSum) & 
      " fTime  = " & Fmt.LongReal(fStop - fStart) & "\n");
  END;

  IO.Put(
   "**********************************************************************\n");

  PROCEDURE Get(whch : CARDINAL) : REF M2.M = 
    BEGIN RETURN s.get(whch) END Get;

  CONST Indxs = 4;

  VAR
    m3Sum, fSum := FLOAT(0, M2.Base);
    m3Start, m3Stop, fStart, fStop : Time.T;
    idxArr : ARRAY [0..Indxs-1] OF CARDINAL;
    wgtArr : ARRAY [0..Indxs-1] OF M2.Base;
  BEGIN
    
    FOR i := FIRST(idxArr) TO LAST(idxArr) DO
      idxArr[i] := r.integer(0,9);
      wgtArr[i] := M2.Rand(r);
    END;

    m3Start := Time.Now();
    FOR i := 0 TO s.size()-1 DO
      FOR j := 0 TO i DO
        
        m3Sum := m3Sum + M2M3.IndexedDot(Get(i)[0], idxArr, wgtArr)
      END
    END;
    m3Stop := Time.Now();

    IO.Put("m3Sum = " & M2.Format(m3Sum) & 
      " m3Time = " & Fmt.LongReal(m3Stop - m3Start) & "\n");


    fStart := Time.Now();
    FOR i := 0 TO s.size()-1 DO
      FOR j := 0 TO i DO
        fSum := fSum + M2.IndexedDot(Get(i)[0], idxArr, wgtArr)
      END
    END;
    fStop := Time.Now();

    IO.Put("fSum  = " & M2.Format(fSum) & 
      " fTime  = " & Fmt.LongReal(fStop - fStart) & "\n");
  END

  

END Main.
