(* $Id$ *)

MODULE Main;
IMPORT Matrix, MatrixM3;
IMPORT Random;
IMPORT RefSeq;
IMPORT XTime AS Time;
IMPORT IO, Fmt;

IMPORT RMatrix2 AS M2, RM2M3 AS M2M3, RLU2 AS LU2;

VAR r := NEW(Random.Default).init();
VAR s := NEW(RefSeq.T).init();

PROCEDURE RandomRMatrix() : REF M2.M =
  VAR 
    res := NEW(REF M2.M, Size, Size);
  BEGIN
    FOR i := FIRST(res^) TO LAST(res^) DO
      FOR j := FIRST(res[0]) TO LAST(res[0]) DO
        res[i,j] := M2.Rand(r)
      END
    END;
    RETURN res
  END RandomRMatrix;

CONST NumMats = 100;

CONST Size = 10;

PROCEDURE PV(nam : TEXT; READONLY v : M2.V) =
  BEGIN
    FOR i := FIRST(v) TO LAST(v) DO
      IO.Put(nam & "[" & Fmt.Int(i) & "] = " & M2.Format(v[i]) & " ")
    END; IO.Put("\n---\n");
  END PV;

BEGIN
  FOR i := 0 TO NumMats-1 DO
    s.addhi(RandomRMatrix())
  END;

  VAR
    vv := NEW(REF M2.V, Size);
    indx := NEW(REF ARRAY OF INTEGER, Size);
    d : M2.Base;
    m : REF M2.M := s.get(0);
    n : REF M2.M := s.get(1);
    m2 : REF M2.M := s.get(2);
    b1, b2, b3, b4 := NEW(REF M2.V, Size);
  BEGIN
    M2.Copy(m^,m2^);

    LU2.DecomposeR(m^,vv^,indx,d);

    b1^ := n[0];
    b2^ := n[0];


    PV("b1",b1^);

    FOR i := FIRST(indx^) TO LAST(indx^) DO
      <* ASSERT indx[i] >= i *>
      IO.Put("indx[" & Fmt.Int(i) & "] = " & Fmt.Int(indx[i]) & " ")
    END; IO.Put("\n---\n");

    LU2.BackSubstitute2(m^, indx, b1^, b4^);

    PV("b4",b4^);

    M2.MulMV(m2^, b4^, b3^);

    PV("b3", b3^);
    
    VAR sum, diff := NEW(REF M2.V, Size); BEGIN
      M2.SubV(b2^,b3^,diff^);
      M2.AddV(b2^,b3^,sum^);

      WITH sumN = M2.SumSqV(sum^), diffN = M2.SumSqV(diff^) DO
        IO.Put("||diff(b2,b3)|| = " & M2.Format(diffN) & "\n");        
        IO.Put("||sum(b2,b3)|| = " & M2.Format(sumN) & "\n");
        IO.Put("ratio = " & M2.Format(diffN/sumN) & "\n");
      END
    END
    
    
  END
END Main.
