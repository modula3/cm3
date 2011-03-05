(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright 1996 by Digital Equipment Corp. *)

MODULE PairSet;

IMPORT IdSet;
IMPORT Word;

CONST
  ModNMask = N-1;
  
PROCEDURE MakeEmpty(VAR t: T) =
  BEGIN
    FOR i := FIRST(t) TO LAST(t) DO t[i] := IdSet.Empty END
  END MakeEmpty;

PROCEDURE IsEmpty(READONLY t: T): BOOLEAN =
  BEGIN
    FOR i := FIRST(t) TO LAST(t) DO
      IF t[i] # IdSet.Empty THEN RETURN FALSE END
    END;
    RETURN TRUE
  END IsEmpty;

PROCEDURE AddPairD(VAR ps: T; i,j: INTEGER) =
  VAR 
    iP := Word.And(i, ModNMask); 
    jP := Word.And(j, ModNMask); 
  BEGIN
    ps[iP] := ps[iP] + IdSet.T{jP}
  END AddPairD;

PROCEDURE Size(READONLY t: T): CARDINAL =
  VAR res := 0; BEGIN
    FOR i := 0 TO N-1 DO
      res := res + PopCount(t[i])
    END;
    RETURN res
  END Size;

PROCEDURE SparseSize(t: Sparse): CARDINAL =
  VAR res := 0; BEGIN
    FOR i := 0 TO LAST(t^) DO
      res := res + PopCount(t[i].bits)
    END;
    RETURN res
  END SparseSize;

PROCEDURE PopCount(s: IdSet.T): CARDINAL =
  VAR res := 0; BEGIN
    FOR j := 0 TO N-1 DO res := res + ORD(j IN s) END;
    RETURN res
  END PopCount;

(* "PopCount" could be made more efficient, but since "Size" is called only from
   statistics code, it doesn't seem worth it. *)

PROCEDURE UnionD(VAR ps1: T; READONLY ps2: T) =
  BEGIN
    FOR i := 0 TO N-1 DO
      WITH row = ps1[i] DO
        row := row + ps2[i]
      END
    END
  END UnionD;

PROCEDURE UnionSparseD(VAR ps1: T; ps2: Sparse) =
  BEGIN
    FOR j := 0 TO LAST(ps2^) DO
      WITH se = ps2[j], row = ps1[se.i] DO
        row := row + se.bits
      END
    END
  END UnionSparseD;

PROCEDURE AddSetCrossSetD(VAR ps: T; s1, s2: IdSet.T) =
  BEGIN
    FOR i := 0 TO N-1 DO
      IF i IN s1 THEN
        WITH row = ps[i] DO
          row := row + s2
        END
      END
    END
  END AddSetCrossSetD;

PROCEDURE AddSetCrossElemD(
    VAR ps: T; 
    s: IdSet.T; 
    id: INTEGER) =
  BEGIN
    AddSetCrossSetD(ps, s, 
      IdSet.T{Word.And(id, ModNMask)})
  END AddSetCrossElemD;

PROCEDURE AddElemCrossSetD(VAR ps: T; id: INTEGER; s: IdSet.T) =
  BEGIN
    WITH row = ps[Word.And(id, ModNMask)] DO
      row := row + s
    END
  END AddElemCrossSetD;

PROCEDURE Overlap(s1: Sparse; READONLY s2: T): BOOLEAN =
  BEGIN
    FOR j := FIRST(s1^) TO LAST(s1^) DO
      WITH se = s1[j], i = se.i, bits = se.bits DO
        IF s2[i] * bits # IdSet.Empty THEN RETURN TRUE END
      END
    END;
    RETURN FALSE
  END Overlap;

PROCEDURE ToSparse(READONLY b: T): Sparse =
  VAR 
    arr: ARRAY [0..N-1] OF SparseElem;
    count := 0;
  BEGIN
    FOR i := 0 TO N-1 DO
      IF b[i] # IdSet.Empty THEN
        arr[count].bits := b[i];
        arr[count].i := i;
        INC(count)
      END
    END;
    VAR res := NEW(Sparse, count); BEGIN
      res^ := SUBARRAY(arr, 0, count);
      RETURN res
    END
  END ToSparse;

PROCEDURE Singleton(i: INTEGER): IdSet.T =
  BEGIN RETURN IdSet.T{Word.And(i, ModNMask)}
  END Singleton;

PROCEDURE SymClose(VAR ps: T) =
  BEGIN
    FOR i := 0 TO N-1 DO
      VAR psi := ps[i]; BEGIN
        IF psi # IdSet.Empty THEN
          VAR isingle := IdSet.T{i}; BEGIN
            FOR j := 0 TO N-1 DO
              IF j IN psi THEN
                WITH psj = ps[j] DO
                  psj := psj + isingle
                END
              END
            END
          END
        END
      END
    END
  END SymClose;
  
BEGIN 
  <* ASSERT 0 = Word.And(N, N-1) AND N = IdSet.N *>
  (* This implementation works only if "N" is a power of two, and if the widths
     of this type and "IdSet" agree. *)

  EmptySparse := NEW(Sparse, 0)
END PairSet.
