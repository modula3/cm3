MODULE ProcessInOut;

IMPORT TextSeq;

PROCEDURE TextSeqToArr (seq: TextSeq.T; ): REF ARRAY OF TEXT =
  VAR arr := NEW(REF ARRAY OF TEXT, seq.size());
  BEGIN
    FOR i := FIRST(arr^) TO LAST(arr^) DO arr[i] := seq.get(i); END;
    RETURN arr;
  END TextSeqToArr;

BEGIN
END ProcessInOut.
