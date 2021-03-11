(* $Id$ *)

MODULE IntSetBits;

TYPE Arr = ARRAY OF BITS 8 FOR BOOLEAN;
REVEAL
  T = Public BRANDED Brand OBJECT
    data : REF Arr;
    base : INTEGER;
    sz : CARDINAL := 0;
  OVERRIDES
    init := Init;
    insert := Insert;
    size := Size;
    delete := Delete;
    member := Member;
  END;

PROCEDURE Size(t : T) : CARDINAL = BEGIN RETURN t.sz END Size;

PROCEDURE Init(t : T; minValHint, maxValHint : INTEGER) : T =
  BEGIN
    IF t.data = NIL THEN t.data := NEW(REF Arr, maxValHint-minValHint) END;
    t.base := minValHint;

    FOR i := FIRST(t.data^) TO LAST(t.data^) DO t.data[i] := FALSE END;
    t.sz := 0;
    RETURN t
  END Init;

PROCEDURE Delete(t : T; what : INTEGER) : BOOLEAN =
  BEGIN
    IF what < t.base OR what > t.base + NUMBER(t.data^) - 1 THEN 
      RETURN FALSE
    ELSE
      WITH d  = t.data[what - t.base] DO
        IF d THEN DEC(t.sz) END;
        TRY
          RETURN d
        FINALLY
          d := FALSE
        END
      END
    END
  END Delete;

PROCEDURE Member(t : T; what : INTEGER) : BOOLEAN =
  BEGIN
    IF what < t.base OR what > t.base + NUMBER(t.data^) - 1 THEN 
      RETURN FALSE
    ELSE
      WITH d  = t.data[what - t.base] DO
        RETURN d
      END
    END
  END Member;

PROCEDURE Insert(t : T; what : INTEGER) : BOOLEAN =
  BEGIN
    (* re-base array if necessary *)
    IF what < t.base THEN
      WITH delta  = MAX(t.base - what,NUMBER(t.data^) DIV 3) ,
           newArr = NEW(REF Arr, delta + NUMBER(t.data^)) DO
        SUBARRAY(newArr^, delta, NUMBER(t.data^)) := t.data^;
        FOR i := 0 TO delta-1 DO newArr[i] := FALSE END;
        t.base := what;
        t.data := newArr
      END
    END;

    (* re-size array if necessary *)
    IF what > t.base + NUMBER(t.data^) - 1 THEN
      WITH oldMax = t.base + NUMBER(t.data^) - 1,
           delta  = MAX(what - oldMax, NUMBER(t.data^) DIV 2), 
           newArr = NEW(REF Arr, delta + NUMBER(t.data^)) DO
        SUBARRAY(newArr^, 0, NUMBER(t.data^)) := t.data^;
        FOR i := 1 TO delta DO newArr[oldMax+i] := FALSE END;
        t.data := newArr
      END
    END;

    WITH d = t.data[what-t.base] DO
      IF NOT d THEN INC(t.sz) END;
      TRY
        RETURN d
      FINALLY
        d := TRUE
      END
    END
  END Insert;

BEGIN END IntSetBits.
