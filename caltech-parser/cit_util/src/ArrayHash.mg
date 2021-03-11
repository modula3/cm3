GENERIC MODULE ArrayHash(Elem);
IMPORT Word;

PROCEDURE Hash(a: T): Word.T =
  VAR
    x := 0;
  BEGIN
    FOR i := 0 TO LAST(a^) DO
      x := 23*x + Elem.Hash(a[i]);
    END;
    RETURN x;
  END Hash;

PROCEDURE Equal(a,b: T): BOOLEAN =
  BEGIN
    IF NUMBER(a^) # NUMBER(b^) THEN RETURN FALSE; END;
    FOR i := FIRST(a^) TO LAST(a^) DO
      IF NOT Elem.Equal(a[i], b[i]) THEN RETURN FALSE; END;
    END;
    RETURN TRUE;
  END Equal;

BEGIN
END ArrayHash.
