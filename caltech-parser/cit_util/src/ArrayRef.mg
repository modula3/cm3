GENERIC MODULE ArrayRef(Elem);

PROCEDURE New(READONLY from: ARRAY OF Elem.T): T =
  VAR
    result := NEW(T, NUMBER(from));
  BEGIN
    result^ := from;
    RETURN result;
  END New;

BEGIN
END ArrayRef.
