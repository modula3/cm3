GENERIC MODULE ArrayServices (Elem);

REVEAL 
  T = BRANDED Brand OBJECT
    data : REF ARRAY OF Elem.T := NIL;
    base : INTEGER;
  END;

PROCEDURE Get(arr : T; READONLY index : INTEGER) : Elem.T =
  BEGIN
    IF arr.data = NIL THEN RETURN NIL END;
    IF index < arr.base OR index > arr.base + LAST(arr.data^) THEN 
      RETURN NIL 
    END;
    RETURN arr.data[index - arr.base]
  END Get;

PROCEDURE Set(arr : T; READONLY index : INTEGER; READONLY elem : Elem.T) =
  BEGIN
    IF arr.data = NIL THEN
      arr.data := NEW(REF ARRAY OF Elem.T, 1);
      arr.base := index
    ELSIF index < arr.base THEN
      VAR
        newArr := NEW(REF ARRAY OF Elem.T, 
                      arr.base - index + NUMBER(arr.data^));
      BEGIN
        SUBARRAY(newArr^,arr.base-index, NUMBER(arr.data^)) := arr.data^;
        arr.data := newArr;
        arr.base := index
      END
    ELSIF index > arr.base + LAST(arr.data^) THEN
      VAR
        newArr := NEW(REF ARRAY OF Elem.T, index - arr.base + 1);
      BEGIN
        SUBARRAY(newArr^,0,NUMBER(arr.data^)) := arr.data^;
        arr.data := newArr;
      END
    END;
    arr.data[index - arr.base] := elem
  END Set;

BEGIN END ArrayServices.
