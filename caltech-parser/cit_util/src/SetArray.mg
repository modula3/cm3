(* $Id$ *)

GENERIC MODULE SetArray(Elem, ElemSet);

TYPE RAE = REF ARRAY OF Elem.T;
  
REVEAL
  T = Public BRANDED Elem.Brand & " ArraySet" OBJECT
    arr : RAE;
    sz : CARDINAL;
    compare : Comparer := NIL;
    compareR : ComparerR := NIL;
  OVERRIDES
    init := Init;
    fromArray := FromArray;
    copy := Copy;
    member := Member;
    insert := Insert;
    delete := Delete;
    size := Size;
    iterate := Iterate;
  END;

PROCEDURE Init(t : T; 
               sizeHint : CARDINAL := 0; 
               compare : Comparer := NIL;
               compareR : ComparerR := NIL) : T =
  BEGIN
    t.arr := NEW(RAE, sizeHint);
    t.sz := 0;
    
    <* ASSERT compare = NIL OR compareR = NIL *>
    t.compare := compare;
    t.compareR := compareR;
    RETURN t
  END Init;

PROCEDURE FromArray(t : T; READONLY a : ARRAY OF Elem.T) : ElemSet.T =
  BEGIN
    t.arr := NEW(RAE, NUMBER(a));
    t.arr^ := a;
    t.sz := NUMBER(a);
    RETURN t
  END FromArray;

PROCEDURE Copy(t : T) : ElemSet.T =
  BEGIN
    WITH new = NEW(T) DO
      new.arr := NEW(RAE, NUMBER(t.arr^));
      new.arr^ := t.arr^;
      new.sz := t.sz;
      new.compare := t.compare;
      new.compareR := t.compareR;
      RETURN new
    END
  END Copy;

PROCEDURE Member(t : T; e : Elem.T) : BOOLEAN =
  BEGIN
    FOR i := 0 TO t.sz-1 DO
      IF Elem.Equal(e,t.arr[i]) THEN RETURN TRUE END
    END;
    RETURN FALSE
  END Member;

PROCEDURE Delete(t : T; e : Elem.T) : BOOLEAN =
  BEGIN
    FOR i := 0 TO t.sz-1 DO
      IF Elem.Equal(e,t.arr[i]) THEN 
        SUBARRAY(t.arr^,i,NUMBER(t.arr^)-i-1) := 
            SUBARRAY(t.arr^,i+1,NUMBER(t.arr^)-i-1);
        DEC(t.sz);
        RETURN TRUE
      END
    END;
    RETURN FALSE
  END Delete;

PROCEDURE Insert(t : T; e : Elem.T) : BOOLEAN =
  BEGIN
    IF t.member(e) THEN RETURN TRUE END;

    IF t.sz = LAST(t.arr^)+1 THEN
      WITH new = NEW(RAE, NUMBER(t.arr^)*2+1) DO
        SUBARRAY(new^,0,NUMBER(t.arr^)) := t.arr^;
        t.arr := new
      END
    END;
    t.arr[t.sz] := e; INC(t.sz);
    RETURN FALSE
  END Insert;
    
PROCEDURE Size(t : T) : CARDINAL = BEGIN RETURN t.sz END Size;

(**********************************************************************)

REVEAL
  Iterator = ElemSet.Iterator BRANDED Elem.Brand & " ArraySet Iterator" OBJECT
    t : T;
    i := 0;
  OVERRIDES
    next := Next;
  END;

PROCEDURE Iterate(t : T) : ElemSet.Iterator = 
  BEGIN RETURN NEW(Iterator, t := t) END Iterate;

PROCEDURE Next(iter : Iterator; VAR e : Elem.T) : BOOLEAN =
  BEGIN
    IF iter.i >= iter.t.sz THEN RETURN FALSE END;
    TRY
      e := iter.t.arr[iter.i];
      RETURN TRUE
    FINALLY
      INC(iter.i)
    END
  END Next;

BEGIN END SetArray.
  




