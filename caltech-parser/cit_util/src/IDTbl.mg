GENERIC MODULE IDTbl(Elem);
IMPORT Integer AS Key;
IMPORT IDGen;

TYPE
  Value_T = Elem.T;

REVEAL
  T = Public BRANDED OBJECT
    idGen: IDGen.T := NIL;

  OVERRIDES
    setIDGen := SetIDGen;
    autoPut  := AutoPut;
    put      := Put;
    delete   := Delete;
  END;

PROCEDURE SetIDGen(self: T; idGen: IDGen.T) =
  BEGIN
    self.idGen := idGen;
  END SetIDGen;

PROCEDURE Put(self: T; READONLY k: Key.T; READONLY v: Value_T): BOOLEAN =
  BEGIN
    Init(self);
    RETURN Public.put(self, self.idGen.alloc(k), v);
  END Put;

PROCEDURE AutoPut(self: T; READONLY value: Elem.T): INTEGER =
  BEGIN
    Init(self);
    WITH key = self.idGen.alloc() DO
      EVAL self.put(key, value);
      (* it is important to use "self.put",
         which may be overridden by a subclass.
         Do not use "Public.put" here. *)
      RETURN key;
    END;
  END AutoPut;

PROCEDURE Delete(self: T; READONLY k: Key.T; VAR v: Value_T): BOOLEAN =
  BEGIN
    Init(self);
    IF Public.delete(self, k, v) THEN
      self.idGen.free(k);
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END Delete;

PROCEDURE Init(self: T) =
  BEGIN
    IF self.idGen = NIL THEN
      self.idGen := NEW(IDGen.Low).init();
    END;
  END Init;

BEGIN
END IDTbl.
