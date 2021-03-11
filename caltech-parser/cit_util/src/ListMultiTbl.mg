GENERIC MODULE ListMultiTbl(Key, Value, ValueList, KeyValueListTbl);

TYPE
  PublicDefault = T OBJECT METHODS
    init(sizeHint: CARDINAL := 0): Default;
    getRep(): KeyValueListTbl.T;
  END;
REVEAL
  Default = PublicDefault BRANDED Brand OBJECT
    rep: KeyValueListTbl.T;
  OVERRIDES
    init   := Init;
    getRep := GetRep;
    get    := Get;
    put    := Put;
    iterate:= Iterate;
  END;

PROCEDURE Init(self: Default; sizeHint: CARDINAL := 0): Default =
  BEGIN
    self.rep := NEW(KeyValueListTbl.Default).init(sizeHint);
    RETURN self;
  END Init;

PROCEDURE GetRep(self: Default): KeyValueListTbl.T =
  BEGIN
    RETURN self.rep;
  END GetRep; 

PROCEDURE Get(self: Default; READONLY k: Key.T): ValueList.T =
  VAR
    result: ValueList.T := NIL;
  BEGIN
    EVAL self.rep.get(k, result);
    RETURN result;
  END Get;

PROCEDURE Put(self: Default; READONLY k: Key.T; READONLY v: Value.T) =
  BEGIN
    EVAL self.rep.put(k, ValueList.Cons(v, self.get(k)));
  END Put;

PROCEDURE Iterate(self: Default): Iterator =
  BEGIN
    RETURN self.rep.iterate();
  END Iterate;

BEGIN
END ListMultiTbl.
