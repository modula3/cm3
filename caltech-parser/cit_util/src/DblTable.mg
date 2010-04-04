GENERIC MODULE DblTable(Key, Value, KeyValueTbl, ValueKeySetTbl, KeySet);
IMPORT Word;

TYPE
  Public = T OBJECT METHODS
    valueEqual(READONLY v1, v2: Value.T) : BOOLEAN;
    valueHash(READONLY v : Value.T) : Word.T
  END;

  RevTbl = ValueKeySetTbl.Default OBJECT 
    fwdTbl : Default;
  OVERRIDES
    keyEqual := CallValueEqual;
    keyHash := CallValueHash;
  END;

REVEAL
  Default = Public BRANDED DefaultBrand OBJECT 
    revTbl : RevTbl;
  OVERRIDES
    init := Init;
    put := Put;
    delete := Delete;
    getKeys := GetKeys;
    valueEqual := ValueEqual;
    valueHash := ValueHash;
    iterateVals := IterateVals;
  END;
    
PROCEDURE IterateVals(self : Default) : ValueKeySetTbl.Iterator =
  BEGIN RETURN self.revTbl.iterate() END IterateVals;

PROCEDURE GetKeys(self : Default; READONLY v : Value.T; VAR s : KeySet.T) : BOOLEAN =
  BEGIN RETURN self.revTbl.get(v,s) END GetKeys;

PROCEDURE ValueEqual(<*UNUSED*>self : Default; READONLY v1, v2: Value.T) : BOOLEAN =
  BEGIN RETURN Value.Equal(v1, v2) END ValueEqual;

PROCEDURE ValueHash(<*UNUSED*>self : Default; READONLY v : Value.T) : Word.T =
  BEGIN RETURN Value.Hash(v) END ValueHash;

PROCEDURE CallValueEqual(obj : RevTbl;
                         READONLY v1, v2 : Value.T) : BOOLEAN =
  BEGIN RETURN obj.fwdTbl.valueEqual(v1, v2) END CallValueEqual;

PROCEDURE CallValueHash(obj : RevTbl; READONLY v : Value.T) : Word.T =
  BEGIN RETURN obj.fwdTbl.valueHash(v) END CallValueHash;

PROCEDURE Init(self : Default; sizeHint : CARDINAL) : KeyValueTbl.Default =
  BEGIN
    self.revTbl := NEW(RevTbl, fwdTbl := self).init(sizeHint);
    RETURN KeyValueTbl.Default.init(self,sizeHint)
  END Init;

PROCEDURE Put(self : Default; READONLY k : Key.T; READONLY v : Value.T) : BOOLEAN =
  VAR
    keySet : KeySet.T;
  BEGIN
    IF NOT self.revTbl.get(v,keySet) THEN keySet := NEW(KeySet.T).init() END;
    EVAL keySet.insert(k);
    EVAL self.revTbl.put(v, keySet);
    RETURN KeyValueTbl.Default.put(self, k, v)
  END Put;

PROCEDURE Delete(self : Default; READONLY k: Key.T; VAR v: Value.T): BOOLEAN =
  VAR 
    res := KeyValueTbl.Default.delete(self, k, v);
    keySet : KeySet.T;
  BEGIN
    IF res THEN
      EVAL self.revTbl.get(v, keySet);
      EVAL keySet.delete(k)
    END;
    RETURN res
  END Delete;

BEGIN END DblTable.
