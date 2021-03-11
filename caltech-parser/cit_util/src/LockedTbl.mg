(* $Id$ *)

GENERIC MODULE LockedTbl(Key, Value, KeyValueTbl);

REVEAL
  Default = PubDefault BRANDED Brand OBJECT
    mu : MUTEX;
  OVERRIDES
    init := Init;
    get := Get;
    put := Put;
    delete := Delete;
    size := Size;
    iterate := Iterate;
    copy := Copy;
  END;

PROCEDURE Init(d : Default; sizeHint : CARDINAL) : KeyValueTbl.Default =
  BEGIN
    d := KeyValueTbl.Default.init(d,sizeHint);
    d.mu := NEW(MUTEX);
    RETURN d
  END Init;

PROCEDURE Get(d : Default; READONLY k: Key.T; VAR v: Value.T): BOOLEAN =
  BEGIN
    LOCK d.mu DO RETURN KeyValueTbl.Default.get(d,k,v) END
  END Get;

PROCEDURE Put(d : Default; READONLY k: Key.T; READONLY v: Value.T): BOOLEAN =
  BEGIN
    LOCK d.mu  DO RETURN KeyValueTbl.Default.put(d,k,v) END
  END Put;

PROCEDURE Delete(d : Default; READONLY k: Key.T; VAR v: Value.T): BOOLEAN =
  BEGIN
    LOCK d.mu DO RETURN KeyValueTbl.Default.delete(d,k,v) END
  END Delete;

PROCEDURE Size(d : Default): CARDINAL =
  BEGIN
    LOCK d.mu DO RETURN KeyValueTbl.Default.size(d) END
  END Size;

PROCEDURE Iterate(d : Default): KeyValueTbl.Iterator =
  BEGIN
    RETURN d.copy().iterate()
  END Iterate;

PROCEDURE Copy(d : Default) : KeyValueTbl.Default =
  VAR k : Key.T; v : Value.T;
  BEGIN
    WITH res = NEW(KeyValueTbl.Default).init(),
         iter = KeyValueTbl.Default.iterate(d) DO
      WHILE iter.next(k,v) DO EVAL res.put(k,v) END;
      RETURN res
    END
  END Copy;

BEGIN END LockedTbl.
