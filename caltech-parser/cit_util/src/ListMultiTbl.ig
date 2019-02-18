GENERIC INTERFACE ListMultiTbl(Key, Value, ValueList, KeyValueListTbl);

(* A multimap. Each "key" is associated to a list of "elem"s.
   Initially, each "key" is associated to the empty list.
   Thus "get" does not need to return a boolean presence status.
   
   Assumes the same things about the "Key" and "Value" interfaces
   as "Tbl.ig" does.

   "put" extends the list associated with a "key".
   It does not check to see if "elem" is already in the list.
   (use "SetMultiTbl.ig" if you want set insertion and deletion).
*)
   
TYPE
  T = OBJECT METHODS
    get(READONLY k: Key.T): ValueList.T;
    put(READONLY k: Key.T; READONLY v: Value.T);
    iterate(): Iterator;
  END;
  Iterator = KeyValueListTbl.Iterator;
  Default <: T OBJECT METHODS
    init(sizeHint: CARDINAL := 0): Default;
    getRep(): KeyValueListTbl.T;
  END;

CONST
  Brand = "ListTbl(" & Key.Brand & "," & Value.Brand & ")";

END ListMultiTbl.
