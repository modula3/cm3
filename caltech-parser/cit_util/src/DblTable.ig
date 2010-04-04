GENERIC INTERFACE DblTable(Key, Value, KeyValueTbl, ValueKeySetTbl, KeySet);
IMPORT Word;

(* A DblTable.T is a doubly-searchable table. *)
(* It has a many<->one mapping from Key.T's to Value.T's, just like a *)
(* normal Table.  However, a DblTable allows the table to be queried from *)
(* the Value side, rather than just from the Key side. *)
(* Key, Value, and KeyValueTbl go together as in the old Table.ig *)

TYPE
  T = KeyValueTbl.Default OBJECT METHODS
    (* return the set of Key.T's that key the Value.T v *)
    getKeys(READONLY v : Value.T; VAR s : KeySet.T) : BOOLEAN;

    (* return an Iterator for iteration thru the (value, keySet) pairs *)
    iterateVals() : ValueKeySetTbl.Iterator;
  END;

  Iterator = KeyValueTbl.Iterator;

  Default <: T OBJECT METHODS
    valueEqual(READONLY v1, v2: Value.T) : BOOLEAN;
    valueHash(READONLY v : Value.T) : Word.T
  END;
  
CONST DefaultBrand = "DblTable (" & Key.Brand & "," & Value.Brand & ")";

END DblTable.
