(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Mon Nov  8 12:34:27 PST 1993 by detlefs *)
(*      modified on Wed Nov  3 15:02:46 PST 1993 by mcjones *)
(*      modified on Sun Jan 19 18:07:07 PST 1992 by gnelson *)

GENERIC MODULE TableList(Key, Value, KeyValueTable);

(* Where "Key.T" and "Value.T" are types that are not open array types
   and "Key" contains

| PROCEDURE Equal(k1, k2: Key.T): BOOLEAN;

   and "KeyValueTableList" is the instantiation of the generic interface
   "TableList" with the interfaces "Key" and "Value".

   "Equal" must be an equivalence relation.

   "Equal" may be declared with a parameter mode of either "VALUE" or
   "READONLY", but not "VAR".
*)

IMPORT RefList;

REVEAL
  T = KeyValueTable.T OBJECT
   METHODS
    init(): T;
    keyEqual(READONLY k1, k2: Key.T): BOOLEAN
  END BRANDED OBJECT
    list: RefList.T; (* An association list. *)
    sz: CARDINAL;
   OVERRIDES
    get := Get;
    put := Put;
    delete := Delete;
    size := Size;
    iterate := Iterate;
    init := Init;
    keyEqual := KeyEqual;
  END (* OBJECT *);

TYPE
  ListIter = KeyValueTable.Iterator OBJECT 
    list: RefList.T;
   OVERRIDES
    next := Next;
  END (* OBJECT *);

PROCEDURE Init(tbl: T): T =
  BEGIN tbl.list := NIL; tbl.sz := 0; RETURN tbl END Init;

PROCEDURE Get(tbl: T; READONLY key: Key.T; VAR val: Value.T): BOOLEAN =
  VAR l := tbl.list; BEGIN
    WHILE l # NIL DO
      VAR pair: RefList.T := l.head; BEGIN
        IF Key.Equal(pair.head, key) THEN
          val := pair.tail.head;
          RETURN TRUE
        END (* IF *)
      END (* BEGIN *);
      l := l.tail
    END (* WHILE *);
    RETURN FALSE
  END Get;

PROCEDURE Put(tbl: T; READONLY key: Key.T; READONLY val: Value.T): BOOLEAN =
  VAR l := tbl.list; BEGIN
    WHILE l # NIL DO
      VAR pair: RefList.T := l.head; BEGIN
        IF Key.Equal(pair.head, key) THEN
          pair.tail.head := val;
          RETURN TRUE
        END (* IF *)
      END;
      l := l.tail
    END (* WHILE *);
    tbl.list := RefList.Cons(RefList.List2(key, val), tbl.list);
    INC(tbl.sz);
    RETURN FALSE
  END Put;
  
PROCEDURE Delete(tbl: T; READONLY key: Key.T; VAR val: Value.T): BOOLEAN =
  VAR l := tbl.list; prev: RefList.T := NIL; BEGIN
    WHILE l # NIL DO
      VAR pair: RefList.T := l.head; BEGIN
        IF Key.Equal(pair.head, key) THEN
          val := pair.tail.head;
          IF prev = NIL THEN
	    tbl.list := l.tail
	  ELSE
	    prev.tail := l.tail
          END (* IF *);
          DEC(tbl.sz);
          RETURN TRUE
        END (* IF *)
      END (* BEGIN *);
      prev := l;
      l := l.tail
    END (* WHILE *);
    RETURN FALSE
  END Delete;

PROCEDURE Size(tbl: T): CARDINAL =
  BEGIN RETURN tbl.sz END Size;

PROCEDURE Iterate(tbl: T): KeyValueTable.Iterator =
  BEGIN RETURN NEW(ListIter, list := tbl.list)
  END Iterate;

PROCEDURE KeyEqual(<*UNUSED*> tbl: T; READONLY k1, k2: Key.T): BOOLEAN =
  BEGIN RETURN Key.Equal(k1, k2) END KeyEqual;

(********************)
(* Iterator methods *)
(********************)

PROCEDURE Next(i: ListIter; VAR key: Key.T; VAR val: Value.T): BOOLEAN =
  BEGIN
    IF i.list = NIL THEN
      RETURN FALSE
    ELSE
      VAR pair: RefList.T := i.list.head; BEGIN
        key := pair.head;
        val := pair.tail.head
      END (* BEGIN *);
      i.list := i.list.tail;
      RETURN TRUE
    END (* IF *)
  END Next;

BEGIN
END TableList.
