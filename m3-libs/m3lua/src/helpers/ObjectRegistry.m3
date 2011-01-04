(* --------------------------------------------------------------------
 * FILE:     ObjectRegistry.m3
 * AUTHOR:   Peter Eiserloh
 * LANUGAGE: Modula-3
 * PURPOSE:  Register traced objects, so can be referenced by an
 *           untraced source.
 * VERSION:  0.0.1 (21-Nov-2010) PPE
 * ----------------------------------------------------------------- *)

UNSAFE MODULE ObjectRegistry;

IMPORT Ctypes;

(* =============================================== *)

TYPE
	EntryPtr = REF Entry;
	Entry = RECORD
		next, prev : EntryPtr;
		id: UNTRACED REF INTEGER;
		obj: ROOT;
		count: INTEGER;
	END;


	List = RECORD
		head, tail: EntryPtr;
	END;

VAR
	gCounter: INTEGER;
	gList: List;
	(* FIXME: gMutex: MUTEX; *)

(* =============================================== *)


PROCEDURE NewEntry(obj: ROOT): EntryPtr =
VAR
	ent: EntryPtr;
BEGIN
	INC(gCounter);
	ent := NEW(EntryPtr);
	ent.id := NEW(UNTRACED REF INTEGER);
	ent.id^ := gCounter;
	ent.obj := obj;
	ent.next := NIL;
	ent.prev := NIL;
	ent.count := 0;
	RETURN ent;
END NewEntry;



PROCEDURE FreeEntry(ent: EntryPtr) =
BEGIN
	DISPOSE(ent.id);
	(* GarbageCollect(ent) *)
END FreeEntry;


PROCEDURE Add(ent: EntryPtr) =
BEGIN
	IF ent = NIL THEN RETURN; END;
	IF gList.head = NIL THEN
		gList.head := ent;
		gList.tail := ent;
		ent.next := NIL;
		ent.prev := NIL;
		RETURN;
	END;

	ent.prev        := gList.tail;
	gList.tail.next := ent;
	gList.tail      := ent;
END Add;


PROCEDURE Remove(ent: EntryPtr) =
BEGIN
	IF ent = gList.head THEN
		gList.head := ent.next;
	END;
	IF ent = gList.tail THEN gList.tail := ent.prev; END;
	IF ent.prev # NIL THEN ent.prev.next := ent.next; END;
	IF ent.next # NIL THEN ent.next.prev := ent.prev; END;
END Remove;



PROCEDURE LookupEntryByObject(obj: ROOT): EntryPtr =
VAR
	ent := gList.head;
BEGIN
	WHILE ent # NIL DO
		IF ent.obj = obj THEN RETURN ent; END;
		ent := ent.next;
	END;
	RETURN NIL;
END LookupEntryByObject;


PROCEDURE LookupEntryById(id: Ctypes.void_star) : EntryPtr =
VAR
	ent := gList.head;
BEGIN
	WHILE ent # NIL DO
		IF ent.id = id THEN RETURN ent; END;
		ent := ent.next;
	END;
	RETURN NIL;
END LookupEntryById;



(* =============================================== *)


PROCEDURE Register(obj: ROOT): Ctypes.void_star =
VAR
	ent : EntryPtr;
BEGIN
	ent := LookupEntryByObject(obj);
	IF ent = NIL THEN
		(* need a new entry *)
		ent := NewEntry(obj);
		Add(ent);
	END;
	INC(ent.count);

	RETURN ent.id;
END Register;


PROCEDURE UnRegister(obj: ROOT) =
VAR
	ent: EntryPtr;
BEGIN
	ent := LookupEntryByObject(obj);
	IF ent = NIL THEN (* ERROR? *) RETURN; END;
	DEC(ent.count);
	IF ent.count = 0 THEN
		Remove(ent);
		FreeEntry(ent);
	END;
END UnRegister;


PROCEDURE LookupObj(obj: ROOT): Ctypes.void_star =
VAR
	ent: EntryPtr;
BEGIN
	ent := LookupEntryByObject(obj);
	IF ent # NIL THEN RETURN ent.id; END;
	RETURN NIL;
END LookupObj;


PROCEDURE LookupId(id: Ctypes.void_star) : ROOT =
VAR
	ent: EntryPtr;
BEGIN
	ent := LookupEntryById(id);
	IF ent # NIL THEN RETURN ent.obj; END;
	RETURN NIL;
END LookupId;


BEGIN
	gCounter := 0;
	gList.head := NIL;
	gList.tail := NIL;
END ObjectRegistry.
