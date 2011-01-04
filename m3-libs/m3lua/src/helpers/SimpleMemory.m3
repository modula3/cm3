(* --------------------------------------------------------------------
 * FILE:     SimpleMemory.m3
 * AUTHOR:   Peter Eiserloh
 * LANUGAGE: Modula-3
 * PURPOSE:  Simple memory allocation
 * VERSION:  0.0.1 (12-Dec-2010) PPE
 * ----------------------------------------------------------------- *)

UNSAFE MODULE SimpleMemory;

IMPORT Ctypes, Cstddef, RTProcess;

TYPE
	MemEntry = RECORD
		next, prev: REF MemEntry;
		ptr: Ctypes.void_star;
		size: Cstddef.size_t;
	END;

VAR
	(* gMutex: MUTEX *)
	gHead, gTail : REF MemEntry;


(* ================================================================ *)

PROCEDURE Add(entry: REF MemEntry) =
BEGIN
	(* LOCK(gMutex) *)
	IF gTail = NIL THEN
		entry.next := NIL;
		entry.prev := NIL;
		gHead := entry;
		gTail := entry;
	ELSE
		entry.next := NIL;
		entry.prev := gTail;
		gTail.next := entry;
		gTail := entry;
	END;
	(* UNLOCK(gMutex) *)
END Add;


PROCEDURE Remove(entry: REF MemEntry) =
BEGIN
	(* LOCK(gMutex) *)
	IF entry.prev # NIL THEN entry.prev.next := entry.next; END;
	IF entry.next # NIL THEN entry.next.prev := entry.prev; END;
	IF gHead = entry THEN gHead := entry.next; END;
	IF gTail = entry THEN gTail := entry.prev; END;
	(* UNLOCK(gMutex) *)
END Remove;


PROCEDURE Find(ptr: Ctypes.void_star): REF MemEntry =
VAR
	entry, found : REF MemEntry := NIL;
BEGIN
	(* LOCK(gMutex) *)
	entry := gHead;
	WHILE (entry # NIL) AND (found = NIL) DO
		IF entry.ptr = ptr THEN found := entry; END;
		entry := entry.next;
	END;
	(* UNLOCK(gMutex) *)
	RETURN found;
END Find;


PROCEDURE FreeEntry(entry: REF MemEntry) =
VAR
	ptr : REF ARRAY OF CHAR;
BEGIN
	Remove(entry);
	ptr := LOOPHOLE(entry.ptr, REF ARRAY OF CHAR);
	DISPOSE(ptr);
END FreeEntry;


(* ================================================================ *)

(**
 * Alloc - Allocates a region of non-garbage collected (untraced) 
 *         heap memory, of the specified size.
 **)
PROCEDURE Alloc(size: Cstddef.size_t): Ctypes.void_star =
VAR
	entry := NEW(REF MemEntry);
BEGIN
	entry.ptr := NEW(UNTRACED REF ARRAY OF CHAR, size);
	entry.size := size;
	Add(entry);
	RETURN entry.ptr;
END Alloc;


(**
 * Alloc - Allocates a region of non-garbage collected (untraced) 
 *         heap memory, of the specified size.
 **)
PROCEDURE Free(ptr: Ctypes.void_star) = 
VAR
	entry := NEW(REF MemEntry);
BEGIN
	entry := Find(ptr);
	IF entry # NIL THEN
		FreeEntry(entry);
	END;
END Free;


(**
 * Cleanup - when the program exits, this routine should be called
 * to ensure all memory has been freed.  Many operating systems
 * such as Unix and Linux will not need this, but some will.
 * NOTE: gHead is volitile, it is modified by FreeEntry().
 **)
PROCEDURE Cleanup() =
BEGIN
	WHILE gHead # NIL DO
		FreeEntry(gHead);
	END;
END Cleanup;


BEGIN
	gHead := NIL;
	gTail := NIL;
	RTProcess.RegisterExitor(Cleanup);
END SimpleMemory.
