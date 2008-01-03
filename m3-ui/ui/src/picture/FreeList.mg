(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Wed Oct 13 17:36:52 PDT 1993 by sfreeman *)

GENERIC MODULE FreeList(Elem);

(* where Elem is an Image.T and so has a field

| next: Elem.T := NIL;

   and a method.

|  destroy();

| New (): Elem.T;

   returns an Elem.T object, possibly from the free list.  Elem.Ts from
   this procedure will be put onto the free list and destroy() will be
   called.

| Free(t: Elem.T)

   explicitly calls t.destroy() and puts it on the free list *)

IMPORT WeakRef, PictureRep;

VAR
  freeMu         := NEW(MUTEX);
  free  : Elem.T := NIL;

PROCEDURE New (): Elem.T =
  VAR res: Elem.T := NIL;
  BEGIN
    LOCK freeMu DO
      IF free # NIL THEN res := free; free := res.next; END;
    END;
    IF res = NIL THEN res := NEW(T); END;
    EVAL WeakRef.FromRef(res, CleanUp);
    RETURN res;
  END New;

PROCEDURE CleanUp (<*UNUSED*> READONLY w: WeakRef.T; r: REFANY) =
  BEGIN
    Free(NARROW(r, Elem.T));
  END CleanUp;

PROCEDURE Free (t: Elem.T) =
  BEGIN
    t.destroy();
    LOCK freeMu DO t.next := free; free := t; END;
  END Free;

BEGIN
END FreeList.
