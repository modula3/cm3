UNSAFE MODULE CPtr;
IMPORT Ctypes;
IMPORT WeakRef;
IMPORT CPtrTbl,CPtrRec;
IMPORT Wr, Stdio, Fmt;
IMPORT Word; 
IMPORT Thread; (* exceptions *)

REVEAL 
  T = Public BRANDED "CPtr" OBJECT
    c : Ctypes.void_star;
    destroy : PROCEDURE( x : Ctypes.void_star );
    clientMu : MUTEX;
  OVERRIDES
    get := Get;
  END;

(* the table and its lock *)
VAR
  table : CPtrTbl.T;
  mu := NEW(MUTEX);

(* locking ordering is: LOCK a.clientMu -> LOCK mu *)

(* To allow pointer comparisons to be used here as in C, the *)
(* following invariant holds:                                *)
(* a : T = b : T <-> a.c : void_star = b.c : void_star       *)

PROCEDURE Equal(a, b : T) : BOOLEAN =
  VAR 
    res := a = b;
  BEGIN
    IF a.c = b.c THEN <* ASSERT res *> END;
    RETURN res
  END Equal;

PROCEDURE Hash(self : T) : Word.T =
  BEGIN RETURN LOOPHOLE(self.c,Word.T) END Hash;
 
PROCEDURE Get(self : T) : Ctypes.void_star =
  BEGIN RETURN self.c END Get;

PROCEDURE Wrap(cptr : Ctypes.void_star; 
               destroy : PROCEDURE( x : Ctypes.void_star );
               clientMu : MUTEX;
               VAR hadIt : BOOLEAN
               ) : T =
  VAR
    rec : CPtrRec.T;
    res : T;
  BEGIN
    LOCK mu DO

      (* the possibilities are as follows:
         hadIt FALSE:  No cptr in table.  Make new table entry and insert.
         hadIt TRUE and WeakRef.ToRef(rec.m) non-NIL.  Live entry, return.
         hadIt TRUE and WeakRef.ToRef(rec.m) NIL.  Dead entry, will be 
         Free()'d later. *)
      hadIt := table.get(cptr,rec);
      IF hadIt THEN
        res := WeakRef.ToRef(rec.m);
        IF res # NIL THEN RETURN res END;
      ELSE
        rec := NEW(CPtrRec.T, c := cptr);
      END;

      (* return a new surrogate in either case since we cannot access the *)
      (* old surrogate *)
      res := NEW(T, c := cptr, destroy := destroy, clientMu := clientMu);
      
      rec.m := WeakRef.FromRef(res,Free);

      (* The refCnt is the number of surrogates outstanding for a pointer. *)
      (* We increment the refCnt whenever a surrogate is created. *)
      INC(rec.refCnt);
      EVAL table.put(cptr,rec);
      RETURN res
    END (* LOCK mu *)
  END Wrap;


(* The Free routine gets called whenever a surrogate is reclaimed *)
(* by the garbage collector.  We check to see how many surrogates are *)
(* outstanding for a given pointer; when this reaches zero, we delete *)
(* the data for that pointer and call the user call-back to destroy  *)
(* the underlying object. *)
PROCEDURE Free(<*UNUSED*>READONLY w : WeakRef.T; r : REFANY) =
  VAR
    obj := NARROW(r,T);
    rec : CPtrRec.T;
  BEGIN
    LOCK obj.clientMu DO LOCK mu DO
      EVAL table.get(obj.c,rec); 
      DEC(rec.refCnt);
      IF rec.refCnt = 0 THEN 
        EVAL table.delete(obj.c,rec);
        obj.destroy(obj.c)
      END
    END END (* LOCK *)
  END Free;


PROCEDURE PrintMems() RAISES { Thread.Alerted, Wr.Failure } =
  BEGIN
    Wr.PutText(Stdio.stdout,"members: " & Fmt.Int(table.size()) & "\n");
  END PrintMems;

BEGIN LOCK mu DO table := NEW(CPtrTbl.Default).init() END END CPtr.
