(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Thu Jun 15 07:58:47 PDT 1995 by kalsow   *)
(*      modified on Wed Mar 22 18:16:26 PST 1995 by msm      *)
(*      modified on Thu Oct 14 16:30:42 PDT 1993 by sfreeman *)

UNSAFE MODULE JvsBuffer;

IMPORT Atom, AtomList, IntRefTbl, JVBuffer, Jvs, OSError,
       OSErrorPosix, Thread;

REVEAL
  Factory = FactoryPublic BRANDED OBJECT
              jvs : Jvs.T;
              type: Jvs.BufferType;
            OVERRIDES
              init    := Init;
              make    := Make;
              reset   := Reset;
              destroy := Destroy;
            END;

PROCEDURE Init (f: Factory; jvs: Jvs.T; type: Jvs.BufferType): Factory =
  BEGIN
    f.jvs := jvs;
    f.type := type;
    RETURN f;
  END Init;

PROCEDURE Make (f: Factory; wait := TRUE; subtype: CARDINAL := 0): JVBuffer.T
  RAISES {Thread.Alerted, OSError.E} =
  BEGIN
    IF subtype = 0 THEN subtype := f.subtype END;
    WITH res     = f.newBuf(),
         buffId  = AllocateBuffer(f.jvs, f.type, wait),
         address = BufferAddress(buffId)          DO
      res.subtype := subtype;
      RETURN res.init(buffId, address);
    END;
  END Make;

PROCEDURE Reset (<* UNUSED*> f: Factory; <* UNUSED *> t: JVBuffer.T) =
  BEGIN
    (* dummy procedure *)
  END Reset;

PROCEDURE Destroy (f: Factory; jv: JVBuffer.T) =
  BEGIN
    FreeBuffer(f.type, jv.shmid);
  END Destroy;

(* jvdriver doesn't recognise Deallocate requests, it deallocates shm
   buffers when the client breaks the connection.  So we want to stash shm
   buffers here for reuse until the process dies *)
TYPE
  BuffElt = REF RECORD
                  id  : Jvs.ShmBufId;
                  next: BuffElt        := NIL;
                END;
  RefAddr = REF RECORD address: ADDRESS;  END;

VAR
  mutex     := NEW(MUTEX);
  lists     := ARRAY Jvs.BufferType OF BuffElt{NIL, NIL};
  addresses := NEW(IntRefTbl.Default).init(5);
(* we only add to /addresses/, so we only need to lock writes, not reads.
   We also assume that shmids are not reused during the run of the
   program *)

PROCEDURE AllocateBuffer (jvs: Jvs.T; type: Jvs.BufferType; 
  <* UNUSED *> wait := TRUE): Jvs.ShmBufId
  RAISES {OSError.E, Thread.Alerted} =
  BEGIN
    LOCK mutex DO
      VAR head := lists[type];
      BEGIN
        IF head # NIL THEN lists[type] := head.next; RETURN head.id; END;
      END;
      (* otherwise we need to allocate a new one *)
      VAR
        id   := jvs.allocateBuffer(type);
        addr: ADDRESS := NIL;
      BEGIN
        IF LOOPHOLE(addr, INTEGER) = -1 THEN
          OSErrorPosix.Raise();
        END;
        EVAL addresses.put(id, NEW(RefAddr, address := addr));
        RETURN id;
      END;
    END;
  END AllocateBuffer;

PROCEDURE FreeBuffer (type: Jvs.BufferType; id: Jvs.ShmBufId) =
  BEGIN
    LOCK mutex DO
      WITH new = NEW(BuffElt, id := id, next := lists[type]) DO
        lists[type] := new;
      END;
    END;
  END FreeBuffer;

PROCEDURE BufferAddress (id: Jvs.ShmBufId): ADDRESS RAISES {OSError.E} =
  VAR ref: REFANY;
  BEGIN
    IF NOT addresses.get(id, ref) THEN
      RAISE OSError.E(shmNotAttached);
    END;
    RETURN NARROW(ref, RefAddr).address;
  END BufferAddress;

PROCEDURE Subtype (<* UNUSED *> width, height: CARDINAL): CARDINAL =
  BEGIN
    RETURN 0
  END Subtype;

PROCEDURE Subtype2 (<* UNUSED *> len: CARDINAL): CARDINAL =
  BEGIN
    RETURN 0
  END Subtype2;

BEGIN
  shmNotAttached :=
    NEW(AtomList.T,
        head := Atom.FromText("JvsBuffer.SharedMem segment not attached"));
END JvsBuffer.
