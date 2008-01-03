(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Wed Mar 22 18:18:59 PST 1995 by msm      *)
(*      modified on Tue Jan 31 10:57:52 PST 1995 by kalsow   *)
(*      modified on Thu Oct 14 16:30:42 PDT 1993 by sfreeman *)

UNSAFE MODULE JvsBuffer;

IMPORT Atom, AtomList, IntRefTbl, JVBuffer, Jvs, OSError,
       OSErrorPosix, Thread, Ushm;

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

PROCEDURE Make (f: Factory; wait := TRUE; subtype: CARDINAL := 0):
  JVBuffer.T RAISES {Thread.Alerted, OSError.E} =
  BEGIN
    IF subtype = 0 THEN subtype := f.subtype END;
    WITH res    = f.newBuf(),
         buffId = AllocateBuffer(f.jvs, f.type, wait, subtype) DO
      res.subtype := subtype;
      IF buffId = 0 THEN
        RETURN NIL
      ELSE
        WITH address = BufferAddress(buffId) DO
          RETURN res.init(buffId, address);
        END
      END
    END
  END Make;

PROCEDURE Reset (<* UNUSED*> f: Factory; <* UNUSED *> t: JVBuffer.T) =
  BEGIN
    (* dummy procedure *)
  END Reset;

PROCEDURE Destroy (f: Factory; jv: JVBuffer.T) =
  BEGIN
    FreeBuffer(f.type, jv.shmid, jv.subtype);
  END Destroy;

(* jvdriver doesn't recognise Deallocate requests, it deallocates shm
   buffers when the client breaks the connection.  So we want to stash shm
   buffers here for reuse until the process dies *)
TYPE
  BuffElt = REF RECORD
                  id     : Jvs.ShmBufId;
                  subtype: CARDINAL;
                  next   : BuffElt        := NIL;
                END;
  RefAddr = REF RECORD address: ADDRESS;  END;

VAR
  mutex                        := NEW(MUTEX);
  cond                         := NEW(Thread.Condition);
  noMem                        := FALSE;
  lists                        := ARRAY Jvs.BufferType OF BuffElt{NIL, ..};
  pacer, water, lowater        := ARRAY Jvs.BufferType OF INTEGER{0, ..};
  thread               : Thread.T;
  jvsglobal            : Jvs.T := NIL;
  addresses                    := NEW(IntRefTbl.Default).init(5);

PROCEDURE NoMemApply (<* UNUSED *> t: Thread.Closure): REFANY =
  VAR
    cnt             := 0;
    pacers: BOOLEAN;
    waters: INTEGER;
    los             := ARRAY Jvs.BufferType OF BuffElt{NIL, ..};
    p     : BuffElt;
    j, k  : INTEGER;
    junk  : REFANY;
    addr  : RefAddr;
  BEGIN
    LOOP
      Thread.Pause(5.0D0);
      LOCK mutex DO
        pacers := noMem;
        waters := 0;
        FOR i := FIRST(Jvs.BufferType) TO LAST(Jvs.BufferType) DO
          pacers := pacers OR pacer[i] > 0;
          pacer[i] := 0;
          INC(waters, water[i]);
          j := lowater[i];
          IF j > 0 THEN
            j := MIN((j + 1) DIV 2, Pace);
            DEC(water[i], j);
            k := 0;
            p := lists[i];
            WHILE p # NIL DO INC(k); p := p.next END;
            k := k - j;
            IF k = 0 THEN
              los[i] := lists[i];
              lists[i] := NIL
            ELSE
              p := lists[i];
              WHILE k > 1 DO p := p.next; DEC(k) END;
              los[i] := p.next;
              p.next := NIL
            END
          ELSE
            los[i] := NIL
          END;
          lowater[i] := water[i]
        END;
        IF noMem THEN noMem := FALSE; Thread.Broadcast(cond) END;
      END;
      IF waters > 0 OR pacers THEN cnt := 0 ELSE INC(cnt) END;
      IF cnt > 30 THEN LOCK mutex DO thread := NIL; RETURN NIL END END;
      FOR i := FIRST(Jvs.BufferType) TO LAST(Jvs.BufferType) DO
        p := los[i];
        WHILE p # NIL DO
          LOCK mutex DO
            IF jvsglobal # NIL THEN
              TRY
                jvsglobal.deallocateBuffer(p.id)
              EXCEPT
                Thread.Alerted, OSError.E =>
              END;
              IF addresses.delete(p.id, junk) THEN
                addr := junk;
                EVAL Ushm.shmdt(addr.address);
              END
            END;
            Thread.Broadcast(cond)
          END;
          p := p.next
        END
      END
    END;
  END NoMemApply;

CONST Pace = 10;

CONST
  Width = ARRAY [0 .. 11] OF
            CARDINAL{
            0, 1024 + 12, 768 + 12, 640 + 12, 512 + 12, 384 + 12, 320 + 12,
            256 + 12, 192 + 12, 160 + 12, 128 + 12, 64 + 12};
  Height = ARRAY [0 .. 11] OF
             CARDINAL{
             0, 800, 576, 480, 400, 288, 240, 200, 144, 120, 100, 50};
  Length = ARRAY [0 .. 5] OF
             CARDINAL{
             0, 192 * 1024, 64 * 1024, 32 * 1024, 16 * 1024, 8 * 1024};

PROCEDURE Subtype (width, height: CARDINAL): CARDINAL =
  BEGIN
    FOR i := LAST(Width) TO 1 BY -1 DO
      IF width + 12 <= Width[i] AND height <= Height[i] THEN RETURN i END
    END;
    RETURN 0
  END Subtype;

PROCEDURE Subtype2 (len: CARDINAL): CARDINAL =
  BEGIN
    FOR i := LAST(Length) TO 1 BY -1 DO
      IF len <= Length[i] THEN RETURN i END
    END;
    RETURN 0
  END Subtype2;

PROCEDURE AllocateBuffer (jvs    : Jvs.T;
                          type   : Jvs.BufferType;
                          wait                      := TRUE;
                          subtype: CARDINAL         := 0     ):
  Jvs.ShmBufId RAISES {OSError.E, Thread.Alerted} =
  BEGIN
    LOCK mutex DO
      jvsglobal := jvs;
      LOOP
        VAR head, ptr := lists[type];
        BEGIN
          IF thread = NIL THEN
            thread := Thread.Fork(NEW(Thread.Closure, apply := NoMemApply))
          END;
          WHILE head # NIL AND head.subtype # subtype DO
            head := head.next
          END;
          IF head # NIL THEN
            IF head = ptr THEN
              lists[type] := head.next
            ELSE
              WHILE ptr.next # head DO ptr := ptr.next END;
              ptr.next := head.next
            END;
            DEC(water[type]);
            lowater[type] := MIN(lowater[type], water[type]);
            RETURN head.id;
          END;
        END;
        IF noMem THEN
          IF wait THEN Thread.AlertWait(mutex, cond) ELSE RETURN 0 END
        ELSE
          (* otherwise we need to allocate a new one *)
          VAR id := 0;
          BEGIN
            IF pacer[type] < Pace THEN
              IF type = Jvs.BufferType.Compress THEN
                id := jvs.allocateBuffer(type, Length[subtype], 0)
              ELSE
                id :=
                  jvs.allocateBuffer(type, Width[subtype], Height[subtype])
              END
            END;
            IF id = 0 THEN
              noMem := TRUE;
            ELSE
              VAR addr := LOOPHOLE(Ushm.shmat(id, NIL, 0), ADDRESS);
              BEGIN
                IF LOOPHOLE(addr, INTEGER) = -1 THEN
                  OSErrorPosix.Raise();
                END;
                INC(pacer[type]);
                EVAL addresses.put(id, NEW(RefAddr, address := addr));
                RETURN id;
              END;
            END
          END
        END
      END
    END;
  END AllocateBuffer;

PROCEDURE FreeBuffer (type   : Jvs.BufferType;
                      id     : Jvs.ShmBufId;
                      subtype: CARDINAL        ) =
  BEGIN
    LOCK mutex DO
      WITH new = NEW(BuffElt, subtype := subtype, id := id,
                     next := lists[type]) DO
        lists[type] := new;
        INC(water[type]);
        Thread.Broadcast(cond)
      END;
    END;
  END FreeBuffer;

PROCEDURE BufferAddress (id: Jvs.ShmBufId): ADDRESS RAISES {OSError.E} =
  VAR ref: REFANY;
  BEGIN
    LOCK mutex DO
      IF NOT addresses.get(id, ref) THEN
        RAISE OSError.E(shmNotAttached);
      END;
      RETURN NARROW(ref, RefAddr).address;
    END
  END BufferAddress;

BEGIN
  shmNotAttached :=
    NEW(AtomList.T,
        head := Atom.FromText("JvsBuffer.SharedMem segment not attached"));
END JvsBuffer.
