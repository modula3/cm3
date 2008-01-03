(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Wed Mar 22 18:11:53 PST 1995 by msm      *)
(*      modified on Tue Jan 31 10:57:40 PST 1995 by kalsow   *)
(*      modified on Sat Oct 23 18:34:10 PDT 1993 by sfreeman *)

UNSAFE MODULE JVBuffer;

IMPORT Ctypes, OSError, OSErrorPosix, Uipc, Ushm, Thread;

(* -- T -- *)

REVEAL
  T = Public BRANDED OBJECT
        count, users: CARDINAL := 0;
        next        : T        := NIL; (* for linked list *)
        pool        : Pool;
      METHODS
        inc () := Inc;
      OVERRIDES
        init := InitT;
        free := Free;
      END;

PROCEDURE Inc (t: T) =
  BEGIN
    LOCK t DO INC(t.count) END
  END Inc;

PROCEDURE InitT (t: T; shmid: Ctypes.int; address: ADDRESS): T
  RAISES {OSError.E} =
  VAR shmid_ds: Ushm.struct_smem;
  BEGIN
    IF Ushm.shmctl(shmid, Uipc.IPC_STAT, ADR(shmid_ds)) = -1 THEN
      OSErrorPosix.Raise();
    END;

    LOCK t DO
      t.length := shmid_ds.sm_size;
      t.shmid := shmid;
      t.addr := address;
    END;
    RETURN t;
  END InitT;

PROCEDURE Free (t: T) =
  VAR pool := t.pool;
  BEGIN
    IF t.ready # NIL THEN
      TRY t.ready.apply() EXCEPT Thread.Alerted => END;
      t.ready := NIL
    END;
    LOCK pool DO
      LOCK t DO
        <* ASSERT t.count > 0 *>
        DEC(t.count);
        IF t.users > 0 THEN DEC(t.users) END;
        IF t = pool.current AND t.users = 0 AND t.count = 1 THEN
          DEC(t.count);
          pool.current := NIL;
          Thread.Broadcast(pool.changeEvent)
        END;
        IF t.count = 0 THEN Return(pool, t); END;
      END
    END;
  END Free;

(* -- Pool -- *)

REVEAL
  Pool = PoolPublic BRANDED OBJECT
           closed      := FALSE;
           current : T := NIL;   (* most recently inserted image *)
           freeList: T := NIL;
           freeBuffers : CARDINAL   := 0; (* num buffers in free list *)
           totalBuffers: CARDINAL   := 0;
           maxBuffers  : CARDINAL;
           factory     : Factory;
           bufferFree : Thread.Condition;
           changeEvent: Thread.Condition;

           clients    : CARDINAL           := 0;
           clientEvent: Thread.Condition;
         OVERRIDES
           init             := Init;
           setSize          := SetSize;
           getCurrentBuffer := GetCurrentBuffer;
           waitForChange    := WaitForChange;
           getFreeBuffer    := GetFreeBuffer;
           insert           := Insert;
           join             := Join;
           leave            := Leave;
           signalClosed     := SignalClosed;
           clearClosed      := ClearClosed;
         END;

PROCEDURE Init (pool: Pool; factory: Factory; maxBuffers: CARDINAL): Pool =
  BEGIN
    LOCK pool DO
      pool.factory := factory;
      pool.maxBuffers := maxBuffers;
      pool.bufferFree := NEW(Thread.Condition);
      pool.changeEvent := NEW(Thread.Condition);
      pool.clientEvent := NEW(Thread.Condition);
    END;
    RETURN pool;
  END Init;

PROCEDURE SetSize (pool: Pool; maxBuffers: CARDINAL)
  RAISES {Thread.Alerted, OSError.E} =
  VAR broadcast := FALSE;
  BEGIN
    LOCK pool DO
      broadcast := pool.maxBuffers < maxBuffers;
      pool.maxBuffers := maxBuffers;

      (* get rid of excess buffers *)
      WHILE pool.totalBuffers > pool.maxBuffers AND pool.freeBuffers > 0 DO
        DEC(pool.totalBuffers);
        pool.factory.destroy(Pop(pool));
      END;
    END;
    IF broadcast THEN Thread.Broadcast(pool.bufferFree); END;
  END SetSize;

PROCEDURE GetCurrentBuffer (pool: Pool): T =
  BEGIN
    LOCK pool DO
      IF pool.current # NIL THEN pool.current.inc(); END;
      RETURN pool.current;
    END;
  END GetCurrentBuffer;

PROCEDURE WaitForChange (pool: Pool): T RAISES {Thread.Alerted, Closed} =
  VAR oldSerial: Serial;
  BEGIN
    LOCK pool DO
      IF pool.current = NIL THEN
        WHILE NOT pool.closed AND pool.current = NIL DO
          Thread.AlertWait(pool, pool.changeEvent);
        END;
      ELSE
        oldSerial := pool.current.serial;
        WHILE NOT pool.closed
                AND (pool.current = NIL OR oldSerial = pool.current.serial) DO
          Thread.AlertWait(pool, pool.changeEvent);
        END;
      END;
      <* ASSERT pool.closed OR pool.current # NIL *>
      IF Thread.TestAlert() THEN RAISE Thread.Alerted; END;

      IF pool.closed THEN RAISE Closed; END;
      pool.current.inc();
      RETURN pool.current;
    END;
  END WaitForChange;

PROCEDURE GetFreeBuffer (pool: Pool; wait := FALSE; subtype: CARDINAL): T
  RAISES {Thread.Alerted, OSError.E} =
  VAR res: T := NIL;
  BEGIN
    LOOP
      LOCK pool DO
        (* only release buffers if someone is listening *)
        WHILE pool.clients = 0 DO
          IF wait THEN
            Thread.AlertWait(pool, pool.clientEvent);
          ELSE
            RETURN NIL;
          END;
        END;

        WHILE pool.totalBuffers >= pool.maxBuffers AND pool.freeBuffers = 0 DO
          (* cannot create any more buffers *)
          IF wait THEN
            Thread.AlertWait(pool, pool.bufferFree);
          ELSE
            RETURN NIL;
          END;
        END;

        <* ASSERT
             pool.totalBuffers < pool.maxBuffers OR pool.freeBuffers > 0 *>
        IF Thread.TestAlert() THEN RAISE Thread.Alerted; END;

        IF pool.freeBuffers > 0 THEN
          WITH free = Pop(pool) DO free.inc(); RETURN free; END;
        END;
        INC(pool.totalBuffers);
      END;
      VAR ok := FALSE;
      BEGIN
        TRY
          res := pool.factory.make(FALSE, subtype);
          ok := TRUE
        FINALLY
          IF res = NIL OR NOT ok THEN
            LOCK pool DO DEC(pool.totalBuffers) END
          END
        END
      END;
      LOCK pool DO
        IF res # NIL THEN
          res.pool := pool;
          res.inc();
          RETURN res
        ELSIF NOT wait THEN
          RETURN NIL
        END
      END;
      Thread.Pause(0.5D0)
    END
  END GetFreeBuffer;

PROCEDURE Insert (pool: Pool; buffer: T) =
  BEGIN
    LOCK pool DO
      buffer.users := pool.clients;
      IF pool.current # NIL THEN
        WITH curr = pool.current DO
          (* free the previous current, with different locking *)
          LOCK curr DO
            <* ASSERT curr.count > 0 *>
            DEC(curr.count);
            IF curr.count = 0 THEN Return(pool, curr); END;
          END;
        END;
      END;
      pool.current := buffer;
    END;
    Thread.Broadcast(pool.changeEvent);
  END Insert;

PROCEDURE Join (pool: Pool) =
  BEGIN
    LOCK pool DO INC(pool.clients); END;
    IF pool.clients = 1 THEN Thread.Signal(pool.clientEvent); END;
  END Join;

PROCEDURE Leave (pool: Pool) =
  BEGIN
    <* ASSERT pool.clients > 0 *>
    LOCK pool DO DEC(pool.clients); END;
  END Leave;

PROCEDURE SignalClosed (pool: Pool) =
  BEGIN
    LOCK pool DO pool.closed := TRUE; END;
    Thread.Broadcast(pool.changeEvent);
  END SignalClosed;

PROCEDURE ClearClosed (pool: Pool) =
  BEGIN
    LOCK pool DO pool.closed := FALSE; END;
  END ClearClosed;

(* restore this buffer to the free list.  If there are now more buffers
   than maxBuffers, this will be sorted out in GetFreeBuffer.  This avoids
   having to manage extra exception handling all over the place for the
   factory.destroy method *)
(* LL >= pool *)
(* Old Return procedure
PROCEDURE Return (pool: Pool; buffer: T) =
  BEGIN
    buffer.next := pool.freeList;
    pool.freeList := buffer;
    INC(pool.freeBuffers);
    <* ASSERT pool.freeBuffers <= pool.totalBuffers *>

    IF pool.freeBuffers = 1 AND pool.totalBuffers <= pool.maxBuffers THEN
      Thread.Broadcast(pool.bufferFree);
    END;
  END Return;
*)

(* LL >= pool, buffer *)
PROCEDURE Return (pool: Pool; buffer: T) =
  BEGIN
    IF pool.current = buffer THEN pool.current := NIL END;
    DEC(pool.totalBuffers);
    TRY
      pool.factory.destroy(buffer);
    EXCEPT OSError.E =>
    | Thread.Alerted => Thread.Alert(Thread.Self())
    END;
    Thread.Broadcast(pool.bufferFree);
  END Return;

(* get the top buffer from the free list *)
(* LL >= pool *)
PROCEDURE Pop (pool: Pool): T =
  VAR res: T;
  BEGIN
    <* ASSERT pool.freeList # NIL AND pool.freeBuffers > 0 *>
    res := pool.freeList;
    pool.freeList := res.next;
    res.next := NIL;
    DEC(pool.freeBuffers);
    RETURN res;
  END Pop;

BEGIN
END JVBuffer.
