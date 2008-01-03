(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * See file COPYRIGHT-COLUMBIA for details.
 * 
 * Author          : Blair MacIntyre
 * Created On      : Wed May 10 16:39:14 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Oct 23 11:39:28 1997
 * Update Count    : 70
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/WorkerPool.m3,v $
 * $Date: 2001-12-02 00:20:38 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 00:06:45  wagner
 * Blair MacIntyre's events library
 *
 * Revision 1.4  1997/10/24 19:31:36  bm
 * Added the ability to flush the readers and worker pool.
 *
 * Revision 1.3  1997/08/04 20:15:14  bm
 * Fixed BRANDs
 *
 * Revision 1.2  1997/01/23 15:26:44  bm
 * Lots of little bug fixes.
 *
 * 
 * HISTORY
 *)

MODULE WorkerPool;

IMPORT Work, Thread, WorkSeq, ZombieSeq, Scheduler, ThreadF;
(* IMPORT RTTypeSRC, IO; *)

(* IMPORT IO; *)
REVEAL
  Private = Thread.Mutex BRANDED "WorkerPool.Private" OBJECT END;
  T = Public BRANDED "WorkerPool.T" OBJECT 
    maxIdleThreads: INTEGER;
    maxThreads: INTEGER;
    idleCount: INTEGER;
    workCount: INTEGER;
    work: WorkSeq.T;
    cv: Thread.Condition;
    flushCV: Thread.Condition;
    worker: Worker;
    workers: REF ARRAY OF ThreadF.Id;
    zombies: ZombieSeq.T;
    clericCV: Thread.Condition;
    cleric: Thread.T;
  OVERRIDES
    init := Init;
    add := Add;
    flush := Flush;
    finish := Finish;
    stealWorker := StealWorker;
  END;

TYPE
  Worker = Thread.SizedClosure OBJECT
    next: Thread.T := NIL;
    pool: T;
  OVERRIDES
    apply := WorkerApply;
  END;
    
(* The cleric is a cleanup thread, running continually in the
   background and destroying and zombies created in the WorkerPool.
   If an excess of threads are created because of a burst of incoming
   messages, some of them will eventually commit suicide and become
   zombies.  The cleric slowly takes care of them. If it notices some
   zombies, it cleans up one and then yields to any other running
   tasks to minimize it's CPU impact.  *)
TYPE
  ClericClosure = Thread.Closure OBJECT 
    pool: T;
  OVERRIDES
    apply := ClericApply;
  END;

PROCEDURE ClericApply(self: ClericClosure): REFANY =
  VAR
    cleanup: BOOLEAN := FALSE;
  BEGIN
    LOOP
      TRY
        LOCK self.pool DO
          WHILE self.pool.zombies.size() = 0 DO
            (* If we've been told to clean up, there is no more work,
               there are no more zombies, and there are no more
               working threads, then we are finished! *)
            IF cleanup AND self.pool.work.size() = 0 AND
              self.pool.workCount = 0 THEN 
              RETURN NIL;
            END;
(*            IO.Put("    Cleric Wait: cleanup=" & Fmt.Bool(cleanup) & 
              " work=" & Fmt.Int(self.pool.work.size()) & 
              " workers=" & Fmt.Int(self.pool.workCount) &
              " idle=" & Fmt.Int(self.pool.idleCount) & "\n");
*)
            Thread.AlertWait(self.pool, self.pool.clericCV);
          END;
          WITH zombie = self.pool.zombies.remlo() DO
(*            IO.Put("    Gettin' another Zombie. " &
              Fmt.Int(self.pool.zombies.size()) & " left.\n");
*)
            EVAL Thread.AlertJoin(zombie);
          END;
        END;
      EXCEPT
      | Thread.Alerted => cleanup := TRUE;
(*        IO.Put("    Cleric Alerted\n");
*)
      END;
      Scheduler.Yield();
    END;
  END ClericApply; 

PROCEDURE Init(self: T; maxThreads: CARDINAL;
               maxIdleThreads: INTEGER; stackSize: CARDINAL): T = 
  BEGIN
    self.maxThreads := maxThreads;
    IF maxIdleThreads = -1 THEN
      maxIdleThreads := MAX(1, maxThreads DIV 2);
    END;
    self.maxIdleThreads := maxIdleThreads;
    self.workers := NEW(REF ARRAY OF ThreadF.Id, maxThreads);
    FOR i := FIRST(self.workers^) TO LAST(self.workers^) DO
      self.workers[i] := -1;
    END;
    self.idleCount := 0;
    self.workCount := 0;
    self.cv := NEW(Thread.Condition);
    self.flushCV := NEW(Thread.Condition);
    self.work := NEW(WorkSeq.T).init();
    self.worker := NEW(Worker, stackSize := stackSize);
    self.worker.pool := self;
    self.zombies := NEW(ZombieSeq.T).init();
    self.clericCV := NEW(Thread.Condition);
    self.cleric := Thread.Fork(NEW(ClericClosure, pool := self));
    RETURN self;
  END Init;

PROCEDURE Add(self: T; work: Work.T) =
  BEGIN
    LOCK self DO
      (* Enqueue some work. *)
      self.work.addhi(work);
      IF self.idleCount > 0 THEN
        Thread.Broadcast(self.cv);
      ELSIF self.workCount < self.maxThreads THEN
        (*IO.Put("WorkerPool.Add: creating new worker to handle work.\n");*)
        EVAL Thread.Fork(self.worker);
      END;
    END;
  END Add;

PROCEDURE StealWorker(self:T): BOOLEAN =
  VAR id := ThreadF.MyId();
  BEGIN
    LOCK self DO
      FOR i := FIRST(self.workers^) TO LAST(self.workers^) DO
        IF self.workers[i] = id THEN
          <*ASSERT self.workCount > 0 *>
          DEC(self.workCount);
          self.workers[i] := -1;
          IF self.work.size() > 0 THEN
            (*IO.Put("WorkerPool.StealWork: creating new worker.\n");*)
            EVAL Thread.Fork(self.worker);
          END;            
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END StealWorker;

PROCEDURE Finish(self: T) =
  BEGIN
    LOCK self DO
      (* This will ensure that all the threads kill themselves when
         they are done.  Also, wake up idle ones: Time to Die!  *)
      self.maxIdleThreads := 0;
    END;
    Thread.Broadcast(self.cv);

    (* Tell the cleric her earthly toils are done. *)
    Thread.Alert(self.cleric);
    EVAL Thread.Join(self.cleric);

    (* Break the reference loop. *)
    self.worker := NIL;
  END Finish;

PROCEDURE Flush(self: T) RAISES {Thread.Alerted} =
  BEGIN
    LOCK self DO
      (* If there is no work being done, or waiting to be done, return *)
      IF self.workCount = 0 AND self.work.size() = 0 THEN RETURN END;

      (* We don't want to loop, because we only care if the condition
         gets met at some point, no if it's met when we eventually
         wake up *)
      Thread.AlertWait(self, self.flushCV);
    END;
  END Flush;

PROCEDURE WorkerApply(self: Worker): REFANY =
  VAR
    work: Work.T;
    id := -1;
    tid := ThreadF.MyId();
    pool := self.pool;
  BEGIN
    (* We are not quite an idle thread right now.  Might get some work
       shortly, but there is the chance we were created to handle a
       piece of work, and another worker thread returned and grabbed
       it before we ran ... *)
    LOCK pool DO
      FOR i := FIRST(pool.workers^) TO LAST(pool.workers^) DO
        IF pool.workers[i] = -1 THEN
          pool.workers[i] := tid;
          id := i;
          INC(pool.workCount);
          EXIT;
        END;
      END;
    END;
    (* if we did not find a free spot in the pool, quit! *)
    IF id = -1 THEN
      pool.zombies.addhi(Thread.Self());
      Thread.Signal(pool.clericCV);
      (*IO.Put("WorkerPool.WorkerApply: worker not needed.  Bye.\n");*)
      RETURN NIL;
    END;

    TRY
      LOOP
        (* Get some work. *)
        LOCK pool DO
          DEC(pool.workCount);
          INC(pool.idleCount);
          WHILE pool.work.size() = 0 DO
            (* Too many idle hands are the devils tools, so get rid of
               them!   Commit suicide, becoming a zombie. *)
            IF pool.idleCount > pool.maxIdleThreads THEN
(*              IO.Put(" Worker committing suicide (workers=" &
                 Fmt.Int(pool.workCount) & ", idle=" &
                 Fmt.Int(pool.idleCount) & ").\n");
*)
              DEC(pool.idleCount);
              pool.zombies.addhi(Thread.Self());
              Thread.Signal(pool.clericCV);
              (*IO.Put("WorkerPool.WorkerApply: too many idle. Bye.\n");*)
              RETURN NIL;
            END;
            (* if there are threads blocked waiting for work to be
               done, and there is no work in the queue, and there are
               no more workers, wake them all up! *)
            IF pool.workCount = 0 THEN 
              Thread.Broadcast(pool.flushCV);
            END;
            Thread.AlertWait(pool, pool.cv);
          END;
          DEC(pool.idleCount);
          INC(pool.workCount);
          work := pool.work.remlo();

          (* If we were the last idle thread, and there is still work,
             create another thread. *)
          (* Since we changed to a fixed number of threads, we don't
             need this anymore 
          IF pool.idleCount = 0 AND pool.work.size() > 0 THEN
            EVAL Thread.Fork(pool.worker);
          END;
          *)
        END;

        (* Do the work! *)
        work.handle();
        work := NIL;

        (* Now, see if we were removed from the pool while working *)
        IF pool.workers[id] # tid THEN
          (* We've been removed!  See if we can get back in. *)
          LOCK pool DO
            IF pool.workCount + pool.idleCount >= pool.maxThreads THEN
              (* Can't get back in.  Say goodnight *)
              pool.zombies.addhi(Thread.Self());
              Thread.Signal(pool.clericCV);
              (*IO.Put("WorkerPool.WorkerApply: stolen. Killing self.\n");*)
              RETURN NIL;
            END;

            (* Find a spot in the pool *)
            FOR i := FIRST(pool.workers^) TO LAST(pool.workers^) DO
              IF pool.workers[i] = -1 THEN
                pool.workers[i] := tid;
                id := i;
                INC(pool.workCount);
                EXIT;
              END;
            END;
            <*ASSERT id > -1 *>
          END;
        END;
      END;
    EXCEPT
    | Thread.Alerted => DEC(pool.idleCount);
    END;
    self.pool := NIL;
    RETURN NIL;
  END WorkerApply;

BEGIN
END WorkerPool.
