(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * 
 * Author          : Blair MacIntyre
 * Created On      : Mon May 15 17:26:50 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Aug  4 12:30:01 1997
 * Update Count    : 51
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/RdWrMutex.m3,v $
 * $Date: 2001-12-02 00:06:45 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.6  1997/08/04 20:15:14  bm
 * Fixed BRANDs
 *
 * Revision 1.5  1997/01/23 15:26:41  bm
 * Lots of little bug fixes.
 *
 * Revision 1.4  1996/11/21 22:51:32  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

MODULE RdWrMutex;

IMPORT Thread, ThreadF, IntList, IntListFuncs, Process, Fmt, IO;
IMPORT ParseParams, Stdio;

VAR
  debug: BOOLEAN := FALSE;

REVEAL 
  T = Public BRANDED "RdWrMutex.T" OBJECT
    count: INTEGER;
    cv: Thread.Condition;
    mu: Thread.Mutex;
    id: INTEGER := -1;
    readers: IntList.T := NIL;
  OVERRIDES
    init := Init;
    acquireRead := AcquireRead;
    acquireWrite := AcquireWrite;
    releaseRead := ReleaseRead;
    releaseWrite := ReleaseWrite;
    wait := Wait;
  END;

PROCEDURE RemoveReader(self: T) =
  BEGIN
    WITH num = IntList.Length(self.readers) DO
      IF IntListFuncs.DeleteD(self.readers, ThreadF.MyId()) = NIL THEN
        Process.Crash("RdWrMutex: releaseRead called by non-locker.\n" &
          "Releasing Reader with " & Fmt.Int(num) & " readers resulting in " &
          Fmt.Int(IntList.Length(self.readers)) & " readers.\n");
      END;
    END;
  END RemoveReader;

PROCEDURE GetReaders(self: T): IntList.T =
   BEGIN
     RETURN IntListFuncs.DeleteAllD(self.readers, ThreadF.MyId());
   END GetReaders;

PROCEDURE Init(self: T): T =
  BEGIN
    self.count := 0;
    self.cv := NEW(Thread.Condition);
    self.mu := NEW(Thread.Mutex);
    RETURN self;
  END Init;

PROCEDURE AcquireRead(self: T) =
  BEGIN
    LOCK self.mu DO
      IF self.count < 0 THEN
        IF self.id # ThreadF.MyId() THEN
          (* If the lock is held by a writer other than ourself, wait
             for the write lock to be released. *) 
          WHILE self.count < 0 DO Thread.Wait(self.mu, self.cv) END;

          (* Now it's ours, so mark it as acquired by another reader *)
          INC(self.count);
        ELSE
          (* Otherwise, it is held by ourself as a writer, so we
             don't change the count and continue on *)
        END;
      ELSE
        (* If the lock is free or held by readers, increment the count of
           readers.  *)
        INC(self.count);
      END;

      (* add to the list of readers. *)
      self.readers := IntList.Cons(ThreadF.MyId(), self.readers);

      IF debug THEN
        IO.Put("RdWrMutex: AcquireRead succeeded for " & 
          Fmt.Int(ThreadF.MyId()) & ", count=" & Fmt.Int(self.count) & "\n");
      END;
    END;
  END AcquireRead;
 
PROCEDURE AcquireWrite(self: T) =
  VAR 
    myReaders: IntList.T := NIL;
  BEGIN
    LOCK self.mu DO
      IF self.count < 0 THEN
        IF self.id = ThreadF.MyId() THEN
          (* We already have the write lock, increment our lock count and 
             return. *)
          DEC(self.count);
          IF debug THEN
            IO.Put("RdWrMutex: AcquireWrite recursively called by " & 
              Fmt.Int(ThreadF.MyId()) & ", count=" & Fmt.Int(self.count) &
              "\n");
          END;
          RETURN;
        END;
      ELSIF self.count > 0 THEN
        (* temporarily "release" our read locks. *)
        WITH num = IntList.Length(self.readers) DO
          myReaders := GetReaders(self);
          <* ASSERT IntList.Length(myReaders) + 
                    IntList.Length(self.readers) = num *>
          IF myReaders # NIL THEN
            DEC(self.count, IntList.Length(myReaders));
          END;
        END;
      END;

      (* Someone else has the write lock or a read lock.  Wait. *)
      WHILE self.count # 0 DO Thread.Wait(self.mu, self.cv) END;
      self.count := -1;
      self.id := ThreadF.MyId();
      self.readers := myReaders;
      IF debug THEN
        IO.Put("RdWrMutex: AcquireWrite succeeded for " & 
          Fmt.Int(ThreadF.MyId()) & ", count=" & Fmt.Int(self.count) & 
          ", numreaders=" & Fmt.Int(IntList.Length(self.readers)) & "\n");
      END;
    END;
  END AcquireWrite;

PROCEDURE ReleaseRead(self: T) =
  BEGIN
    LOCK self.mu DO 
      (* First, remove our entry. *)
      RemoveReader(self);

      (* If the lock is held by readers, decrease the count. *)
      IF self.count > 0 THEN
        DEC(self.count);
      END;

      IF self.count = 0 THEN
        Thread.Broadcast(self.cv) 
      END;
      IF debug THEN
        IO.Put("RdWrMutex: ReleaseRead succeeded for " & 
          Fmt.Int(ThreadF.MyId()) & ", count=" & Fmt.Int(self.count) & "\n");
      END;
    END;
  END ReleaseRead;

PROCEDURE ReleaseWrite(self: T) =
  BEGIN
    LOCK self.mu DO 
      (* replace asserts by if statements for now *)
      IF (self.count >= 0) OR (self.id # ThreadF.MyId()) THEN
        Process.Crash("RdWrMutex: releaseWrite called by non-locker, " &
          "count=" & Fmt.Int(self.count) & " \n");
      END;

      IF self.count = -1 THEN
        self.id := -1;
        self.count := IntList.Length(self.readers);
        Thread.Broadcast(self.cv);
      ELSE
        INC(self.count);
      END;  
      IF debug THEN
        IO.Put("RdWrMutex: ReleaseWrite succeeded for " & 
          Fmt.Int(ThreadF.MyId()) & ", count=" & Fmt.Int(self.count) & "\n");
      END;
    END;
  END ReleaseWrite;

PROCEDURE Wait(self: T; mu: Thread.Mutex; cv: Thread.Condition) =
  VAR 
    myReaders: IntList.T := NIL;
    writeLockCount: INTEGER := 0;
    myId: INTEGER := -1;
  BEGIN
    LOCK self.mu DO
      IF debug THEN
        IO.Put("RdWrMutex: Wait going to sleep for " & 
          Fmt.Int(ThreadF.MyId()) & ", count=" & Fmt.Int(self.count) &
          "\n");
      END;

      (* remember our state *)
      IF self.count < 0 THEN
        myId := self.id;
        myReaders := self.readers;
        writeLockCount := self.count;

        self.id := -1;
        self.count := 0;
        self.readers := NIL;
      ELSIF self.count > 0 THEN
        (* temporarily "release" our read locks. *)
        WITH num = IntList.Length(self.readers) DO
          myReaders := GetReaders(self);
          <* ASSERT IntList.Length(myReaders) + 
                    IntList.Length(self.readers) = num *>
          IF myReaders # NIL THEN
            DEC(self.count, IntList.Length(myReaders));
          END;
        END;
      END;

      (* Wait *)
      IF self.count = 0 THEN
        Thread.Broadcast(self.cv) 
      END;
    END;
    Thread.Wait(mu, cv);
    Thread.Release(mu);
    LOCK self.mu DO
      IF debug THEN
        IO.Put("RdWrMutex: Wait awoken, attempting to reacquire " &
          "lock for " & Fmt.Int(ThreadF.MyId()) & "\n");
      END;

      IF writeLockCount < 0 THEN
        (* Now, attempt to reacquire the write lock! *)
        (* While someone else has the write lock or a read lock,  wait. *)
        WHILE self.count # 0 DO Thread.Wait(self.mu, self.cv) END;

        (* Restore state *)
        self.count := writeLockCount;
        self.id := myId;
        self.readers := myReaders;
      ELSIF myReaders # NIL THEN
        (* Now, attempt to reacquire the read lock! *)
        (* While someone else has the write lock,  wait. *)
        WHILE self.count < 0 DO Thread.Wait(self.mu, self.cv) END;
        
        (* add to the list of readers. *)
        self.readers := IntList.AppendD(self.readers, myReaders);
        INC(self.count, IntList.Length(myReaders));
      END;
        
      IF debug THEN
        IO.Put("RdWrMutex: Wait has reacquired write lock for " & 
          Fmt.Int(ThreadF.MyId()) & ", count=" & Fmt.Int(self.count) & 
          ", numreaders=" & Fmt.Int(IntList.Length(self.readers)) & "\n");
      END;
    END;
    Thread.Acquire(mu);
  END Wait;

BEGIN
  debug := NEW(ParseParams.T).init(Stdio.stderr).keywordPresent("@EVdebugrwm");
END RdWrMutex.
