(* Copyright 1996-2003 John D. Polstra.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id$ *)

MODULE Reaper;

IMPORT RefSet, RefSetList, Thread;

REVEAL
  Private = MUTEX BRANDED OBJECT END;

  T = Public BRANDED OBJECT
    cond: Thread.Condition;
    liveThreads: RefSet.T;
    dyingThreads: RefSet.T;
  OVERRIDES
    init := Init;
  END;

PROCEDURE Init(self: T): T =
  BEGIN
    self.cond := NEW(Thread.Condition);
    self.liveThreads := NEW(RefSetList.T).init();
    self.dyingThreads := NEW(RefSetList.T).init();
    RETURN self;
  END Init;

PROCEDURE Fork(reaper: T; cl: Thread.Closure): Thread.T =
  VAR
    thr: Thread.T;
  BEGIN
    LOCK reaper DO
      thr := Thread.Fork(cl);
      EVAL reaper.liveThreads.insert(thr);
    END;
    RETURN thr;
  END Fork;

PROCEDURE Dying(reaper: T) =
  BEGIN
    WITH thread = Thread.Self() DO
      LOCK reaper DO
	EVAL reaper.liveThreads.delete(thread);
	EVAL reaper.dyingThreads.insert(thread);
	Thread.Signal(reaper.cond);
      END;
    END;
  END Dying;

PROCEDURE JoinNext(reaper: T;
                   VAR thr: Thread.T;
		   VAR ret: REFANY): BOOLEAN =
  VAR
    ok: BOOLEAN;
    ref: REFANY;
  BEGIN
    LOCK reaper DO
      IF reaper.dyingThreads.isEmpty() THEN
	IF reaper.liveThreads.isEmpty() THEN RETURN FALSE END;
	REPEAT
	  Thread.Wait(reaper, reaper.cond);
	UNTIL NOT reaper.dyingThreads.isEmpty();
      END;
      ok := reaper.dyingThreads.iterate().next(ref);
      <* ASSERT ok *>
      thr := NARROW(ref, Thread.T);
      EVAL reaper.dyingThreads.delete(thr);
    END;
    ret := Thread.Join(thr);
    RETURN TRUE;
  END JoinNext;

PROCEDURE AlertJoinNext(reaper: T;
                        VAR thr: Thread.T;
		        VAR ret: REFANY): BOOLEAN
  RAISES {Thread.Alerted} =
  VAR
    ok: BOOLEAN;
    ref: REFANY;
  BEGIN
    LOCK reaper DO
      IF reaper.dyingThreads.isEmpty() THEN
	IF reaper.liveThreads.isEmpty() THEN RETURN FALSE END;
	REPEAT
	  Thread.AlertWait(reaper, reaper.cond);
	UNTIL NOT reaper.dyingThreads.isEmpty();
      END;
      ok := reaper.dyingThreads.iterate().next(ref);
      <* ASSERT ok *>
      thr := NARROW(ref, Thread.T);
      EVAL reaper.dyingThreads.delete(thr);
    END;
    ret := Thread.AlertJoin(thr);
    RETURN TRUE;
  END AlertJoinNext;

PROCEDURE AlertAll(reaper: T) =
  VAR
    iter: RefSetList.Iterator;
    ref: REFANY;
    thr: Thread.T;
  BEGIN
    LOCK reaper DO
      iter := reaper.liveThreads.iterate();
      WHILE iter.next(ref) DO
	thr := NARROW(ref, Thread.T);
	Thread.Alert(thr);
      END;
    END;
  END AlertAll;

BEGIN
END Reaper.
