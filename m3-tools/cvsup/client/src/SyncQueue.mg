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
 * $Id: SyncQueue.mg,v 1.1.1.1 2009-04-09 17:01:40 jkrell Exp $ *)

GENERIC MODULE SyncQueue(Elem, Seq);
(* Where "Seq" is a generic instance "Sequence(Elem)". *)

IMPORT Thread;

REVEAL
  Private = MUTEX BRANDED OBJECT END;

  T = Public BRANDED OBJECT
    seq: Seq.T;
    cond: Thread.Condition;
    qsize: CARDINAL := 0;
    closed: BOOLEAN := FALSE;
  OVERRIDES
    init := Init;
    put := Put;
    close := Close;
    get := Get;
    EOF := TestEOF;
    size := Size;
  END;

PROCEDURE Close(self: T) =
  VAR
    doSignal: BOOLEAN;
  BEGIN
    LOCK self DO
      doSignal := self.qsize = 0 AND NOT self.closed;
      self.closed := TRUE;
    END;
    IF doSignal THEN Thread.Signal(self.cond) END;
  END Close;

PROCEDURE Init(self: T; sizeHint: CARDINAL := 5): T =
  BEGIN
    self.seq := NEW(Seq.T).init(sizeHint);
    self.cond := NEW(Thread.Condition);
    RETURN self;
  END Init;

PROCEDURE Get(self: T): Elem.T
  RAISES {EndOfFile, Thread.Alerted} =
  BEGIN
    LOCK self DO
      WHILE self.qsize = 0 AND NOT self.closed DO
	Thread.AlertWait(self, self.cond);
      END;
      IF self.qsize = 0 THEN RAISE EndOfFile END;
      DEC(self.qsize);
      RETURN self.seq.remlo();
    END;
  END Get;

PROCEDURE Put(self: T; READONLY e: Elem.T) =
  VAR
    doSignal: BOOLEAN;
  BEGIN
    LOCK self DO
      <* ASSERT NOT self.closed *>
      doSignal := self.qsize = 0;
      self.seq.addhi(e);
      INC(self.qsize);
    END;
    IF doSignal THEN Thread.Signal(self.cond) END;
  END Put;

PROCEDURE Size(self: T): CARDINAL =
  VAR
    size: CARDINAL;
  BEGIN
    LOCK self DO
      size := self.qsize;
      IF self.closed THEN INC(size) END;
    END;
    RETURN size;
  END Size;


PROCEDURE TestEOF(self: T): BOOLEAN
  RAISES {Thread.Alerted} =
  BEGIN
    LOCK self DO
      WHILE self.qsize = 0 AND NOT self.closed DO
	Thread.AlertWait(self, self.cond);
      END;
      RETURN self.qsize = 0;
    END;
  END TestEOF;

BEGIN
END SyncQueue.
