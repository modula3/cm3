(* Copyright 1999-2003 John D. Polstra.
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

(* This module works as follows.  A Unix pipe is created, but unlike
   typical pipes, it is used entirely within a single process.  When
   a signal comes in, a single byte containing the signal number is
   written to the pipe.  This is a safe thing to do from a signal
   handler on POSIX systems, and it doesn't involve the garbage
   collector or thread scheduler in any way.  All Unix systems that I
   know of allow many bytes to be written before the write call will
   block.

   A background thread executes a loop, reading bytes from the pipe
   and dispatching control to the Handlers selected by the signal
   numbers they contain.  The Handlers execute as normal Modula-3
   thread code.  Thus they can do anything. *)

UNSAFE MODULE SigHandler;

IMPORT
  Ctypes, SchedulerPosix, Thread, Uerror, Unix, UnixMisc, Uuio, Word;

IMPORT Cerrno;

TYPE
  Dispatcher = Thread.Closure OBJECT
    handlers := ARRAY SigNum OF T { NIL, .. };
    rfd: INTEGER;
    mu: MUTEX;
    state, desiredState: State;
    changeState: Thread.Condition;
    stateChanged: Thread.Condition;
    thread: Thread.T;
  OVERRIDES
    apply := DispatcherRun;
  END;

  State = { Running, Blocked, ShutDown };

VAR
  wfd: INTEGER;
  dispatcher := NEW(Dispatcher,
    mu := NEW(MUTEX),
    changeState := NEW(Thread.Condition),
    stateChanged := NEW(Thread.Condition),
    desiredState := State.ShutDown,
    state := State.ShutDown);

EXCEPTION Fatal(TEXT);
<* FATAL Fatal *>

PROCEDURE Block() =
  BEGIN
    ChangeState(dispatcher, State.Blocked);
  END Block;

PROCEDURE Catch(sig: Ctypes.int) =
  VAR
    ch: Ctypes.unsigned_char := sig;
  BEGIN
    EVAL Uuio.write(wfd, ADR(ch), 1);
  END Catch;

PROCEDURE ChangeState(d: Dispatcher; state: State) =
  (* Ask the dispatcher thread to change to a new state, and wait until
     it has made the change. *)
  BEGIN
    LOCK d.mu DO
      d.desiredState := state;
      IF d.state # d.desiredState THEN
	IF d.state = State.Running THEN
	  (* Send dummy signal 0 to wake up the dispatcher. *)
	  Catch(0);
	ELSE
	  Thread.Signal(d.changeState);
	END;
	WHILE d.state # d.desiredState DO
	  Thread.Wait(d.mu, d.stateChanged);
	END;
      END;
    END;
  END ChangeState;

PROCEDURE DispatcherRun(self: Dispatcher): REFANY =
  VAR
    oldState, newState: State;
  BEGIN
    LOOP
      LOCK self.mu DO
	oldState := self.state;
	newState := self.desiredState;
	self.state := newState;
      END;
      IF oldState # newState THEN
	Thread.Signal(self.stateChanged);
      END;
      CASE newState OF
      | State.Running  => DoDispatch(self);
      | State.Blocked  => DoBlock(self);
      | State.ShutDown => DoShutDown(self);  EXIT;
      END;
    END;
    RETURN NIL;
  END DispatcherRun;

PROCEDURE DoBlock(self: Dispatcher) =
  (* Pause doing nothing until a different state is requested. *)
  BEGIN
    LOCK self.mu DO
      WHILE self.desiredState = State.Blocked DO
	Thread.Wait(self.mu, self.changeState);
      END;
    END;
  END DoBlock;

PROCEDURE DoDispatch(self: Dispatcher) =
  (* Wait for a signal and handle it.  Signal 0 is a no-op which is
     used to break us out of the read when we need to change states. *)
  VAR
    sig: INTEGER;
    handler: T;
  BEGIN
    TRY
      sig := ORD(ReadPipe(self.rfd));
      IF sig # 0 THEN
	LOCK self.mu DO
	  handler := self.handlers[sig];
	END;
	IF handler # NIL THEN
	  handler.apply(sig);
	END;
      END;
    EXCEPT Thread.Alerted => END;
  END DoDispatch;

PROCEDURE DoShutDown(self: Dispatcher) =
  BEGIN
    EVAL Unix.close(wfd);
    EVAL Unix.close(self.rfd);
    LOCK self.mu DO
      FOR sig := FIRST(self.handlers) TO LAST(self.handlers) DO
	IF self.handlers[sig] # NIL THEN
	  EVAL UnixMisc.Signal(sig, NIL);
	  self.handlers[sig] := NIL;
	END;
      END;
    END;
  END DoShutDown;

PROCEDURE Init() =
  VAR
    fds: ARRAY [0..1] OF Ctypes.int;
  BEGIN
    IF Unix.pipe(fds) = -1 THEN
      RAISE Fatal("Cannot create pipe");
    END;
    MakeNonBlocking(fds[0]);
    MakeNonBlocking(fds[1]);
    wfd := fds[1];
    dispatcher.rfd := fds[0];
    dispatcher.desiredState := State.Running;
    dispatcher.state := State.Running;
    dispatcher.thread := Thread.Fork(dispatcher);
  END Init;

PROCEDURE MakeNonBlocking(fd: INTEGER) =
  BEGIN
    IF Unix.fcntl(fd, Unix.F_SETFL,
      Word.Or(Unix.fcntl(fd, Unix.F_GETFL, 0), Unix.M3_NONBLOCK)) # 0
    THEN
      RAISE Fatal("Cannot set M3_NONBLOCK on pipe");
    END;
  END MakeNonBlocking;

PROCEDURE ReadPipe(fd: INTEGER): CHAR
  RAISES { Thread.Alerted } =
  VAR
    ch: Ctypes.unsigned_char;
    n: INTEGER;
  BEGIN
    LOOP
      n := Uuio.read(fd, ADR(ch), 1);
      IF n > 0 THEN
	RETURN VAL(ch, CHAR);
      END;
      IF n = 0 THEN
	RAISE Fatal("Unexpected EOF from pipe");
      END;
      WITH errno = Cerrno.GetErrno() DO
	IF errno # Uerror.EAGAIN AND errno # Uerror.EWOULDBLOCK THEN
	  RAISE Fatal("Unexpected errno value");
	END;
      END;
      EVAL SchedulerPosix.IOAlertWait(fd, TRUE);
    END;
  END ReadPipe;

PROCEDURE Register(sig: SigNum;
                   handler: T) =
  BEGIN
    LOCK dispatcher.mu DO
      IF dispatcher.state = State.ShutDown THEN
	Init();
      END;
      dispatcher.handlers[sig] := handler;
      IF handler # NIL THEN
	EVAL UnixMisc.Signal(sig, Catch);
      ELSE
	EVAL UnixMisc.Signal(sig, NIL);
      END;
    END;
  END Register;

PROCEDURE ShutDown() =
  BEGIN
    ChangeState(dispatcher, State.ShutDown);
  END ShutDown;

PROCEDURE Unblock() =
  BEGIN
    ChangeState(dispatcher, State.Running);
  END Unblock;

BEGIN
END SigHandler.
