(* Copyright 1999-2002 elego Software Solutions GmbH, Berlin, Germany.
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
 * $Id: SystemPosix.m3,v 1.5 2009-01-01 00:55:29 jkrell Exp $ *)

(*---------------------------------------------------------------------------*)
UNSAFE MODULE SystemPosix EXPORTS System;

IMPORT Unix, Text, Process, Thread, Fmt, Cerrno, Uerror;
FROM Ctypes IMPORT int;
FROM Sysutils_Uwaitpid IMPORT waitpid_status_t, waitpid, WNOHANG;
IMPORT Sysutils_DoesWaitPidYield;

(*---------------------------------------------------------------------------*)
PROCEDURE Hostname() : TEXT =
  VAR
    buf : ARRAY [0..1024] OF CHAR;
    len := 1024;
  BEGIN
    IF Unix.gethostname(ADR(buf), len) = 0 THEN
      buf[1024] := '\000';
      len := 0;
      WHILE len < 1024 AND buf[len] # '\000' DO
        INC(len);
      END;
      RETURN Text.FromChars(SUBARRAY(buf, 0, len));
    END;
    RETURN "amnesiac";
  END Hostname;


PROCEDURE WaitError(e: int) RAISES {Error} =
  VAR
    err : TEXT;
  BEGIN
    CASE e OF
      Uerror.ECHILD => err := "The process specified in pid does not exist or is not a child of the calling process.";
    | Uerror.EINTR => err := "WNOHANG was not set and an unblocked signal or a SIGCHLD was caught.";
    | Uerror.EINVAL => err := "The options argument was invalid.";
    ELSE
      err := "Unexpected return value " & Fmt.Int(e);
    END;
    RAISE Error("Could not wait: " & err);
  END WaitError;

PROCEDURE WaitSlow(pid: int): Process.ExitCode RAISES {Error} =
  VAR
    result: int;
    status: waitpid_status_t;
  CONST Delay = 0.1D0;
  BEGIN
    LOOP
      result := waitpid(pid, status, WNOHANG);
      <* ASSERT(result >= -1) *>
      IF result # 0 THEN
        EXIT
      END;

      (* Let parent threads run while waiting for child process, to avoid deadlock
      in case they depend on each other. Slow, because the child process might
      end "soon", but we still wait a full Delay. *)

      Thread.Pause(Delay)

    END;
    IF result < 0 THEN 
      WaitError(Cerrno.GetErrno());
    END;
    RETURN MIN(LAST(Process.ExitCode), status.w_Loophole);
  END WaitSlow;

PROCEDURE WaitFast(pid: int): Process.ExitCode RAISES {Error} =
  VAR
    result : int;
    status : waitpid_status_t;
    e : int;
  BEGIN
    LOOP
      result := waitpid(pid, status, 0);
      <* ASSERT((result = -1) OR (result > 0)) *>
      IF result > 0 THEN
        RETURN MIN(LAST(Process.ExitCode), status.w_Loophole);
      END;
      IF result < 0 THEN 
        e := Cerrno.GetErrno();
        IF e = Uerror.EINTR THEN
          (* keep looping *)
        ELSE
          WaitError(e);
        END;
      END;
    END;
  END WaitFast;

PROCEDURE Wait (p: Process.T): Process.ExitCode RAISES {Error} =
  VAR pid := Process.GetID(p);
  BEGIN
    (* IF SchedulerPosix.DoesWaitPidYield() THEN *)
    IF Sysutils_DoesWaitPidYield.Value THEN
        RETURN WaitFast(pid);
    ELSE
        RETURN WaitSlow(pid);
    END;
  END Wait;

BEGIN
END SystemPosix.
