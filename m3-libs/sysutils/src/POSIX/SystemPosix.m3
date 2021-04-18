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
 *)

(*---------------------------------------------------------------------------*)
UNSAFE MODULE SystemPosix EXPORTS System;

IMPORT Text, Ctypes, (*Uexec,*) Process, Fmt, Uerror, Cerrno;
IMPORT SchedulerPosix, Word, Unix;

(*---------------------------------------------------------------------------*)
PROCEDURE Hostname() : TEXT =
  VAR
    buf : ARRAY [0..1024] OF CHAR;
    len := 1024;
  BEGIN
    IF Unix.gethostname(LOOPHOLE(ADR(buf), Ctypes.char_star), len) = 0 THEN
      buf[1024] := '\000';
      len := 0;
      WHILE len < 1024 AND buf[len] # '\000' DO
        INC(len);
      END;
      RETURN Text.FromChars(SUBARRAY(buf, 0, len));
    END;
    RETURN "amnesiac";
  END Hostname;

PROCEDURE Wait(p: Process.T): Process.ExitCode RAISES {Error} =
  VAR
    result, status: Ctypes.int;
    pid := Process.GetID(p);
    e : Ctypes.int;
    err : TEXT;
  BEGIN
    result := SchedulerPosix.WaitProcess (pid, status);
    IF result < 0 THEN
      e := Cerrno.GetErrno();
      IF (e = Uerror.ECHILD) THEN err := "The process specified in pid does not exist or is not a child of the calling process.";
      ELSIF (e = Uerror.EINTR) THEN err := "WNOHANG was not set and an unblocked signal or a SIGCHLD was caught.";
      ELSIF (e = Uerror.EINVAL) THEN err := "The options argument was invalid.";
      ELSE
        err := "Unexpected return value " & Fmt.Int(e);
      END;
      RAISE Error("Could not wait: " & err);
    END;
(* Use this once m3core is new enough.
    Uexec.RepackStatus(status);
*)
    (* ensure non-zero implies lower bits non-zero *)
    IF (status # 0) AND (Word.And(status, 16_FF) = 0) THEN
      status := 1;
    END;
    RETURN MIN(LAST(Process.ExitCode), status);
  END Wait;

BEGIN
END SystemPosix.
