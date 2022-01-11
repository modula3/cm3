(* Copyright (C) 2005, Purdue Research Foundation                  *)
(* All rights reserved.                                            *)
(* See the file COPYRIGHT-PURDUE for a full description.           *)

(* This is the implementation for Cygwin. *)

UNSAFE MODULE SchedulerPosix;

FROM ThreadWin32 IMPORT PerfChanged, PerfRunning, XTestAlert, perfOn;
FROM ThreadF IMPORT State, MyId;
FROM Thread IMPORT Alerted, Self, T, TestAlert;
IMPORT Cerrno, Time, Uerror, Uexec;
FROM Ctypes IMPORT int;
FROM ThreadInternal IMPORT Poll;

PROCEDURE IOWait (fd: CARDINAL; read: BOOLEAN;
                  timeoutInterval: LONGREAL := -1.0D0): WaitResult =
  <*FATAL Alerted*>
  VAR self := Self();
  BEGIN
    TRY
      IF perfOn THEN PerfChanged(State.blocking) END;
      RETURN XIOWait(self, fd, read, timeoutInterval, alertable := FALSE);
    FINALLY
      IF perfOn THEN PerfRunning() END;
    END;
  END IOWait;

PROCEDURE IOAlertWait (fd: CARDINAL; read: BOOLEAN;
                       timeoutInterval: LONGREAL := -1.0D0): WaitResult
  RAISES {Alerted} =
  VAR self := Self();
  BEGIN
    TRY
      IF perfOn THEN PerfChanged(State.blocking) END;
      RETURN XIOWait(self, fd, read, timeoutInterval, alertable := TRUE);
    FINALLY
      IF perfOn THEN PerfRunning() END;
    END;
  END IOAlertWait;

PROCEDURE XIOWait (self: T; fd: CARDINAL; read: BOOLEAN; interval: LONGREAL;
                   alertable: BOOLEAN): WaitResult
  RAISES {Alerted} =
  VAR res: WaitResult;
      subInterval: LONGREAL := 1.0d0;
      err: int := 0;
      again := FALSE;
  BEGIN
    IF NOT alertable THEN
      subInterval := interval;
    ELSIF interval < 0.0d0 THEN
      interval := LAST(LONGREAL);
    ELSIF interval < subInterval THEN
      subInterval := interval;
    END;

    IF alertable AND TestAlert() THEN RAISE Alerted END;
    LOOP
      res := VAL(Poll(fd, 1 * ORD(read), subInterval), WaitResult);

      IF alertable AND TestAlert() THEN RAISE Alerted END;

      CASE res OF
        | WaitResult.FDError, WaitResult.Ready =>
          RETURN res;
        | WaitResult.Error =>
          err := Cerrno.GetErrno();
          IF err = Uerror.EINTR THEN
            (* spurious wakeups are OK *)
          ELSIF err = Uerror.EAGAIN AND NOT again THEN
            again := TRUE;
            (* try just once more *)
          ELSE
            RETURN WaitResult.Error;
          END;
        | WaitResult.Timeout =>
          interval := interval - subInterval;
          IF interval <= 0.0d0 THEN RETURN WaitResult.Timeout END;
          IF interval < subInterval THEN
            subInterval := interval;
          END;
      END;
    END;
  END XIOWait;

PROCEDURE WaitProcess (pid: int; VAR status: int): int =
  (* ThreadPThread.m3 and ThreadPosix.m3 are very similar. *)
  BEGIN
    LOOP
      WITH r = Uexec.waitpid(pid, ADR(status), 0) DO
        <*ASSERT r # 0*>
        IF r > 0 THEN RETURN r END;
        IF Cerrno.GetErrno() # Uerror.EINTR THEN RETURN r END;
      END;
    END;
  END WaitProcess;

BEGIN
END SchedulerPosix.
