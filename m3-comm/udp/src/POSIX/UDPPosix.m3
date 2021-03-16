(* Copyright 1998, Compaq Computer Corporation               *)
(*                                                           *)
(* Last modified on Fri Dec 11 15:17:51 PST 1998 by heydon   *)

UNSAFE MODULE UDPPosix EXPORTS UDP;

IMPORT Atom, AtomList, Ctypes, IP, M3toC;
IMPORT OSErrorPosix, SchedulerPosix, Thread;
IMPORT Cerrno, Uerror, Unix, UDPInternal;
FROM Ctypes IMPORT int;

REVEAL
  T = Public BRANDED "UDPPosix.T" OBJECT
    open: BOOLEAN := FALSE;   (* TRUE iff the UDP connection is open *)
    myEnd := IP.NullEndPoint; (* this socket's port *)
    fileno: INTEGER := -1;    (* this socket's file descriptor *)
  OVERRIDES
    init := Init;
    send := Send;
    sendText := SendText;
    receive := Receive;
    close := Close;
  END;

(* An open UDP socket "udp" has "udp.open", "udp.myEnd" set to this
   socket's endpoint, and "udp.fileno" set to the socket's corresponding
   file descriptor. *)

PROCEDURE Raise(a: Atom.T) RAISES {IP.Error} =
  BEGIN
    RAISE IP.Error(AtomList.List2(a, OSErrorPosix.ErrnoAtom(Cerrno.GetErrno())));
  END Raise;

PROCEDURE RaiseUnexpected(syscall: TEXT) RAISES {IP.Error} =
  BEGIN
    Raise(Atom.FromText("Unexpected error calling " & syscall));
  END RaiseUnexpected;

PROCEDURE Init(self: T; myPort: IP.Port; myAddr: IP.Address): T
    RAISES {IP.Error} =
  VAR err, status: int := 0;
  BEGIN
    <* ASSERT NOT self.open *>
    self.myEnd.port := myPort;
    self.myEnd.addr := myAddr;

    UDPInternal.Init(self.fileno, ADR(myAddr.a[0]), myPort, err, status);

    (* create socket via socket(2) system call *)
    IF self.fileno = -1 THEN
      IF err = Uerror.EMFILE OR err = Uerror.ENFILE
        THEN Raise(IP.NoResources)
        ELSE RaiseUnexpected("socket(2)")
      END
    END;

    (* bind socket via bind(2) system call *)
    IF status # 0 THEN
      IF err = Uerror.EADDRINUSE
        THEN Raise(IP.PortBusy)
        ELSE RaiseUnexpected("bind(2)")
      END
    END;

    self.open := TRUE;

    RETURN self
  END Init;

PROCEDURE Send(self: T; READONLY d: Datagram): INTEGER RAISES {IP.Error} =
  VAR numSent := 0;
      data := ADR(d.bytes[0]);
  BEGIN
    <* ASSERT self.open AND d.len <= NUMBER(d.bytes^) *>
    numSent := UDPInternal.Send(self.fileno, data, d.len,
                                ADR(d.other.addr.a[0]),
                                d.other.port);
    IF numSent < 0 THEN RaiseUnexpected("sendto(2)") END;
    RETURN numSent
  END Send;

PROCEDURE Len(cstr: Ctypes.char_star) : INTEGER =
  VAR l := 0;
  BEGIN
    WHILE LOOPHOLE(cstr^, CHAR) # '\000' DO INC(l) END;
    RETURN l;
  END Len;

PROCEDURE SendText(self: T; READONLY other: IP.Endpoint; t: TEXT): INTEGER
    RAISES {IP.Error} =
  VAR numSent := 0;
      cstr := M3toC.SharedTtoS(t);
  BEGIN
    <* ASSERT self.open *>
    numSent := UDPInternal.Send(self.fileno,
                                LOOPHOLE(cstr, ADDRESS), Len(cstr),
                                ADR(other.addr.a[0]),
                                other.port);
    M3toC.FreeSharedS(t, cstr);
    IF numSent < 0 THEN RaiseUnexpected("sendto(2)") END;
    RETURN numSent
  END SendText;

PROCEDURE Receive(self: T; VAR (*INOUT*) d: Datagram; timeout: LONGREAL)
    RAISES {Timeout, IP.Error, Thread.Alerted} =
  VAR
    waitRes: SchedulerPosix.WaitResult;
    addr: IP.Address4;
    port: int := 0;
    numRead := 0;
    data := ADR(d.bytes[0]);
  BEGIN
    <* ASSERT self.open *>
    waitRes := SchedulerPosix.IOAlertWait(self.fileno, TRUE, timeout);
    CASE waitRes OF
    | SchedulerPosix.WaitResult.Ready   => (* SKIP *)
    | SchedulerPosix.WaitResult.Error   => <* ASSERT FALSE *>
    | SchedulerPosix.WaitResult.FDError => <* ASSERT FALSE *>
    | SchedulerPosix.WaitResult.Timeout => RAISE Timeout;
    END;
    numRead := UDPInternal.Receive(self.fileno, data, NUMBER(d.bytes^), ADR(addr.a[0]), port);
    IF numRead < 0
      THEN RaiseUnexpected("recvfrom(2)")
      ELSE d.len := numRead
    END;
    d.other.port := port;
    d.other.addr := addr;
  END Receive;

PROCEDURE Close(self: T) RAISES {IP.Error} =
  BEGIN
    <* ASSERT self.open *>
    VAR status := Unix.close(self.fileno); BEGIN
      IF status # 0 THEN RaiseUnexpected("close(2)") END
    END;
    self.open := FALSE;
    self.myEnd := IP.NullEndPoint;
    self.fileno := -1
  END Close;

BEGIN
END UDPPosix.
