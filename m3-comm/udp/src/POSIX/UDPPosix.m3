(* Copyright 1998, Compaq Computer Corporation               *)
(*                                                           *)
(* Last modified on Fri Dec 11 15:17:51 PST 1998 by heydon   *)

UNSAFE MODULE UDPPosix EXPORTS UDP;

IMPORT Atom, AtomList, Ctypes, IP, M3toC;
IMPORT OSErrorPosix, SchedulerPosix, Thread;
IMPORT Cerrno, Uerror, Uin, Unix, Usocket, Utypes;

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

CONST SinZero = ARRAY [0 .. 7] OF Ctypes.char{VAL(0, Ctypes.char), ..};

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
  BEGIN
    <* ASSERT NOT self.open *>
    self.myEnd.port := myPort;
    self.myEnd.addr := myAddr;

    (* create socket via socket(2) system call *)
    self.fileno := Usocket.socket(Usocket.AF_INET, Usocket.SOCK_DGRAM, 0);
    IF self.fileno = -1 THEN
      WITH errno = Cerrno.GetErrno() DO
        IF errno = Uerror.EMFILE OR errno = Uerror.ENFILE
        THEN Raise(IP.NoResources)
        ELSE RaiseUnexpected("socket(2)")
        END
      END
    END;

    (* bind socket via bind(2) system call *)
    VAR sockaddr: Uin.struct_sockaddr_in; status: INTEGER; BEGIN
      sockaddr.sin_family := Usocket.AF_INET;
      sockaddr.sin_port := Uin.htons(myPort);
      sockaddr.sin_addr.s_addr := LOOPHOLE(myAddr, Utypes.u_int);
      sockaddr.sin_zero := SinZero;
      status := Usocket.bind(self.fileno,
        (*INOUT*) ADR(sockaddr), BYTESIZE(Uin.struct_sockaddr_in));
      IF status # 0 THEN
        IF Cerrno.GetErrno() = Uerror.EADDRINUSE
          THEN Raise(IP.PortBusy)
          ELSE RaiseUnexpected("bind(2)")
        END
      END
    END;
    self.open := TRUE;
    RETURN self
  END Init;

PROCEDURE Send(self: T; READONLY d: Datagram): INTEGER RAISES {IP.Error} =
  VAR numSent: INTEGER; sockaddr: Uin.struct_sockaddr_in; BEGIN
    <* ASSERT self.open AND d.len <= NUMBER(d.bytes^) *>
    sockaddr.sin_family := Usocket.AF_INET;
    sockaddr.sin_port := Uin.htons(d.other.port);
    sockaddr.sin_addr.s_addr := LOOPHOLE(d.other.addr, Utypes.u_int);
    sockaddr.sin_zero := SinZero;
    numSent := Usocket.sendto(self.fileno,
      LOOPHOLE(ADR(d.bytes[0]), Ctypes.char_star), d.len, (*flags=*) 0,
      ADR(sockaddr), BYTESIZE(Uin.struct_sockaddr_in));
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
  VAR 
    numSent: INTEGER; 
    sockaddr: Uin.struct_sockaddr_in; 
    cstr: Ctypes.char_star;
  BEGIN
    <* ASSERT self.open *>
    sockaddr.sin_family := Usocket.AF_INET;
    sockaddr.sin_port := Uin.htons(other.port);
    sockaddr.sin_addr.s_addr := LOOPHOLE(other.addr, Utypes.u_int);
    sockaddr.sin_zero := SinZero;
    cstr := M3toC.SharedTtoS(t);
    numSent := Usocket.sendto(self.fileno, cstr, Len(cstr), (*flags=*) 0,
(*    LOOPHOLE(ADR(t[0]), Ctypes.char_star), NUMBER(t^) - 1, (*flags=*) 0, *)
      ADR(sockaddr), BYTESIZE(Uin.struct_sockaddr_in));
    M3toC.FreeSharedS(t, cstr);
    IF numSent < 0 THEN RaiseUnexpected("sendto(2)") END;
    RETURN numSent
  END SendText;

PROCEDURE Receive(self: T; VAR (*INOUT*) d: Datagram; timeout: LONGREAL)
    RAISES {Timeout, IP.Error, Thread.Alerted} =
  VAR
    waitRes: SchedulerPosix.WaitResult;
    numRead: INTEGER;
    sockaddr: Uin.struct_sockaddr_in;
    saSize: Usocket.socklen_t;
  BEGIN
    <* ASSERT self.open *>
    waitRes := SchedulerPosix.IOAlertWait(self.fileno, TRUE, timeout);
    CASE waitRes OF
    | SchedulerPosix.WaitResult.Ready   => (* SKIP *)
    | SchedulerPosix.WaitResult.Error   => <* ASSERT FALSE *>
    | SchedulerPosix.WaitResult.FDError => <* ASSERT FALSE *>
    | SchedulerPosix.WaitResult.Timeout => RAISE Timeout;
    END;
    sockaddr.sin_zero := SinZero;
    saSize := BYTESIZE(Uin.struct_sockaddr_in);
    numRead := Usocket.recvfrom(self.fileno,
      LOOPHOLE(ADR(d.bytes[0]), Ctypes.char_star), NUMBER(d.bytes^),
      (*flags=*) 0, ADR(sockaddr), ADR(saSize));
    IF numRead < 0
      THEN RaiseUnexpected("recvfrom(2)")
      ELSE d.len := numRead
    END;
    <* ASSERT saSize = BYTESIZE(Uin.struct_sockaddr_in) *>
    d.other.port := Uin.ntohs(sockaddr.sin_port);
    d.other.addr := LOOPHOLE(sockaddr.sin_addr.s_addr, IP.Address);
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
