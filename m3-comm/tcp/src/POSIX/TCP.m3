(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by gnelson *)
(* Last modified on Mon Mar 27 17:18:33 PST 1995 by wobber *)
(*      modified on Tue Feb  7 15:48:33 PST 1995 by kalsow *)
(*      modified on Fri Jan  7 13:31:11 PST 1994 by msm    *)
(*      modified on Sun Jan 12 16:16:54 PST 1992 by meehan *)
(*      modified on Sat Jan 11 16:55:00 PST 1992 by gnelson *)

UNSAFE MODULE TCP EXPORTS TCP, TCPMisc, TCPSpecial;

IMPORT Atom, AtomList, IP, IPInternal, Rd, Wr, Thread;
IMPORT Usocket, Uerror, Uin, Unix, Uuio, Utypes,
       SchedulerPosix, Fmt, Word;
IMPORT ConnFD;
IMPORT Cerrno;
IMPORT TCPHack;
IMPORT TCPPosix;
FROM Usocket IMPORT socklen_t;
FROM Ctypes IMPORT char, int;

REVEAL
  Connector = MUTEX BRANDED "TCP.Connector" OBJECT
    fd: INTEGER;       (*CONST*)
    ep: IP.Endpoint;   (*CONST*)
    closed: BOOLEAN := FALSE;
  END;

REVEAL
  T = TCPPosix.Public BRANDED "TCP.T" OBJECT
    ep: IP.Endpoint;
    error: AtomList.T := NIL;
  OVERRIDES
    get := GetBytesFD;
    put := PutBytesFD;
    shutdownIn := ShutdownIn;
    shutdownOut := ShutdownOut;
    close := Close;
  END;

TYPE SockAddrIn = Uin.struct_sockaddr_in;

CONST TCP_NODELAY = 1;

CONST Sin_Zero = ARRAY [0 .. 7] OF char{VAL(0, char), ..};

VAR ClosedErr: AtomList.T;

PROCEDURE NewConnector (ep: IP.Endpoint): Connector RAISES {IP.Error} =
  VAR
    res                := NEW(Connector, ep := ep);
    status: INTEGER;
    True: int := 1;  (* CONST *)
  BEGIN
    res.fd := Usocket.socket(Usocket.AF_INET, Usocket.SOCK_STREAM, 0 (* TCP*));
    IF res.fd = -1 THEN
      WITH errno = Cerrno.GetErrno() DO
        IF errno = Uerror.EMFILE OR errno = Uerror.ENFILE THEN
          Raise(IP.NoResources);
        ELSE
          RaiseUnexpected();
        END
      END
    END;
    MakeNonBlocking (res.fd);
    EVAL Usocket.setsockopt(res.fd, Usocket.SOL_SOCKET, Usocket.SO_REUSEADDR,
      ADR(True), BYTESIZE(True));
    status := IPInternal.bind_in(res.fd, ADR(ep.addr.a[0]), ep.port);
    IF status # 0 THEN
      IF Cerrno.GetErrno() = Uerror.EADDRINUSE THEN
        Raise(IP.PortBusy);
      ELSE
        RaiseUnexpected();
      END
    END;
    IF Usocket.listen(res.fd, 8(*backlog*)) # 0 THEN RaiseUnexpected(); END;
    RETURN res
  END NewConnector;

PROCEDURE GetEndPoint(c: Connector): IP.Endpoint =
VAR port := 0;
  BEGIN
    IF c.ep.addr = IP.NullAddress THEN
      c.ep.addr := IP.GetHostAddr();
    END;
    IF c.ep.port = IP.NullPort THEN
      IF IPInternal.getsockname_in(c.fd, NIL, port) # 0 THEN
        Die()
      END;
      c.ep.port := port;
    END;
    RETURN c.ep
  END GetEndPoint;

PROCEDURE Connect (ep: IP.Endpoint): T
    RAISES {IP.Error, Thread.Alerted} =
  VAR
    t := StartConnect(ep);
    ok := FALSE;
  BEGIN
    TRY
      EVAL FinishConnect(t);
      ok := TRUE;
    FINALLY
     IF NOT ok THEN Close(t); END;
    END;
    RETURN t;
  END Connect;

PROCEDURE StartConnect(to: IP.Endpoint;
                       from: IP.Endpoint := IP.NullEndPoint): T
    RAISES {IP.Error} =
  VAR
    fd: INTEGER;
    status: int;
    True: int := 1;  (* CONST *)
    fromName: SockAddrIn;
    ok := FALSE;
  BEGIN
    fd := Usocket.socket(Usocket.AF_INET, Usocket.SOCK_STREAM, 0 (*TCP*));
    IF fd < 0 THEN
      WITH errno = Cerrno.GetErrno() DO
        IF errno = Uerror.EMFILE OR errno = Uerror.ENFILE THEN
          Raise(IP.NoResources);
        ELSE
          RaiseUnexpected();
        END;
      END;
    END;
    InitFD(fd);

    IF from # IP.NullEndPoint THEN  (* Bind to the "from" address. *)
      EVAL Usocket.setsockopt(fd, Usocket.SOL_SOCKET, Usocket.SO_REUSEADDR,
        ADR(True), BYTESIZE(True));
      fromName.sin_family := Usocket.AF_INET;
      fromName.sin_port := Uin.htons(from.port);
      fromName.sin_addr.s_addr := LOOPHOLE(from.addr, Utypes.u_int);
      fromName.sin_zero := Sin_Zero;
      status := Usocket.bind(fd, ADR(fromName), BYTESIZE(SockAddrIn));
      IF status # 0 THEN
        IF Cerrno.GetErrno() = Uerror.EADDRINUSE THEN
          Raise(IP.PortBusy);
        ELSE
          RaiseUnexpected();
        END
      END;
    END;

    TRY
      EVAL CheckConnect(fd, to);
      ok := TRUE;
    FINALLY
      IF NOT ok THEN EVAL Unix.close(fd); END;
    END;
    RETURN NEW(T, fd := fd, ep := to);
  END StartConnect;

PROCEDURE FinishConnect(t: T; timeout: LONGREAL := -1.0D0): BOOLEAN
    RAISES {IP.Error, Thread.Alerted} =
  BEGIN
    LOOP
      EVAL SchedulerPosix.IOAlertWait(t.fd, FALSE, timeout);
      LOCK t DO
        IF t.error # NIL THEN RAISE IP.Error(t.error); END;
        IF CheckConnect(t.fd, t.ep) THEN EXIT; END;
      END;
      IF timeout >= 0.0D0 THEN RETURN FALSE; END;
    END;
    RETURN TRUE;
  END FinishConnect;

VAR seenBadFBug: BOOLEAN := FALSE;

PROCEDURE CheckConnect(fd: INTEGER; ep: IP.Endpoint) : BOOLEAN
    RAISES {IP.Error} =
  VAR
    name: SockAddrIn;
    status: INTEGER;
  BEGIN
    name.sin_family := Usocket.AF_INET;
    name.sin_port := Uin.htons(ep.port);
    name.sin_addr.s_addr := LOOPHOLE(ep.addr, Utypes.u_int);
    name.sin_zero := Sin_Zero;
    status := Usocket.connect(fd, ADR(name), BYTESIZE(SockAddrIn));
    IF status = 0 THEN RETURN TRUE; END;
    WITH errno = Cerrno.GetErrno() DO
      IF errno = Uerror.EINVAL THEN
        (* special hack to try to get real errno, hidden due to NBIO bug in connect *)
        EVAL TCPHack.RefetchError(fd);
      ELSIF errno = Uerror.EBADF THEN
        (* we'll try the same for EBADF, which we've seen on Alpha *)
        IF TCPHack.RefetchError(fd) 
        THEN seenBadFBug := TRUE 
        END;
      END;
    END;
    WITH errno = Cerrno.GetErrno() DO
      IF (errno = Uerror.EISCONN) THEN
        RETURN TRUE;
      ELSIF  (errno = Uerror.EADDRNOTAVAIL)
          OR (errno = Uerror.ECONNREFUSED)
          OR (errno = Uerror.EINVAL)
          OR (errno = Uerror.ECONNRESET)
          OR (errno = Uerror.EBADF) THEN
        Raise(Refused);
      ELSIF (errno = Uerror.ETIMEDOUT) THEN
        Raise(Timeout);
      ELSIF  (errno = Uerror.ENETUNREACH)
          OR (errno = Uerror.EHOSTUNREACH)
          OR (errno = Uerror.EHOSTDOWN)
          OR (errno = Uerror.ENETDOWN) THEN
        Raise(IP.Unreachable);
      ELSIF  (errno = Uerror.EWOULDBLOCK)
          OR (errno = Uerror.EAGAIN)
          OR (errno = Uerror.EINPROGRESS)
          OR (errno = Uerror.EALREADY) THEN
      ELSE RaiseUnexpected();
      END;
    END;
    RETURN FALSE;
  END CheckConnect;

PROCEDURE Accept(c: Connector): T
    RAISES {IP.Error, Thread.Alerted} =
  VAR
    ep: IP.Endpoint;
  BEGIN
    RETURN AcceptFrom(c, ep);
  END Accept;

PROCEDURE CloseConnector(c: Connector) =
  BEGIN
    LOCK c DO
      IF NOT c.closed THEN
        EVAL Unix.close(c.fd);
        c.closed := TRUE;
      END;
    END;
  END CloseConnector;
  

PROCEDURE EOF(t: T) : BOOLEAN =
  VAR status: INTEGER;  charsToRead: int;
  BEGIN
    LOCK t DO
      IF SchedulerPosix.IOWait(t.fd, TRUE, 0.0D0) =
                            SchedulerPosix.WaitResult.Ready THEN
        status := Unix.ioctl(t.fd, Unix.FIONREAD, ADR(charsToRead));
        RETURN (status = 0) AND (charsToRead = 0);
      END;
    END;
    RETURN FALSE;
  END EOF;


(* methods of TCP.T *)

PROCEDURE InitFD(fd: CARDINAL) =
  (* We assume that the runtime ignores SIGPIPE signals *)
  VAR
    one: int := 1;
    linger := Usocket.struct_linger{1, 1};
  BEGIN
    EVAL Usocket.setsockopt(fd, Usocket.SOL_SOCKET, Usocket.SO_LINGER,
                            ADR(linger), BYTESIZE(linger));
    EVAL Usocket.setsockopt(
           fd, Uin.IPPROTO_TCP, TCP_NODELAY, ADR(one), BYTESIZE(one));
    MakeNonBlocking (fd);
  END InitFD;

PROCEDURE MakeNonBlocking(fd: INTEGER) =
  BEGIN
    IF Unix.fcntl(fd, Unix.F_SETFL, 
        Word.Or(Unix.fcntl(fd, Unix.F_GETFL, 0), Unix.M3_NONBLOCK)) # 0
    THEN
      Die();
    END;
  END MakeNonBlocking;
  
PROCEDURE Close(t: T) =
  BEGIN
    LOCK t DO
      IF NOT t.closed THEN
        EVAL Unix.close(t.fd);
        t.closed := TRUE;
        t.error := ClosedErr;
      END;
    END;
  END Close;
  
PROCEDURE GetBytesFD(
    t: T; VAR arr: ARRAY OF CHAR; timeout: LONGREAL) : CARDINAL
    RAISES {Rd.Failure, ConnFD.TimedOut, Thread.Alerted} =
  VAR 
    len: INTEGER;  
  BEGIN
    LOOP
      LOCK t DO
        IF t.error # NIL THEN RAISE Rd.Failure(t.error); END;
        len := Uuio.read(t.fd, ADR(arr[0]), NUMBER(arr));
      END;
      IF len >= 0 THEN
        RETURN len;
      ELSE
        WITH errno = Cerrno.GetErrno() DO
          IF (errno = Uerror.ECONNRESET) THEN
            RETURN 0;
          ELSIF  (errno = Uerror.EPIPE)
              OR (errno = Uerror.ENETRESET) THEN
            SetError(t,ConnLost);
          ELSIF  (errno = Uerror.ETIMEDOUT) THEN
            SetError(t,Timeout);
          ELSIF  (errno = Uerror.ENETUNREACH)
              OR (errno = Uerror.EHOSTUNREACH)
              OR (errno = Uerror.EHOSTDOWN)
              OR (errno = Uerror.ENETDOWN) THEN
            SetError(t,IP.Unreachable);
          ELSIF  (errno = Uerror.EWOULDBLOCK)
              OR (errno = Uerror.EAGAIN) THEN
            IF timeout = 0.0D0 OR
                   SchedulerPosix.IOAlertWait(t.fd, TRUE, timeout) =
                       SchedulerPosix.WaitResult.Timeout THEN
              RAISE ConnFD.TimedOut;
            END;
          ELSE
            SetError(t,Unexpected);
          END;
        END;
      END;
    END;
  END GetBytesFD;

PROCEDURE PutBytesFD(t: T; READONLY arr: ARRAY OF CHAR)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR pos := 0;
      len: INTEGER;
  BEGIN
    WHILE pos # NUMBER(arr) DO
      LOCK t DO
        IF t.error # NIL THEN RAISE Wr.Failure(t.error); END;
        len := Uuio.write(t.fd, ADR(arr[pos]), NUMBER(arr)-pos);
      END;
      IF len >= 0 THEN
        INC(pos, len)
      ELSE
        WITH errno = Cerrno.GetErrno() DO
          IF     (errno = Uerror.EPIPE)
              OR (errno = Uerror.ECONNRESET)
              OR (errno = Uerror.ENETRESET) THEN
            SetError(t,ConnLost);
          ELSIF (errno = Uerror.ETIMEDOUT) THEN
            SetError(t,Timeout);
          ELSIF  (errno = Uerror.ENETUNREACH)
              OR (errno = Uerror.EHOSTUNREACH)
              OR (errno = Uerror.EHOSTDOWN)
              OR (errno = Uerror.ENETDOWN) THEN
            SetError(t,IP.Unreachable);
          ELSIF  (errno = Uerror.EWOULDBLOCK)
              OR (errno = Uerror.EAGAIN) THEN
            EVAL SchedulerPosix.IOAlertWait(t.fd, FALSE);
            (* IF Thread.TestAlert() THEN RAISE Thread.Alerted END *)
          ELSE
            SetError(t,Unexpected);
          END
        END
      END
    END;
  END PutBytesFD;

VAR lastErrorMu := NEW(MUTEX);
    lastErrors: ARRAY [0..19] OF INTEGER;
    lastErrorPos: CARDINAL := 0;

PROCEDURE SetError(t: T; atom: Atom.T) =
  BEGIN
    LOCK t DO
      WITH errno = Cerrno.GetErrno() DO
        t.error := AtomList.List2(atom, Atom.FromText(Fmt.Int(errno)));
        LOCK lastErrorMu DO
          lastErrors[lastErrorPos] := errno;
          INC(lastErrorPos);
          IF lastErrorPos >= NUMBER(lastErrors) THEN lastErrorPos := 0; END;
        END;
      END;
    END;
  END SetError;

PROCEDURE ShutdownIn(t: T) RAISES {Rd.Failure} =
  BEGIN
    LOCK t DO
      IF t.error # NIL THEN RAISE Rd.Failure(t.error); END;
      EVAL Usocket.shutdown(t.fd, 0);
    END;
  END ShutdownIn;

PROCEDURE ShutdownOut(t: T) RAISES {Wr.Failure} =
  BEGIN
    LOCK t DO
      IF t.error # NIL THEN RAISE Wr.Failure(t.error); END;
      EVAL Usocket.shutdown(t.fd, 1);
    END;
  END ShutdownOut;

PROCEDURE Raise(a: Atom.T) RAISES {IP.Error} =
  BEGIN
    RAISE IP.Error(AtomList.List2(a,
                                  Atom.FromText(Fmt.Int(Cerrno.GetErrno()))));
  END Raise;

PROCEDURE RaiseUnexpected() RAISES {IP.Error} =
  BEGIN
    Raise(Unexpected);
  END RaiseUnexpected;

 PROCEDURE RaiseNoEC(a: Atom.T) RAISES {IP.Error} =
  BEGIN
    RAISE IP.Error(AtomList.List1(a));
  END RaiseNoEC;
  
EXCEPTION FatalError;

PROCEDURE Die() RAISES {} =
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError;
  END Die;

(*****************************************************************************)
(* TCPMisc procedures. *)
(*****************************************************************************)

PROCEDURE AcceptFrom(c: Connector; VAR (*OUT*) peer: IP.Endpoint): T
    RAISES {IP.Error, Thread.Alerted} =
  VAR
    addr                 : SockAddrIn;
    addrSize             : socklen_t := BYTESIZE(addr);
    fd                   : INTEGER;
  BEGIN
    LOOP
      LOCK c DO
        IF c.closed THEN RaiseNoEC(Closed); END;
        fd := Usocket.accept(c.fd, ADR(addr), ADR(addrSize));
      END;
      WITH errno = Cerrno.GetErrno() DO
        IF fd >= 0 THEN
          EXIT
        ELSIF errno = Uerror.EMFILE OR errno = Uerror.ENFILE THEN
          Raise(IP.NoResources);
        ELSIF
          errno = Uerror.EWOULDBLOCK OR errno = Uerror.EAGAIN
          THEN
          EVAL SchedulerPosix.IOAlertWait(c.fd, TRUE);
        ELSE
          RaiseUnexpected();
        END
      END
    END;
    InitFD(fd);
    peer.addr := LOOPHOLE(addr.sin_addr, IP.Address);
    peer.port := Uin.ntohs(addr.sin_port);
    RETURN NEW(T, fd := fd, ep := IP.NullEndPoint);
  END AcceptFrom;

PROCEDURE CoalesceWrites(tcp: T; allow: BOOLEAN)
  RAISES {IP.Error} =
  VAR
    noDelay: int;
  BEGIN
    IF allow THEN noDelay := 0 ELSE noDelay := 1 END;

    LOCK tcp DO
      IF tcp.closed THEN
        RAISE IP.Error(AtomList.List1(Closed));
      END;
      IF Usocket.setsockopt(tcp.fd, Uin.IPPROTO_TCP, TCP_NODELAY,
        ADR(noDelay), BYTESIZE(noDelay)) = -1 THEN
        RaiseUnexpected();
      END;
    END;
  END CoalesceWrites;

PROCEDURE ConnectFrom(to, from: IP.Endpoint): T
  RAISES {IP.Error, Thread.Alerted} =
  VAR
    t := StartConnect(to, from);
    ok := FALSE;
  BEGIN
    TRY
      EVAL FinishConnect(t);
      ok := TRUE;
    FINALLY
     IF NOT ok THEN Close(t); END;
    END;
    RETURN t;
  END ConnectFrom;

PROCEDURE GetPeerName(tcp: T): IP.Endpoint
  RAISES {IP.Error} =
  VAR
    addr: SockAddrIn;
    len: socklen_t := BYTESIZE(addr);
    ep: IP.Endpoint;
  BEGIN
    LOCK tcp DO
      IF tcp.closed THEN
        RAISE IP.Error(AtomList.List1(Closed));
      END;
      IF Usocket.getpeername(tcp.fd, ADR(addr), ADR(len)) = -1 THEN
        RaiseUnexpected();
      END;
    END;

    ep.addr := LOOPHOLE(addr.sin_addr, IP.Address);
    ep.port := Uin.ntohs(addr.sin_port);
    RETURN ep;
  END GetPeerName;

PROCEDURE GetSockName(tcp: T): IP.Endpoint
  RAISES {IP.Error} =
  VAR
    addr: SockAddrIn;
    len: socklen_t := BYTESIZE(addr);
    ep: IP.Endpoint;
  BEGIN
    LOCK tcp DO
      IF tcp.closed THEN
        RAISE IP.Error(AtomList.List1(Closed));
      END;
      IF Usocket.getsockname(tcp.fd, ADR(addr), ADR(len)) = -1 THEN
        RaiseUnexpected();
      END;
    END;

    ep.addr := LOOPHOLE(addr.sin_addr, IP.Address);
    ep.port := Uin.ntohs(addr.sin_port);
    RETURN ep;
  END GetSockName;

PROCEDURE KeepAlive(tcp: T; allow: BOOLEAN)
  RAISES {IP.Error} =
  VAR
    keepAlive: int;
  BEGIN
    IF allow THEN keepAlive := 1 ELSE keepAlive := 0 END;

    LOCK tcp DO
      IF tcp.closed THEN
        RAISE IP.Error(AtomList.List1(Closed));
      END;
      IF Usocket.setsockopt(tcp.fd, Usocket.SOL_SOCKET, Usocket.SO_KEEPALIVE,
        ADR(keepAlive), BYTESIZE(keepAlive)) = -1 THEN
        RaiseUnexpected();
      END;
    END;
  END KeepAlive;

PROCEDURE LingerOnClose(tcp: T; allow: BOOLEAN)
  RAISES {IP.Error} =
  VAR
    linger: Usocket.struct_linger;
  BEGIN
    IF allow THEN
      linger.l_onoff := 1;
      linger.l_linger := 1;
    ELSE
      linger.l_onoff := 0;
      linger.l_linger := 0;
    END;

    LOCK tcp DO
      IF tcp.closed THEN
        RAISE IP.Error(AtomList.List1(Closed));
      END;
      IF Usocket.setsockopt(tcp.fd, Usocket.SOL_SOCKET, Usocket.SO_LINGER,
        ADR(linger), BYTESIZE(linger)) = -1 THEN
        RaiseUnexpected();
      END;
    END;
  END LingerOnClose;

BEGIN
  Refused := Atom.FromText("TCP.Refused");
  Closed := Atom.FromText("TCP.Closed");
  Timeout := Atom.FromText("TCP.Timeout");
  ConnLost := Atom.FromText("TCP.ConnLost");
  Unexpected := Atom.FromText("TCP.Unexpected");
  ClosedErr := AtomList.List1(Closed);
END TCP.
