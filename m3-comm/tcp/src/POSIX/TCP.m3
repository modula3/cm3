(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by gnelson *)
(* Last modified on Mon Mar 27 17:18:33 PST 1995 by wobber *)
(*      modified on Tue Feb  7 15:48:33 PST 1995 by kalsow *)
(*      modified on Fri Jan  7 13:31:11 PST 1994 by msm    *)
(*      modified on Sun Jan 12 16:16:54 PST 1992 by meehan *)
(*      modified on Sat Jan 11 16:55:00 PST 1992 by gnelson *)

UNSAFE MODULE TCP EXPORTS TCP, TCPSpecial;

IMPORT Atom, AtomList, ConnFD, IP, IPError, Rd, Wr, Thread;
IMPORT Usocket, Cerrno, Uerror, Uin, Unix, Uuio, Utypes,
       TCPHack, TCPPosix, SchedulerPosix, Fmt, Word;
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

PROCEDURE NewConnector (ep: IP.Endpoint): Connector RAISES {IP.Error} =
  VAR
    res                := NEW(Connector, ep := ep);
    name  : SockAddrIn;
    status: INTEGER;
    True               := 1;
  BEGIN
    res.fd := Usocket.socket(Usocket.AF_INET, Usocket.SOCK_STREAM, 0 (* TCP*));
    IF res.fd = -1 THEN
      IF Cerrno.errno = Uerror.EMFILE OR Cerrno.errno = Uerror.ENFILE THEN
        IPError.Raise(IP.NoResources);
      ELSE
        IPError.RaiseUnexpected();
      END
    END;
    MakeNonBlocking (res.fd);
    EVAL Usocket.setsockopt(
           res.fd, Usocket.SOL_SOCKET, Usocket.SO_REUSEADDR, ADR(True),
           BYTESIZE(True));
    name.sin_family := Usocket.AF_INET;
    name.sin_port := Uin.htons(ep.port);
    name.sin_addr.s_addr := LOOPHOLE(ep.addr, Utypes.u_int);
    name.sin_zero := Sin_Zero;
    status := Usocket.bind(res.fd, ADR(name), BYTESIZE(SockAddrIn));
    IF status # 0 THEN
      IF Cerrno.errno = Uerror.EADDRINUSE THEN
        IPError.Raise(IP.PortBusy);
      ELSE
        IPError.RaiseUnexpected();
      END
    END;
    IF Usocket.listen(res.fd, 8) # 0 THEN IPError.RaiseUnexpected(); END;
    RETURN res
  END NewConnector;

PROCEDURE GetEndPoint(c: Connector): IP.Endpoint =
  VAR
    namelen  : INTEGER;
    name  : SockAddrIn;
  BEGIN
    IF c.ep.addr = IP.NullAddress THEN
      c.ep.addr := IP.GetHostAddr();
    END;
    IF c.ep.port = IP.NullPort THEN
      namelen := BYTESIZE(SockAddrIn);
      IF Usocket.getsockname(c.fd, ADR(name), ADR(namelen)) # 0 THEN
        IPError.Die()
      END;
      c.ep.port := Uin.ntohs(name.sin_port);
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

PROCEDURE StartConnect(ep: IP.Endpoint): T
    RAISES {IP.Error} =
  VAR
    fd: INTEGER;
    ok := FALSE;
  BEGIN
    fd := Usocket.socket(Usocket.AF_INET, Usocket.SOCK_STREAM, 0 (* TCP*));
    IF fd < 0 THEN
      IF Cerrno.errno = Uerror.EMFILE OR Cerrno.errno = Uerror.ENFILE THEN
        IPError.Raise(IP.NoResources);
      ELSE
        IPError.RaiseUnexpected();
      END;
    END;
    InitFD(fd);
    TRY
      EVAL CheckConnect(fd, ep);
      ok := TRUE;
    FINALLY
      IF NOT ok THEN EVAL Unix.close(fd); END;
    END;
    RETURN NEW(T, fd := fd, ep := ep);
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
    IF Cerrno.errno = Uerror.EINVAL THEN
      (* special hack to try to get real errno, hidden due to NBIO bug in connect *)
      EVAL TCPHack.RefetchError(fd);
    ELSIF Cerrno.errno = Uerror.EBADF THEN
      (* we'll try the same for EBADF, which we've seen on Alpha *)
      IF TCPHack.RefetchError(fd) THEN seenBadFBug := TRUE END;
    END;
    CASE Cerrno.errno OF
    | Uerror.EISCONN => RETURN TRUE;
    | Uerror.EADDRNOTAVAIL,  Uerror.ECONNREFUSED, Uerror.EINVAL,
               Uerror.ECONNRESET, Uerror.EBADF =>
        IPError.Raise(Refused);
    | Uerror.ETIMEDOUT =>
        IPError.Raise(Timeout);
    | Uerror.ENETUNREACH, Uerror.EHOSTUNREACH,  Uerror.EHOSTDOWN, Uerror.ENETDOWN =>
        IPError.Raise(IP.Unreachable);
    | <*NOWARN*> Uerror.EWOULDBLOCK, Uerror.EAGAIN,  Uerror.EINPROGRESS, Uerror.EALREADY =>
    ELSE IPError.RaiseUnexpected();
    END;
    RETURN FALSE;
  END CheckConnect;

PROCEDURE Accept (c: Connector): T
    RAISES {IP.Error, Thread.Alerted} =
  VAR
    name                 : SockAddrIn;
    nameSize             : INTEGER      := BYTESIZE(name);
    fd                   : INTEGER;
  BEGIN
    LOOP
      LOCK c DO
        IF c.closed THEN IPError.Raise(Closed); END;
        fd := Usocket.accept(c.fd, ADR(name), ADR(nameSize));
      END;
      IF fd >= 0 THEN
        EXIT
      ELSIF Cerrno.errno = Uerror.EMFILE OR Cerrno.errno = Uerror.ENFILE THEN
        IPError.Raise(IP.NoResources);
      ELSIF
        Cerrno.errno = Uerror.EWOULDBLOCK OR Cerrno.errno = Uerror.EAGAIN
        THEN
        EVAL SchedulerPosix.IOAlertWait(c.fd, TRUE);
      ELSE
        IPError.RaiseUnexpected();
      END
    END;
    InitFD(fd);
    RETURN NEW(T, fd := fd, ep := IP.NullEndPoint);
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

(*
VAR SysSendBufSize: INTEGER := 128000;
VAR SysRcvBufSize: INTEGER := 128000;
*)

PROCEDURE InitFD(fd: CARDINAL) =
  (* We assume that the runtime ignores SIGPIPE signals *)
  VAR
    one := 1;
    linger := Usocket.struct_linger{1, 1};
  BEGIN
(*
    EVAL Usocket.setsockopt(fd, Usocket.SOL_SOCKET, Usocket.SO_SNDBUF,
                            ADR(SysSendBufSize), BYTESIZE(SysSendBufSize));
    EVAL Usocket.setsockopt(fd, Usocket.SOL_SOCKET, Usocket.SO_RCVBUF,
                            ADR(SysRcvBufSize), BYTESIZE(SysRcvBufSize));
*)
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
      IPError.Die();
    END;
  END MakeNonBlocking;
  
PROCEDURE Close(t: T) =
  BEGIN
    LOCK t DO
      IF NOT t.closed THEN
        EVAL Unix.close(t.fd);
        t.closed := TRUE;
        t.error := IPError.ClosedErr;
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
        CASE Cerrno.errno OF
        | Uerror.ECONNRESET => RETURN 0;
        | Uerror.EPIPE, Uerror.ENETRESET => SetError(t,ConnLost);
        | Uerror.ETIMEDOUT => SetError(t,Timeout);
        | Uerror.ENETUNREACH, Uerror.EHOSTUNREACH,
             Uerror.EHOSTDOWN, Uerror.ENETDOWN => SetError(t,IP.Unreachable);
        | <*NOWARN*> Uerror.EWOULDBLOCK, Uerror.EAGAIN =>
            IF timeout = 0.0D0 OR
                   SchedulerPosix.IOAlertWait(t.fd, TRUE, timeout) =
                       SchedulerPosix.WaitResult.Timeout THEN
              RAISE ConnFD.TimedOut;
            END;
        ELSE
            SetError(t, IPError.Unexpected);
        END;
      END;
    END;
  END GetBytesFD;

PROCEDURE PutBytesFD(t: T; VAR arr: ARRAY OF CHAR)
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
        CASE Cerrno.errno OF
        | Uerror.EPIPE, Uerror.ECONNRESET, Uerror.ENETRESET => SetError(t,ConnLost);
        | Uerror.ETIMEDOUT => SetError(t,Timeout);
        | Uerror.ENETUNREACH, Uerror.EHOSTUNREACH,
             Uerror.EHOSTDOWN, Uerror.ENETDOWN => SetError(t,IP.Unreachable);
        | <*NOWARN*> Uerror.EWOULDBLOCK, Uerror.EAGAIN =>
             EVAL SchedulerPosix.IOAlertWait(t.fd, FALSE);
             (* IF Thread.TestAlert() THEN RAISE Thread.Alerted END *)
        ELSE
            SetError(t, IPError.Unexpected);
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
      t.error := AtomList.List2(atom, Atom.FromText(Fmt.Int(Cerrno.errno)));
      LOCK lastErrorMu DO
        lastErrors[lastErrorPos] := Cerrno.errno;
        INC(lastErrorPos);
        IF lastErrorPos >= NUMBER(lastErrors) THEN lastErrorPos := 0; END;
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

BEGIN
END TCP.


