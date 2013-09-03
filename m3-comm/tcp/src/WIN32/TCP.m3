(* Copyright (C) 1995, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by gnelson       *)
(*                                                          *)
(* Last modified on Tue Aug 15 13:41:05 PDT 1995 by steveg  *)
(*      modified on Mon Apr 10 16:36:03 PDT 1995 by kalsow  *)
(*      modified on Wed Feb  8 11:16:58 PST 1995 by wobber  *)
(*      modified on Fri Jan  7 13:31:11 PST 1994 by msm     *)
(*      modified on Sun Jan 12 16:16:54 PST 1992 by meehan  *)
(*      modified on Sat Jan 11 16:55:00 PST 1992 by gnelson *)

UNSAFE MODULE TCP EXPORTS TCP, TCPMisc, TCPSpecial;

IMPORT Atom, AtomList, ConnFD, IP, IPError, Rd, Wr, Thread;
IMPORT Ctypes, WinSock, TCPWin32, Fmt;
FROM WinDef IMPORT BOOL;

REVEAL
  Connector = MUTEX BRANDED "TCP.Connector" OBJECT
    sock: WinSock.SOCKET;  (*CONST*)
    ep: IP.Endpoint;   (*CONST*)
    closed: BOOLEAN := FALSE;
  END;

REVEAL
  T = TCPWin32.Public BRANDED "TCP.T" OBJECT
    ep: IP.Endpoint;
    error: AtomList.T := NIL;
  OVERRIDES
    get := GetBytesFD;
    put := PutBytesFD;
    shutdownIn := ShutdownIn;
    shutdownOut := ShutdownOut;
    close := Close;
  END;

TYPE SockAddrIn = WinSock.struct_sockaddr_in;

TYPE WaitResult = {Ready, Error, Timeout};

CONST Sin_Zero = ARRAY [0 .. 7] OF Ctypes.char{VAL(0, Ctypes.char), ..};
CONST SockErr  = WinSock.SOCKET_ERROR;

CONST SpinTimeout = 1.0D0;        (* one second *)

PROCEDURE NewSocket (): WinSock.SOCKET RAISES {IP.Error} =
  VAR
    sock := WinSock.socket(WinSock.AF_INET, WinSock.SOCK_STREAM, 0(*TCP*));
    err  : INTEGER;
  BEGIN
    IF sock = WinSock.INVALID_SOCKET THEN
      err := WinSock.WSAGetLastError();
      IF err = WinSock.WSAEMFILE
        THEN IPError.Raise(IP.NoResources, err);
        ELSE Ouch(err, "TCP.NewSocket");
      END;
    END;
    RETURN sock;
  END NewSocket;

PROCEDURE NewConnector (ep: IP.Endpoint): Connector RAISES {IP.Error} =
  VAR
    res   := NEW(Connector, ep := ep);
    name  : SockAddrIn;
    True: Ctypes.int := 1;
    err   : INTEGER;
  BEGIN
    res.sock := NewSocket();
    InitSock(res.sock);
    EVAL WinSock.setsockopt(
           res.sock, WinSock.SOL_SOCKET, WinSock.SO_REUSEADDR,
           ADR(True), BYTESIZE(True));
    name.sin_family := WinSock.AF_INET;
    name.sin_port := WinSock.htons(ep.port);
    name.sin_addr.s_addr := LOOPHOLE(ep.addr, WinSock.u_long);
    name.sin_zero := Sin_Zero;
    IF WinSock.bind(res.sock, ADR(name), BYTESIZE(SockAddrIn)) = SockErr THEN
      err := WinSock.WSAGetLastError();
      IF err = WinSock.WSAEADDRINUSE
        THEN IPError.Raise(IP.PortBusy, err);
        ELSE Ouch(err, "TCP.NewConnector(bind)");
      END
    END;
    IF WinSock.listen(res.sock, 8) = SockErr THEN
      Ouch(WinSock.WSAGetLastError(), "TCP.NewConnector(listen)");
    END;
    RETURN res
  END NewConnector;

PROCEDURE GetEndPoint(c: Connector): IP.Endpoint =
  VAR
    namelen : INTEGER;
    name    : SockAddrIn;
  BEGIN
    IF c.ep.addr = IP.NullAddress THEN
      c.ep.addr := IP.GetHostAddr();
    END;
    IF c.ep.port = IP.NullPort THEN
      namelen := BYTESIZE(SockAddrIn);
      IF WinSock.getsockname(c.sock, ADR(name), ADR(namelen)) = SockErr THEN
        IPError.Die()
      END;
      c.ep.port := WinSock.ntohs(name.sin_port);
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
    sock : WinSock.SOCKET;
    ok   := FALSE;
    status: Ctypes.int;
    True: Ctypes.int := 1;
    fromName: SockAddrIn;
  BEGIN
    sock := NewSocket();
    InitSock(sock);

    IF from # IP.NullEndPoint THEN  (* Bind to the "from" address. *)
      EVAL WinSock.setsockopt(sock, WinSock.SOL_SOCKET, WinSock.SO_REUSEADDR,
        ADR(True), BYTESIZE(True));
      fromName.sin_family := WinSock.AF_INET;
      fromName.sin_port := WinSock.htons(from.port);
      fromName.sin_addr.s_addr := LOOPHOLE(from.addr, WinSock.u_long);
      fromName.sin_zero := Sin_Zero;
      status := WinSock.bind(sock, ADR(fromName), BYTESIZE(SockAddrIn));
      IF status # 0 THEN
        IF WinSock.WSAGetLastError() = WinSock.WSAEADDRINUSE THEN
          RaiseWSA(IP.PortBusy);
        ELSE
          RaiseUnexpected();
        END
      END;
    END;

    TRY
      EVAL CheckConnect(sock, to);
      ok := TRUE;
    FINALLY
      IF NOT ok THEN EVAL WinSock.closesocket(sock); END;
    END;
    RETURN NEW(T, sock := sock, ep := to);
  END StartConnect;

PROCEDURE FinishConnect(t: T; timeout: LONGREAL := -1.0D0): BOOLEAN
    RAISES {IP.Error, Thread.Alerted} =
  VAR 
    fdw, fde: WinSock.struct_fd_set;
    tm: WinSock.struct_timeval;
    tmo := SpinTimeout;
  BEGIN
    LOCK t DO
      IF t.error # NIL THEN RAISE IP.Error(t.error); END;

      (* Wait for the connection to finish *)
      LOOP
        IF timeout >= 0.0D0 THEN
          tmo := MIN(tmo, timeout);
        END;
        WinSock.FD_ZERO(fdw);
        WinSock.FD_SET(t.sock, fdw);
        WinSock.FD_ZERO(fde);
        WinSock.FD_SET(t.sock, fde);
        tm.tv_sec := FLOOR(tmo);
        tm.tv_usec := FLOOR(1.0D6 * (tmo - FLOAT(tm.tv_sec, LONGREAL)));
        WITH x = WinSock.select(t.sock+1, NIL, ADR(fdw), 
                                ADR(fde), ADR(tm)) DO
          IF Thread.TestAlert() THEN RAISE Thread.Alerted; END;
          IF x = SockErr THEN 
            Ouch(WinSock.WSAGetLastError(), "TCP.FinishConnect");
          END;
          IF WinSock.FD_ISSET(t.sock, fdw) THEN
            (* connect succeeded *)
            RETURN TRUE;
          END;
          IF WinSock.FD_ISSET(t.sock, fde) THEN
            (* connect failed *)
            IPError.Raise(Refused, 0);
          END;
        END;
        IF timeout >= 0.0D0 THEN
          IF timeout <= tmo THEN RETURN FALSE; END;
          timeout := timeout - tmo;
        END;
      END;
    END;
  END FinishConnect;

PROCEDURE CheckConnect(sock: WinSock.SOCKET; ep: IP.Endpoint) : BOOLEAN
    RAISES {IP.Error} =
  VAR 
    name: SockAddrIn;  
    res, err: INTEGER;
  BEGIN
    name.sin_family := WinSock.AF_INET;
    name.sin_port := WinSock.htons(ep.port);
    name.sin_addr.s_addr := LOOPHOLE(ep.addr, WinSock.u_long);
    name.sin_zero := Sin_Zero;
    res := WinSock.connect(sock, ADR(name), BYTESIZE(SockAddrIn));
    IF res = 0 THEN RETURN TRUE; END;
    err := WinSock.WSAGetLastError();
    (*IO.Put("TCP:  err = " & Fmt.Int(err) & "\n");*)
    CASE err OF
    | WinSock.WSAEISCONN =>
        RETURN TRUE;
    | WinSock.WSAEADDRNOTAVAIL,
      WinSock.WSAECONNREFUSED,
      WinSock.WSAECONNRESET =>
        IPError.Raise(Refused, err);
    | WinSock.WSAETIMEDOUT =>
        IPError.Raise(Timeout, err);
    | WinSock.WSAENETUNREACH,
      WinSock.WSAEHOSTUNREACH,
      WinSock.WSAEHOSTDOWN,
      WinSock.WSAENETDOWN =>
        IPError.Raise(IP.Unreachable, err);
    | WinSock.WSAEWOULDBLOCK =>
        (* fall through => return false *)
    | WinSock.WSAEINVAL =>
        (* WindowsNT 3.5 acts as though EINVAL means "not ready" *)
    ELSE
        Ouch(err, "TCP.CheckConnect");
    END;
    RETURN FALSE;
  END CheckConnect;

PROCEDURE Accept (c: Connector): T
    RAISES {IP.Error, Thread.Alerted} =
  VAR
    name      : SockAddrIn;
    nameSize  : INTEGER      := BYTESIZE(name);
    sock      : WinSock.SOCKET;
    err       : INTEGER;
  BEGIN
    LOOP
      LOCK c DO
        IF c.closed THEN IPError.Raise(Closed, 0); END;
        sock := WinSock.accept(c.sock, ADR(name), ADR(nameSize));
        IF sock # WinSock.INVALID_SOCKET THEN EXIT; END;
        err := WinSock.WSAGetLastError();
      END;
      IF    err = WinSock.WSAEMFILE      THEN  IPError.Raise(IP.NoResources, err);
      ELSIF err = WinSock.WSAEWOULDBLOCK THEN  EVAL IOWait(c.sock, TRUE, TRUE);
      ELSE                                     Ouch(err, "TCP.Accept");
      END;
    END;
    InitSock(sock);
    RETURN NEW(T, sock := sock, ep := IP.NullEndPoint);
  END Accept;

PROCEDURE CloseConnector(c: Connector) =
  BEGIN
    LOCK c DO
      IF NOT c.closed THEN
        EVAL WinSock.closesocket(c.sock);
        c.closed := TRUE;
      END;
    END;
  END CloseConnector;
  
PROCEDURE EOF(t: T) : BOOLEAN =
  VAR
    ec: Ctypes.int;
    charsToRead: WinSock.u_long;
    <* FATAL Thread.Alerted *>
  BEGIN
    LOCK t DO
      IF IOWait(t.sock, TRUE, FALSE, 0.0D0) = WaitResult.Ready THEN
        ec := WinSock.ioctlsocket(t.sock, WinSock.FIONREAD, ADR(charsToRead));
        RETURN (ec = 0) AND (charsToRead = 0);
      END;
    END;
    RETURN FALSE;
  END EOF;


(* methods of TCP.T *)

PROCEDURE InitSock(sock: WinSock.SOCKET) =
  (* We assume that the runtime ignores SIGPIPE signals *)
  VAR
    one : BOOL := 1;
    linger := WinSock.struct_linger{0, 0};
  BEGIN
    EVAL WinSock.setsockopt(
           sock, WinSock.SOL_SOCKET, WinSock.SO_LINGER,
           ADR(linger), BYTESIZE(linger));
    EVAL WinSock.setsockopt(
           sock, WinSock.IPPROTO_TCP, WinSock.TCP_NODELAY,
           ADR(one), BYTESIZE(one));
    IF WinSock.ioctlsocket(sock, WinSock.FIONBIO, ADR(one)) = SockErr THEN
      IPError.Die();
    END;
  END InitSock;

PROCEDURE Close(t: T) =
  BEGIN
    LOCK t DO
      IF NOT t.closed THEN
        EVAL WinSock.closesocket(t.sock);
        t.closed := TRUE;
        t.error := IPError.ClosedErr;
      END;
    END;
  END Close;
  
PROCEDURE GetBytesFD(
    t: T; VAR arr: ARRAY OF CHAR; timeout: LONGREAL) : CARDINAL
    RAISES {Rd.Failure, ConnFD.TimedOut, Thread.Alerted} =
  VAR len: Ctypes.int;  err: INTEGER;
  BEGIN
    LOOP
      LOCK t DO
        IF t.error # NIL THEN RAISE Rd.Failure(t.error); END;
        len := WinSock.recv(t.sock, ADR(arr[0]), NUMBER(arr), 0);
        IF len # SockErr THEN RETURN len; END;
        err := WinSock.WSAGetLastError();
      END;
      CASE err OF
      | WinSock.WSAECONNRESET =>
          RETURN 0;
      | WinSock.WSAENETRESET =>
          SetError(t, ConnLost, err);
      | WinSock.WSAENETUNREACH,
        WinSock.WSAEHOSTUNREACH,
        WinSock.WSAEHOSTDOWN,
        WinSock.WSAENETDOWN =>
          SetError(t, IP.Unreachable, err);
      | WinSock.WSAEWOULDBLOCK =>
          IF timeout = 0.0D0 OR
                IOWait(t.sock, TRUE, TRUE, timeout) = WaitResult.Timeout THEN
            RAISE ConnFD.TimedOut;
          END;
      ELSE
          SetError(t, IPError.Unexpected, err, "TCP.GetBytesFD");
      END;
      (* loop to raise error *)
    END;
  END GetBytesFD;

PROCEDURE PutBytesFD(t: T; READONLY arr: ARRAY OF CHAR)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR pos := 0;  len: Ctypes.int;  err: INTEGER;
  BEGIN
    WHILE pos # NUMBER(arr) DO
      LOCK t DO
        IF t.error # NIL THEN RAISE Wr.Failure(t.error); END;
        len := WinSock.send(t.sock, ADR(arr[pos]), NUMBER(arr)-pos, 0);
        IF len = SockErr THEN  err := WinSock.WSAGetLastError();  END;
      END;
      IF len = SockErr THEN
        CASE err OF
        | WinSock.WSAECONNRESET,
          WinSock.WSAENETRESET =>
            SetError(t, ConnLost, err);
        | WinSock.WSAENETUNREACH,
          WinSock.WSAEHOSTUNREACH,
          WinSock.WSAEHOSTDOWN,
          WinSock.WSAENETDOWN =>
            SetError(t, IP.Unreachable, err);
        | WinSock.WSAEWOULDBLOCK =>
            EVAL IOWait(t.sock, FALSE, TRUE);
        ELSE
            SetError(t, IPError.Unexpected, err, "TCP.PutBytesFD");
        END;
      ELSE
        INC(pos, len)
      END;
    END;
  END PutBytesFD;

PROCEDURE SetError(t: T; atom: Atom.T;  err: INTEGER;  msg: TEXT := NIL) =
  VAR xx: AtomList.T := NIL;
  BEGIN
    IF (msg # NIL) THEN xx := AtomList.Cons (Atom.FromText(msg), NIL); END;
    xx := AtomList.Cons(Atom.FromText(Fmt.Int(err)), xx);
    xx := AtomList.Cons(atom, xx);
    LOCK t DO
      t.error := xx;
    END;
  END SetError;

PROCEDURE Ouch(err: INTEGER;  msg: TEXT) RAISES {IP.Error} =
  BEGIN
    RAISE IP.Error(AtomList.List3(IPError.Unexpected,
                                  Atom.FromText(Fmt.Int(err)),
                                  Atom.FromText(msg)));
  END Ouch;

PROCEDURE ShutdownIn(t: T) RAISES {Rd.Failure} =
  BEGIN
    LOCK t DO
      IF t.error # NIL THEN RAISE Rd.Failure(t.error); END;
      EVAL WinSock.shutdown(t.sock, 0);
    END;
  END ShutdownIn;

PROCEDURE ShutdownOut(t: T) RAISES {Wr.Failure} =
  BEGIN
    LOCK t DO
      IF t.error # NIL THEN RAISE Wr.Failure(t.error); END;
      EVAL WinSock.shutdown(t.sock, 1);
    END;
  END ShutdownOut;

PROCEDURE IOWait(sock: WinSock.SOCKET; read: BOOLEAN; alert: BOOLEAN;
                  timeoutInterval: LONGREAL := -1.0D0): WaitResult
                  RAISES {Thread.Alerted} =
  VAR
    x: Ctypes.int;
    fds: WinSock.struct_fd_set;
    tm: WinSock.struct_timeval;
    tmo := SpinTimeout;
  BEGIN
    LOOP
      WinSock.FD_ZERO(fds);
      WinSock.FD_SET(sock, fds);
      IF timeoutInterval >= 0.0D0 THEN
        tmo := MIN(tmo, timeoutInterval);
      END;
      tm.tv_sec := FLOOR(tmo);
      tm.tv_usec := FLOOR(1.0D6 * (tmo - FLOAT(tm.tv_sec, LONGREAL)));
      IF read THEN
        x := WinSock.select(sock+1, ADR(fds), NIL, ADR(fds), ADR(tm));
      ELSE
        x := WinSock.select(sock+1, NIL, ADR(fds), ADR(fds), ADR(tm));
      END;
      IF alert AND Thread.TestAlert() THEN RAISE Thread.Alerted; END;
      IF x > 0 THEN RETURN WaitResult.Ready; END;
      IF x = SockErr THEN RETURN WaitResult.Error; END;
      IF timeoutInterval >= 0.0D0 THEN
        IF timeoutInterval <= tmo THEN RETURN WaitResult.Timeout; END;
        timeoutInterval := timeoutInterval - tmo;
      END;
    END;
  END IOWait;

PROCEDURE RaiseWSA(a: Atom.T) RAISES {IP.Error} =
  BEGIN
    RAISE IP.Error(AtomList.List2(
                       a, Atom.FromText(Fmt.Int(WinSock.WSAGetLastError()))));
  END RaiseWSA;

PROCEDURE RaiseUnexpected() RAISES {IP.Error} =
  BEGIN
    RaiseWSA(Unexpected);
  END RaiseUnexpected;

 PROCEDURE RaiseNoEC(a: Atom.T) RAISES {IP.Error} =
  BEGIN
    RAISE IP.Error(AtomList.List1(a));
  END RaiseNoEC;
  
(*****************************************************************************)
(* TCPMisc procedures. *)
(*****************************************************************************)

PROCEDURE AcceptFrom(c: Connector; VAR (*OUT*) peer: IP.Endpoint): T
    RAISES {IP.Error, Thread.Alerted} =
  VAR
    addr: SockAddrIn;
    addrSize: Ctypes.int := BYTESIZE(addr);
    sock: WinSock.SOCKET;
  BEGIN
    LOOP
      LOCK c DO
        IF c.closed THEN RaiseNoEC(Closed); END;
        sock := WinSock.accept(c.sock, ADR(addr), ADR(addrSize));
      END;
      IF sock # WinSock.INVALID_SOCKET THEN EXIT; END;
      WITH errno = WinSock.WSAGetLastError() DO
        IF errno = WinSock.WSAEMFILE (* OR errno = WinSock.WSAENFILE *) THEN
          RaiseWSA(IP.NoResources);
        ELSIF
          errno = WinSock.WSAEWOULDBLOCK (* OR errno = WinSock.WSAEAGAIN *)
          THEN
          EVAL IOWait(c.sock, TRUE, TRUE);
          (* SchedulerPosix.IOAlertWait(c.sock, TRUE); *)
        ELSE
          RaiseUnexpected();
        END
      END
    END;
    InitSock(sock);
    peer.addr := LOOPHOLE(addr.sin_addr, IP.Address);
    peer.port := WinSock.ntohs(addr.sin_port);
    RETURN NEW(T, sock := sock, ep := IP.NullEndPoint);
  END AcceptFrom;

PROCEDURE CoalesceWrites(tcp: T; allow: BOOLEAN)
  RAISES {IP.Error} =
  VAR
    noDelay: Ctypes.int;
  BEGIN
    IF allow THEN noDelay := 0 ELSE noDelay := 1 END;

    LOCK tcp DO
      IF tcp.closed THEN
	RAISE IP.Error(AtomList.List1(Closed));
      END;
      IF WinSock.setsockopt(tcp.sock, WinSock.IPPROTO_TCP, WinSock.TCP_NODELAY,
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
    len: Ctypes.int := BYTESIZE(addr);
    ep: IP.Endpoint;
  BEGIN
    LOCK tcp DO
      IF tcp.closed THEN
        RAISE IP.Error(AtomList.List1(Closed));
      END;
      IF WinSock.getpeername(tcp.sock, ADR(addr), ADR(len)) = -1 THEN
        RaiseUnexpected();
      END;
    END;

    ep.addr := LOOPHOLE(addr.sin_addr, IP.Address);
    ep.port := WinSock.ntohs(addr.sin_port);
    RETURN ep;
  END GetPeerName;

PROCEDURE GetSockName(tcp: T): IP.Endpoint
  RAISES {IP.Error} =
  VAR
    addr: SockAddrIn;
    len: Ctypes.int := BYTESIZE(addr);
    ep: IP.Endpoint;
  BEGIN
    LOCK tcp DO
      IF tcp.closed THEN
        RAISE IP.Error(AtomList.List1(Closed));
      END;
      IF WinSock.getsockname(tcp.sock, ADR(addr), ADR(len)) = -1 THEN
        RaiseUnexpected();
      END;
    END;

    ep.addr := LOOPHOLE(addr.sin_addr, IP.Address);
    ep.port := WinSock.ntohs(addr.sin_port);
    RETURN ep;
  END GetSockName;

PROCEDURE KeepAlive(tcp: T; allow: BOOLEAN)
  RAISES {IP.Error} =
  VAR
    keepAlive: Ctypes.int;
  BEGIN
    IF allow THEN keepAlive := 1 ELSE keepAlive := 0 END;

    LOCK tcp DO
      IF tcp.closed THEN
        RAISE IP.Error(AtomList.List1(Closed));
      END;
      IF WinSock.setsockopt(tcp.sock, WinSock.SOL_SOCKET, WinSock.SO_KEEPALIVE,
        ADR(keepAlive), BYTESIZE(keepAlive)) = -1 THEN
        RaiseUnexpected();
      END;
    END;
  END KeepAlive;

PROCEDURE LingerOnClose(tcp: T; allow: BOOLEAN)
  RAISES {IP.Error} =
  VAR
    linger: WinSock.struct_linger;
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
      IF WinSock.setsockopt(tcp.sock, WinSock.SOL_SOCKET, WinSock.SO_LINGER,
        ADR(linger), BYTESIZE(linger)) = -1 THEN
        RaiseUnexpected();
      END;
    END;
  END LingerOnClose;

BEGIN
END TCP.

(*
PROCEDURE Connect (ep: IP.Endpoint): T
    RAISES {IP.Error, Thread.Alerted} =
  VAR
    sock := NewSocket();
    name : SockAddrIn;
    err  : INTEGER;
  BEGIN
    InitSock(sock);
    name.sin_family := WinSock.AF_INET;
    name.sin_port := WinSock.htons(ep.port);
    name.sin_addr.s_addr := LOOPHOLE(ep.addr, WinSock.u_long);
    name.sin_zero := Sin_Zero;
    IF WinSock.connect(sock, ADR(name), BYTESIZE(SockAddrIn)) = SockErr THEN
      err := WinSock.WSAGetLastError();
      EVAL WinSock.closesocket(sock);
      CASE err OF
      | WinSock.WSAEISCONN =>
          (*ok*)
      | WinSock.WSAEADDRNOTAVAIL,
        WinSock.WSAECONNREFUSED,
        WinSock.WSAECONNRESET =>
          IPError.Raise(Refused, err);
      | WinSock.WSAETIMEDOUT =>
          IPError.Raise(Timeout, err);
      | WinSock.WSAENETUNREACH,
        WinSock.WSAEHOSTUNREACH,
        WinSock.WSAEHOSTDOWN,
        WinSock.WSAENETDOWN =>
          IPError.Raise(IP.Unreachable, err);
      ELSE
          Ouch(err, "TCP.Connect");
      END;
    END;
    RETURN NEW(T, sock := sock, ep := ep);
  END Connect;
*)

(***************
Here is a new version of tcp/src/WIN32/TCP.m3 that fixes the connection
timeout problems.

The problem was that connect was being called over and over on the
same socket.  I don't know what the motivation for that was.  But, I 
changed things to call connect once, and then use select() as described
in the docs:  when it is writable, the connection has succeeded, and when
there is an exceptional condition, the connection has failed.  

It seems to work, timing out in a nice short time.

  - Blair
****************)
