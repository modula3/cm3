(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE SocketPosix EXPORTS Socket;

IMPORT Atom, AtomList, Compiler, Cerrno, Ctypes, File, FilePosix;
IMPORT OSError, OSErrorPosix, SchedulerPosix, Thread, Unix;
IMPORT Uin, Uuio, Uerror, Usocket, Ustat, Unetdb, Utypes, Word;

CONST
  TCP_NODELAY = 1;

REVEAL
  T = Public BRANDED "Socket.T" OBJECT
    ep: EndPoint   := NullEndPoint;
  OVERRIDES
    (* File.T methods *)
    read            := Read;
    write           := Write;
    status          := Status;
    close           := Close;
    (* Socket.T methods *)
    bind            := Bind;
    connect         := Connect;
    accept          := Accept;
    listen          := Listen;
    bytes_available := BytesAvailable;
    peek            := Peek;
    this_end        := ThisEnd;
    other_end       := OtherEnd;
    recv_from       := ReceiveFrom;
    send_to         := SendTo;
  END;

TYPE SockAddrIn = Uin.struct_sockaddr_in;

PROCEDURE Create (reliable: BOOLEAN): T
  RAISES {OSError.E} =
  CONST
    Map = ARRAY BOOLEAN OF INTEGER { Usocket.SOCK_DGRAM, Usocket.SOCK_STREAM };
  VAR
    t    := NEW (T, ds := FilePosix.ReadWrite);
    True := 1;
  BEGIN
    t.fd := Usocket.socket (Usocket.AF_INET, Map[reliable], 0);
    IF t.fd = -1 THEN
      VAR err := Unexpected; BEGIN
        WITH errno = Cerrno.GetErrno() DO
          IF errno = Uerror.EMFILE OR errno = Uerror.ENFILE THEN
            err := NoResources;
          END;
        END;
        IOError (err);
      END;
    END;
    MakeNonBlocking (t.fd);
    EVAL Usocket.setsockopt (t.fd, Usocket.SOL_SOCKET, Usocket.SO_REUSEADDR,
                             ADR (True), BYTESIZE (True));
    RETURN t;
  END Create;

PROCEDURE Close (t: T)
  RAISES {OSError.E} =
  BEGIN
    IF Unix.close (t.fd) < 0 THEN
      IOError (Unexpected);
    END;
  END Close;

PROCEDURE Status (t: T): File.Status
  RAISES {OSError.E} =
  VAR
    statBuf : Ustat.struct_stat;
    status  : File.Status;
  BEGIN
    IF Ustat.fstat (t.fd, ADR (statBuf)) < 0 THEN IOError (Unexpected); END;
    status.type             := FileType;
    status.modificationTime := FLOAT (statBuf.st_mtime, LONGREAL);
    status.size             := Utypes.asLong(statBuf.st_size);
    IF status.size < 0 THEN IOError (Unexpected); END;
    RETURN status
  END Status;  

PROCEDURE Bind (t: T;  READONLY ep: EndPoint)
  RAISES {OSError.E} =
  VAR
    name  : SockAddrIn;
    status: INTEGER;
  BEGIN
    SetAddress (t, ep, name);
    status := Usocket.bind (t.fd, ADR (name), BYTESIZE (name));
    IF status # 0 THEN
      VAR err := Unexpected; BEGIN
        IF Cerrno.GetErrno() = Uerror.EADDRINUSE THEN err := PortBusy; END;
        IOError (err);
      END;
    END;
  END Bind;

PROCEDURE Listen (t: T;  max_queue: CARDINAL)
  RAISES {OSError.E} =
  BEGIN
    IF Usocket.listen (t.fd, max_queue) # 0 THEN
      IOError (Unexpected);
    END;
  END Listen;

PROCEDURE Connect (t: T;  READONLY ep: EndPoint)
  RAISES {OSError.E, Thread.Alerted} =
  VAR
    name: SockAddrIn;
    status: INTEGER;
  BEGIN
    SetAddress (t, ep, name);
    InitStream (t.fd);

    LOOP
      status := Usocket.connect (t.fd, ADR(name), BYTESIZE(name));
      IF status = 0 THEN EXIT; END;

      WITH errno = Cerrno.GetErrno() DO
        IF errno = Uerror.EINVAL THEN
          (* hack to try to get real errno, hidden due to NBIO bug in connect *)
          RefetchError (t.fd);
        ELSIF errno = Uerror.EBADF THEN
          (* we'll try the same for EBADF, which we've seen on Alpha *)
          RefetchError (t.fd);
        END;
      END;

      CASE Cerrno.GetErrno() OF
      | Uerror.EISCONN =>
          EXIT;
      | Uerror.EADDRNOTAVAIL,
        Uerror.ECONNREFUSED,
        Uerror.EINVAL,
        Uerror.ECONNRESET,
        Uerror.EBADF =>
          IOError (Refused);
      | Uerror.ETIMEDOUT =>
          IOError (Timeout);
      | Uerror.ENETUNREACH,
        Uerror.EHOSTUNREACH,
        Uerror.EHOSTDOWN,
        Uerror.ENETDOWN =>
          IOError (Unreachable);
      | Uerror.EWOULDBLOCK,  <*NOWARN*>
        Uerror.EAGAIN,       <*NOWARN*>
        Uerror.EINPROGRESS,
        Uerror.EALREADY =>
          (* nope, not yet *)
      ELSE
          IOError (Unexpected);
      END;

      EVAL SchedulerPosix.IOAlertWait (t.fd, FALSE);
    END;
  END Connect;

PROCEDURE Accept (t: T): T
  RAISES {OSError.E, Thread.Alerted} =
  VAR
    name : SockAddrIn;
    len  : INTEGER   := BYTESIZE(name);
    fd   : INTEGER;
    res  : T;
  BEGIN
    LOOP
      fd := Usocket.accept (t.fd, ADR (name), ADR (len));
      IF fd >= 0 THEN EXIT; END;

      CASE Cerrno.GetErrno() OF
      | Uerror.EMFILE,
        Uerror.ENFILE =>
          IOError (NoResources);
      | Uerror.EWOULDBLOCK,  <*NOWARN*>
        Uerror.EAGAIN =>     <*NOWARN*>
          (* nope, not yet *)
      ELSE
          IOError (Unexpected);
      END;

      EVAL SchedulerPosix.IOAlertWait (t.fd, TRUE);
    END;

    res := NEW (T, fd := fd, ds := FilePosix.ReadWrite);
    AddressToEndPoint (name, res.ep);
    InitStream (fd);
    RETURN res;
  END Accept;

PROCEDURE ReceiveFrom (t: T;  VAR(*OUT*) ep: EndPoint;
                              VAR(*OUT*) b: ARRAY OF File.Byte;
                                         mayBlock := TRUE): INTEGER
  RAISES {OSError.E} =
  VAR
    name  : SockAddrIn;
    nmLen : INTEGER;
    len   : INTEGER;
    p_b   : ADDRESS := ADR (b[0]);
  BEGIN
    LOOP
      nmLen := BYTESIZE (name);
      len := Usocket.recvfrom (t.fd, p_b, NUMBER (b), 0,
                               ADR (name), ADR (nmLen));
      IF len >= 0 THEN
        AddressToEndPoint (name, ep);
        RETURN len;
      END;

      CASE Cerrno.GetErrno() OF
      | Uerror.ECONNRESET =>
          RETURN 0;
      | Uerror.EPIPE,
        Uerror.ENETRESET =>
          IOError (ConnLost);
      | Uerror.ETIMEDOUT =>
          IOError (Timeout);
      | Uerror.ENETUNREACH,
        Uerror.EHOSTUNREACH,
        Uerror.EHOSTDOWN,
        Uerror.ENETDOWN =>
          IOError (Unreachable);
      | Uerror.EWOULDBLOCK, <*NOWARN*>
        Uerror.EAGAIN =>    <*NOWARN*>
          IF NOT mayBlock THEN RETURN -1; END;
      ELSE
          IOError (Unexpected);
      END;

      EVAL SchedulerPosix.IOWait (t.fd, TRUE);
    END;
  END ReceiveFrom;

PROCEDURE Read (t: T;  VAR(*OUT*) b: ARRAY OF File.Byte;  mayBlock := TRUE): INTEGER
  RAISES {OSError.E} =
  VAR len: INTEGER;  p_b: ADDRESS := ADR (b[0]);
  BEGIN
    LOOP
      len := Uuio.read (t.fd, p_b, NUMBER (b));
      IF len >= 0 THEN RETURN len; END;

      CASE Cerrno.GetErrno() OF
      | Uerror.ECONNRESET =>
          RETURN 0;
      | Uerror.EPIPE,
        Uerror.ENETRESET =>
          IOError (ConnLost);
      | Uerror.ETIMEDOUT =>
          IOError (Timeout);
      | Uerror.ENETUNREACH,
        Uerror.EHOSTUNREACH,
        Uerror.EHOSTDOWN,
        Uerror.ENETDOWN =>
          IOError (Unreachable);
      | Uerror.EWOULDBLOCK, <*NOWARN*>
        Uerror.EAGAIN =>    <*NOWARN*>
          IF NOT mayBlock THEN RETURN -1; END;
      ELSE
          IOError (Unexpected);
      END;

      EVAL SchedulerPosix.IOWait (t.fd, TRUE);
    END;
  END Read;

PROCEDURE SendTo (t: T;  READONLY ep: EndPoint;
                         READONLY b: ARRAY OF File.Byte)
  RAISES {OSError.E} =
  VAR
    len : INTEGER;
    p   : ADDRESS    := ADR(b[0]);
    n   : Ctypes.int := NUMBER(b);
    name: SockAddrIn;
  BEGIN
    WHILE n > 0 DO
      EndPointToAddress (ep, name);
      len := Usocket.sendto (t.fd, p, n, 0, ADR (name), BYTESIZE (name));

      IF len >= 0 THEN
        INC (p, len);  DEC (n, len);
      ELSE
        CASE Cerrno.GetErrno() OF
        | Uerror.EPIPE,
          Uerror.ECONNRESET,
          Uerror.ENETRESET =>
            IOError (ConnLost);
        | Uerror.ETIMEDOUT =>
            IOError (Timeout);
        | Uerror.ENETUNREACH,
          Uerror.EHOSTUNREACH,
          Uerror.EHOSTDOWN,
          Uerror.ENETDOWN =>
            IOError (Unreachable);
        | Uerror.EWOULDBLOCK, <*NOWARN*>
          Uerror.EAGAIN =>    <*NOWARN*>
            (* OK, wait to write out a bit more... *)
        ELSE
            IOError (Unexpected);
        END;
      END;

      IF (n > 0) THEN
        EVAL SchedulerPosix.IOWait (t.fd, FALSE);
        (* IF Thread.TestAlert() THEN RAISE Thread.Alerted END *)
      END;
    END;
  END SendTo;

PROCEDURE Write (t: T;  READONLY b: ARRAY OF File.Byte)
  RAISES {OSError.E} =
  VAR
    len : INTEGER;
    p   : ADDRESS    := ADR(b[0]);
    n   : Ctypes.int := NUMBER(b);
  BEGIN
    WHILE n > 0 DO
      len := Uuio.write (t.fd, p, n);

      IF len >= 0 THEN
        INC (p, len);  DEC (n, len);
      ELSE
        CASE Cerrno.GetErrno() OF
        | Uerror.EPIPE,
          Uerror.ECONNRESET,
          Uerror.ENETRESET =>
            IOError (ConnLost);
        | Uerror.ETIMEDOUT =>
            IOError (Timeout);
        | Uerror.ENETUNREACH,
          Uerror.EHOSTUNREACH,
          Uerror.EHOSTDOWN,
          Uerror.ENETDOWN =>
            IOError (Unreachable);
        | Uerror.EWOULDBLOCK, <*NOWARN*>
          Uerror.EAGAIN =>    <*NOWARN*>
            (* OK, wait to write out a bit more... *)
        ELSE
            IOError (Unexpected);
        END;
      END;

      IF (n > 0) THEN
        EVAL SchedulerPosix.IOWait (t.fd, FALSE);
        (* IF Thread.TestAlert() THEN RAISE Thread.Alerted END *)
      END;
    END;
  END Write;

PROCEDURE BytesAvailable (t: T): CARDINAL
  RAISES {OSError.E} =
  VAR status: INTEGER;  charsToRead: Ctypes.int;
  BEGIN
    IF SchedulerPosix.IOWait (t.fd, TRUE, 0.0D0) =
                            SchedulerPosix.WaitResult.Ready THEN
      status := Unix.ioctl (t.fd, Unix.FIONREAD, ADR(charsToRead));
      IF status # 0 THEN IOError (Unexpected); END;
      RETURN MAX (0, charsToRead);
    END;
    RETURN 0;
  END BytesAvailable;

PROCEDURE Peek (t: T): EndPoint
  RAISES {OSError.E} =
  VAR
    name : SockAddrIn;
    len  : INTEGER     := BYTESIZE (name);
    ep   : EndPoint;
  BEGIN
    IF Usocket.recvfrom (t.fd, NIL, 0, Usocket.MSG_PEEK,
                         ADR (name), ADR (len)) < 0 THEN
      IOError (Unexpected);
    END;
    AddressToEndPoint (name, ep);
    RETURN ep;
  END Peek;

PROCEDURE ThisEnd (t: T): EndPoint
  RAISES {OSError.E} =
  VAR
    name : SockAddrIn;
    len  : INTEGER     := BYTESIZE (name);
  BEGIN
    IF t.ep.addr = NullAddress THEN
      t.ep.addr := GetHostAddr ();
    END;
    IF t.ep.port = NullPort THEN
      IF Usocket.getsockname (t.fd, ADR (name), ADR (len)) # 0 THEN
        IOError (Unexpected);
      END;
      t.ep.port := Uin.ntohs (name.sin_port);
    END;
    RETURN t.ep
  END ThisEnd;

PROCEDURE GetHostAddr (): Address
  RAISES {OSError.E} =
  VAR
    host : ARRAY [0..255] OF CHAR;
    info : Unetdb.struct_hostent_star;
    ua   : Uin.struct_in_addr;
  BEGIN
    IF Unix.gethostname (ADR (host[0]), BYTESIZE (host)) # 0 THEN
      IOError (Unexpected);
    END;

    info := Unetdb.gethostbyname (ADR (host[0]));
    IF info = NIL THEN IOError (Unexpected); END;
    <* ASSERT info.h_length <= BYTESIZE (Address) *>

    ua := LOOPHOLE(info.h_addr_list,
                   UNTRACED REF UNTRACED REF Uin.struct_in_addr)^^;
    RETURN LOOPHOLE (ua.s_addr, Address);
  END GetHostAddr;

PROCEDURE OtherEnd (t: T): EndPoint
  RAISES {OSError.E} =
  VAR
    addr : SockAddrIn;
    len  : Ctypes.int := BYTESIZE (addr);
    ep   : EndPoint;
  BEGIN
    IF Usocket.getpeername (t.fd, ADR (addr), ADR (len)) < 0 THEN
      IOError (Unexpected);
    END;
    AddressToEndPoint (addr, ep);
    RETURN ep;
  END OtherEnd;

(*------------------------------------------------ internal utilities ---*)

PROCEDURE SetAddress (t: T;  READONLY ep: EndPoint;  VAR(*OUT*) name: SockAddrIn) =
  (* LL = mu *)
  BEGIN
    t.ep := ep;
    EndPointToAddress (ep, name);
  END SetAddress;

PROCEDURE EndPointToAddress (READONLY ep: EndPoint;  VAR(*OUT*) name: SockAddrIn) =
  (* LL = mu *)
  CONST Sin_Zero = ARRAY [0 .. 7] OF Ctypes.char{VAL(0, Ctypes.char), ..};
  BEGIN
    name.sin_family      := Usocket.AF_INET;
    name.sin_port        := Uin.htons (ep.port);
    name.sin_addr.s_addr := LOOPHOLE (ep.addr, Utypes.u_int);
    name.sin_zero        := Sin_Zero;
  END EndPointToAddress;

PROCEDURE AddressToEndPoint (READONLY name: SockAddrIn;  VAR(*OUT*) ep: EndPoint) =
  (* LL = mu *)
  BEGIN
    ep.addr := LOOPHOLE (name.sin_addr.s_addr, Address);
    ep.port := Uin.ntohs (name.sin_port);
  END AddressToEndPoint;

(*
VAR SysSendBufSize: INTEGER := 128000;
VAR SysRcvBufSize: INTEGER := 128000;
*)

PROCEDURE InitStream (fd: CARDINAL)
  RAISES {OSError.E} =
  (* We assume that the runtime ignores SIGPIPE signals *)
  VAR
    one := 1;
    linger := Usocket.struct_linger{1, 1};
  BEGIN
    (*****
    EVAL Usocket.setsockopt(fd, Usocket.SOL_SOCKET, Usocket.SO_SNDBUF,
                            ADR(SysSendBufSize), BYTESIZE(SysSendBufSize));
    EVAL Usocket.setsockopt(fd, Usocket.SOL_SOCKET, Usocket.SO_RCVBUF,
                            ADR(SysRcvBufSize), BYTESIZE(SysRcvBufSize));
    ******)
    EVAL Usocket.setsockopt(fd, Usocket.SOL_SOCKET, Usocket.SO_LINGER,
                            ADR(linger), BYTESIZE(linger));
    EVAL Usocket.setsockopt(fd, Uin.IPPROTO_TCP, TCP_NODELAY,
                            ADR(one), BYTESIZE(one));

    MakeNonBlocking (fd);
  END InitStream;

PROCEDURE MakeNonBlocking (fd: INTEGER)
  RAISES {OSError.E} =
  VAR
    old_mode := Unix.fcntl (fd, Unix.F_GETFL, 0);
    new_mode := Word.Or (old_mode, Unix.M3_NONBLOCK);  
  BEGIN
    IF Unix.fcntl (fd, Unix.F_SETFL, new_mode) # 0 THEN
      IOError (Unexpected);
    END;
  END MakeNonBlocking;

PROCEDURE RefetchError(fd: INTEGER) =
(* Awful hack to retrieve a meaningful error from a TCP accept
   socket.  Only works on Ultrix and OSF.  Leaves result
   in Cerrno.GetErrno().  *)
  VAR optbuf: INTEGER := 0;   optlen := BYTESIZE(optbuf);
  BEGIN
    IF Compiler.ThisPlatform = Compiler.Platform.ALPHA_OSF
    OR Compiler.ThisPlatform = Compiler.Platform.DS3100 THEN
      EVAL Usocket.getsockopt (fd, Uin.IPPROTO_TCP, TCP_NODELAY,
                                 ADR(optbuf), ADR(optlen));
    END;
  END RefetchError;

PROCEDURE IOError (a: Atom.T) RAISES {OSError.E} =
  VAR ec: AtomList.T := NIL;
  BEGIN
    IF (Cerrno.GetErrno() # 0) THEN
      ec := AtomList.List1 (OSErrorPosix.ErrnoAtom (Cerrno.GetErrno()));
    END;
    RAISE OSError.E (AtomList.Cons (a, ec));
  END IOError;

BEGIN
END SocketPosix.
