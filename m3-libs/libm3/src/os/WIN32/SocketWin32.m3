(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

UNSAFE MODULE SocketWin32 EXPORTS Socket;

IMPORT Atom, AtomList, Ctypes, File, FileWin32;
IMPORT OSError, OSErrorWin32, Process, Thread, WinSock;

CONST
  SockErr = WinSock.SOCKET_ERROR;

REVEAL
  T = Public BRANDED "Socket.T" OBJECT
    sock : WinSock.SOCKET := 0;
    ep   : EndPoint       := NullEndPoint;
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

TYPE SockAddrIn = WinSock.struct_sockaddr_in;

VAR
  init_done := FALSE;
  mu: MUTEX := NIL;

PROCEDURE Create (reliable: BOOLEAN): T
  RAISES {OSError.E} =
  CONST
    Map = ARRAY BOOLEAN OF INTEGER { WinSock.SOCK_DGRAM, WinSock.SOCK_STREAM };
  VAR
    t    := NEW (T, handle := NIL, ds := FileWin32.ReadWrite);
    True := 1;
  BEGIN
    IF NOT init_done THEN Init (); END;
    LOCK mu DO
      t.sock := WinSock.socket (WinSock.AF_INET, Map[reliable], 0);
      IF t.sock = WinSock.INVALID_SOCKET THEN
        VAR err := Unexpected;  x := WinSock.WSAGetLastError ();  BEGIN
          IF x = WinSock.WSAEMFILE THEN err := NoResources; END;
          IOError (err, x);
        END;
      END;
      InitSock (t.sock);
      EVAL WinSock.setsockopt (t.sock, WinSock.SOL_SOCKET, WinSock.SO_REUSEADDR,
                               ADR(True), BYTESIZE(True));
    END;
    RETURN t;
  END Create;

PROCEDURE Close (t: T) =
  BEGIN
    EVAL WinSock.closesocket (t.sock);
  END Close;

PROCEDURE Status (<*UNUSED*> t: T): File.Status =
  VAR status: File.Status;
  BEGIN
    status.type := FileType;
    RETURN status
  END Status;  

PROCEDURE Bind (t: T;  READONLY ep: EndPoint)
  RAISES {OSError.E} =
  VAR name: SockAddrIn;
  BEGIN
    LOCK mu DO
      SetAddress (t, ep, name);
      IF WinSock.bind (t.sock, ADR (name), BYTESIZE (name)) = SockErr THEN
        VAR err := Unexpected; x := WinSock.WSAGetLastError ();  BEGIN
          IF x = WinSock.WSAEADDRINUSE THEN err := PortBusy; END;
          IOError (err, x);
        END
      END;
    END;
  END Bind;

PROCEDURE Listen (t: T;  max_queue: CARDINAL)
  RAISES {OSError.E} =
  BEGIN
    LOCK mu DO
      IF WinSock.listen (t.sock, max_queue) = SockErr THEN
        IOFailed ();
      END;
    END;
  END Listen;

PROCEDURE Connect (t: T;  READONLY ep: EndPoint)
  RAISES {OSError.E, Thread.Alerted} =
  VAR name: SockAddrIn;
  BEGIN
    IF Thread.TestAlert() THEN RAISE Thread.Alerted; END;
    LOCK mu DO
      SetAddress (t, ep, name);
      InitSock (t.sock);
      IF WinSock.connect (t.sock, ADR(name), BYTESIZE(SockAddrIn)) # 0 THEN
        ConnectError ();
      END;
    END;
  END Connect;

PROCEDURE ConnectError ()
  RAISES {OSError.E} =
  (* LL = mu *)
  VAR err := WinSock.WSAGetLastError();
  BEGIN
    CASE err OF
    | WinSock.WSAEISCONN =>
        (* ok, connected *)
        RETURN;
    | WinSock.WSAEADDRNOTAVAIL,
      WinSock.WSAECONNREFUSED,
      WinSock.WSAECONNRESET =>
        IOError (Refused, err);
    | WinSock.WSAETIMEDOUT =>
        IOError (Timeout, err);
    | WinSock.WSAENETUNREACH,
      WinSock.WSAEHOSTUNREACH,
      WinSock.WSAEHOSTDOWN,
      WinSock.WSAENETDOWN =>
        IOError (Unreachable, err);
    ELSE
        IOError (Unexpected, err);
    END;
  END ConnectError;

PROCEDURE Accept (t: T): T
  RAISES {OSError.E, Thread.Alerted} =
  VAR
    name : SockAddrIn;
    len  : INTEGER   := BYTESIZE(name);
    sock : WinSock.SOCKET;
    err  : INTEGER;
    res  : T;
  BEGIN
    IF Thread.TestAlert() THEN RAISE Thread.Alerted; END;
    LOCK mu DO
      sock := WinSock.accept (t.sock, ADR (name), ADR (len));
      IF sock = WinSock.INVALID_SOCKET THEN
        err := WinSock.WSAGetLastError ();
        IF err = WinSock.WSAEMFILE
          THEN IOError (NoResources, err);
          ELSE IOError (Unexpected, err);
        END;
      END;

      res := NEW (T, sock := sock, ds := FileWin32.ReadWrite, handle := NIL);
      AddressToEndPoint (name, res.ep);
      InitSock (res.sock);
    END;
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
    LOCK mu DO
      IF (NOT mayBlock) AND (BytesAvail (t) <= 0) THEN RETURN -1; END;
      nmLen := BYTESIZE (name);
      len := WinSock.recvfrom (t.sock, p_b, NUMBER (b), 0, ADR (name), ADR (nmLen));
      IF len = SockErr THEN RETURN ReceiveError (); END;
    END;
    AddressToEndPoint (name, ep);
    RETURN len;
  END ReceiveFrom;

PROCEDURE Read (t: T;  VAR(*OUT*) b: ARRAY OF File.Byte;  mayBlock := TRUE): INTEGER
  RAISES {OSError.E} =
  VAR len: INTEGER;  p_b: ADDRESS := ADR (b[0]);
  BEGIN
    LOCK mu DO
      IF (NOT mayBlock) AND (BytesAvail (t) <= 0) THEN RETURN -1; END;
      len := WinSock.recv (t.sock, p_b, NUMBER (b), 0);
      IF len = SockErr THEN RETURN ReceiveError (); END;
    END;
    RETURN len;
  END Read;

PROCEDURE ReceiveError (): INTEGER
  RAISES {OSError.E} =
  (* LL = mu *)
  VAR err := WinSock.WSAGetLastError ();
  BEGIN
    CASE err OF
    | WinSock.WSAECONNRESET =>
        RETURN 0;
    | WinSock.WSAENETRESET =>
        IOError (ConnLost, err);
    | WinSock.WSAETIMEDOUT =>
        IOError (Timeout, err);
    | WinSock.WSAENETUNREACH,
      WinSock.WSAEHOSTUNREACH,
      WinSock.WSAEHOSTDOWN,
      WinSock.WSAENETDOWN =>
        IOError (Unreachable, err);
    ELSE
        IOError (Unexpected, err);
    END;
    RETURN 0;
  END ReceiveError;

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
      LOCK mu DO
        EndPointToAddress (ep, name);
        len := WinSock.sendto (t.sock, p, n, 0, ADR (name), BYTESIZE (name));
        IF len = SockErr THEN SendError (); END;
        INC (p, len);  DEC (n, len);
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
      LOCK mu DO
        len := WinSock.send (t.sock, p, n, 0);
        IF len = SockErr THEN SendError (); END;
        INC (p, len);  DEC (n, len);
      END;
    END;
  END Write;

PROCEDURE SendError ()
  RAISES {OSError.E} =
  (* LL = mu *)
  VAR err := WinSock.WSAGetLastError();
  BEGIN
    CASE err OF
    | WinSock.WSAECONNRESET,
      WinSock.WSAENETRESET =>
        IOError (ConnLost, err);
    | WinSock.WSAETIMEDOUT =>
        IOError (Timeout, err);
    | WinSock.WSAENETUNREACH,
      WinSock.WSAEHOSTUNREACH,
      WinSock.WSAEHOSTDOWN,
      WinSock.WSAENETDOWN =>
        IOError (Unreachable, err);
    ELSE
        IOError (Unexpected, err);
    END;
  END SendError;

PROCEDURE BytesAvailable (t: T): CARDINAL
  RAISES {OSError.E} =
  BEGIN
    LOCK mu DO
      RETURN BytesAvail (t);
    END;
  END BytesAvailable;

PROCEDURE BytesAvail (t: T): CARDINAL
  RAISES {OSError.E} =
  (* LL = mu *)
  VAR ec: Ctypes.int;  charsToRead: WinSock.u_long;
  BEGIN
    ec := WinSock.ioctlsocket (t.sock, WinSock.FIONREAD, ADR(charsToRead));
    IF ec # 0 THEN IOError (Unexpected, ec); END;
    RETURN MAX (0, charsToRead);
  END BytesAvail;

PROCEDURE Peek (t: T): EndPoint
  RAISES {OSError.E} =
  VAR
    name : SockAddrIn;
    len  : INTEGER     := BYTESIZE (name);
    ep   : EndPoint;
  BEGIN
    LOCK mu DO
      IF WinSock.recvfrom (t.sock, NIL, 0, WinSock.MSG_PEEK,
                           ADR (name), ADR (len)) # 0 THEN
        IOFailed ();
      END;
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
    LOCK mu DO
      IF t.ep.addr = NullAddress THEN
        t.ep.addr := GetHostAddr ();
      END;
      IF t.ep.port = NullPort THEN
        IF WinSock.getsockname (t.sock, ADR (name), ADR (len)) = SockErr THEN
          IOFailed ();
        END;
        t.ep.port := WinSock.ntohs (name.sin_port);
      END;
    END;
    RETURN t.ep
  END ThisEnd;

PROCEDURE GetHostAddr (): Address
  RAISES {OSError.E} =
  (* LL = mu *)
  VAR
    host : ARRAY [0..255] OF CHAR;
    info : WinSock.struct_hostent_star;
    ua   : WinSock.struct_in_addr;
  BEGIN
    IF WinSock.gethostname (ADR (host[0]), BYTESIZE (host)) # 0 THEN
      IOFailed ();
    END;

    info := WinSock.gethostbyname (ADR (host[0]));
    IF info = NIL THEN IOFailed (); END;
    <* ASSERT info.h_length <= BYTESIZE (Address) *>

    ua := LOOPHOLE(info.h_addr_list,
                   UNTRACED REF UNTRACED REF WinSock.struct_in_addr)^^;
    RETURN LOOPHOLE (ua.s_addr, Address);
  END GetHostAddr;

PROCEDURE OtherEnd (t: T): EndPoint
  RAISES {OSError.E} =
  VAR
    addr : SockAddrIn;
    len  : Ctypes.int := BYTESIZE (addr);
    ep   : EndPoint;
  BEGIN
    LOCK mu DO
      IF WinSock.getpeername (t.sock, ADR (addr), ADR (len)) < 0 THEN
        IOFailed ();
      END;
    END;
    AddressToEndPoint (addr, ep);
    RETURN ep;
  END OtherEnd;

(*------------------------------------------------ internal utilities ---*)

PROCEDURE Init() =
  (* LL = 0 *)
  CONST WinSockVersion = 16_0101;  (* App version 1.1 *)
  VAR data: WinSock.WSAData;
  BEGIN
    IF init_done THEN RETURN; END;
    IF mu = NIL THEN mu := NEW (MUTEX); END;
    LOCK mu DO
      IF init_done THEN RETURN; END;
      IF WinSock.WSAStartup (WinSockVersion, ADR (data)) # 0 THEN
        <*ASSERT FALSE*>
      END;
      Process.RegisterExitor (Exitor);
      init_done := TRUE;
    END;
  END Init;

PROCEDURE Exitor () =
  BEGIN
    EVAL WinSock.WSACleanup ();
  END Exitor;

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
    name.sin_family      := WinSock.AF_INET;
    name.sin_port        := WinSock.htons (ep.port);
    name.sin_addr.s_addr := LOOPHOLE (ep.addr, WinSock.u_long);
    name.sin_zero        := Sin_Zero;
  END EndPointToAddress;

PROCEDURE AddressToEndPoint (READONLY name: SockAddrIn;  VAR(*OUT*) ep: EndPoint) =
  (* LL = mu *)
  BEGIN
    ep.addr := LOOPHOLE (name.sin_addr.s_addr, Address);
    ep.port := WinSock.ntohs (name.sin_port);
  END AddressToEndPoint;

(*
VAR SysSendBufSize: INTEGER := 65535;
VAR SysRcvBufSize: INTEGER := 65535;
*)

PROCEDURE InitSock (sock: WinSock.SOCKET) =
  (* We assume that the runtime ignores SIGPIPE signals *)
  (* LL = mu *)
  VAR
    one := 1;
    linger := WinSock.struct_linger{0, 0};
  BEGIN
    (*****
    EVAL WinSock.setsockopt (sock, WinSock.SOL_SOCKET, WinSock.SO_SNDBUF,
                             ADR(SysSendBufSize), BYTESIZE(SysSendBufSize));
    EVAL WinSock.setsockopt (sock, WinSock.SOL_SOCKET, WinSock.SO_RCVBUF,
                             ADR(SysRcvBufSize), BYTESIZE(SysRcvBufSize));
    ******)

    EVAL WinSock.setsockopt (sock, WinSock.SOL_SOCKET, WinSock.SO_LINGER,
                             ADR(linger), BYTESIZE(linger));

    (**** WinSock documentation warns that this may cause problems
    ****)
    EVAL WinSock.setsockopt (sock, WinSock.IPPROTO_TCP, WinSock.TCP_NODELAY,
                             ADR(one), BYTESIZE(one));
  END InitSock;

PROCEDURE IOFailed ()
  RAISES {OSError.E} =
  (* LL = mu *)
  BEGIN
    IOError (Unexpected, WinSock.WSAGetLastError ());
  END IOFailed;

PROCEDURE IOError (a: Atom.T; err: INTEGER)
  RAISES {OSError.E} =
  VAR ec: AtomList.T := NIL;
  BEGIN
    IF (err # 0) THEN
      ec := AtomList.List1 (OSErrorWin32.ErrnoAtom (err));
    END;
    RAISE OSError.E (AtomList.Cons (a, ec));
  END IOError;

BEGIN
END SocketWin32.


