(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE SocketWin32 EXPORTS Socket;

IMPORT Atom, AtomList, Ctypes, File, FileWin32;
IMPORT OSError, OSErrorWin32, Process, Thread;
FROM WinSock IMPORT accept, AF_INET, bind, closesocket, connect, FIONREAD,
  gethostbyname, gethostname, getpeername, getsockname, htons,
  INVALID_SOCKET, ioctlsocket, IPPROTO_TCP, listen, MSG_PEEK, ntohs,
  recv, recvfrom, send, sendto, setsockopt, SO_LINGER, SO_REUSEADDR,
  SOCK_DGRAM, SOCK_STREAM, SOCKET, socket, SOCKET_ERROR, SOL_SOCKET,
  struct_hostent_star, struct_in_addr, struct_linger, struct_sockaddr_in,
  TCP_NODELAY, u_long, WSAEADDRINUSE, WSAEADDRNOTAVAIL, WSAECONNREFUSED,
  WSAECONNRESET, WSAEHOSTDOWN, WSAEHOSTUNREACH, WSAEISCONN, WSAEMFILE,
  WSAENETDOWN, WSAENETRESET, WSAENETUNREACH, WSAETIMEDOUT, WSACleanup,
  WSAData, WSAGetLastError, WSAStartup;
FROM Ctypes IMPORT int, char;

CONST
  SockErr = SOCKET_ERROR;
  GetError = WSAGetLastError;

REVEAL
  T = Public BRANDED "Socket.T" OBJECT
    sock : SOCKET := 0;
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

TYPE SockAddrIn = struct_sockaddr_in;

VAR
  init_done := FALSE;
  mu := NEW (MUTEX);

PROCEDURE Create (reliable: BOOLEAN): T
  RAISES {OSError.E} =
  CONST
    Map = ARRAY BOOLEAN OF INTEGER { SOCK_DGRAM, SOCK_STREAM };
  VAR
    t    := NEW (T, handle := NIL, ds := FileWin32.ReadWrite);
    True : BOOL := 1;
  BEGIN
    IF NOT init_done THEN Init (); END;
    t.sock := socket (AF_INET, Map[reliable], 0);
    IF t.sock = INVALID_SOCKET THEN
      VAR err := Unexpected;  x := GetError ();  BEGIN
        IF x = WSAEMFILE THEN err := NoResources; END;
        IOError (err, x);
      END;
    END;
    InitSock (t.sock);
    EVAL setsockopt (t.sock, SOL_SOCKET, SO_REUSEADDR,
                     ADR(True), BYTESIZE(True));
    RETURN t;
  END Create;

PROCEDURE Close (t: T) =
  BEGIN
    EVAL closesocket (t.sock);
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
    SetAddress (t, ep, name);
    IF bind (t.sock, ADR (name), BYTESIZE (name)) = SockErr THEN
      VAR err := Unexpected; x := GetError ();  BEGIN
        IF x = WSAEADDRINUSE THEN err := PortBusy; END;
        IOError (err, x);
      END
    END;
  END Bind;

PROCEDURE Listen (t: T;  max_queue: CARDINAL)
  RAISES {OSError.E} =
  BEGIN
    IF listen (t.sock, max_queue) = SockErr THEN
      IOFailed ();
    END;
  END Listen;

PROCEDURE Connect (t: T;  READONLY ep: EndPoint)
  RAISES {OSError.E, Thread.Alerted} =
  VAR name: SockAddrIn;
  BEGIN
    IF Thread.TestAlert() THEN RAISE Thread.Alerted; END;
    SetAddress (t, ep, name);
    InitSock (t.sock);
    IF connect (t.sock, ADR(name), BYTESIZE(SockAddrIn)) # 0 THEN
      ConnectError ();
    END;
  END Connect;

PROCEDURE ConnectError ()
  RAISES {OSError.E} =
  VAR err := GetError();
  BEGIN
    CASE err OF
    | WSAEISCONN =>
        (* ok, connected *)
        RETURN;
    | WSAEADDRNOTAVAIL,
      WSAECONNREFUSED,
      WSAECONNRESET =>
        IOError (Refused, err);
    | WSAETIMEDOUT =>
        IOError (Timeout, err);
    | WSAENETUNREACH,
      WSAEHOSTUNREACH,
      WSAEHOSTDOWN,
      WSAENETDOWN =>
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
    sock : SOCKET;
    err  : INTEGER;
    res  : T;
  BEGIN
    IF Thread.TestAlert() THEN RAISE Thread.Alerted; END;
    sock := accept (t.sock, ADR (name), ADR (len));
    IF sock = INVALID_SOCKET THEN
      err := GetError ();
      IF err = WSAEMFILE
        THEN IOError (NoResources, err);
        ELSE IOError (Unexpected, err);
      END;
    END;

    res := NEW (T, sock := sock, ds := FileWin32.ReadWrite, handle := NIL);
    AddressToEndPoint (name, res.ep);
    InitSock (res.sock);
    RETURN res;
  END Accept;

PROCEDURE ReceiveFrom (t: T;  VAR(*OUT*) ep: EndPoint;
                              VAR(*OUT*) b: ARRAY OF File.Byte;
                                         mayBlock := TRUE): INTEGER
  RAISES {OSError.E} =
  VAR
    name  : SockAddrIn;
    nmLen : int;
    len   : int;
    p_b   : ADDRESS := ADR (b[0]);
  BEGIN
    IF (NOT mayBlock) AND (BytesAvailable (t) <= 0) THEN RETURN -1; END;
    nmLen := BYTESIZE (name);
    len := recvfrom (t.sock, p_b, NUMBER (b), 0, ADR (name), ADR (nmLen));
    IF len = SockErr THEN RETURN ReceiveError (); END;
    AddressToEndPoint (name, ep);
    RETURN len;
  END ReceiveFrom;

PROCEDURE Read (t: T;  VAR(*OUT*) b: ARRAY OF File.Byte;  mayBlock := TRUE): INTEGER
  RAISES {OSError.E} =
  VAR len: INTEGER;  p_b: ADDRESS := ADR (b[0]);
  BEGIN
    IF (NOT mayBlock) AND (BytesAvailable (t) <= 0) THEN RETURN -1; END;
    len := recv (t.sock, p_b, NUMBER (b), 0);
    IF len = SockErr THEN RETURN ReceiveError (); END;
    RETURN len;
  END Read;

PROCEDURE ReceiveError (): INTEGER
  RAISES {OSError.E} =
  VAR err := GetError ();
  BEGIN
    CASE err OF
    | WSAECONNRESET =>
        RETURN 0;
    | WSAENETRESET =>
        IOError (ConnLost, err);
    | WSAETIMEDOUT =>
        IOError (Timeout, err);
    | WSAENETUNREACH,
      WSAEHOSTUNREACH,
      WSAEHOSTDOWN,
      WSAENETDOWN =>
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
    n   : int := NUMBER(b);
    name: SockAddrIn;
  BEGIN
    WHILE n > 0 DO
      EndPointToAddress (ep, name);
      len := sendto (t.sock, p, n, 0, ADR (name), BYTESIZE (name));
      IF len = SockErr THEN SendError (); END;
      INC (p, len);  DEC (n, len);
    END;
  END SendTo;

PROCEDURE Write (t: T;  READONLY b: ARRAY OF File.Byte)
  RAISES {OSError.E} =
  VAR
    len : INTEGER;
    p   : ADDRESS    := ADR(b[0]);
    n   : int := NUMBER(b);
  BEGIN
    WHILE n > 0 DO
      len := send (t.sock, p, n, 0);
      IF len = SockErr THEN SendError (); END;
      INC (p, len);  DEC (n, len);
    END;
  END Write;

PROCEDURE SendError ()
  RAISES {OSError.E} =
  VAR err := GetError();
  BEGIN
    CASE err OF
    | WSAECONNRESET,
      WSAENETRESET =>
        IOError (ConnLost, err);
    | WSAETIMEDOUT =>
        IOError (Timeout, err);
    | WSAENETUNREACH,
      WSAEHOSTUNREACH,
      WSAEHOSTDOWN,
      WSAENETDOWN =>
        IOError (Unreachable, err);
    ELSE
        IOError (Unexpected, err);
    END;
  END SendError;

PROCEDURE BytesAvailable (t: T): CARDINAL
  RAISES {OSError.E} =
  VAR ec: int;  charsToRead: u_long;
  BEGIN
    ec := ioctlsocket (t.sock, FIONREAD, ADR(charsToRead));
    IF ec # 0 THEN IOError (Unexpected, ec); END;
    RETURN MAX (0, charsToRead);
  END BytesAvailable;

PROCEDURE Peek (t: T): EndPoint
  RAISES {OSError.E} =
  VAR
    name : SockAddrIn;
    len  : int := BYTESIZE (name);
    ep   : EndPoint;
  BEGIN
    IF recvfrom (t.sock, NIL, 0, MSG_PEEK,
                 ADR (name), ADR (len)) # 0 THEN
      IOFailed ();
    END;
    AddressToEndPoint (name, ep);
    RETURN ep;
  END Peek;

PROCEDURE ThisEnd (t: T): EndPoint
  RAISES {OSError.E} =
  VAR
    name : SockAddrIn;
    len  : int := BYTESIZE (name);
  BEGIN
    IF t.ep.addr = NullAddress THEN
      t.ep.addr := GetHostAddr ();
    END;
    IF t.ep.port = NullPort THEN
      IF getsockname (t.sock, ADR (name), ADR (len)) = SockErr THEN
        IOFailed ();
      END;
      t.ep.port := ntohs (name.sin_port);
    END;
    RETURN t.ep
  END ThisEnd;

PROCEDURE GetHostAddr (): Address
  RAISES {OSError.E} =
  VAR
    host : ARRAY [0..255] OF CHAR;
    info : struct_hostent_star;
    ua   : struct_in_addr;
  BEGIN
    IF gethostname (ADR (host[0]), BYTESIZE (host)) # 0 THEN
      IOFailed ();
    END;

    info := gethostbyname (ADR (host[0]));
    IF info = NIL THEN IOFailed (); END;
    <* ASSERT info.h_length <= BYTESIZE (Address) *>

    ua := LOOPHOLE(info.h_addr_list,
                   UNTRACED REF UNTRACED REF struct_in_addr)^^;
    RETURN LOOPHOLE (ua.s_addr, Address);
  END GetHostAddr;

PROCEDURE OtherEnd (t: T): EndPoint
  RAISES {OSError.E} =
  VAR
    addr : SockAddrIn;
    len  : int := BYTESIZE (addr);
    ep   : EndPoint;
  BEGIN
    IF getpeername (t.sock, ADR (addr), ADR (len)) < 0 THEN
      IOFailed ();
    END;
    AddressToEndPoint (addr, ep);
    RETURN ep;
  END OtherEnd;

(*------------------------------------------------ internal utilities ---*)

PROCEDURE Init() =
  (* LL = 0 *)
  CONST WinSockVersion = 16_0202;  (* App version 2.2 *)
  VAR data: WSAData;
  BEGIN
    IF init_done THEN RETURN; END;
    LOCK mu DO
      IF init_done THEN RETURN; END;
      IF WSAStartup (WinSockVersion, ADR (data)) # 0 THEN
        <*ASSERT FALSE*>
      END;
      Process.RegisterExitor (Exitor);
      init_done := TRUE;
    END;
  END Init;

PROCEDURE Exitor () =
  BEGIN
    LOCK mu DO
      EVAL WSACleanup ();
      init_done := FALSE;
    END;
  END Exitor;

PROCEDURE SetAddress (t: T;  READONLY ep: EndPoint;  VAR(*OUT*) name: SockAddrIn) =
  BEGIN
    t.ep := ep;
    EndPointToAddress (ep, name);
  END SetAddress;

PROCEDURE EndPointToAddress (READONLY ep: EndPoint;  VAR(*OUT*) name: SockAddrIn) =
  CONST Sin_Zero = ARRAY [0 .. 7] OF char{VAL(0, char), ..};
  BEGIN
    name.sin_family      := AF_INET;
    name.sin_port        := htons (ep.port);
    name.sin_addr.s_addr := LOOPHOLE (ep.addr, u_long);
    name.sin_zero        := Sin_Zero;
  END EndPointToAddress;

PROCEDURE AddressToEndPoint (READONLY name: SockAddrIn;  VAR(*OUT*) ep: EndPoint) =
  BEGIN
    ep.addr := LOOPHOLE (name.sin_addr.s_addr, Address);
    ep.port := ntohs (name.sin_port);
  END AddressToEndPoint;

PROCEDURE InitSock (sock: SOCKET) =
  (* We assume that the runtime ignores SIGPIPE signals *)
  VAR
    one: int := 1;
    linger := struct_linger{0, 0};
  BEGIN
    EVAL setsockopt (sock, SOL_SOCKET, SO_LINGER,
                     ADR(linger), BYTESIZE(linger));

    (**** WinSock documentation warns that this may cause problems
    ****)
    EVAL setsockopt (sock, IPPROTO_TCP, TCP_NODELAY,
                     ADR(one), BYTESIZE(one));
  END InitSock;

PROCEDURE IOFailed ()
  RAISES {OSError.E} =
  BEGIN
    IOError (Unexpected, GetError ());
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
