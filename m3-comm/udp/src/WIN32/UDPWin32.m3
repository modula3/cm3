UNSAFE MODULE UDPWin32 EXPORTS UDP;

(* 
   By Darko Volaric May 2003 darko@peter.com.au 

   Note that this version does not support timeouts when
   reading data (the Posix version does, though).

*)

IMPORT
  WinSock, WinDef, IP, Thread, Ctypes, Text, Fmt, Atom, AtomList, ETimer;

REVEAL
  T = Public BRANDED "UDPWin32.T" OBJECT
    open: BOOLEAN := FALSE;   (* TRUE iff the UDP connection is open *)
    data: WinSock.WSAData;
    socket: WinSock.SOCKET;
  OVERRIDES
    init := Init;
    send := Send;
    sendText := SendText;
    receive := Receive;
    close := Close;
  END;

CONST SinZero = ARRAY [0 .. 7] OF Ctypes.char{VAL(0, Ctypes.char), ..};


PROCEDURE Error(num: Ctypes.int; place: TEXT) RAISES {IP.Error} =
VAR
  list: AtomList.T;
  atom: Atom.T := NIL;
BEGIN
  (* FIXME: the following are guesses and probably wrong *)
  CASE num OF
  | WinSock.WSAHOST_NOT_FOUND => atom := IP.LookupFailure;
  | WinSock.WSAEHOSTUNREACH => atom := IP.Unreachable;
  | WinSock.WSAECONNREFUSED => atom := IP.PortBusy;
  | WinSock.WSAENOBUFS => atom := IP.NoResources;
  ELSE
  END;
  list := AtomList.List1(Atom.FromText(
    "Winsock error "&Fmt.Int(num)&" occurred when calling '"&place&"'."));
  IF atom # NIL THEN
    list := AtomList.Cons(atom, list);
  END;
  RAISE IP.Error(list);
END Error;

PROCEDURE Init(this: T; myPort: IP.Port; myAddr: IP.Address): T
    RAISES {IP.Error} =
VAR
  local: WinSock.struct_sockaddr_in;
  ret: Ctypes.int;
BEGIN
  <* ASSERT NOT this.open *>
  (* init *)
  ret := WinSock.WSAStartup(WinDef.MAKEWORD(1, 1), ADR(this.data));
  IF ret # 0 THEN
    Error(ret, "WSAStartup");
  END;
  (* create the socket *)
  this.socket := WinSock.socket(WinSock.AF_INET, WinSock.SOCK_DGRAM, 0);
  IF this.socket =  WinSock.INVALID_SOCKET THEN
    Error(WinSock.WSAGetLastError(), "socket");
  END;
  (* bind the socket *)
  local.sin_family := WinSock.AF_INET;
  local.sin_port := WinSock.htons(myPort);
  local.sin_addr.s_addr := LOOPHOLE(myAddr, WinSock.u_long);
  local.sin_zero := SinZero;
  
  ret := WinSock.bind(this.socket, ADR(local),BYTESIZE(WinSock.struct_sockaddr_in));
  IF ret = WinSock.SOCKET_ERROR THEN
    Error(WinSock.WSAGetLastError(), "socket");
  END;
  (* done *)
  this.open := TRUE;
  RETURN this;
END Init;

PROCEDURE Send(this: T; READONLY d: Datagram): INTEGER RAISES {IP.Error} =
VAR
  remote: WinSock.struct_sockaddr_in;
BEGIN
  <* ASSERT this.open AND d.len <= NUMBER(d.bytes^) *>
  remote.sin_family := WinSock.AF_INET;
  remote.sin_port := WinSock.htons(d.other.port);
  remote.sin_addr.s_addr := LOOPHOLE(d.other.addr, WinSock.u_long);
  remote.sin_zero := SinZero;
  WITH 
    sent = WinSock.sendto(this.socket, LOOPHOLE(ADR(d.bytes[0]), Ctypes.char_star),
      d.len, 0, ADR(remote), BYTESIZE(WinSock.struct_sockaddr_in)) 
  DO
    IF sent = WinSock.SOCKET_ERROR THEN
      Error(WinSock.WSAGetLastError(), "sendto");
    END;
    RETURN sent;
  END;
END Send;

PROCEDURE SendText(this: T; READONLY other: IP.Endpoint; t: TEXT): INTEGER RAISES {IP.Error} =
VAR
  data: Datagram;
BEGIN
  (* make a datagram with the text data*)
  data.other := other;
  data.len := Text.Length(t);
  data.bytes := NEW(REF ARRAY OF CHAR, data.len);
  Text.SetChars(data.bytes^, t);
  (* send it *)
  RETURN this.send(data);
END SendText;

PROCEDURE Receive(this: T; VAR (*INOUT*) d: Datagram; timeout: LONGREAL) RAISES {Timeout, IP.Error,Thread.Alerted} =
VAR
  size: Ctypes.int;
  remote: WinSock.struct_sockaddr_in;
  set: WinSock.struct_fd_set;
  timeval: WinSock.struct_timeval;
  timer: ETimer.T := NIL;
BEGIN
  <* ASSERT this.open *>
  IF timeout >= 0.0d0 THEN 
  	timer := ETimer.New("");
  	ETimer.Enable(); 
  	ETimer.Push(timer);
  END;
	timeval.tv_sec := 0;
	timeval.tv_usec := 100000; (* check for alert or timeout every 1/10th of a second *)
	LOOP
		set.fd_count := 1;
		set.fd_array[0] := this.socket;
		WITH ret = WinSock.select(0, ADR(set), NIL, NIL, ADR(timeval)) DO
	    IF ret = WinSock.SOCKET_ERROR THEN
	      Error(WinSock.WSAGetLastError(), "select");
	    END;
	  	IF ret = 1 THEN EXIT END;
	  	IF Thread.TestAlert() THEN
	  		RAISE Thread.Alerted;
	  	END;
	  	IF timer # NIL AND timeout <= ETimer.Elapsed(timer) THEN
	  		RAISE Timeout;
	  	END; 
		END;
	END;
  remote.sin_zero := SinZero;
  size := BYTESIZE(WinSock.struct_sockaddr_in);
  WITH 
    read = WinSock.recvfrom(this.socket, LOOPHOLE(ADR(d.bytes[0]), Ctypes.char_star),
    NUMBER(d.bytes^), 0, ADR(remote), ADR(size))
  DO
    IF read = WinSock.SOCKET_ERROR THEN
      Error(WinSock.WSAGetLastError(), "recvfrom");
    END;
    d.len := read;
    d.other.port := WinSock.ntohs(remote.sin_port);
    d.other.addr := LOOPHOLE(remote.sin_addr.s_addr, IP.Address);
  END;
END Receive;

PROCEDURE Close(this: T) RAISES {IP.Error} =
BEGIN
  IF WinSock.closesocket(this.socket) = WinSock.SOCKET_ERROR  THEN
    Error(WinSock.WSAGetLastError(), "closesocket");
  END;
  this.open := FALSE;
END Close;

BEGIN
END UDPWin32.

