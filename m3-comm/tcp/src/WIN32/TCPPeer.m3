(* Copyright (C) 1995, Digital Equipment Corporation             *)
(* All rights reserved.                                          *)
(* See the file COPYRIGHT for a full description.                *)
(*                                                               *)
(* Last modified on Tue Jun 27 15:23:47 PDT 1995 by steveg       *)
(*      modified on Tue Mar  7 13:32:10 PST 1995 by kalsow       *)
(*                                                               *)
(* Contributed by Peter Klein (pk@i3.informatik.rwth-aachen.de)  *)
(*    - Mar 7, 1995                                              *)

UNSAFE MODULE TCPPeer;

IMPORT Ctypes, IP, IPError, TCP, TCPWin32, WinSock, Word;

TYPE Addr = WinSock.struct_sockaddr_in;

PROCEDURE Get (channel: TCP.T): IP.Endpoint RAISES {IP.Error} =
  VAR addr: Addr;  endpoint: IP.Endpoint;
  BEGIN
    GetSockAddr(channel, addr);
    endpoint.port := WinSock.ntohs (addr.sin_port);
    endpoint.addr := LOOPHOLE (addr.sin_addr.s_addr, IP.Address);
    RETURN endpoint;
  END Get;

PROCEDURE GetName (channel: TCP.T): TEXT RAISES {IP.Error} =
  VAR addr: Addr;
  BEGIN
    GetSockAddr (channel, addr);
    RETURN IP.GetCanonicalByAddr (LOOPHOLE (addr.sin_addr.s_addr, IP.Address));
  END GetName;

PROCEDURE Match (channel: TCP.T; address: IP.Address; maskBits: [0 .. 32]):
  BOOLEAN RAISES {IP.Error} =
  VAR addr: Addr;  peer, mask: INTEGER;
      addrWord: Ctypes.int := LOOPHOLE(address, Ctypes.int);
  BEGIN
    GetSockAddr(channel, addr);
    peer := Word.Extract (addr.sin_addr.s_addr, 32 - maskBits, maskBits);
    mask := Word.Extract (addrWord, 32 - maskBits, maskBits);
    RETURN (peer = mask);
  END Match;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE GetSockAddr (channel: TCP.T;  VAR(*OUT*) addr: Addr)
  RAISES {IP.Error} =
  VAR len: Ctypes.int := BYTESIZE (addr);
  BEGIN
    LOCK channel DO
      IF (channel.closed) THEN IPError.Raise (TCP.Closed); END;
      IF (WinSock.getpeername (channel.sock, ADR (addr), ADR (len)) < 0) THEN
        IPError.RaiseUnexpected ();
      END;
    END;
  END GetSockAddr;

BEGIN
END TCPPeer.
