(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Tue Jun 27 15:08:05 PDT 1995 by steveg *)

UNSAFE MODULE TCPExtras;

IMPORT Ctypes, IP, IPError, TCP, TCPWin32, WinSock;

PROCEDURE LocalEndpoint (conn: TCP.T): IP.Endpoint RAISES {IP.Error} =
  VAR
    addr : WinSock.struct_sockaddr_in;
    len  : Ctypes.int := BYTESIZE (addr);
    ep   : IP.Endpoint;
  BEGIN
    LOCK conn DO
      IF conn.closed THEN IPError.Raise (TCP.Closed); END;
      IF WinSock.getsockname (conn.sock, ADR (addr), ADR (len)) # 0 THEN
        IPError.RaiseUnexpected ();
      END;
    END;
    ep.addr := LOOPHOLE (addr.sin_addr, IP.Address);
    ep.port := WinSock.ntohs (addr.sin_port);
    RETURN ep;
  END LocalEndpoint;

PROCEDURE htons (s: Ctypes.unsigned_short_int): Ctypes.unsigned_short_int =
  BEGIN
    RETURN WinSock.htons (s);
  END htons;

PROCEDURE ntohs (s: Ctypes.unsigned_short_int): Ctypes.unsigned_short_int =
  BEGIN
    RETURN WinSock.ntohs (s);
  END ntohs;

BEGIN
END TCPExtras.
