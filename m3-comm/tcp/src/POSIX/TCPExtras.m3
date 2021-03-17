(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Tue Jun 27 15:07:52 PDT 1995 by steveg *)

UNSAFE MODULE TCPExtras;

IMPORT Ctypes, IP, IPError, TCP, TCPPosix, Uin, Usocket;

PROCEDURE LocalEndpoint (conn: TCP.T): IP.Endpoint RAISES {IP.Error} =
  VAR
    addr : Uin.struct_sockaddr_in;
    len  : Usocket.socklen_t := BYTESIZE (addr);
    ep   : IP.Endpoint;
  BEGIN
    LOCK conn DO
      IF conn.closed THEN IPError.Raise (TCP.Closed); END;
      IF Usocket.getsockname (conn.fd, ADR (addr), ADR(len)) < 0 THEN
        IPError.RaiseUnexpected ();
      END;
    END;
    ep.addr := LOOPHOLE (addr.sin_addr, IP.Address);
    ep.port := Uin.ntohs (addr.sin_port);
    RETURN ep;
  END LocalEndpoint;

PROCEDURE htons (s: Ctypes.unsigned_short_int): Ctypes.unsigned_short_int =
  BEGIN
    RETURN Uin.htons (s);
  END htons;

PROCEDURE ntohs (s: Ctypes.unsigned_short_int): Ctypes.unsigned_short_int =
  BEGIN
    RETURN Uin.ntohs (s);
  END ntohs;

BEGIN
END TCPExtras.
