(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Tue Jun 27 15:07:52 PDT 1995 by steveg *)

UNSAFE MODULE TCPExtras;

IMPORT Ctypes, IP, IPError, IPInternal, TCP, TCPPosix, Uin;

PROCEDURE LocalEndpoint (conn: TCP.T): IP.Endpoint RAISES {IP.Error} =
  VAR port := 0;
      ep   : IP.Endpoint;
  BEGIN
    LOCK conn DO
      IF conn.closed THEN IPError.Raise (TCP.Closed); END;
      IF IPInternal.getsockname (conn.fd, ADR(ep.addr.a[0]), port) < 0 THEN
        IPError.RaiseUnexpected ();
      END;
    END;
    ep.port := Uin.ntohs(port);
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
