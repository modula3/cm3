(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by wobber *)
(* $Id: SupTCPHack.m3,v 1.2 2009-04-15 11:09:55 jkrell Exp $ *)

UNSAFE MODULE SupTCPHack;

IMPORT Uin, Usocket;
FROM Ctypes IMPORT int;

CONST TCP_NODELAY = 1;

PROCEDURE RefetchError(fd: INTEGER): BOOLEAN =
  VAR optbuf: int := 0;   optlen := BYTESIZE(optbuf);
  BEGIN
    RETURN Usocket.getsockopt(fd, Uin.IPPROTO_TCP, TCP_NODELAY,
                              ADR(optbuf), ADR(optlen)) >= 0;
  END RefetchError;

BEGIN
END SupTCPHack.
