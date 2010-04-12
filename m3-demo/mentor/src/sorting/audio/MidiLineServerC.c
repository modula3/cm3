/* Copyright 1992 Digital Equipment Corporation.             */
/* Distributed only by permission.                           */
/* Last modified on Thu Jun 20 17:23:58 PDT 1996 by heydon   */
/*      modified on Tue May  3 19:06:17 PDT 1994 by najork   */
/*      modified on Tue Dec 22 13:56:41 PST 1992 by mhb      */
/*      modified on Fri Nov 20 15:49:27 PST 1992 by sclafani */

  DelayMicroseconds = 250000;

PROCEDURE Rpc (t: T; op: Operation; VAR request: Command; reqSize: INTEGER)
  RAISES {Failure} =
  VAR
    readfds: Unix.FDSet;
    timeout: Utime.struct_timeval;
    result : INTEGER;
    reply  : Command;
  BEGIN

    timeout.tv_sec := 0;
    timeout.tv_usec := DelayMicroseconds;

    FOR i := 1 TO MaxTries DO
      result := Usocket.send (t.socket, ADR (request), reqSize, 0);
      IF result < 0 THEN UnixFail ("send"); END;
      IF result # reqSize THEN RAISE Failure ("partial send!?!"); END;
      readfds := Unix.FDSet {t.socket};
      result := Unix.select (t.socket + 1, ADR (readfds), NIL, NIL,
                             ADR (timeout));
