(* Copyright (C) 1995, Digital Equipment Corporation             *)
(* All rights reserved.                                          *)
(* See the file COPYRIGHT for a full description.                *)
(*                                                               *)
(* Last modified on Tue Mar  7 14:41:39 PST 1995 by kalsow       *)
(*                                                               *)
(* Enhanced by Peter Klein (pk@i3.informatik.rwth-aachen.de) to  *)
(* reject connections from outside this domain.  - Mar 7, 1995   *)

MODULE TCPServer;

IMPORT Text, Thread, TCP, IP, ConnFD, Rd, Wr, AtomList;
IMPORT TCPPeer, Wx, OS;

REVEAL
  T = BRANDED "TCPServer.T" REF RECORD
    workers : REF ARRAY OF Thread.T := NIL;
    err_log : ErrorLogger           := NIL;
    port    : TCP.Connector         := NIL;
  END;

TYPE
  Worker = Thread.Closure BRANDED "TCPServer.Worker" OBJECT
    server  : T              := NIL;
    handler : RequestHandler := NIL;
  OVERRIDES
    apply := Server;
  END;

TYPE
  RefreshWorker = Thread.Closure BRANDED "TCPServer.Refresher" OBJECT
    server    : T         := NIL;
    refresher : Refresher := NIL;
    timeout   : INTEGER   := 0;
  OVERRIDES
    apply := Refresh;
  END;

(*---------------------------------------------------- external interface ---*)

PROCEDURE Fork (READONLY host_addr : IP.Address;
                         socket    : CARDINAL;
                         n_threads : CARDINAL;
                         handler   : RequestHandler;
                         refresher : Refresher;
                         refresh_interval: INTEGER;
                         err_log   : ErrorLogger): T =
  VAR t := NEW (T);
  BEGIN
    IF (err_log = NIL) THEN err_log := DumpErr; END;
    t.workers   := NEW (REF ARRAY OF Thread.T, n_threads+1);
    t.err_log   := err_log;

    (* open a TCP connection *)
    TRY
      t.port := TCP.NewConnector (IP.Endpoint {host_addr, socket});
    EXCEPT IP.Error(ec) =>
      err_log ("cannot open TCP connection" & OS.Err (ec));
      RETURN NIL;
    END;

    (* fire up the refresh thread *)
    t.workers[0] := NIL;
    IF (refresher # NIL) AND (refresh_interval > 0) THEN
      t.workers[0] := Thread.Fork (NEW (RefreshWorker, server := t,
                                        refresher := refresher,
                                        timeout := refresh_interval));
    END;

    (* fire up the server threads *)
    FOR i := 1 TO n_threads DO
      t.workers[i] := Thread.Fork (NEW (Worker, server := t, handler := handler));
    END;

    RETURN t;
  END Fork;

PROCEDURE Join (t: T) =
  VAR z: Thread.T;
  BEGIN
    IF (t = NIL) THEN RETURN END;
    FOR i := 0 TO LAST (t.workers^) DO
      z := t.workers [i];
      IF (z # NIL) THEN
        EVAL Thread.Join (z);
        t.workers[i] := NIL;
      END;
    END;
    IF (t.port # NIL) THEN
      (** TCP.CloseConnector (t.port); *** NOT YET IMPLEMENTED 2/8/95 ***)
      t.port := NIL;
    END;
  END Join;

PROCEDURE Abort (t: T) =
  BEGIN
    Alert (t);
    Join (t);
  END Abort;

(*---------------------------------------------- request server thread ---*)

PROCEDURE Server (self: Worker): REFANY =
  VAR
    server  : T := self.server;
    channel : TCP.T;
    wx      : Wx.T := NEW (Wx.T);
  BEGIN
    TRY
      LOOP
        TRY
          channel := TCP.Accept (server.port);
          TRY
            EVAL wx.init (channel);
            IF DomainOK (channel) THEN
              self.handler (ReadLine (channel), wx);
            ELSE
              server.err_log ("illegal request from " & TCPPeer.GetName (channel));
              wx.put ("HTTP/1.0 403 Service not available from outside, sorry\r\n");
            END;
            wx.flush ();
          FINALLY
            TCP.Close (channel);
            channel := NIL;
            EVAL wx.init (NIL);
          END;
        EXCEPT
        | ConnFD.TimedOut =>
            server.err_log ("ConnFD.TimedOut => client is non-responsive");
        | IP.Error(ec) =>
            IF FatalError (server, ec, "IP.Error") THEN EXIT; END;
        | Rd.Failure(ec) =>
            IF FatalError (server, ec, "Rd.Failure") THEN EXIT; END;
        | Wr.Failure(ec) =>
            IF FatalError (server, ec, "Wr.Failure") THEN EXIT; END;
        END;
      END;
    EXCEPT Thread.Alerted => (* bail out... *)
      (***  server.err_log ("TCPServer: server thread was alerted");  ***)
      Alert (server);
    END;
    RETURN NIL;
  END Server;

PROCEDURE ReadLine (channel: TCP.T): TEXT
  RAISES {Rd.Failure, Thread.Alerted, ConnFD.TimedOut} =
  (* read a new-line terminated request *)
  CONST Second = 1000.0d0;
  VAR
    result : TEXT := "";
    len, j : INTEGER;
    buf    : ARRAY [0..2047] OF CHAR;
  BEGIN
    REPEAT
      len := channel.get (buf, 30.0d0 * Second);
      j := 0;  WHILE (j < len) AND (buf[j] # '\n') DO INC (j) END;
      result := result & Text.FromChars (SUBARRAY (buf, 0, j));
    UNTIL (j < len);
    RETURN result;
  END ReadLine;

PROCEDURE DomainOK (<*UNUSED*> channel: TCP.T): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END DomainOK;

(******* We don't know how to compute our own mask ****
PROCEDURE DomainOK (channel: TCP.T): BOOLEAN RAISES {IP.Error} =
  VAR mask : IP.Address := IP.NullAddress;
  BEGIN
    RETURN TCPPeer.Match (channel, mask, 0);
  END DomainOK;
*********************************************************)

PROCEDURE FatalError (server: T;  ec: AtomList.T;  msg: TEXT): BOOLEAN =
  BEGIN
    server.err_log ("TCPServer: " & msg & OS.Err (ec));
    IF (ec # NIL) THEN
      IF (ec.head = TCP.Refused)  THEN RETURN FALSE; END;
      IF (ec.head = TCP.Closed)   THEN RETURN FALSE; END;
      IF (ec.head = TCP.Timeout)  THEN RETURN FALSE; END;
      IF (ec.head = TCP.ConnLost) THEN RETURN FALSE; END;
    END;

    (* Don't know what's happening => bail out ... *)
    server.err_log ("TCPServer: aborting...");
    Alert (server);
    RETURN TRUE;
  END FatalError;

(*----------------------------------------------- periodic refresh thread ---*)

PROCEDURE Refresh (self: RefreshWorker): REFANY =
  VAR pause := 60.0D0 * FLOAT (MAX (1, self.timeout), LONGREAL);
  BEGIN
    TRY
      LOOP
        self.refresher (self.server);
        Thread.AlertPause (pause);
      END;
    EXCEPT Thread.Alerted =>
      (* bail out... *)
      (*** self.server.err_log ("TCPServer: refresh thread was alerted");  ***)
      Alert (self.server);
    END;
    RETURN NIL;
  END Refresh;

(*------------------------------------------------------------------ misc ---*)

PROCEDURE Alert (t: T) =
  VAR z: Thread.T;
  BEGIN
    IF (t = NIL) THEN RETURN END;
    FOR i := 0 TO LAST (t.workers^) DO
      z := t.workers[i];
      IF (z # NIL) THEN Thread.Alert (z); END;
    END;
  END Alert;

PROCEDURE DumpErr (<*UNUSED*> x: TEXT) =
  BEGIN
  END DumpErr;

BEGIN
END TCPServer.
