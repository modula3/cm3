(* Copyright (C) 1995, Digital Equipment Corporation             *)
(* All rights reserved.                                          *)
(* See the file COPYRIGHT for a full description.                *)
(*                                                               *)
(* Last modified on Tue Mar  7 14:41:39 PST 1995 by kalsow       *)
(*                                                               *)
(* Enhanced by Peter Klein (pk@i3.informatik.rwth-aachen.de) to  *)
(* reject connections from outside this domain.  - Mar 7, 1995   *)

MODULE TCPServer;

IMPORT Text, Thread, Time, TCP, IP, ConnFD, Rd, Wr, Atom, AtomList;
IMPORT TCPPeer;

REVEAL
  T = Thread.Closure BRANDED OBJECT
    workers   : REF ARRAY OF Thread.T := NIL;
    port      : TCP.Connector         := NIL;
    handler   : RequestHandler        := NIL;
    refresher : Refresher             := NIL;
    timeout   : Time.T                := 0.0d0;
    err_log   : ErrorLogger           := NIL;
    address   : IP.Address;
    maskBits  : [0 .. 32];
  OVERRIDES
    apply := Server;
  END;

TYPE
  TT = Thread.Closure OBJECT
    self: T := NIL;
  OVERRIDES
    apply := Refresh;
  END;

(*---------------------------------------------------- external interface ---*)

PROCEDURE Fork (socket    : CARDINAL;
                n_threads : CARDINAL;
                handler   : RequestHandler;
                refresher : Refresher;
                refresh_interval: Time.T;
                err_log   : ErrorLogger;
                address   : IP.Address := IP.NullAddress;
                maskBits  : [0 .. 32] := 0): T =

  VAR t := NEW (T);
  BEGIN
    IF (err_log = NIL) THEN err_log := DumpErr; END;
    t.workers   := NEW (REF ARRAY OF Thread.T, n_threads+1);
    t.handler   := handler;
    t.refresher := refresher;
    t.timeout   := refresh_interval;
    t.err_log   := err_log;
    t.address   := address;
    t.maskBits  := maskBits;

    (* open a TCP connection *)
    TRY
      t.port := TCP.NewConnector (IP.Endpoint {IP.GetHostAddr (), socket});
    EXCEPT IP.Error(ec) =>
      err_log ("cannot open TCP connection" & OSErr (ec));
      RETURN NIL;
    END;

    (* fire up the refresh thread *)
    IF (refresher # NIL) AND (refresh_interval > 0.0d0)
      THEN t.workers[0] := Thread.Fork (NEW (TT, self := t));
      ELSE t.workers[0] := NIL;
    END;

    (* fire up the server threads *)
    FOR i := 1 TO n_threads DO  t.workers[i] := Thread.Fork (t);  END;

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

(*------------------------------------------------- request server thread ---*)

PROCEDURE Server (closure: Thread.Closure): REFANY =
  CONST Second = 1000.0d0;
  VAR
    self    : T := closure;
    channel : TCP.T;
    len, j, n : INTEGER;
    request : TEXT;
    buf     : ARRAY [0..2047] OF CHAR;
  BEGIN
    TRY
      LOOP
        TRY
          request := ""; (* give the collector a chance while we wait... *)
          channel := TCP.Accept (self.port);
          TRY

            IF DomainOK (channel,self.address,self.maskBits) THEN
              (* read a new-line terminated request *)
              REPEAT
                len := channel.get (buf, 30.0d0 * Second);
                j := 0;  WHILE (j < len) AND (buf[j] # '\n') DO INC (j) END;
                request := request & Text.FromChars (SUBARRAY (buf, 0, j));
              UNTIL (j < len OR len = 0);

              (* process it *)
              request := self.handler (self, request);
            ELSE
              request :=
                "HTTP/1.0 403 Service not available from outside, sorry\r\n";
              self.err_log("illegal request from " & TCPPeer.GetName(channel));
            END;

            (* send the reply *)
            len := Text.Length (request);
            j := 0;
            WHILE (j < len) DO
              n := MIN (NUMBER (buf), len-j);
              FOR k := 0 TO n-1 DO  buf[k] := Text.GetChar (request, k+j); END;
              channel.put (SUBARRAY (buf, 0, n));
              INC (j, NUMBER (buf));
            END;

          FINALLY
            TCP.Close (channel);
          END;
        EXCEPT
        | ConnFD.TimedOut =>
            self.err_log ("TCPServer: ConnFD.TimedOut => client is non-responsive");
        | IP.Error(ec) =>
            IF FatalError (self, ec, "IP.Error") THEN RETURN NIL; END;
        | Rd.Failure(ec) =>
            IF FatalError (self, ec, "Rd.Failure") THEN RETURN NIL; END;
        | Wr.Failure(ec) =>
            IF FatalError (self, ec, "Wr.Failure") THEN RETURN NIL; END;
        END;
      END;
    EXCEPT Thread.Alerted =>
      (* bail out... *)
      self.err_log ("TCPServer: server thread was alerted");
      Alert (self);
    END;
    RETURN NIL;
  END Server;

PROCEDURE DomainOK (channel: TCP.T; address: IP.Address; maskBits: [0 .. 32]):
    BOOLEAN RAISES {IP.Error} =
  BEGIN
    RETURN TCPPeer.Match (channel, address, maskBits);
  END DomainOK;

PROCEDURE FatalError (self: T;  ec: AtomList.T;  msg: TEXT): BOOLEAN =
  BEGIN
    self.err_log ("TCPServer: " & msg & OSErr (ec));
    IF (ec # NIL) THEN
      IF (ec.head = TCP.Refused)  THEN RETURN FALSE; END;
      IF (ec.head = TCP.Closed)   THEN RETURN FALSE; END;
      IF (ec.head = TCP.Timeout)  THEN RETURN FALSE; END;
      IF (ec.head = TCP.ConnLost) THEN RETURN FALSE; END;
    END;

    (* Don't know what's happening => bail out ... *)
    self.err_log ("TCPServer: aborting...");
    Alert (self);
    RETURN TRUE;
  END FatalError;

(*----------------------------------------------- periodic refresh thread ---*)

PROCEDURE Refresh (closure: Thread.Closure): REFANY =
  VAR tt: TT := closure;  self := tt.self;
  BEGIN
    TRY
      LOOP
        Thread.AlertPause (self.timeout);
        self.refresher (self);
      END;
    EXCEPT Thread.Alerted =>
      (* bail out... *)
      self.err_log ("TCPServer: refresh thread was alerted");
      Alert (self);
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

PROCEDURE OSErr (args: AtomList.T): TEXT =
  VAR msg : TEXT := NIL;
  BEGIN
    WHILE (args # NIL) DO
      IF (msg = NIL) THEN  msg := ": ";  ELSE  msg := msg & "  ***  ";  END;
      msg  := msg & Atom.ToText (args.head);
      args := args.tail;
    END;
    IF (msg = NIL) THEN msg := ": ** NO INFO **"; END;
    RETURN msg;
  END OSErr;

PROCEDURE DumpErr (<*UNUSED*> x: TEXT) =
  BEGIN
  END DumpErr;

BEGIN
END TCPServer.
