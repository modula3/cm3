(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun  1 14:01:33 PDT 1993 by kalsow                   *)

INTERFACE TCPServer;

IMPORT Thread, Time, IP;


TYPE
  T <: REFANY;

TYPE
  RequestHandler = PROCEDURE (t: T;  request: TEXT): TEXT
                      RAISES {Thread.Alerted};

TYPE
  Refresher = PROCEDURE (t: T) RAISES {Thread.Alerted};

TYPE
  ErrorLogger = PROCEDURE (x: TEXT);

PROCEDURE Fork (socket    : CARDINAL;
                n_threads : CARDINAL;
                handler   : RequestHandler;
                refresher : Refresher;
                refresh_interval: Time.T;
                err_log   : ErrorLogger;
                address   : IP.Address := IP.NullAddress;
                maskBits  : [0 .. 32] := 0): T;

PROCEDURE Join (t: T);
PROCEDURE Abort (t: T);

END TCPServer.
