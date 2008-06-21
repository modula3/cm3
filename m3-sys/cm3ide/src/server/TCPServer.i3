(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun  1 14:01:33 PDT 1993 by kalsow                   *)

INTERFACE TCPServer;

IMPORT IP, Thread, Wx, Wr;

TYPE
  T <: REFANY;

TYPE
  RequestHandler = PROCEDURE (request: TEXT;  response: Wx.T)
                      RAISES {Wr.Failure, Thread.Alerted};

TYPE
  Refresher = PROCEDURE (t: T) RAISES {Thread.Alerted};

TYPE
  ErrorLogger = PROCEDURE (x: TEXT);

PROCEDURE Fork (READONLY address   : IP.Address;
                         socket    : CARDINAL;
                         n_threads : CARDINAL;
                         handler   : RequestHandler;
                         refresher : Refresher;
                         refresh_interval: INTEGER;
                         err_log   : ErrorLogger): T;

PROCEDURE Join (t: T);
PROCEDURE Abort (t: T);

END TCPServer.
