(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Sun Dec 29 15:46:24 PST 1996 by steveg *)

INTERFACE HTTPProxy;

IMPORT App, HTTP, HTTPApp, Rd, Wr;

(* An HTTPApp module that handles proxying. *)

TYPE
  RequestHandler <: RequestHandlerPublic;
  RequestHandlerPublic = HTTPApp.RequestHandler OBJECT
    service: INTEGER;
    (* the service number given to the "HTTPApp.Server" call *)
    logRequest, logReply: BOOLEAN := FALSE;
    (* if "logRequest" = TRUE, then the body of the request is 
       copied to "log".  if "logReply" = TRUE, then the body
       of the reply is logged. *)
  METHODS
    replyHandlerGenerator(request: HTTP.Request; 
                          serverData, acceptState: REFANY;
            rd: Rd.T; wr: Wr.T; log: App.Log): HTTPApp.ReplyHandler;
  END;
    
  ReplyHandler <: HTTPApp.ReplyHandler;

  (* The default request handler, copies the
     request through to the request destination.

     The default reply handler, copies the reply 
     back to the request source.
  *)

END HTTPProxy.
