(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Thu Feb  1 15:36:21 PST 1996 by steveg *)

INTERFACE HTTPProxy;

IMPORT App, HTTP, HTTPApp, Rd, Wr;

(* An HTTPApp module that handles proxying. *)

TYPE
  RequestHandler <: RequestHandlerPublic;
  RequestHandlerPublic = HTTPApp.RequestHandler OBJECT
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
