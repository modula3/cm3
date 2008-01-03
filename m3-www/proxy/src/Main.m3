(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Mon Oct 17 22:51:33 PDT 1994 by glassman *)
(* modified on Thu Sep 29 12:18:36 PDT 1994 by mhb *)

MODULE Main EXPORTS Main;
IMPORT Fmt, IP, Params, Process, Rd, RdUtils, Text, Thread, Wr, Web, RdCopy;
FROM Stdio IMPORT stderr, stdout;

<* FATAL Thread.Alerted, Wr.Failure, Rd.Failure *>

PROCEDURE ReportError (msg: TEXT := NIL) =
  BEGIN
   IF msg = NIL THEN 
      Wr.PutText(stderr," Usage? webcat [-debug -noCache -proxy XXX] URL\n"); 
    ELSE
      Wr.PutText(stderr, msg & "\n"); 
    END;
    Process.Exit(1);
  END ReportError;

PROCEDURE Get (url        : TEXT;
               debug, forceCache: BOOLEAN := FALSE;
               server: TEXT    := NIL   )
  RAISES {IP.Error, Web.Error} =
  VAR
    rd: Rd.T;
    proxyServer: Web.T := NIL;
    h : Web.Header;
  BEGIN
    IF url = NIL THEN ReportError(NIL); END;
    IF server # NIL THEN
       proxyServer := Web.Setup(server); END;
    IF debug THEN
      rd := Web.GetHead(url, Web.DefaultRequestFields, forceCache, proxyServer);
      EVAL RdCopy.ToWriter(Web.GetHead(url), stdout);
    ELSE
      rd := Web.Get(url, h, Web.DefaultRequestFields, forceCache, proxyServer);
      IF h.statusCode >= 400 THEN
        ReportError( Fmt.F("Error '%s' for url %s", h.reason, url));
      ELSE
        EVAL RdCopy.ToWriter(rd, stdout);
      END;
    END;
    Rd.Close(rd);
  END Get;


PROCEDURE main () =
  VAR
    url, arg: TEXT    := NIL;
    i       : INTEGER := 1;
    debug, forceCache   : BOOLEAN := FALSE;
    proxyServer  : TEXT    := NIL;
  BEGIN
    TRY
      WHILE i < Params.Count DO
        arg := Params.Get(i);
        IF Text.Length(arg) = 0 THEN
          ReportError("empty argument #" & Fmt.Int(i))
        END;
        IF Text.GetChar(arg, 0) = '-' THEN
          IF Text.Equal(arg, "-debug") THEN
            debug := TRUE;
          ELSIF Text.Equal(arg, "-noCache") THEN
            forceCache := TRUE;
          ELSIF Text.Equal(arg, "-proxy") THEN
            INC(i);
            proxyServer := Params.Get(i);
          ELSE
            ReportError(NIL);
          END;
        ELSE
          url := Params.Get(i);
        END;
        INC(i);
      END;                       (* while *)
     Get(url, debug, forceCache, proxyServer);
    EXCEPT
(****
      Rd.Failure (ec) =>
        ReportError(Fmt.F("reader failure: %s for URL: %s",
                                 RdUtils.FailureText(ec), url));
    | Wr.Failure =>              (* probably broken pipe through "head";
                                    okay by me *)
***)
    | IP.Error (ec) =>
        ReportError(Fmt.F("IP error: %s for URL: %s",
                                 RdUtils.FailureText(ec), url));
    | Web.Error (t) => ReportError(Fmt.F("Webcat error: %s", t));
    END;
  END main;


BEGIN
  main()
END Main.

