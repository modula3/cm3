(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Tue Oct 10 13:49:05 PDT 1995 by steveg   *)
(*      modified on Tue Jul 25 15:59:38 PDT 1995 by glassman *)
(* modified on Thu Sep 29 12:18:36 PDT 1994 by mhb *)

MODULE Main EXPORTS Main;
IMPORT Env, Fmt, IP, Params, Process, Rd, RdUtils, Text, Thread, Wr, Web,
       RdCopy;
FROM Stdio IMPORT stderr, stdout;

<* FATAL Thread.Alerted *>

CONST HomePageEnvVar = "WWW_HOME";
TYPE RequestType = {Get, Post, GetDebug, Head};

PROCEDURE ReportError (msg: TEXT := NIL) =
  <* FATAL Wr.Failure *>
  BEGIN
    IF msg = NIL THEN
      Wr.PutText(
        stderr, " Usage? webcat [-post args -debug -head -noCache -proxy XXX] URL\n"); 
    ELSE
      Wr.PutText(stderr, msg & "\n");
    END;
    Process.Exit(1);

  END ReportError;


PROCEDURE Get (url       : TEXT; args: TEXT := NIL;
               forceCache: BOOLEAN       := FALSE;
               type      : RequestType;
               server    : TEXT          := NIL    )
  RAISES {IP.Error, Web.Error} =
  VAR
    rd         : Rd.T;
    proxyServer: Web.T      := NIL;
    h          : Web.Header;
  BEGIN
    IF url = NIL THEN ReportError(NIL); END;
    IF server # NIL THEN proxyServer := Web.Setup(server); END;
    TRY
    CASE type OF
      RequestType.Head =>
        rd := Web.GetHead(
                url, Web.DefaultRequestFields, forceCache, proxyServer);
        EVAL RdCopy.ToWriter(Web.GetHead(url), stdout);
    | RequestType.GetDebug =>
        rd := Web.Get(url, h, Web.DefaultRequestFields, forceCache, TRUE, proxyServer);
        IF h.statusCode >= 400 THEN
          ReportError(Fmt.F("Error '%s' for url %s", h.reason, url));
        ELSE
          EVAL RdCopy.ToWriter(rd, stdout); 
        END;
    | RequestType.Get =>
        rd := Web.Get(url, h, Web.DefaultRequestFields, forceCache, FALSE, proxyServer);
        IF h.statusCode >= 400 THEN
          ReportError(Fmt.F("Error '%s' for url %s", h.reason, url));
        ELSIF h.contentType # Web.MIMEType.Text THEN
          ReportError(Fmt.F("Error for url %s: The content type of this URL is not 'text'\n", url));
        ELSE
          EVAL RdCopy.ToWriter(rd, stdout);
        END;
    | RequestType.Post =>
        rd := Web.Post(url, args, h, Web.DefaultRequestFields, proxyServer);
        IF h.statusCode >= 400 THEN
          ReportError(Fmt.F("Error '%s' for url %s", h.reason, url));
        ELSIF h.contentType # Web.MIMEType.Text THEN
          ReportError(Fmt.F("Error for url %s: The content type of this URL is not 'text'\n", url));
        ELSE
          EVAL RdCopy.ToWriter(rd, stdout);
        END;
    END;
    Rd.Close(rd);
    EXCEPT 
    | Wr.Failure =>
    | Rd.Failure => 
        Wr.PutText(stderr, "Error reading from WWW server\n"); <* NOWARN *>
    END;
  END Get;



PROCEDURE main () =
  VAR
    url, arg   : TEXT        := NIL;
    i          : INTEGER     := 1;
    forceCache : BOOLEAN     := FALSE;
    type       : RequestType := RequestType.Get;
    proxyServer: TEXT        := NIL;
    args: TEXT := NIL; (* for Post methods *)
  BEGIN
    TRY
      WHILE i < Params.Count DO
        arg := Params.Get(i);
        IF Text.Length(arg) = 0 THEN
          ReportError("empty argument #" & Fmt.Int(i))
        END;
        IF Text.GetChar(arg, 0) = '-' THEN
          IF Text.Equal(arg, "-debug") THEN
            type := RequestType.GetDebug;
          ELSIF Text.Equal(arg, "-head") THEN
            (* if it's already debug, then leave that way *)
            IF type = RequestType.Get THEN
              type := RequestType.Head;
            END;
          ELSIF Text.Equal(arg, "-post") THEN
            type := RequestType.Post;      
            INC(i); (* the -post part *)
            IF i = Params.Count THEN ReportError(); RETURN; END;
            url := Params.Get(i); INC(i);
            IF i = Params.Count THEN ReportError(); RETURN; END;
            args := Params.Get(i); 
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
      url := Web.AbsoluteURL(url, Env.Get(HomePageEnvVar));
      Get(url, args, forceCache, type, proxyServer);

    EXCEPT
    | IP.Error (ec) =>
        ReportError(
          Fmt.F("IP error: %s for URL: %s", RdUtils.FailureText(ec), url));
    | Web.Error (t) => ReportError(Fmt.F("Webcat error: %s", t));
    END;
  END main;



BEGIN
  main()
END Main.
