(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Tue Aug 27 16:00:14 PDT 1996 by steveg   *)
(*      modified on Tue Jul 25 15:59:38 PDT 1995 by glassman *)
(* modified on Thu Sep 29 12:18:36 PDT 1994 by mhb *)

MODULE HTTPCat EXPORTS Main;
IMPORT App, FloatMode, Fmt, HTTP, HTTPApp, Lex, Rd, Stdio, Text, TextRd,
       Thread, Wr;

VAR
  method := HTTP.Method.Get;
  noCache := FALSE;
  post: TEXT := NIL;
  url: TEXT := NIL;
  fieldName: TEXT := NIL;
  fieldValue: TEXT := NIL;
  authName, authPassword: TEXT := NIL;
  proxy: HTTPApp.Proxy;
  version := HTTP.Version1_1;

TYPE
  Arg = {Auth, Field, Head, NoCache, Post, URL, Version};

  ArgHandler = App.ArgHandler OBJECT
  OVERRIDES
    set := SetArg;
  END;

CONST
  NonColon = SET OF CHAR{'\000'..'\377'} - SET OF CHAR{':'};

PROCEDURE SetArg (self : ArgHandler;
                  src  : App.ArgSource;
                  value: TEXT;
                  log  : App.Log        ) RAISES {App.Error} =
  BEGIN
    CASE VAL(self.id, Arg) OF
    | Arg.Head =>
        IF src # App.ArgSource.Default THEN method := HTTP.Method.Head END;
    | Arg.NoCache => noCache := src # App.ArgSource.Default;
    | Arg.Field =>
        TRY
          WITH trd = TextRd.New(value) DO
            fieldName := Lex.Scan(trd);
            Lex.Skip(trd);
            fieldValue := Lex.Scan(trd);
          END;
        EXCEPT
        | Rd.Failure, Thread.Alerted =>
            log.log(
              Fmt.F("Bad field argument: %s", value), App.LogStatus.Error);
        END;
    | Arg.Version =>
        TRY
          WITH trd = TextRd.New(value) DO
            version.major := Lex.Int(trd);
            EVAL Rd.GetChar(trd);
            version.minor := Lex.Int(trd);
          END;
        EXCEPT
        | FloatMode.Trap, Lex.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted =>
            log.log(Fmt.F("Bad version argument: %s", value),
                    App.LogStatus.Error);
        END;
    | Arg.Auth =>
        IF src # App.ArgSource.Default THEN
          TRY
            WITH trd = TextRd.New(value) DO
              authName := Lex.Scan(trd, NonColon);
              EVAL Rd.GetChar(trd);
              authPassword := Rd.GetText(trd, LAST(INTEGER));
            END;
          EXCEPT
          | Rd.EndOfFile, Rd.Failure, Thread.Alerted =>
              log.log(
                Fmt.F("Bad auth argument: %s", value), App.LogStatus.Error);
          END;
        END;
    | Arg.Post =>
        IF src # App.ArgSource.Default THEN
          method := HTTP.Method.Post;
          post := value;
        END;
    | Arg.URL =>
        url := value;
        IF src = App.ArgSource.Default AND value = NIL THEN
          log.log("Must give URL", App.LogStatus.Error);
        END;
    END;
  END SetArg;

TYPE
  ReplyHandler = HTTPApp.ReplyHandler OBJECT
  OVERRIDES
    reply := Reply;
  END;

PROCEDURE Reply(<* UNUSED *> self: HTTPApp.ReplyHandler; reply: HTTP.Reply; 
                rd: Rd.T; wr: Wr.T; log: App.Log) 
          RAISES {App.Error} =
  BEGIN
    IF App.Debug() OR method = HTTP.Method.Head THEN
      reply.write(wr, HTTP.DefaultStyle(version), log);
    END;
    IF App.Debug() OR method # HTTP.Method.Head THEN
      HTTP.WriteBody(reply, wr, NEW(HTTP.RdSrc).init(rd), log);
    END;
  END Reply;

BEGIN
  EVAL NEW(ArgHandler, id := ORD(Arg.Head), hasParam := FALSE).init(
         switchName := "head");
  EVAL NEW(ArgHandler, id := ORD(Arg.NoCache), hasParam := FALSE).init(
         switchName := "noCache");
  EVAL NEW(ArgHandler, id := ORD(Arg.Post), hasParam := FALSE).init(
         switchName := "post");
  EVAL
    NEW(ArgHandler, id := ORD(Arg.Version), paramName := "<major.minor>",
        default := "1.1").init(switchName := "version");
  EVAL NEW(ArgHandler, id := ORD(Arg.Field),
           paramName := "<HTTP field: value>").init(switchName := "field");
  EVAL
    NEW(
      ArgHandler, id := ORD(Arg.Auth), paramName := "<name:password>").init(
      switchName := "auth");
  EVAL NEW(ArgHandler, id := ORD(Arg.URL), hasParam := FALSE).init(
         switchName := App.AnyArgument);

  TRY
    App.InitializeArguments(App.defaultLog, "/proj/m3/pkg/webcat/config", FALSE);
    proxy := HTTPApp.DefaultProxy();
    HTTP.SetProgramInfo(
      HTTP.ProgramInfo{type := HTTP.ProgramType.Client, name :=
                       "webcat/1.1 (SRC Modula-3)"});
    VAR
      rd       : Rd.T         := NIL;
      request  : HTTP.Request;
      urlParsed: HTTP.URL;
    BEGIN
      TRY
        urlParsed := NEW(HTTP.URL).init(url, App.nullLog);
        IF Text.Length(urlParsed.host) = 0 THEN RAISE App.Error(NIL) END;
      EXCEPT
      | App.Error =>
          TRY
            (* try again, to handle www.foo.bar/xxx URL's *)
            urlParsed := NEW(HTTP.URL).init("http://" & url, App.nullLog);
          EXCEPT
          | App.Error =>
              (* still no go, then complain about the original *)
              urlParsed := NEW(HTTP.URL).init(url, App.defaultLog);
          END;
      END;

      request := NEW(HTTP.Request, method := method, url := urlParsed);
      IF noCache THEN
        EVAL
          request.addField(
            NEW(HTTP.Field).init(name := "Pragma", value := "no-cache"));
      END;
      IF fieldName # NIL AND Text.Length(fieldName) # 0 THEN
        EVAL request.addField(NEW(HTTP.Field).init(
                                name := fieldName, value := fieldValue));
      END;

      IF authName # NIL THEN
        EVAL request.addField(
               HTTP.BasicAuthField(
                 authName & ":" & authPassword, HTTP.AuthType.Server));
      END;

      IF method = HTTP.Method.Post THEN
        rd := TextRd.New(post);
      ELSE
        rd := TextRd.New("");
      END;

      IF App.Verbose() THEN
        request.write(
          Stdio.stderr, HTTP.DefaultStyle(version), FALSE, App.defaultLog);
      END;

      HTTPApp.Client(request, proxy, HTTP.DefaultStyle(version), rd,
                     Stdio.stdout, NEW(ReplyHandler), App.defaultLog);

    END;
  EXCEPT
  | App.Error =>
  END;
END HTTPCat.
