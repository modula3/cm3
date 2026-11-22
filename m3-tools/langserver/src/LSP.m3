(* Copyright (C) 2023, Peter McKinna *)
(* All rights reserved. *)
(* Licensed under the MIT license. *)

MODULE LSP;

(*
  This is a Modula-3 language server.

  It waits for a message on stdin, parses it into a Json.T,
  extracts the id and method and checks if it handles the message.
  The handler may send a response on stdout.
  The protocol has two types of messages, those with and id which should
  be responded to and those without, which  are notification messages only.

  The server calls the real compiler (cm3) on startup to generate the .M3IMPTAB
  file in the target directory. This is used by m3tk, our semantic analyser
  to find and parse all the imports in the current message (uri).
*)


IMPORT Fmt, Wr, Stdio, Thread, Text, TextSeq, TextUtils, Process;
IMPORT Json, Msg, AstCompile, Feature, Utils, Debug;
IMPORT IO, Buf, RefSeq, FS, Pathname, TextRefTbl;
IMPORT RTCollectorSRC;

(*
<*FATAL ANY*>
*)

CONST
  MsgHeader    = "Content-Length: $len \r\n\r\n";
  CompileDelay = 2.0D0;

  (* error codes *)
  ParseError     = -32700;
  InvalidParams  = -32602;
  MethodNotFound = -32601;
(*
  InvalidRequest = -32600;
  InternalError = -32603;

  ServerNotInitialized = -32002;
  UnknownErrorCode = -32001;
  lspReservedErrorRangeStart = -32899;
  RequestFailed = -32803;
  ServerCancelled = -32802;
  ContentModified = -32801;
  RequestCancelled = -32800;
  lspReservedErrorRangeEnd = -32800;
*)

TYPE
  MsgType = {Initialize, Shutdown, Hover, FoldingRange, CodeLens,
             Declaration, TypeDefinition,
             (* notifications *)
             Initialized, Exit, Open, Close, Save, Change,
             (* not part of protocol *)
             Error};

  MsgClass = {Request, Notification};

  VS = OBJECT
         id       : INTEGER;
         type     : MsgType;
         method   : TEXT;
         msgClass : MsgClass;
         json     : Json.T;
         cm3Ok    : BOOLEAN    := TRUE;
         errorCode: INTEGER    := 0;
       END;

  CompilerClosure = Thread.Closure OBJECT
                      count     : CARDINAL;
                      text, uri : TEXT;
                      cnt1, cnt2: CARDINAL   := 0;
                      finish    : BOOLEAN    := FALSE;
                      mu        : MUTEX;
                      textSeqTbl: TextRefTbl.T := NIL;
                    METHODS
                      update (uri, txt: TEXT) := Update;
                      exit   ()               := Exit;
                    OVERRIDES
                      apply := Compile;
                    END;

VAR
  compilerClosure: CompilerClosure;
  compThread     : Thread.T;
  wc                               := AstCompile.wc;
  msgMu          : MUTEX;
  useCompThread                    := TRUE;


(* Debug proc for testing via command line *)
PROCEDURE CheckFile (uri: TEXT) =
  VAR
    text                    : TEXT;
    buf                     : Buf.T;
    line, col, startCol, len: INTEGER;
    hover                   : TEXT;
  BEGIN
    IF NOT Pathname.Absolute(uri) THEN
      IO.Put("relative");
      uri := FS.GetAbsolutePathname(uri);
    END;
    wc.rootPath := Pathname.Prefix(uri);
    buf := Buf.FromFile(uri, NIL);
    text := Text.FromChars(buf^);
    uri := "file://" & uri;
    IO.Put("uri:" & uri & "\n");

    CheckText(uri, text);

    line := 30;
    col := 8;
    hover := Feature.GetSymbolType(
               wc.getContext(), wc.getUnit(uri), line, col, startCol, len);

  END CheckFile;

(* run the real compiler to create the .M3IMPTAB file for use by m3tk *)
PROCEDURE RunCM3 (): INTEGER =
  VAR
    handle  : Process.T;
    exitCode: Process.ExitCode;
    params  : ARRAY [0 .. 1] OF TEXT;
    msg,targetDir : TEXT;
  BEGIN
    targetDir := wc.getTargetDir(wc.rootUri & "//Main.m3");

    Debug.Write("RunCM target dir " & targetDir & "\n");

    (* maybe periodically run cm3 to refresh in case m3makefile *)
    params[0] := "-tfile";
    params[1] := "-silent";
    handle := Process.Create("cm3", params, NIL, targetDir);
    exitCode := Process.Wait(handle);
    (* save exitCode in vs so check on each message if cm3 error then
       m3imptab prob not exist and cannot do any m3tk actions have to
       return error code in message reply and ignore notifications *)
    msg := "cm3 exit code: " & Fmt.Int(exitCode) & "\n";
    Utils.Msg(msg);
    Debug.Write("RunCM3 exit code-" & Fmt.Int(exitCode) & "<<<<<\n");
    (* start the background collector thread *)
    RTCollectorSRC.StartBackgroundCollection();
    RETURN exitCode;
  END RunCM3;

PROCEDURE CheckText (uri, text: TEXT) =
  VAR
    errors: RefSeq.T;
    msg   : TEXT;
  BEGIN
    (* FIXME compile could raise an exception eg dir not found, invalid
       extension etc and we should avoid sending a reply *)
    (*
       Dont compile if rootPath is not equal to base(uri) which means
       any of the imported units which are in libraries. The m3tk frontend
       will report an error since they were probably not built with .M3IMPTAB
       support.
    *)

    IF Text.Equal(wc.rootPath, wc.path(uri)) THEN
      errors := wc.compile(uri, text);
      (* even if no errors still send reply *)
      msg := Msg.BuildChangeReply(wc.fileName(), uri, errors);
      SendResult(msg);
    END;
  END CheckText;

PROCEDURE Compile (cl: CompilerClosure): REFANY =
  VAR
    doCompile: BOOLEAN;
    uri, text: TEXT;
  BEGIN
    LOOP
      Thread.Pause(CompileDelay);
      LOCK cl.mu DO
        IF cl.finish THEN EXIT; END;
        IF cl.cnt1 > cl.cnt2 THEN
          cl.cnt2 := cl.cnt1;
          uri := cl.uri;
          text := cl.text;
          doCompile := TRUE;
        ELSE
          doCompile := FALSE;
        END;
      END;
      IF doCompile THEN CheckText(uri, text); END;
    END;
    RETURN NIL;
  END Compile;

PROCEDURE Update (cl: CompilerClosure; uri, txt: TEXT) =
  BEGIN
    LOCK cl.mu DO INC(cl.cnt1); cl.uri := uri; cl.text := txt; END;
  END Update;

PROCEDURE Exit (cl: CompilerClosure) =
  BEGIN
    LOCK cl.mu DO cl.finish := TRUE; END;
    EVAL Thread.Join(compThread);
  END Exit;

PROCEDURE Classify (vs: VS): MsgType =
  VAR
    node: Json.T;
    ret : MsgType;
  BEGIN
    node := vs.json.find("id");
    IF node = NIL THEN
      vs.id := -1;
      vs.msgClass := MsgClass.Notification;
    ELSE
      vs.id := node.getInt();
      vs.msgClass := MsgClass.Request;
    END;

    node := vs.json.find("method");
    IF node = NIL THEN
      vs.errorCode := InvalidParams;
      RETURN MsgType.Error;
    END;
    vs.method := node.value();

    (* Messages *)

    IF Text.Equal(vs.method, "initialize") THEN
      ret := MsgType.Initialize;
    ELSIF Text.Equal(vs.method, "shutdown") THEN
      ret := MsgType.Shutdown;
    ELSIF Text.Equal(vs.method, "textDocument/hover") THEN
      ret := MsgType.Hover;
    ELSIF Text.Equal(vs.method, "textDocument/foldingRange") THEN
      ret := MsgType.FoldingRange;
    ELSIF Text.Equal(vs.method, "textDocument/codeLens") THEN
      ret := MsgType.CodeLens;
    ELSIF Text.Equal(vs.method, "textDocument/declaration") THEN
      ret := MsgType.Declaration;
    ELSIF Text.Equal(vs.method, "textDocument/typeDefinition") THEN
      ret := MsgType.TypeDefinition;

      (* Notifications *)

    ELSIF Text.Equal(vs.method, "initialized") THEN
      ret := MsgType.Initialized;
    ELSIF Text.Equal(vs.method, "exit") THEN
      ret := MsgType.Exit;
    ELSIF Text.Equal(vs.method, "textDocument/didOpen") THEN
      ret := MsgType.Open;
    ELSIF Text.Equal(vs.method, "textDocument/didClose") THEN
      ret := MsgType.Close;
    ELSIF Text.Equal(vs.method, "textDocument/didChange") THEN
      ret := MsgType.Change;
    ELSIF Text.Equal(vs.method, "textDocument/didSave") THEN
      ret := MsgType.Save;
    ELSE
      vs.errorCode := MethodNotFound;
      ret := MsgType.Error;
    END;

    RETURN ret;
  END Classify;

PROCEDURE CheckResult (msg: TEXT) =
  VAR json: Json.T;
  BEGIN
    (*
        Debug.Write("Result json " & msg & "\n");
    *)
    (* debug check if result is correct json.*)
    Debug.Write("Checking json reply\n");
    json := Json.ParseBuf(msg);
  END CheckResult;

PROCEDURE SendResult (msg: TEXT) =
  VAR header, len: TEXT;
  BEGIN
    CheckResult(msg);            (* remove when debugged *)
    LOCK msgMu DO
      len := Fmt.Int(Text.Length(msg));
      header := MsgHeader;
      header := TextUtils.Substitute(header, "$len", len, 1);
      msg := header & msg;

      (* send to client *)
      Wr.PutText(Stdio.stdout, msg);
      Wr.Flush(Stdio.stdout);
    END;
    Debug.Write("Response sent " & msg & "\n");
  END SendResult;

PROCEDURE InitResponse (vs: VS) =
  VAR
    node: Json.T;
    msg : TEXT;
  BEGIN
    (* get the root path ie where we were invoked *)
    (* or the workspaceFolders/0/uri ?? *)
    node := vs.json.find("/params/rootPath");
    (* get the root path *)
    wc.rootPath := node.value();
    node := vs.json.find("/params/rootUri");
    (* get the uri which is the root path with file:/// prepended *)
    wc.rootUri := node.value();
    msg := Msg.BuildInitReply(vs.id);
    SendResult(msg);
    vs.cm3Ok := RunCM3() = 0;

    (*
    example of dynamic registration and unregistration
    not used
    msg := Msg.BuildRegistration("AAAA1111BBBB","textDocument/diagnostic");
    Debug.Write("Reg msg " & msg & "\n");
    msg := Msg.BuildUnregistration("AAAA1111BBBB","textDocument/diagnostic");
    Debug.Write("UnReg msg " & msg & "\n");
    SendResult(msg);
    *)
  END InitResponse;

PROCEDURE ShutdownResponse (vs: VS) =
  VAR msg: TEXT;
  BEGIN
    compilerClosure.exit();
    msg := Msg.BuildShutdownReply(vs.id);
    SendResult(msg);
  END ShutdownResponse;

PROCEDURE CodeLensResponse (vs: VS) =
  VAR
    uri, msg: TEXT;
    node    : Json.T;
  BEGIN
    IF NOT vs.cm3Ok THEN
      (* send error *)
    END;
    (* get the input message details *)
    node := vs.json.find("/params/textDocument/uri");
    (* get the uri *)
    uri := node.value();
    Debug.Write("CodeLens uri-" & uri & "\n");
    (*
        msg := Msg.BuildCodeLensReply(vs.id);
        SendResult(msg);
    *)
  END CodeLensResponse;

PROCEDURE DeclarationResponse (vs: VS) =
  VAR
    line, col, defLine, defCol, defLen: INTEGER;
    uri, msg, targetUri               : TEXT;
    node                              : Json.T;
  BEGIN
    IF NOT vs.cm3Ok THEN
      (* send error *)
    END;
    (* get the input message details *)
    node := vs.json.find("/params/textDocument/uri");
    (* get the uri *)
    uri := node.value();
    node := vs.json.find("/params/position/line");
    line := node.getInt();
    node := vs.json.find("/params/position/character");
    col := node.getInt();

    Debug.Write("Declare uri-" & uri & " line " & Fmt.Int(line) & " col "
                  & Fmt.Int(col) & "\n");

    (* call m3tk to get declaration uri *)
    targetUri :=
      Feature.GetDeclaration(wc.getContext(), wc.getUnit(uri), line, col,
                             defLine, defCol, defLen);
    msg :=
      Msg.BuildDeclarationReply(vs.id, targetUri, defLine, defCol, defLen);

    SendResult(msg);

  END DeclarationResponse;

PROCEDURE TypeDefinitionResponse (vs: VS) =
  VAR
    line, col, defLine, defCol, defLen: INTEGER;
    uri, msg, targetUri               : TEXT;
    node                              : Json.T;
  BEGIN
    IF NOT vs.cm3Ok THEN
      (* send error *)
    END;
    (* get the input message details *)
    node := vs.json.find("/params/textDocument/uri");
    (* get the uri *)
    uri := node.value();
    node := vs.json.find("/params/position/line");
    line := node.getInt();
    node := vs.json.find("/params/position/character");
    col := node.getInt();

    Debug.Write("TypeDefinition uri-" & uri & " line " & Fmt.Int(line)
                  & " col " & Fmt.Int(col) & "\n");

    (* call m3tk to get declaration uri *)
    targetUri :=
      Feature.GetTypeDefinition(wc.getContext(), wc.getUnit(uri), line,
                                col, defLine, defCol, defLen);
    msg := Msg.BuildTypeDefinitionReply(
             vs.id, targetUri, defLine, defCol, defLen);

    SendResult(msg);

  END TypeDefinitionResponse;

PROCEDURE HoverResponse (vs: VS) =
  VAR
    uri, msg, hover         : TEXT;
    node                    : Json.T;
    line, col, startCol, len: INTEGER;
  BEGIN
    IF NOT vs.cm3Ok THEN
      (* send error *)
    END;
    (* get the input message details *)
    node := vs.json.find("/params/textDocument/uri");
    (* get the uri *)
    uri := node.value();
    node := vs.json.find("/params/position/line");
    line := node.getInt();
    node := vs.json.find("/params/position/character");
    col := node.getInt();

    Debug.Write("Hover uri-" & uri & " line " & Fmt.Int(line) & " col "
                  & Fmt.Int(col) & "\n");

    (* call m3tk to get symbol and type *)
    hover := Feature.GetSymbolType(
               wc.getContext(), wc.getUnit(uri), line, col, startCol, len);

    msg := Msg.BuildHoverReply(vs.id, hover, line, startCol, len);

    SendResult(msg);

  END HoverResponse;

PROCEDURE FoldingRangeResponse (vs: VS) =
  VAR
    uri, msg: TEXT;
    node    : Json.T;
  BEGIN
    (* get the input message details *)
    node := vs.json.find("/params/textDocument/uri");
    (* get the uri *)
    uri := node.value();
    Debug.Write("FoldingRanger uri-" & uri & "\n");
    (*
        msg := Msg.BuildFoldingRangeReply(vs.id);
        SendResult(msg);
    *)
  END FoldingRangeResponse;

PROCEDURE DidOpen (vs: VS) =
  VAR
    uri, text, version: TEXT;
    node, n           : Json.T;
    textSeq           : TextSeq.T;
  BEGIN
    IF NOT vs.cm3Ok THEN RETURN; END;
    (* get the input message details *)
    node := vs.json.find("/params/textDocument/text");
    (* get the file contents *)
    text := node.value();
    n := vs.json.find("/params/textDocument/uri");
    (* get the uri *)
    uri := n.value();
    (* get the version although not using it *)
    n := vs.json.find("/params/textDocument/version");
    version := n.value();

    Debug.Write("DidOpen Uri: " & uri & "\n");
    Debug.Write("DidOpen Version: " & version & "\n");

    text := Utils.UnEncode(text);
    textSeq := TextUtils.Split(text, "\n");

    (* save each open unit as a textSeq located via its uri *)
    EVAL compilerClosure.textSeqTbl.put(uri, textSeq);

    (* Initial compile *)
    CheckText(uri, text);
  END DidOpen;

PROCEDURE DidClose (vs: VS) =
  VAR
    uri    : TEXT;
    node   : Json.T;
    textSeq: REFANY;
  BEGIN
    IF NOT vs.cm3Ok THEN RETURN; END;
    (* get the input message details *)
    node := vs.json.find("/params/textDocument/uri");
    (* get the uri *)
    uri := node.value();
    Debug.Write("DidClose uri-" & uri & "\n");

    EVAL compilerClosure.textSeqTbl.delete(uri, textSeq);
    (* not doing anything if closed - not sending response *)
  END DidClose;

PROCEDURE DidSave (vs: VS) =
  VAR
    uri, version: TEXT;
    node        : Json.T;
  BEGIN
    IF NOT vs.cm3Ok THEN RETURN; END;
    node := vs.json.find("/params/textDocument/uri");
    uri := node.value();
    node := vs.json.find("/params/textDocument/version");
    version := node.value();
    (* not doing anything if saved - not sending response *)
    Debug.Write("DidSave uri-" & uri & "\n");
  END DidSave;

PROCEDURE DidChangeResponse (vs: VS) =
  VAR
    uri, change, comp, version, name: TEXT;
    node, n, n1                     : Json.T;
    iter                            : Json.Iterator;
    start, end                      : Utils.Range;
    incrCap, found                  : BOOLEAN;
    ts                              : REFANY;
    textSeq                         : TextSeq.T;
  BEGIN
    IF NOT vs.cm3Ok THEN RETURN; END;
    (* For incremental we also get range - start end and
       (deprc)rangeLength(int) as well as text.  So start with orig
       document get a TEXT and apply the changes to the doc.
       A text length of zero implies delete get the start pos delete to end.
       If text length > 0 and ranges identical then inserttext at start pos.
       If text length > 0 and ranges different then replace text.
       There can be multiple changes in one message.
       Change the textDocumentSync from 1 to 2 to get incremental
       in msg.json init reply *)

    (* get the input message details *)
    n := vs.json.find("/params/textDocument/uri");
    uri := n.value();
    node := vs.json.find("/params/textDocument/version");
    version := node.value();

    incrCap := Msg.GetIncrCap();

    (* Get the textSeq (Master document) for this uri *)
    found := compilerClosure.textSeqTbl.get(uri, ts);
    <* ASSERT found *>
    textSeq := NARROW(ts, TextSeq.T);
    node := vs.json.find("/params/contentChanges/");
    iter := node.iterate();
    (* iterate over the changes *)
    WHILE iter.next(name, n) DO
      n1 := n.find("/text");
      change := n1.value();
      change := Utils.UnEncode(change);

      (* ranges only if incremental is true *)
      IF incrCap THEN
        n1 := n.find("/range/start/line");
        start.line := n1.getInt();
        n1 := n.find("/range/start/character");
        start.col := n1.getInt();
        n1 := n.find("/range/end/line");
        end.line := n1.getInt();
        n1 := n.find("/range/end/character");
        end.col := n1.getInt();

        comp := UpdateDoc(textSeq, change, start, end);

        Debug.Write("didChange changed>>" & change & "<<\n");
        Debug.Write("start range line" & Fmt.Int(start.line) & "\n");
        Debug.Write("start range col" & Fmt.Int(start.col) & "\n");
        Debug.Write("end range line" & Fmt.Int(end.line) & "\n");
        Debug.Write("end range col" & Fmt.Int(end.col) & "\n");
(*
        Debug.Write("didChange comp>>" & comp & "<<\n");
*)
      ELSE
        Debug.Write("Incremental False\n");
        comp := change;
      END;
    END;

    EVAL compilerClosure.textSeqTbl.put(uri, textSeq);

    IF useCompThread THEN
      compilerClosure.update(uri, comp);
    ELSE
      (* compile on every message *)
      CheckText(uri, comp);
    END;
  END DidChangeResponse;

PROCEDURE UpdateDoc
  (VAR textSeq: TextSeq.T; change: TEXT; start, end: Utils.Range; ): TEXT =
  VAR
    del : BOOLEAN;
    incr: Utils.Incr;
    res : TEXT;
  BEGIN
    (* updates the master doc represented by the textSeq with change. *)
    del := Text.Length(change) = 0;
    incr := NEW(Utils.Incr);
    IF del THEN
      Debug.Write("didChange doing delete\n");
      incr.delete(textSeq, start, end);
    ELSIF start = end THEN
      Debug.Write("didChange doing insert\n");
      incr.insert(textSeq, change, start);
    ELSE
      Debug.Write("didChange doing replace\n");
      incr.replace(textSeq, change, start, end);
    END;
    res := TextUtils.TextSeqToText(textSeq, sep := "\n");
    RETURN res;
  END UpdateDoc;

PROCEDURE ErrorResponse (vs: VS) =
  VAR msg: TEXT;
  BEGIN
    msg := Msg.BuildErrorReply(vs.id, vs.errorCode);
    SendResult(msg);
  END ErrorResponse;

PROCEDURE HandleMsg (msg: TEXT): BOOLEAN =
  VAR
    json: Json.T;
    type: MsgType;
    exit: BOOLEAN := FALSE;
    vs  : VS;
  BEGIN
    TRY
      json := Json.ParseBuf(msg);
      vs := NEW(VS, json := json);
      type := Classify(vs);
    EXCEPT
    | Json.E =>
       type := MsgType.Error;
       vs.errorCode := ParseError;
       Debug.Write("Json ERROR\n");
    END;

    Debug.Write("New Message:\nMessage Type: " & Fmt.Int(ORD(vs.msgClass))
                  & "\nMessage Id: " & Fmt.Int(ORD(vs.id)) & "\n");
    Debug.Write("Message Contents:" & json.format() & "\n");

    CASE type OF
    | MsgType.Initialize => InitResponse(vs);
    | MsgType.Initialized =>     (* notification - no response *)
    | MsgType.Open => DidOpen(vs);
    | MsgType.Close => DidClose(vs);
    | MsgType.Save => DidSave(vs);
    | MsgType.Change => DidChangeResponse(vs);
    | MsgType.FoldingRange => FoldingRangeResponse(vs);
    | MsgType.CodeLens => CodeLensResponse(vs);
    | MsgType.Declaration => DeclarationResponse(vs);
    | MsgType.TypeDefinition => TypeDefinitionResponse(vs);
    | MsgType.Hover => HoverResponse(vs);
    | MsgType.Shutdown => ShutdownResponse(vs);
    | MsgType.Error =>
        (* not sending errors for notifications *)
        IF vs.msgClass = MsgClass.Request THEN ErrorResponse(vs); END;
    | MsgType.Exit => exit := TRUE;
    END;

    RETURN exit;
  END HandleMsg;

BEGIN
  msgMu := NEW(MUTEX);
  IF useCompThread THEN
    (* fork the thread for compiling the uri *)
    compilerClosure := NEW(CompilerClosure, mu := NEW(MUTEX),
                           textSeqTbl := NEW(TextRefTbl.Default).init(20));
    compThread := Thread.Fork(compilerClosure);
  END;
END LSP.
