(* Copyright (C) 2023, Peter McKinna *)
(* All rights reserved.              *)
(* Licensed under the MIT license.   *)

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


IMPORT Fmt,Wr,Stdio,Thread,Text,TextUtils,Process;
IMPORT Json,Msg,AstCompile,Feature,Utils,Debug;
IMPORT IO,Buf,RefSeq,FS,Pathname;
(*
IMPORT IO,Scan,TextExtras;
*)

CONST
  MsgHeader = "Content-Length: $len \r\n\r\n";
  CompileDelay = 2.0D0;

  (* error codes *)
  ParseError = -32700;
(*
  InvalidRequest = -32600;
  MethodNotFound = -32601;
  InvalidParams = -32602;
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
  MsgType = {Initialize,
             Shutdown,
             Hover,
             FoldingRange,
             CodeLens,
             Declaration,
             TypeDefinition,
             (* notifications *)
             Initialized,
             Exit,
             Open,
             Close,
             Save,
             Change,
             (* not part of protocol *)
             NotFound,
             Error};

  VS = OBJECT
    id : INTEGER;
    type : MsgType;
    method : TEXT;
    mtype : TEXT;
    json : Json.T;
    cm3Ok : BOOLEAN := TRUE;
  END;

  CompilerClosure = Thread.Closure OBJECT
    count : CARDINAL;
    text,uri : TEXT;
    cnt1,cnt2 : INTEGER := 0;
    finish : BOOLEAN := FALSE;
    mu : MUTEX;
  METHODS
    update(uri,txt : TEXT) := Update;
    exit() := Exit;
  OVERRIDES
    apply := Compile;
  END;

VAR
  compilerClosure : CompilerClosure;
  compThread : Thread.T;
  wc := AstCompile.wc;
  msgMu : MUTEX;
  useCompThread := TRUE;
  shutdown := FALSE;

(* Debug proc for testing via command line *)
PROCEDURE CheckFile(uri : TEXT) =
  VAR 
    text : TEXT;
    buf : Buf.T;
    line,col,startCol,len : INTEGER;
    hover : TEXT;
  BEGIN
    IF NOT Pathname.Absolute(uri) THEN
      IO.Put("relative");
      uri := FS.GetAbsolutePathname(uri);
    END;
    wc.rootPath := Pathname.Prefix(uri);
    buf := Buf.FromFile(uri,NIL);
    text := Text.FromChars(buf^);
    uri := "file://" & uri;
    IO.Put("uri:" & uri & "\n");

    CheckText(uri, text);

    line := 30; col := 8;
    hover := Feature.GetSymbolType(wc.getContext(), wc.getUnit(uri),
                                   line, col, startCol, len);

  END CheckFile;

(* run the real compiler to create the .M3IMPTAB file for use by m3tk *)
PROCEDURE RunCM3() : INTEGER =
  VAR
    handle : Process.T;
    exitCode : Process.ExitCode;
    params : ARRAY[0..1] OF TEXT;
    msg : TEXT;
  BEGIN
    (* maybe periodically run cm3 to refresh in case m3makefile *)
    params[0] := "-tfile";
    params[1] := "-silent";
    handle := Process.Create("cm3",params);
    exitCode := Process.Wait(handle);
    (* save exitCode in vs so check on each message if cm3 error
       then m3imptab prob not exist and cannot do any m3tk actions
       have to return error code in message reply and ignore notifications
    *) 
    msg := "cm3 exit code: " & Fmt.Int(exitCode) & "\n";
    Wr.PutText(Stdio.stderr, msg);
    Wr.Flush(Stdio.stderr);
    Debug.Write("RunCM3 exit code-" & Fmt.Int(exitCode) & "<<<<<\n");
    RETURN exitCode;
  END RunCM3;

PROCEDURE CheckText (uri,text : TEXT) =
  VAR
    errors : RefSeq.T;
    msg : TEXT;
  BEGIN
(* FIXME compile could raise an exception eg dir not found, invalid extension
   etc and we should avoid sending a reply *)
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

PROCEDURE Compile (cl : CompilerClosure) : REFANY =
  VAR
    doCompile : BOOLEAN;
    uri,text : TEXT;
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
      IF doCompile THEN
        CheckText(uri, text);
      END;
    END;
    RETURN NIL;
  END Compile;

PROCEDURE Update (cl : CompilerClosure; uri,txt : TEXT) =
  BEGIN
    LOCK cl.mu DO
      INC(cl.cnt1);
      cl.uri := uri;
      cl.text := txt;
    END;
  END Update;

PROCEDURE Exit (cl : CompilerClosure) =
  BEGIN
    LOCK cl.mu DO
      cl.finish := TRUE;
    END;
    EVAL Thread.Join(compThread);
  END Exit;

PROCEDURE Classify(vs : VS) : MsgType =
  VAR
    node : Json.T;
    ret : MsgType;
  BEGIN
    node := vs.json.find("id");
    IF node = NIL THEN
      vs.id := -1; vs.mtype := "notification";
    ELSE
      vs.id := node.getInt(); vs.mtype :="request";
    END;

    node := vs.json.find("method");
    IF node = NIL THEN
      RETURN MsgType.Error;
    END;
    vs.method := node.value();

    IF Text.Equal(vs.method,"initialize") THEN
      ret := MsgType.Initialize;
    ELSIF Text.Equal(vs.method,"shutdown") THEN
      ret := MsgType.Shutdown;
    ELSIF Text.Equal(vs.method,"textDocument/hover") THEN
      ret := MsgType.Hover;
    ELSIF Text.Equal(vs.method,"textDocument/foldingRange") THEN
      ret := MsgType.FoldingRange;
    ELSIF Text.Equal(vs.method,"textDocument/codeLens") THEN
      ret := MsgType.CodeLens;
    ELSIF Text.Equal(vs.method,"textDocument/declaration") THEN
      ret := MsgType.Declaration;
    ELSIF Text.Equal(vs.method,"textDocument/typeDefinition") THEN
      ret := MsgType.TypeDefinition;

    (* Notifications *)

    ELSIF Text.Equal(vs.method,"initialized") THEN
      ret := MsgType.Initialized;
    ELSIF Text.Equal(vs.method,"exit") THEN
      ret := MsgType.Exit;
    ELSIF Text.Equal(vs.method,"textDocument/didOpen") THEN
      ret := MsgType.Open;
    ELSIF Text.Equal(vs.method,"textDocument/didClose") THEN
      ret := MsgType.Close;
    ELSIF Text.Equal(vs.method,"textDocument/didChange") THEN
      ret := MsgType.Change;
    ELSIF Text.Equal(vs.method,"textDocument/didSave") THEN
      ret := MsgType.Save;
    ELSE
      ret := MsgType.NotFound;
    END;

    RETURN ret;
  END Classify;

PROCEDURE CheckResult(msg : TEXT) =
  VAR
    json : Json.T;
  BEGIN
    Debug.Write("Result json " & msg & "\n");

    (* debug check if result is correct json.*)
    json := Json.ParseBuf(msg);
  END CheckResult;

PROCEDURE SendResult(msg : TEXT) =
  VAR
    header,len : TEXT;
  BEGIN
    CheckResult(msg); (* remove when debugged *)
    LOCK msgMu DO
      len := Fmt.Int(Text.Length(msg));
      header := MsgHeader;
      header := TextUtils.Substitute(header,"$len", len, 1);
      msg := header & msg;

      (* send to client *)
      Wr.PutText(Stdio.stdout,msg);
      Wr.Flush(Stdio.stdout);
    END;
    Debug.Write("Response sent "  & msg & "\n");
  END SendResult;

PROCEDURE InitResponse(vs : VS) =
  VAR
    node : Json.T;
    msg : TEXT;
  BEGIN
    (* get the root path ie where we were invoked *)
    (* or the workspaceFolders/0/uri ?? *)
    node := vs.json.find("/params/rootPath"); 
    (* get the uri *)
    wc.rootPath := node.value();
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

PROCEDURE ShutdownResponse(vs : VS) =
  VAR
    msg : TEXT;
  BEGIN
    compilerClosure.exit();
    msg := Msg.BuildShutdownReply(vs.id);
    SendResult(msg);
  END ShutdownResponse;

PROCEDURE CodeLensResponse(vs : VS) =
  VAR
    uri,msg : TEXT;
    node : Json.T;
  BEGIN
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

PROCEDURE DeclarationResponse(vs : VS) =
  VAR
    line,col,defLine,defCol,defLen : INTEGER;
    uri,msg,targetUri : TEXT;
    node : Json.T;
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

    (* call m3tk to get declaration uri *)
    targetUri := Feature.GetDeclaration(wc.getContext(), wc.getUnit(uri),
                                        line, col, defLine, defCol, defLen);
    msg := Msg.BuildDeclarationReply(vs.id, targetUri,
                                     defLine, defCol, defLen);
    SendResult(msg);

    Debug.Write("Declare uri-" & uri & " line " & Fmt.Int(line) & " col " & Fmt.Int(col) & "\n");
  END DeclarationResponse;

PROCEDURE TypeDefinitionResponse(vs : VS) =
  VAR
    line,col,defLine,defCol,defLen : INTEGER;
    uri,msg,targetUri : TEXT;
    node : Json.T;
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

    (* call m3tk to get declaration uri *)
    targetUri := Feature.GetTypeDefinition(wc.getContext(), wc.getUnit(uri),
                                           line, col, defLine, defCol, defLen);
    msg := Msg.BuildTypeDefinitionReply(vs.id, targetUri,
                                        defLine, defCol, defLen);
    SendResult(msg);

    Debug.Write("TypeDefinition uri-" & uri & " line " & Fmt.Int(line) & " col " & Fmt.Int(col) & "\n");
  END TypeDefinitionResponse;

PROCEDURE HoverResponse(vs : VS) =
  VAR
    uri,msg,hover : TEXT;
    node : Json.T;
    line,col,startCol,len : INTEGER;
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

    (* call m3tk to get symbol and type *)
    hover := Feature.GetSymbolType(wc.getContext(), wc.getUnit(uri),
                                   line, col, startCol, len);
    msg := Msg.BuildHoverReply(vs.id, hover, line, startCol, len);
    SendResult(msg);

    Debug.Write("Hover uri-" & uri & " line " & Fmt.Int(line) & " col " & Fmt.Int(col) & "\n");
  END HoverResponse;

PROCEDURE FoldingRangeResponse(vs : VS) =
  VAR
    uri,msg : TEXT;
    node : Json.T;
  BEGIN
    (* get the input message details *)
    node := vs.json.find("/params/textDocument/uri"); 
    (* get the uri *)
    uri := node.value();
(* 
    msg := Msg.BuildFoldingRangeReply(vs.id);
    SendResult(msg);
*)
    Debug.Write("FoldingRanger uri-" & uri & "\n");
  END FoldingRangeResponse;

PROCEDURE DidOpen(vs : VS) =
  VAR 
    uri,text,version : TEXT;
    node,n : Json.T;
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

    Debug.Write("DidOpen uri-" & uri & "version " & version & "\n");

    text := Utils.Substitute(text);

(* test our subs algorithm
Debug.Write("Substitute-" & text & "<<<<<\n");
*)

    CheckText(uri, text);
    Debug.Write("DidOpen uri-" & uri & "\n");
  END DidOpen;

PROCEDURE DidClose(vs : VS) =
  VAR 
    uri : TEXT;
    node : Json.T;
  BEGIN
    IF NOT vs.cm3Ok THEN RETURN; END;
    (* get the input message details *)
    node := vs.json.find("/params/textDocument/uri"); 
    (* get the uri *)
    uri := node.value();
    (* not doing anything if closed - not sending response *)
    Debug.Write("DidClose uri-" & uri & "\n");
  END DidClose;

PROCEDURE DidSave(vs : VS) =
  VAR 
    uri,version : TEXT;
    node : Json.T;
  BEGIN
    IF NOT vs.cm3Ok THEN RETURN; END;
    node := vs.json.find("/params/textDocument/uri"); 
    uri := node.value();
    node := vs.json.find("/params/textDocument/version"); 
    version := node.value();
    (* not doing anything if saved - not sending response *)
    Debug.Write("DidSave uri-" & uri & "\n");
  END DidSave;

PROCEDURE DidChangeResponse(vs : VS) =
  VAR 
    uri,text,version : TEXT;
    node,n : Json.T;
  BEGIN
    IF NOT vs.cm3Ok THEN RETURN; END;
(* for incremental we also get range - start end  and (deprc)rangeLength(int)
   as well as text. So start with orig document get a TEXT and apply the
   changes to the doc this might ameliorate the substitue problem.
   If text length is zero implies delete get the start pos and delete to
   end. If text length > 0 and range is zero len then add text at start
   pos. If text length > 0 and range identical then replace text.
   Changed the textDocumentSync from 1 to 2 to get incremental in msg.json
   init reply
*) 
    (* get the input message details *)
    node := vs.json.find("/params/contentChanges/0/text"); 
    (* get the file contents *)
    text := node.value();
    n := vs.json.find("/params/textDocument/uri"); 
    uri := n.value();
    node := vs.json.find("/params/textDocument/version"); 
    version := node.value();

    text := Utils.Substitute(text);

    IF useCompThread THEN
      compilerClosure.update(uri,text);
    ELSE
      (* compile on every message *)
      CheckText(uri, text);
    END;
  END DidChangeResponse;

PROCEDURE ErrorResponse(vs : VS; error : INTEGER) =
  VAR
    msg : TEXT;
  BEGIN
    msg := Msg.BuildErrorReply(vs.id, error);
    SendResult(msg);
  END ErrorResponse;

PROCEDURE HandleMsg(msg : TEXT) : BOOLEAN =
  VAR
    json : Json.T;
    type : MsgType;
    vs : VS;
  BEGIN
    TRY
      json := Json.ParseBuf(msg);
      vs := NEW(VS, json := json);
      type := Classify(vs);
    EXCEPT
    | Json.E => type := MsgType.Error;
    END;

    IF type = MsgType.Error THEN
      Debug.Write("Parse Error\n");
      ErrorResponse(vs,ParseError);
      RETURN FALSE;
    END;

    Debug.Write("New Message: type " & vs.mtype & " id: " & Fmt.Int(ORD(vs.id)) & "\n");
(* even if debug is off we still format the json - need a way to not do it *)
    Debug.Write(json.format() & "\n");

    IF type = MsgType.Initialize THEN
      InitResponse(vs);
    ELSIF type = MsgType.Open THEN
      DidOpen(vs);
    ELSIF type = MsgType.Close THEN
      DidClose(vs);
    ELSIF type = MsgType.Save THEN
      DidSave(vs);
    ELSIF type = MsgType.Change THEN
      DidChangeResponse(vs);
    ELSIF type = MsgType.FoldingRange THEN
      FoldingRangeResponse(vs);
    ELSIF type = MsgType.CodeLens THEN
      CodeLensResponse(vs);
    ELSIF type = MsgType.Declaration THEN
      DeclarationResponse(vs);
    ELSIF type = MsgType.TypeDefinition THEN
      TypeDefinitionResponse(vs);
    ELSIF type = MsgType.Hover THEN
      HoverResponse(vs);
    ELSIF type = MsgType.Shutdown THEN
      ShutdownResponse(vs);
      shutdown := TRUE;
    END;

    RETURN type = MsgType.Exit;

  END HandleMsg;

BEGIN
  msgMu := NEW(MUTEX);
  IF useCompThread THEN
    (* fork the thread for compiling the uri *) 
    compilerClosure := NEW(CompilerClosure, mu := NEW(MUTEX));
    compThread := Thread.Fork(compilerClosure);
  END;
END LSP.
