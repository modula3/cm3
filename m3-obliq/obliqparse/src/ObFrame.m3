(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
MODULE ObFrame;
IMPORT ObErr, SynWr, SynScan, Rd, TextRd, Lex, FileRd, Text, OSError, Pathname, ObLib, ObValue, SynLocation, ObEval, Thread;
IMPORT Env AS ProcessEnv;

PROCEDURE FmtSearchPath(searchPath: SearchPath): TEXT  =
  BEGIN
    IF searchPath=NIL THEN RETURN "";
    ELSIF searchPath.rest=NIL THEN RETURN searchPath.first;
    ELSE RETURN
      searchPath.first & 
        Text.FromChar(SearchPathSeparator) & 
        FmtSearchPath(searchPath.rest);
    END;
  END FmtSearchPath;

PROCEDURE LexSearchPath(rd: TextRd.T): SearchPath =
  <*FATAL Rd.Failure, Thread.Alerted*>
  VAR item, junk: TEXT; rest: SearchPath;
  BEGIN
    IF Rd.EOF(rd) THEN RETURN NIL
    ELSE
      junk := Lex.Scan(rd, Lex.Blanks + SET OF CHAR{SearchPathSeparator});
      item := Lex.Scan(rd, Lex.NonBlanks - SET OF CHAR{SearchPathSeparator});
      IF Text.Empty(junk) AND Text.Empty(item) THEN RETURN NIL END;
      rest := LexSearchPath(rd);
      IF Text.Empty(item) THEN RETURN rest;
      ELSIF NOT Pathname.Valid(item) THEN RETURN rest;
      ELSE RETURN NEW(SearchPath, first:=item, rest:=rest);
      END;
    END;
  END LexSearchPath;

PROCEDURE PostFile(sc: SynScan.T; filename: Pathname.T): BOOLEAN =
  VAR rd: Rd.T; 
  BEGIN
    TRY
      rd:= FileRd.Open(filename);
      SynWr.Text(SynWr.out, "Loading '" & filename & "'\n");
      SynScan.PushInput(sc, Pathname.Last(filename), rd, TRUE, TRUE);
      RETURN TRUE;
    EXCEPT OSError.E => RETURN FALSE
    END;
  END PostFile;

PROCEDURE LoadFile(sc: SynScan.T; filename: Pathname.T; 
  complain: BOOLEAN:=TRUE) =
  VAR scan: SearchPath;
  BEGIN
    IF Pathname.Valid(filename) THEN
      IF Pathname.Absolute(filename) THEN 
        IF PostFile(sc, filename) THEN RETURN END;
      ELSE
        scan := searchPath;
        WHILE scan # NIL DO
          IF PostFile(sc,
               Pathname.Join(scan.first, filename, NIL))
          THEN RETURN
          END;
          scan := scan.rest;
        END;
      END;
    END;
    IF complain THEN 
      SynScan.ErrorMsg(sc, "Could not open file '" & filename
        & "' along path '" & FmtSearchPath(searchPath) & "'");
    END;
  END LoadFile;

PROCEDURE ModuleFrame(sc: SynScan.T; name, for: TEXT;
  imports: NameList; env: Env) RAISES {ObErr.Fail} =
(* Push scanner inputs so it will first load the imports first
   to last, then establish a frame for this module, and then
   finish reading this module. The last PushInput is executed first. *)
  BEGIN
    SynScan.PushInput(sc, "<none>", 
      TextRd.New("establish " & name & " for " & for & ";\n"),
      TRUE, TRUE);
    LoadImports(sc, imports, env);
  END ModuleFrame;

PROCEDURE ModuleEnd(sc: SynScan.T) =
  BEGIN
    SynScan.PushInput(sc, "<none>", TextRd.New("qualify;\n"), TRUE, TRUE);
  END ModuleEnd;

PROCEDURE LoadImports(sc: SynScan.T; imports: NameList; env: Env) 
  RAISES {ObErr.Fail} =
(* last to first, so the scanner will see them first to last *)
  BEGIN
    IF imports#NIL THEN
      LoadImports(sc, imports.rest, env);
      ImportFrame(sc, imports.first, env);
    END;
  END LoadImports;

PROCEDURE ImportFrame(sc: SynScan.T; name: TEXT; env: Env) =
  VAR scan: Env;
  BEGIN
    scan:=FindFrame(name, env);
    IF scan=NIL THEN LoadFile(sc, name & ".obl");
    ELSIF SynScan.TopLevel(sc) THEN
      SynWr.Text(SynWr.out, "(Frame '" & name & 
        "' already exists and has not been reloaded)\n");
    END;
  END ImportFrame;

PROCEDURE ModAndLib(name, for: TEXT): TEXT =
  BEGIN
    IF Text.Equal(name, for) THEN RETURN "'" & name & "'"
    ELSE RETURN "'" & name & "' for '" & for & "'" END;
  END ModAndLib;

PROCEDURE EstablishFrame(name, for: TEXT; env: Env): Env 
    RAISES {ObErr.Fail} =
  VAR moduleExists, frameExists: BOOLEAN; 
  BEGIN
    SynWr.Text(SynWr.out, "Establishing " & ModAndLib(name,for) & "\n");
    moduleExists := ObLib.Lookup(name, env.libEnv)#NIL;
    frameExists := FindFrame(name, env)#NIL;
    IF frameExists THEN
      RETURN SaveFrame(name, for, DeleteFrame(name, env));
    ELSIF moduleExists THEN
      ObErr.Fault(SynWr.out, 
        "Module name conflicts with existing library: '" & name & "_'");
      <*ASSERT FALSE*>
    ELSE
      RETURN SaveFrame(name, for, env);
    END;
  END EstablishFrame;

PROCEDURE SaveFrame(name, for: TEXT; env: Env): Env 
    RAISES {ObErr.Fail} =
  VAR scan: Env;
  BEGIN
    scan:=FindFrame(name, env);
    IF scan#NIL THEN 
      ObErr.Fault(SynWr.out, "Frame already exists: '" & name & "'");
      RETURN env;
    END;
    IF NOT Text.Empty(name) THEN
      SynWr.Text(SynWr.out, "(Created frame " & ModAndLib(name,for) & ")\n");
    END;
    RETURN
      NEW(Env,
        frameName := name,
        forName := for,
        libEnv := env.libEnv,
        scopeEnv := env.scopeEnv,
        checkEnv := env.checkEnv,
        valueEnv := env.valueEnv,
        nextFrame := env);
  END SaveFrame;

PROCEDURE DeleteFrame(name: TEXT; env: Env): Env =
  VAR scan: Env;
  BEGIN
    scan:=FindFrame(name, env);
    IF scan=NIL THEN 
      RETURN env;
    ELSE
      LOOP
        SynWr.Text(SynWr.out, 
          "(Deleted frame " & ModAndLib(env.frameName,env.forName) & ")\n");
	IF env=scan THEN EXIT END;
	env:=env.nextFrame;
      END;
      RETURN scan.nextFrame;
    END;
  END DeleteFrame;

PROCEDURE FindFrame(name: TEXT; env: Env): Env =
  VAR scan: Env;
  BEGIN
    scan:=env;
    LOOP
      IF scan=NIL THEN EXIT END;
      IF Text.Equal(scan.frameName, name) THEN EXIT END;
      scan := scan.nextFrame;
    END;
    RETURN scan;
  END FindFrame;

TYPE 
  FrameLib =
    ObLib.T OBJECT
      OVERRIDES
        Eval := FrameLibEval;
      END;

PROCEDURE QualifyFrame(env: Env): Env =
  VAR scanValueEnv: ObValue.Env;
    frameSize: INTEGER; opCodes: REF ObLib.OpCodes;
    library: ObLib.T; newLibEnv: ObLib.Env; newEnv: Env;
  BEGIN
    IF Text.Empty(env.frameName) THEN RETURN env END;
    scanValueEnv := env.valueEnv;
    frameSize := 0;
    LOOP 
      IF scanValueEnv=env.nextFrame.valueEnv THEN EXIT END; 
      INC(frameSize);
      scanValueEnv:=scanValueEnv.rest;
    END; 
    opCodes := NEW(REF ObLib.OpCodes, frameSize);
    scanValueEnv := env.valueEnv;
    FOR i:=0 TO frameSize-1 DO
      opCodes[i] := 
          NEW(FrameOpCode, name:=scanValueEnv.name.text,
              arity := -2, fixity := ObLib.OpFixity.Qualified,
              val := NARROW(scanValueEnv, ObValue.LocalEnv).val);
      scanValueEnv:=scanValueEnv.rest;
    END;
    library := NEW(FrameLib, name:=env.forName, opCodes:=opCodes);
    newLibEnv := ObLib.Extend(library, env.libEnv);
    newEnv := 
      NEW(Env,
          frameName := env.frameName,
          forName := env.forName,
          libEnv := newLibEnv,
          scopeEnv := env.nextFrame.scopeEnv,
          checkEnv := env.nextFrame.checkEnv,
          valueEnv := env.nextFrame.valueEnv,
          nextFrame := env.nextFrame);
    SynWr.Text(SynWr.out,
      "(Closed frame " & ModAndLib(env.frameName,env.forName) & ")\n");
    RETURN newEnv;
  END QualifyFrame;

PROCEDURE FrameLibEval(<*UNUSED*>self: FrameLib; opCode: ObLib.OpCode;
                       arity: ObLib.OpArity; READONLY args: ObValue.ArgArray; 
                       <*UNUSED*>temp: BOOLEAN; loc: SynLocation.T)
  : ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR frameOpCode: FrameOpCode;
  BEGIN
    frameOpCode := NARROW(opCode, FrameOpCode);
    IF arity = -1 THEN
      RETURN frameOpCode.val;
    ELSIF arity > NUMBER(args) THEN
      ObValue.RaiseError("Too many arguments", loc);
      <*ASSERT FALSE*>
    ELSE
      RETURN ObEval.Call(frameOpCode.val, SUBARRAY(args, 0, arity), loc);
    END;
  END FrameLibEval;

PROCEDURE Setup()  =
  VAR envPath: TEXT;
  BEGIN
    envPath := ProcessEnv.Get("OBLIQPATH");
    IF envPath=NIL THEN
      searchPath := 
        NEW(SearchPath, 
            first:=Pathname.Current, 
            rest:= NIL);
    ELSE 
      searchPath := LexSearchPath(TextRd.New(envPath));
    END;
  END Setup;

BEGIN
END ObFrame.
