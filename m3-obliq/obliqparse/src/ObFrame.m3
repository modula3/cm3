(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
MODULE ObFrame;
IMPORT ObErr, SynWr, SynScan, Rd, TextRd, Lex, FileRd, Text, OSError,
       Pathname, ObLib, ObValue, SynLocation, ObEval, Thread,
       ObCommand, Fmt, Fingerprint, Pickle2 AS Pickle, Wr,
       PickleStubs, NetObj, TextList, SharedObj;
IMPORT Env AS ProcessEnv;
IMPORT ObPathSep;

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
  VAR item, junk: TEXT; rest: SearchPath;
  BEGIN
    TRY
      IF Rd.EOF(rd) (* NOWARN *) THEN RETURN NIL
      ELSE
        junk := 
            Lex.Scan(rd, (* NOWARN *)
                     Lex.Blanks + SET OF CHAR{SearchPathSeparator}); (*NOWARN*)
        item := 
            Lex.Scan(rd, (* NOWARN *)
                     Lex.NonBlanks - SET OF CHAR{SearchPathSeparator});(*NOWARN*)
        IF Text.Empty(junk) AND Text.Empty(item) THEN RETURN NIL END;
        rest := LexSearchPath(rd);
        IF Text.Empty(item) THEN RETURN rest;
        ELSIF NOT Pathname.Valid(item) THEN RETURN rest;
        ELSE RETURN NEW(SearchPath, first:=item, rest:=rest);
        END;
      END;
    EXCEPT
    | Rd.Failure, Thread.Alerted => RETURN NIL; 
    END;
  END LexSearchPath;

PROCEDURE PostFile(sc: SynScan.T; filename: Pathname.T): BOOLEAN =
  VAR rd: Rd.T; 
  BEGIN
    TRY
      rd:= FileRd.Open(filename);
      SynWr.Text(SynScan.GetWriter(sc), "Loading '" & filename & "'\n");
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

PROCEDURE ModuleEnd(sc: SynScan.T; ideList: NameList) =
  VAR qual := "qualify";
      first := TRUE;
  BEGIN
    IF ideList # NIL THEN
      qual := qual & " exporting";
    END;
    WHILE ideList # NIL DO
      IF first THEN 
        first := FALSE;
        qual := qual & " " & ideList.first;
      ELSE
        qual := qual & ", " & ideList.first;
      END;
      ideList := ideList.rest;
    END;
    SynScan.PushInput(sc, "<none>", TextRd.New(qual & ";\n"), TRUE, TRUE);
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
      SynWr.Text(SynScan.GetWriter(sc), "(Frame '" & name & 
        "' already exists and has not been reloaded)\n");
    END;
  END ImportFrame;

PROCEDURE ModAndLib(name, for: TEXT): TEXT =
  BEGIN
    IF Text.Equal(name, for) THEN RETURN "'" & name & "'"
    ELSE RETURN "'" & name & "' for '" & for & "'" END;
  END ModAndLib;

PROCEDURE EstablishFrame(wr: SynWr.T; name, for: TEXT; env: Env): Env 
    RAISES {ObErr.Fail} =
  VAR moduleExists, frameExists: BOOLEAN; 
  BEGIN
    SynWr.Text(wr, "Establishing " & ModAndLib(name,for) & "\n");
    moduleExists := ObLib.Lookup(name, env.libEnv)#NIL;
    frameExists := FindFrame(name, env)#NIL;
    IF frameExists THEN
      RETURN SaveFrame(wr, name, for, DeleteFrame(wr, name, env));
    ELSIF moduleExists THEN
      ObErr.Fault(wr, 
        "Module name conflicts with existing library: '" & name & "_'");
      <*ASSERT FALSE*>
    ELSE
      RETURN SaveFrame(wr, name, for, env);
    END;
  END EstablishFrame;

PROCEDURE SaveFrame(wr: SynWr.T; name, for: TEXT; env: Env): Env 
    RAISES {ObErr.Fail} =
  VAR scan: Env;
  BEGIN
    scan:=FindFrame(name, env);
    IF scan#NIL THEN 
      ObErr.Fault(wr, "Frame already exists: '" & name & "'");
      RETURN env;
    END;
    IF NOT Text.Empty(name) THEN
      SynWr.Text(wr, "(Created frame " & ModAndLib(name,for) & ")\n");
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

PROCEDURE DeleteFrame(wr: SynWr.T; name: TEXT; env: Env): Env =
  VAR scan: Env;
  BEGIN
    scan:=FindFrame(name, env);
    IF scan=NIL THEN 
      RETURN env;
    ELSE
      LOOP
        SynWr.Text(wr, 
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

PROCEDURE InNameList(name: TEXT; list: NameList): BOOLEAN = 
  BEGIN
    WHILE list # NIL DO
      IF Text.Equal(name, list.first) THEN RETURN TRUE END;
      list := list.rest;
    END;
    RETURN FALSE;
  END InNameList;

PROCEDURE QualifyFrame(wr: SynWr.T; env: Env; ideList: NameList): Env =
  VAR scanValueEnv: ObValue.Env;
      frameCount, frameSize: INTEGER; opCodes: REF ObLib.OpCodes;
      library: ObLib.T; newLibEnv: ObLib.Env; newEnv: Env;
      seen, tail: TextList.T := NIL;
  BEGIN
    IF Text.Empty(env.frameName) THEN RETURN env END;
    scanValueEnv := env.valueEnv;
    frameSize := 0;
    frameCount := 0;
    LOOP 
      IF scanValueEnv=env.nextFrame.valueEnv THEN EXIT END; 
      IF ideList = NIL OR InNameList(scanValueEnv.name.text, ideList) THEN
        IF NOT TextList.Member(seen, scanValueEnv.name.text) THEN
          (* want a list of the elements in the same order *)
          IF seen = NIL THEN
            tail := TextList.List1(scanValueEnv.name.text);
            seen := tail;
          ELSE
            tail.tail := TextList.List1(scanValueEnv.name.text);
            tail := tail.tail;
          END;
          INC(frameSize);
        END;
      END;
      INC(frameCount);
      scanValueEnv:=scanValueEnv.rest;
    END; 
    opCodes := NEW(REF ObLib.OpCodes, frameSize);
    scanValueEnv := env.valueEnv;
    frameSize := 0;
    FOR i:=0 TO frameCount-1 DO
      IF seen # NIL AND Text.Equal(scanValueEnv.name.text, seen.head) THEN
        seen := seen.tail;
        opCodes[frameSize] := 
            NEW(FrameOpCode, name:=scanValueEnv.name.text,
                arity := -2, fixity := ObLib.OpFixity.Qualified,
                val := NARROW(scanValueEnv, ObValue.LocalEnv).val);
        INC(frameSize);
      END;
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
    SynWr.Text(wr,
      "(Closed frame " & ModAndLib(env.frameName,env.forName) & ")\n");
    RETURN newEnv;
  END QualifyFrame;

PROCEDURE FrameLibEval(self: FrameLib; opCode: ObLib.OpCode;
                       arity: ObLib.OpArity; READONLY args: ObValue.ArgArray; 
                       <*UNUSED*>temp: BOOLEAN; 
                       swr: SynWr.T; 
                       loc: SynLocation.T)
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
      TYPECASE frameOpCode.val OF
      | ObValue.ValFun(fun) =>
        RETURN ObEval.Call(fun, SUBARRAY(args, 0, arity), swr, loc);
      | ObValue.ValVar (node) =>
        TRY
          TYPECASE node.Get() OF
          | ObValue.ValFun(fun) =>
            RETURN ObEval.Call(fun, SUBARRAY(args, 0, arity), swr, loc);
          ELSE
            ObValue.RaiseError("Not expecting argument list for: " &
              self.name & "_" & opCode.name, loc);
            <*ASSERT FALSE*>
          END;
        EXCEPT
        | NetObj.Error (atoms) =>
          ObValue.RaiseNetException("on remote access to variable: " &
            self.name & "_" & opCode.name, atoms, loc);
          <*ASSERT FALSE*>
        | SharedObj.Error (atoms) =>
          ObValue.RaiseSharedException("on access to replicated variable: " &
            self.name & "_" & opCode.name, atoms, loc);
          <*ASSERT FALSE*>
        | Thread.Alerted =>
          ObValue.RaiseException(
              ObValue.threadAlerted,
              "on remote access to variable: " & 
              self.name & "_" & opCode.name, loc);
          <*ASSERT FALSE*>
        END;
      ELSE
        ObValue.RaiseError("Not expecting argument list for: " &
          self.name & "_" & opCode.name, loc);
        <*ASSERT FALSE*>
      END;
    END;
  END FrameLibEval;

TYPE
  HelpCommand = ObCommand.T OBJECT
    short, long: TEXT;
  END;

PROCEDURE AddHelpFrame(name, sort, short, long: TEXT; 
                          <*UNUSED*>env: Env) =
  (* add a help file for this module *)
  BEGIN
    ObCommand.Register(ObLib.helpCommandSet,
                       NEW(HelpCommand, name:=name, 
                           sortingName:= sort,
                           short := short, long := long,
                           Exec:= Help));
  END AddHelpFrame;

PROCEDURE Help (wr: SynWr.T; comm: ObCommand.T; arg : TEXT; 
                <*UNUSED*> data : REFANY) =
  VAR self := NARROW(comm, HelpCommand);
  BEGIN
    IF Text.Equal (arg, "!") THEN
      SynWr.Text (wr, 
                  "  " & Fmt.Pad (self.name, 18, ' ', Fmt.Align.Left) & 
                  "(" & self.short & ")\n");
    ELSIF Text.Equal (arg, "?") THEN
      SynWr.Text (wr, self.long);
      SynWr.NewLine (wr);
    ELSE
      SynWr.Text(wr, "Command " & self.name & ": bad argument: " & arg);
      SynWr.NewLine (wr);
    END;
  END Help;

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

TYPE
  ObFrameSpecial = Pickle.Special BRANDED "ObFrame.ObFrameSpecial" OBJECT
                       OVERRIDES
                         write := WriteLib;
                         read := ReadLib;
                       END;
  
TYPE
  LocalHandle = OpCodeHandle OBJECT
                       OVERRIDES
                         getOpCodes := GetOpCodes;
                       END;

VAR
  handle := NEW(LocalHandle);

(* Implementation note:  I think this is guaranteed to work.  The only
   worry is if the library can disappear via garbage collection
   between the time the pickle is written and the time GetOpCodes is
   called.  I don't think this can happen.  Since all netobj calls are
   synchronous, the higher level caller of the pickler will write all
   the info and then wait for the return value from the remote call.
   The remote process reads all the info, which includes calling
   ReadLib below and potentially calling GetOpCodes, before the remote
   procedure is executed and the return value written. *)

PROCEDURE GetOpCodes (<*UNUSED*>self: LocalHandle; 
                      ts: Fingerprint.T): REF ObLib.OpCodes=
  VAR lib := ObLib.LookupFP(ts);
  BEGIN
    IF lib = NIL THEN RETURN NIL END;
    RETURN lib.opCodes;
  END GetOpCodes; 

PROCEDURE WriteLib (<*UNUSED*>ts: ObFrameSpecial; 
                    ref: REFANY; out: Pickle.Writer)
  RAISES {Pickle.Error, Wr.Failure, Thread.Alerted} =
  VAR o := NARROW(ref, FrameLib);
  BEGIN
    ObLib.CheckFP(o);
    PickleStubs.OutBytes(out, o.ts.byte);
    PickleStubs.OutText(out, o.name);
    PickleStubs.OutRef(out, handle);
  END WriteLib; 

PROCEDURE ReadLib (<*UNUSED*>ts: ObFrameSpecial;
                   in: Pickle.Reader;
                   id: Pickle.RefID):REFANY
  RAISES {Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR fp: Fingerprint.T;
      rHandle: OpCodeHandle;
      lib: FrameLib := NIL;
      name: TEXT;
  BEGIN
    PickleStubs.InBytes(in, fp.byte);
    name := PickleStubs.InText(in);
    rHandle := PickleStubs.InRef(in);

    lib := ObLib.LookupFP(fp);
    IF lib = NIL THEN
      TRY
        lib := NEW(FrameLib, name:=name, opCodes:= NIL, ts:=fp);
        in.noteRef(lib, id);
        lib.opCodes := rHandle.getOpCodes(fp);
        (* check again, using the full object *)
        lib := ObLib.LookupFP(fp, lib);
      EXCEPT NetObj.Error => END;
    ELSE
      in.noteRef(lib, id);
    END;
    RETURN lib;
  END ReadLib;

BEGIN
  Pickle.RegisterSpecial(NEW(ObFrameSpecial, sc := TYPECODE(FrameLib)));
  SearchPathSeparator := ObPathSep.SearchPathSeparator;
END ObFrame.
