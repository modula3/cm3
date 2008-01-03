(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Created by Susan Owicki                                     *)
(* Last modified on Fri Feb 11 14:16:02 PST 1994 by wobber     *)
(*      modified on Wed Jun  9 12:12:32 PDT 1993 by owicki     *)
(*      modified on Mon May 17 14:59:50 PDT 1993 by mjordan    *)

MODULE ModuleStubCode;

IMPORT Atom, CodeForType, Fmt, Formatter, RefList, Protocol, AtomRefTbl,
       StubCode, StubUtils, Text, Type, Value, Wr;
IMPORT TextRefTbl AS TextSet;

<* FATAL Wr.Failure, StubUtils.Error *>

CONST PerfComment = "  (* Performance Monitoring  *)"; 

PROCEDURE Header(modWr: Formatter.T; 
                 <* UNUSED *>t: Type.Object; 
                 typeName: Atom.T;
                 objName: Type.Qid; 
                 methods: StubCode.MethodList;
                 lastNewMethod: INTEGER;
                 VAR returnCodes: RefList.T;
                 importList: AtomRefTbl.T) = 
  BEGIN
    Formatter.PutText(modWr, "MODULE " & StubUtils.FileName(typeName) & 
      " EXPORTS " &  Atom.ToText(objName.intf) & ", " & 
       StubUtils.FileName(typeName) & ";\n\n");
    CodeForType.ProduceImports(modWr, objName, importList);
    CodeForType.ImportSuperStubs(modWr, methods, lastNewMethod, typeName);
    IF StubUtils.perfMon THEN
      Formatter.PutText(modWr, "IMPORT NetObjPerf, PerfUtil;" & PerfComment);
      Formatter.NewLine(modWr, freshLine := FALSE);
    END;
    Formatter.PutText(modWr, 
      "CONST Protocol: StubLib.StubProtocol = " & Fmt.Int(Protocol.Version)
      & ";\n\n"); 
    Formatter.PutText(modWr, "TYPE "); 
    Formatter.Begin(modWr, 1);
    Formatter.NewLine(modWr, freshLine := FALSE);
    EnumerateMethods(modWr, methods);
    Formatter.NewLine(modWr, freshLine := FALSE);
    EnumerateReturnCodes(modWr, methods, lastNewMethod, returnCodes);
    Formatter.End(modWr);
    Formatter.NewLine(modWr);
  END Header;

  PROCEDURE EnumerateMethods(modWr: Formatter.T; 
        methods: StubCode.MethodList) =
    BEGIN
      Formatter.Begin(modWr, 2);
      Formatter.PutText(modWr, "Methods = {");
      FOR i := LAST(methods^) TO 0 BY -1 DO
        IF i < LAST(methods^) THEN Formatter.PutText(modWr, ", "); END;
        Formatter.Break(modWr, type := Formatter.BreakType.NonOptimal);
        Formatter.PutText(modWr,  Atom.ToText(methods[i].name));
      END;
      Formatter.PutText(modWr, "};");
      Formatter.End(modWr);
    END EnumerateMethods;

  PROCEDURE EnumerateReturnCodes(modWr: Formatter.T; 
                                 methods: StubCode.MethodList;
                                 lastNewMethod: INTEGER;
                                 VAR returnList: RefList.T) =
    VAR returnCodes := NEW(TextSet.Default).init(8);
        ename: TEXT;
    BEGIN
      Formatter.Begin(modWr, 2);
      Formatter.PutText(modWr, "ReturnCodes = {OK");
      FOR i := 0 TO lastNewMethod DO
        IF methods[i].sig.raises # NIL THEN
          FOR j := 0 TO LAST(methods[i].sig.raises^) DO
            ename := QidToText(methods[i].sig.raises[j].qid, "_");
            IF NOT returnCodes.put(ename, NIL) AND 
               NOT Text.Equal(ename, "Thread_Alerted") AND
               NOT Text.Equal(ename, "NetObj_Error") THEN
              returnList := RefList.Cons(methods[i].sig.raises[j], returnList);
              Formatter.PutText(modWr, ", ");
              Formatter.Break(modWr, type := Formatter.BreakType.NonOptimal);
              Formatter.PutText(modWr, ename);
            END;
          END;
        END;
      END;
      Formatter.PutText(modWr, "};\n");
      Formatter.End(modWr);
    END EnumerateReturnCodes;

PROCEDURE QidToText(qid: Type.Qid; sep: TEXT): TEXT =
  BEGIN
    RETURN  Atom.ToText(qid.intf) & sep & Atom.ToText(qid.item);
  END QidToText;

PROCEDURE Surrogates(modWr: Formatter.T; 
                     t: Type.Object;
                     methods: StubCode.MethodList;
                     lastNewMethod: INTEGER) RAISES {StubUtils.Failure} =
  VAR procedureName, eName, text: TEXT;
  BEGIN
    Formatter.Begin(modWr, 2);
    FOR i := 0 TO lastNewMethod DO
      procedureName := "Surrogate_" & Atom.ToText(methods[i].name);
      Formatter.NewLine(modWr, freshLine := FALSE);
      CodeForType.ProcHeader(modWr, t, procedureName, 
                             methods[i].sig, suffix := "_arg");
      PutLine(modWr,  " = ");
      Formatter.Begin(modWr, 2);
      Formatter.NewLine(modWr, freshLine := FALSE);
      Formatter.Begin(modWr, 4);
      PutLine(modWr, "VAR reuse := FALSE;");
      PutLine(modWr, "rep: StubLib.DataRep;");
      PutLine(modWr, "c: StubLib.Conn;");
      PutLine(modWr, "dataPresent: BOOLEAN; <* NOWARN *>");
(*      PutLine(modWr, "stubProt: StubLib.StubProtocol;"); *)
      IF methods[i].sig.result # NIL THEN
        PutLine(modWr, "res: " & 
          CodeForType.ToText(methods[i].sig.result) & ";");
      END;
      IF StubUtils.perfMon THEN
        PutLine(modWr, "wridx, rdidx: INTEGER;" & PerfComment);
      END;
      Formatter.End(modWr);
      Formatter.NewLine(modWr, freshLine := FALSE);
      Formatter.Begin(modWr, 2);
      PutLine(modWr, "BEGIN");
      Formatter.Begin(modWr, 2);
      PutLine(modWr, "TRY");
      IF StubUtils.perfMon THEN
        PutLine(modWr, "IF NetObjPerf.enabled THEN" & PerfComment);
        PutLine(modWr, "  NetObjPerf.StartCall(PerfUtil.ThreadId(), " &
          Fmt.Int(NUMBER(methods[i].sig.formals^)) & ");");
        PutLine(modWr, "END;");
      END;
      PutLine(modWr, "c := StubLib.StartCall(self, Protocol);");
      Formatter.Begin(modWr, 2);
      PutLine(modWr, "TRY");
      PutLine(modWr, "StubLib.OutInt32(c, ORD(Methods." &
        Atom.ToText(methods[i].name) & "));");
      IF StubUtils.perfMon THEN
        PutLine(modWr, "wridx := Wr.Index(c.wr);" & PerfComment);
      END;
      FOR j := 0 TO LAST(methods[i].sig.formals^) DO
        WITH f = methods[i].sig.formals[j] DO
          MarshalTypedVal(modWr, Atom.ToText(f.name) & "_arg", 
                          f.type, Direction.Out, calling := TRUE, 
                          maySuppress := TRUE);
        END;
      END;
      IF StubUtils.perfMon THEN
        PutLine(modWr, "wridx := Wr.Index(c.wr) - wridx;" & PerfComment);
      END;
      PutLine(modWr, "rep := StubLib.AwaitResult(c);");
      PutLine(modWr, "CASE StubLib.InInt32(c, rep) OF");
      Formatter.Begin(modWr, 2);  
      PutLine(modWr, "| ORD(ReturnCodes.OK) => ");
      IF StubUtils.perfMon THEN
        PutLine(modWr, "rdidx := Rd.Index(c.rd);" & PerfComment);
      END;
      FOR j := 0 TO LAST(methods[i].sig.formals^) DO
        WITH f = methods[i].sig.formals[j] DO
          IF f.mode = Type.Mode.Var THEN
            MarshalTypedVal(modWr, Atom.ToText(f.name) & "_arg", 
                            f.type, Direction.In, calling := FALSE);
          END;
        END;
      END;
      IF methods[i].sig.result # NIL THEN
        MarshalTypedVal(modWr, "res", methods[i].sig.result, Direction.In,
                        calling := FALSE);
      END;
      Formatter.End(modWr);
      PutLine(modWr, "reuse := TRUE;");
      IF methods[i].sig.raises = NIL THEN
        StubUtils.Message("Network object method cannot have RAISES ANY.");
        RAISE StubUtils.Failure;
      END;
      (* A network object method can't have a RAISES ANY clause *)
      FOR j := 0 TO LAST(methods[i].sig.raises^) DO
        WITH excp = methods[i].sig.raises[j] DO
          eName := QidToText(excp.qid, "_");
          IF NOT Text.Equal(eName, "Thread_Alerted")  AND
             NOT Text.Equal(eName, "NetObj_Error") THEN
            PutLine(modWr, "| ORD(ReturnCodes." & 
                                       eName & ") => ");
            IF StubUtils.perfMon THEN
              PutLine(modWr, "  rdidx := Rd.Index(c.rd);" & PerfComment);
            END;
            IF excp.arg # NIL THEN
              Formatter.Begin(modWr, 2);
              PutLine(modWr, "  VAR arg: " & CodeForType.ToText(excp.arg)
                & ";");
              Formatter.Begin(modWr, 2);
              PutLine(modWr, "BEGIN");
              MarshalTypedVal(modWr, "arg", excp.arg, Direction.In,
                              calling := FALSE);
              text := "(arg)";
            ELSE
              text := "";
            END;
            PutLine(modWr, "reuse := TRUE;");
            PutLine(modWr, "RAISE " & QidToText(excp.qid, ".") & text & ";");
            IF excp.arg # NIL THEN
              Formatter.End(modWr);
              Formatter.NewLine(modWr);
              Formatter.End(modWr);
              PutLine(modWr, "END;");
            END;
          END;
        END;
      END;
      PutLine(modWr, "ELSE");
      IF StubUtils.perfMon THEN
        PutLine(modWr, "  rdidx := Rd.Index(c.rd);" & PerfComment);
      END;
      PutLine(modWr, "  StubLib.RaiseUnmarshalFailure();");
      Formatter.PutText(modWr, "END");
      Formatter.End(modWr);
      Formatter.NewLine(modWr, freshLine := FALSE);
      PutLine(modWr, "FINALLY");
      IF StubUtils.perfMon THEN
        PutLine(modWr, "  rdidx := Rd.Index(c.rd) - rdidx;" & PerfComment);
      END;
      PutLine(modWr, "  StubLib.EndCall(c, reuse);");
      IF StubUtils.perfMon THEN
        PutLine(modWr, "  IF NetObjPerf.enabled THEN" & PerfComment);
        PutLine(modWr, "    NetObjPerf.EndCall(PerfUtil.ThreadId(), " &
          "wridx, rdidx);");
        PutLine(modWr, "  END;");
      END;
      PutLine(modWr, "END;");
      Formatter.End(modWr);
      Formatter.NewLine(modWr);
      PutLine(modWr, "EXCEPT");
      PutLine(modWr, 
              "| Rd.Failure(ec) => StubLib.RaiseCommFailure(ec);");
      PutLine(modWr, 
              "| Wr.Failure(ec) => StubLib.RaiseCommFailure(ec);");
      Formatter.PutText(modWr, "END;");
      IF methods[i].sig.result # NIL THEN
        Formatter.NewLine(modWr, freshLine := FALSE);
        Formatter.PutText(modWr, "RETURN res;");
      END;
      Formatter.End(modWr);
      Formatter.NewLine(modWr, freshLine := FALSE);
      Formatter.PutText(modWr, "END " & procedureName  & ";");
      Formatter.End(modWr);
      Formatter.NewLine(modWr, freshLine := FALSE);
    END;
    Formatter.End(modWr);
  END Surrogates;

PROCEDURE Dispatcher(modWr: Formatter.T; 
                     t: Type.Object; 
                     typeName: Atom.T;
                     methods: StubCode.MethodList;
                     returnCodes: RefList.T) RAISES {StubUtils.Failure} =
  VAR e: Type.Exception;
      ename: TEXT;
      l: RefList.T;
  BEGIN
    Formatter.PutText(modWr, "\nPROCEDURE Invoke(");
    Formatter.PutText(modWr, "\n    c: StubLib.Conn;");
    Formatter.PutText(modWr, "\n    obj: NetObj.T;");
    Formatter.PutText(modWr, "\n    rep: StubLib.DataRep;");
    Formatter.PutText(modWr, "\n    stubProt: StubLib.StubProtocol)");
    Formatter.PutText(modWr, "\n    RAISES {NetObj.Error, Rd.Failure,");
    Formatter.PutText(modWr, "\n            Wr.Failure, Thread.Alerted} =");
    Formatter.PutText(modWr, "\n  VAR t := NARROW(obj, " & 
                              CodeForType.ToText(t) & ");");
    Formatter.PutText(modWr, "\n  BEGIN");
    Formatter.PutText(modWr, "\n    IF stubProt # Protocol" & 
      " THEN StubLib.RaiseUnmarshalFailure() END;");
    Formatter.PutText(modWr, "\n    TRY");
    Formatter.Begin(modWr, -1); 
    Formatter.PutText(modWr, "\n      CASE StubLib.InInt32(c, rep) OF");
    FOR i := FIRST(methods^) TO LAST(methods^) DO
      Formatter.NewLine(modWr, freshLine := FALSE);
      Formatter.PutText(modWr,"| ORD(Methods." & Atom.ToText(methods[i].name) &
        ") => ");
      IF methods[i].intf # typeName THEN
        Formatter.PutText(modWr, Atom.ToText(methods[i].intf) & ".");
      END;
      Formatter.PutText(modWr, "Stub_" & 
        Atom.ToText(methods[i].name) & "(t, c, rep);");
    END;
    Formatter.End(modWr);
    Formatter.PutText(modWr, "\n      ELSE");
    Formatter.PutText(modWr, "\n        StubLib.RaiseUnmarshalFailure();");
    Formatter.PutText(modWr, "\n      END;");
    Formatter.PutText(modWr, "\n    EXCEPT");
    l := returnCodes;
    IF l = NIL THEN
      PutLine(modWr, "");
    END;
    WHILE l # NIL DO
      e := NARROW(l.head, Type.Exception);
      l := l.tail;
      ename :=  QidToText(e.qid, "_");
      Formatter.PutText(modWr, "\n    | " & QidToText(e.qid, ".") );
      IF e.arg # NIL THEN
        Formatter.PutText(modWr, "(arg)");        
      END; 
      Formatter.PutText(modWr, " => ");
      Formatter.PutText(modWr, "\n        StubLib.StartResult(c);");
      Formatter.PutText(modWr, 
                        "\n        StubLib.OutInt32(c, ORD(ReturnCodes." 
                        & ename & "));");
      IF e.arg # NIL THEN 
        Formatter.PutText(modWr, "\n        ");
        MarshalTypedVal(modWr, "arg", e.arg, Direction.Out, calling := FALSE);
      ELSE
        PutLine(modWr, "");
      END
    END;
    Formatter.PutText(modWr, "    END;");
    Formatter.PutText(modWr, "\n  END Invoke;\n\n");
  END Dispatcher;

PROCEDURE OwnerStubs(modWr: Formatter.T; 
                     t: Type.Object;
                     methods: StubCode.MethodList;
                     lastNewMethod: INTEGER) RAISES {StubUtils.Failure} = 
  VAR varType: Type.T;
  BEGIN
    FOR i := 0 TO lastNewMethod DO
      CodeForType.ProcHeader(modWr, t,
             "Stub_" & Atom.ToText(methods[i].name), 
             StubCode.SigForStub(methods[i].sig),
             StubCode.PragmasForStub());
      Formatter.PutText(modWr, "=\n");
      WITH sig = methods[i].sig DO
        IF NUMBER(sig.formals^) > 0 OR sig.result # NIL THEN
          Formatter.Begin(modWr, 6);
          Formatter.PutText(modWr, "  VAR ");
          FOR j := 0 TO LAST(sig.formals^) DO
            WITH f = sig.formals[j] DO
              TYPECASE f.type OF
              | Type.OpenArray (oa) => varType := oa.refArray
              ELSE varType := f.type;
              END;
              PutLine(modWr, Atom.ToText(f.name) & "_arg: " &
                CodeForType.ToText(varType) & ";");
            END;
          END;
          IF sig.result # NIL THEN
            PutLine(modWr, "res: " & CodeForType.ToText(sig.result) & ";");
          END;
          PutLine(modWr, "dataPresent: BOOLEAN <* NOWARN *>;");
          Formatter.End(modWr); 
        END;
        Formatter.NewLine(modWr);
        Formatter.Begin(modWr, 4);
        PutLine(modWr, "  BEGIN");
        FOR j := 0 TO LAST(sig.formals^) DO
          WITH f = sig.formals[j] DO
            MarshalTypedVal(modWr, Atom.ToText(f.name) & "_arg",
              f.type, Direction.In, calling := TRUE, maySuppress := TRUE);
          END;
        END;
        IF sig.result # NIL THEN
          Formatter.PutText(modWr, "res := ");
        END;
        Formatter.PutText(modWr, "self." & 
          Atom.ToText(methods[i].name) &"(");
        FOR j := 0 TO LAST(methods[i].sig.formals^) DO
          IF j > 0 THEN
            Formatter.PutText(modWr, ", ");
          END;
          Formatter.PutText(modWr, 
                      Atom.ToText(methods[i].sig.formals[j].name) & "_arg");
          TYPECASE methods[i].sig.formals[j].type OF
          | Type.OpenArray =>
            Formatter.PutText(modWr, "^");
          ELSE
          END;
        END;
        PutLine(modWr, ");");
        PutLine(modWr, "StubLib.StartResult(c);");
        PutLine(modWr, 
             "StubLib.OutInt32(c, ORD(ReturnCodes.OK));");
        FOR j := 0 TO LAST(sig.formals^) DO
          WITH f = sig.formals[j] DO
            IF f.mode = Type.Mode.Var THEN
              MarshalTypedVal(modWr, Atom.ToText(f.name) & "_arg",
                f.type, Direction.Out, calling := FALSE);
            END;
          END;
        END;
        IF sig.result # NIL THEN
          MarshalTypedVal(modWr, "res", sig.result, Direction.Out,
                          calling := FALSE);
        END;
        Formatter.End(modWr);  Formatter.NewLine(modWr);
        Formatter.PutText(modWr, "  END " & "Stub_" & 
                     Atom.ToText(methods[i].name) & ";\n\n");
      END;
    END;
  END OwnerStubs;

TYPE Direction = {In, Out};

PROCEDURE MarshalTypedVal(fmtWr: Formatter.T;
                       varName: TEXT;
                       t: Type.T;
                       d: Direction;
                       calling: BOOLEAN;
                       indexDepth := 0;
                       maySuppress := FALSE) RAISES {StubUtils.Failure} =
  BEGIN
    TYPECASE t OF
    | Type.Char (ch) => 
          Enumeration(fmtWr, varName, ch, d, 0, ORD(LAST(CHAR)));
    | Type.UserDefined (ud) => 
          Enumeration(fmtWr, varName, t, d, 0, LAST(ud.elts^));
    | Type.Subrange (sub) => 
        IF t = Type.integer THEN
          StubLibCall(fmtWr, "Integer", varName, d); 
        ELSE
          SubRange(fmtWr, varName, t, d, 
                   NARROW(sub.min, Value.Ordinal).ord, 
                   NARROW(sub.max, Value.Ordinal).ord);
        END;
    | Type.Real => 
        StubLibCall(fmtWr, "Real", varName, d); 
    | Type.LongReal => 
        StubLibCall(fmtWr, "Longreal", varName, d); 
    | Type.Extended => 
        StubLibCall(fmtWr, "Extended", varName, d); 
    | Type.Reference (r) => 
        IF Type.MayBeRefAny(r) OR NOT Type.NamedType(r) THEN
          StubLibCall(fmtWr, "Ref", varName, d, ", -1");
        ELSE
          StubLibCall(fmtWr, "Ref", varName, d, 
                      ", TYPECODE(" & CodeForType.ToText(r) & ")"); 
        END;
    | Type.Array (a) =>
        IF a.index = NIL THEN
          MarshalOpenArray(fmtWr, varName, t, d, calling, indexDepth,
                           maySuppress);
        ELSE
          BeginOutOnly(fmtWr, t, d, maySuppress);
          Formatter.Begin(fmtWr, 2);
          PutLine(fmtWr, "FOR i" & Fmt.Int(indexDepth) & " := FIRST(" & 
                          CodeForType.ToText(a.index) &
                          ") TO LAST(" & CodeForType.ToText(a.index) &
                          ") DO");
          MarshalTypedVal(fmtWr, varName & "[i" & Fmt.Int(indexDepth) & 
                          "]", a.element, d, calling, indexDepth+1);
          Formatter.End(fmtWr);
          PutLine(fmtWr, "END;");
          EndOutOnly(fmtWr, maySuppress);
        END;
    | Type.Packed (p) =>
      BeginOutOnly(fmtWr, t, d, maySuppress);
      MarshalTypedVal(fmtWr, varName, p.base, d, calling, indexDepth);
      EndOutOnly(fmtWr, maySuppress);
    | Type.Record (rec) =>
      BeginOutOnly(fmtWr, t, d, maySuppress);
      FOR i := 0 TO LAST(rec.fields^) DO
        MarshalTypedVal(fmtWr, 
                         varName & "." & Atom.ToText(rec.fields[i].name),
                         rec.fields[i].type, d, calling, indexDepth);
      END;
      EndOutOnly(fmtWr, maySuppress);
    | Type.Set (s) =>
      BeginOutOnly(fmtWr, t, d, maySuppress);
      IF d = Direction.In THEN
        PutLine(fmtWr, varName & ":=" & CodeForType.ToText(s) & "{};");
        Formatter.Begin(fmtWr, 2);
        PutLine(fmtWr, "FOR i" & Fmt.Int(indexDepth) & " := FIRST(" 
                        & CodeForType.ToText(s.range) &
                       ") TO LAST(" & CodeForType.ToText(s.range) &
                                     ") DO");
        Formatter.Begin(fmtWr, 2);
        PutLine(fmtWr, "IF StubLib.InBoolean(c) THEN");
        PutLine(fmtWr, varName & " := " & varName & " + " & 
                CodeForType.ToText(s) & "{i" & Fmt.Int(indexDepth) & "};");
        Formatter.End(fmtWr);
        PutLine(fmtWr, "END")
      ELSE
        Formatter.Begin(fmtWr, 2);
        PutLine(fmtWr, "FOR i" & Fmt.Int(indexDepth) & " := FIRST(" & 
                       CodeForType.ToText(s.range) &
                       ") TO LAST(" & CodeForType.ToText(s.range) &
                                     ") DO");
        PutLine(fmtWr, "StubLib.OutBoolean(c, i" & Fmt.Int(indexDepth) & 
                       " IN " & varName & ");");
      END;
      Formatter.End(fmtWr);
      PutLine(fmtWr, "END;");
      EndOutOnly(fmtWr, maySuppress);
    | Type.Procedure => 
        StubUtils.Message("Can't have a procedure as argument or result " &
          "of a network object method.");
        RAISE StubUtils.Failure;
    ELSE  RAISE StubUtils.Error("Run time error -- shouldn't occur");
    END;
  END MarshalTypedVal;         

PROCEDURE SubRange(fmtWr: Formatter.T;
                   varName: TEXT;
                   t: Type.Subrange;
                   d: Direction;
                   min, max: INTEGER) =
  BEGIN
    IF t.base = Type.integer OR t.base = t THEN
      StubLibCall(fmtWr, "Integer", varName, d, 
                  ", "  & Fmt.Int(min) & ", " & Fmt.Int(max));
    ELSE
      TYPECASE t.base OF
      | Type.Enumeration => Enumeration(fmtWr, varName, t.base, d, min, max);
      | Type.Subrange => SubRange(fmtWr, varName, t.base, d, min, max);
      ELSE RAISE StubUtils.Error("Run time error -- shouldn't occur");
      END;
    END;
  END SubRange;

PROCEDURE Enumeration(fmtWr: Formatter.T;
                      varName: TEXT;
                      t: Type.Enumeration;
                      d: Direction;
                      min, max: INTEGER) =
  BEGIN
    IF d = Direction.In THEN
      PutLine(fmtWr, varName & " := VAL(StubLib.InInteger(c, rep, "
                       & Fmt.Int(min) & "," & Fmt.Int(max) & "), "
                       & CodeForType.ToText(t) &");");
    ELSE
      PutLine(fmtWr, "StubLib.OutInteger(c, ORD(" & varName &"));");
    END;
  END Enumeration;

PROCEDURE PutLine(fmtWr: Formatter.T; text: TEXT) =
  BEGIN              
    Formatter.PutText(fmtWr, text);
    Formatter.NewLine(fmtWr, freshLine := FALSE);
  END PutLine;

PROCEDURE StubLibCall(fmtWr: Formatter.T; 
                      proc: TEXT; 
                      varName: TEXT;
                      d: Direction;
                      extra := "") = 
  BEGIN
    IF d = Direction.In THEN
      PutLine(fmtWr, varName & " := StubLib.In" & proc & "(c, rep"
                                        & extra & ");");
    ELSE
      PutLine(fmtWr, "StubLib.Out" & proc & "(c, " & varName & ");");
    END;
  END StubLibCall;

PROCEDURE StubLibCallNoRep(fmtWr: Formatter.T; 
                      proc: TEXT; 
                      varName: TEXT;
                      d: Direction;
                      extra := "") = 
  BEGIN
    IF d = Direction.In THEN
      PutLine(fmtWr, varName & " := StubLib.In" & proc & "(c"
                                        & extra & ");");
    ELSE
      PutLine(fmtWr, "StubLib.Out" & proc & "(c, " & varName & ");");
    END;
  END StubLibCallNoRep;

PROCEDURE BeginOutOnly(fmtWr: Formatter.T; 
                       <*UNUSED*> t: Type.T;
                       d: Direction;
                       maySuppress: BOOLEAN) =
  VAR dataPresent:= TRUE;  (* Could check for size *)
     (* When recognizing pragma, dataPresent determined by
        methods[i].formals[j].outOnly *)
  BEGIN
    IF maySuppress THEN
      IF d = Direction.Out THEN 
        PutLine(fmtWr, "dataPresent := " & Fmt.Bool(dataPresent) &";" );
      END;
      StubLibCallNoRep(fmtWr, "Boolean", "dataPresent", d); 
      Formatter.Begin(fmtWr, 2);
      PutLine(fmtWr, "IF dataPresent THEN ");
    END;
  END BeginOutOnly;
  
PROCEDURE EndOutOnly(fmtWr: Formatter.T; maySuppress: BOOLEAN) =
  BEGIN
    IF maySuppress THEN
      PutLine(fmtWr, "END;");
      Formatter.End(fmtWr);
    END;
  END EndOutOnly;

PROCEDURE MarshalOpenArray(fmtWr: Formatter.T;
                           varName: TEXT;
                           a: Type.OpenArray;
                           d: Direction;
                           calling: BOOLEAN;
                           indexDepth: INTEGER;
                           maySuppress: BOOLEAN) RAISES {StubUtils.Failure} =
  VAR nDimensions:= a.openDimensions;
      aName, baseName, boundList: Text.T;
      component: Type.T;
  BEGIN
    IF calling THEN (* Must marshal/unmarshal array bounds *)
      IF d = Direction.Out THEN
        StubLibCall(fmtWr, "Integer", "NUMBER(" & varName & ")",d);
        aName := varName & "[0";
        FOR i := 2 TO nDimensions DO
          StubLibCall(fmtWr, "Integer", "NUMBER(" & aName & "])",d);
          aName := aName & ", 0";
        END;
        baseName := varName;
      ELSE
        Formatter.PutText(fmtWr, "WITH n1 = StubLib.InInteger(c, rep)");
        boundList := "n1";
        FOR i := 2 TO nDimensions DO
          PutLine(fmtWr, ",");
          Formatter.PutText(fmtWr, "    n" & Fmt.Int(i) &
            " = StubLib.InInteger(c, rep)");
          boundList := boundList & ", n" & Fmt.Int(i);
        END;
        PutLine(fmtWr, " DO");
        PutLine(fmtWr, "  " &  varName & " := NEW(" & 
                CodeForType.ToText(a.refArray) & ", " & boundList & ");");
        PutLine(fmtWr, "END;");
        baseName := varName & "^";
      END;
    ELSE
      IF d = Direction.Out THEN
        baseName := varName & "^";
      ELSE
        baseName := varName;
      END;
    END;
                
    (* Suppress actual data for <*OUTPUT*> params on call *)
    BeginOutOnly(fmtWr, a, d, maySuppress);

    Formatter.Begin(fmtWr, 2);
    PutLine(fmtWr, "FOR n1 := 0 TO LAST(" & baseName & ") DO" );
    aName := varName & "[n1";
    component := a.element;
    FOR i := 2 TO nDimensions DO
      Formatter.Begin(fmtWr, 2);
      PutLine(fmtWr, "FOR n" & Fmt.Int(i) & " := 0 TO LAST(" & aName
                      & "]) DO");
      aName := aName & ",  n" & Fmt.Int(i);
      component := NARROW(component, Type.OpenArray).element;
    END;
    MarshalTypedVal(fmtWr, aName & "]", component, d, calling, indexDepth);
    FOR i := 1 TO nDimensions DO
      Formatter.End(fmtWr);
      PutLine(fmtWr, "END;"); (* End FOR Loop *)
    END;
    EndOutOnly(fmtWr, maySuppress);
  END MarshalOpenArray;
  
BEGIN
END ModuleStubCode.
