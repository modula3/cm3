(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * See file COPYRIGHT-COLUMBIA for details.
 * 
 * Author          : Tobias Hoellerer (htobias)
 * Created On      : Fri Nov 10 17:37:04 EST 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Apr  6 20:51:30 1998
 * Update Count    : 183
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1.1.1  2001/12/02 13:15:54  wagner
 * Blair MacIntyre's sharedobjgen package
 *
 * Revision 1.9  1998/05/11 02:34:29  bm
 * bug fixes, added SharedObj.Wait
 *
 * Revision 1.8  1997/11/13 23:45:06  bm
 * Changed lock location of generated picklers, to allow recursion
 *
 * Revision 1.7  1997/10/22 14:45:13  bm
 * Bug fix.  Naming conflicts.
 *
 * Revision 1.6  1997/08/11 20:36:41  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

MODULE SOxModuleSOCode;

IMPORT SOxCodeUtils, SOxCoder, Formatter, ImportList, SOxCodeGenError,
       Type, SOxCodeFiles, Wr, CodeForType, Atom, AtomList,
       AtomListFuncs, Text, Value, Fmt, StubCode;

REVEAL
  T = SOxCoder.T BRANDED OBJECT
        initializers:= "";
      OVERRIDES
        InitImports := initImports;
        Import      := import;
        Head        := head;
        Decls       := decls;
        Main        := main;
        Bottom      := bottom;
      END;

<* FATAL Wr.Failure*>

PROCEDURE PutLine (fmtWr: Formatter.T; text: TEXT) =
  BEGIN
    Formatter.PutText(fmtWr, text);
    Formatter.NewLine(fmtWr, freshLine := FALSE);
  END PutLine;

PROCEDURE EndLine (fmtWr: Formatter.T; text: TEXT) =
  BEGIN
    Formatter.PutText(fmtWr, text);
    Formatter.End(fmtWr);
    Formatter.NewLine(fmtWr, freshLine := FALSE);
  END EndLine;

VAR
  extraImports := ARRAY [1 .. 16] OF
                    Atom.T{
                    Atom.FromText("SharedObj"),
                    Atom.FromText("SharedObjRep"),
                    Atom.FromText("SharedObjStubLib"),
                    Atom.FromText("WeakRef"),
                    Atom.FromText("EventStubLib"),
                    Atom.FromText("EventProtocol"), Atom.FromText("Rd"),
                    Atom.FromText("Wr"), Atom.FromText("SharedObjError"),
                    Atom.FromText("ThreadF"), Atom.FromText("Thread"), 
                    Atom.FromText("Event"),
                    Atom.FromText("Pickle2 AS Pickle"), 
                    Atom.FromText("EmbProxiedObj"), 
                    Atom.FromText("PickleStubs"), 
                    Atom.FromText("ObjectSpace")
                    (*
                    Atom.FromText("Time"), Atom.FromText("Fmt"),
                    Atom.FromText("IO")
                    *)
                    };
PROCEDURE initImports (<*UNUSED*> self    : T;
                                  basename: TEXT;
                                  imports : ImportList.T) =
  VAR
    imp := ARRAY [1 .. 1] OF
             Atom.T{Atom.FromText(SOxCodeUtils.FileName(
                                    basename, SOxCodeFiles.T.CB_I3))};
  BEGIN
    CodeForType.AugmentImportList(imports, extraImports);
    CodeForType.AugmentImportList(imports, imp);
  END initImports;

PROCEDURE import (<*UNUSED*> self    : T;
                             type    : Type.Object;
                             methods : ImportList.MethodList;
                             umethods: AtomList.T;
                             imports : ImportList.T           ) =
  BEGIN
    (* CodeForType.ImportLst(type, imports, methods, umethods); *)
    CodeForType.ImportSOLst(type, imports, methods, umethods);
  END import;

PROCEDURE head (self    : T;
                wr      : Formatter.T;
                fname   : TEXT;
                basename: TEXT;
                imports : ImportList.T ) =
  BEGIN
    WITH Put = Formatter.PutText,
         Tab = Formatter.Begin,
         Nl  = Formatter.NewLine  DO
      SOxCodeUtils.HeaderComment(wr, fname);
      self.basename := basename;
      self.fbasename :=
        SOxCodeUtils.FileName(basename, SOxCodeFiles.T.SO_M3);

      Put(wr, "MODULE " & self.fbasename & " EXPORTS ");
      Tab(wr, 0);
      PutLine(wr, SOxCodeUtils.FileName(basename,
                                        SOxCodeFiles.T.PKL_I3) & ", ");
      EndLine(wr, self.basename & ", " & SOxCodeUtils.FileName(
                                           basename, SOxCodeFiles.T.PRX_I3)
                    & ";");

      CodeForType.ProduceImports(wr, imports);

      Nl(wr, freshLine := FALSE);
      PutLine(wr, "CONST SharedObj_Protocol: EventProtocol.StubProtocol = 1;");

      PutLine(wr, "EXCEPTION DuplicateSpecial;");
      Nl(wr, freshLine := TRUE);
    END;
  END head;

PROCEDURE decls (           self    : T;
                            wr      : Formatter.T;
                            typeID  : Type.Qid;
                            stypeID : Type.Qid;
                 <*UNUSED*> implName: TEXT;
                            methods : ImportList.MethodList;
                            umethods: AtomList.T             ) =
  VAR
    typTxt               : TEXT;
    identfTxt, identfsTxt: TEXT;
    meth                 : Atom.T;
    firstMeth            : BOOLEAN;
  BEGIN
    WITH Put    = Formatter.PutText,
         Nl     = Formatter.NewLine,
         Tab    = Formatter.Begin,
         Br     = Formatter.Break,
         Grp    = Formatter.Group,
         EndTab = Formatter.End      DO
      typTxt := CodeForType.QidToText(typeID);
      identfTxt := CodeForType.QidToIdentf(typeID);
      identfsTxt := CodeForType.QidToIdentf(stypeID);

      Put(wr, "TYPE " & identfTxt & "_SOMethods = {");
      Tab(wr, 0);
      firstMeth := TRUE;
      FOR i := 0 TO LAST(methods^) DO
        meth := methods[i].name;
        IF AtomList.Member(umethods, meth) THEN
          IF firstMeth THEN
            firstMeth := FALSE
          ELSE
            Put(wr, ", ");
            Br(wr);
          END;
          Put(wr, Atom.ToText(meth));
        END;
      END;
      EndLine(wr, "};");
      Nl(wr, freshLine := FALSE);

      Tab(wr, 2);
      Put(wr, "REVEAL");

      Nl(wr, freshLine := FALSE);
      PutLine(wr, identfTxt & " = " & identfsTxt & " BRANDED \"Shared"
                    & " " & self.basename & "." & identfTxt &
                    " v1.0\" OBJECT");

      Tab(wr, 2);
      PutLine(wr, "OVERRIDES");

      IF SOxCodeUtils.genProxyCode THEN
        PutLine(wr, "makeProxy := MakeProxy_" & identfTxt & ";");
      END;
      PutLine(wr, "applyUpdate := ApplyUpdate_" & identfTxt & ";");
      FOR i := 0 TO LAST(methods^) DO
        meth := methods[i].name;
        Nl(wr, freshLine := FALSE);
        Grp(wr);
        Put(wr, Atom.ToText(meth) & " := ");
        Put(wr, "Shared_" & Atom.ToText(meth) & "_" & identfTxt & ";");
        EndTab(wr);
      END;
      EndTab(wr);
      Nl(wr, freshLine := FALSE);
      PutLine(wr, "END;");

      EndTab(wr);
      Nl(wr, freshLine := FALSE);
    END;
  END decls;

PROCEDURE main (           self    : T;
                           wr      : Formatter.T;
                           typeID  : Type.Qid;
                           type    : Type.Object;
                           stypeID : Type.Qid;
                <*UNUSED*> implName: TEXT;
                           methods : ImportList.MethodList;
                           umethods: AtomList.T             )
  RAISES {SOxCodeGenError.E} =
  VAR
    methTxt, typTxt      : TEXT;
    identfTxt, identfsTxt: TEXT;
    meth                 : Atom.T;
    exceptions           : AtomList.T := NIL;
    SOexports := AtomList.List2(Atom.FromText(self.basename), 
                                Atom.FromText(SOxCodeUtils.FileName(
                                                  self.basename, 
                                                  SOxCodeFiles.T.PRX_I3)));
  BEGIN
    WITH Put    = Formatter.PutText,
         Br     = Formatter.Break,
         Nl     = Formatter.NewLine,
         Tab    = Formatter.Begin,
         EndTab = Formatter.End      DO
      typTxt := CodeForType.QidToText(typeID);
      identfTxt := CodeForType.QidToIdentf(typeID);
      identfsTxt := CodeForType.QidToIdentf(stypeID);

      FOR i := 0 TO LAST(methods^) DO
        IF AtomList.Member(umethods, methods[i].name) THEN
          CodeForType.AugmentExceptionList(exceptions,
                                           methods[i].sig.raises, SOexports);
        END;
      END;

      IF SOxCodeUtils.genProxyCode THEN
        Tab(wr, 2);
        PutLine(wr, "PROCEDURE MakeProxy_" & identfTxt & 
          " (self: " & identfTxt & ") =");
        Tab(wr, 2);
        PutLine(wr, "BEGIN");
        Tab(wr, 2);
        PutLine(wr, "IF MkProxy" & identfTxt & " # NIL THEN");
        EndLine(wr, "MkProxy" & identfTxt & "(self);");
        EndLine(wr, "END;");
        EndLine(wr, "END MakeProxy_" & identfTxt & ";");
      END;
      Nl(wr, freshLine := FALSE);

      Tab(wr, 2);
      Put(wr, "PROCEDURE ApplyUpdate_" & identfTxt & " (");
      Tab(wr, 0);
      Put(wr, "self: " & identfTxt & "; ");
      Br(wr);
      Put(wr, "ev: Event.T; ");
      Br(wr);
      Put(wr, "h: EventStubLib.Handle) ");
      EndTab(wr);
      Br(wr);
      Put(wr, "RAISES {");
      Tab(wr, 0);
      Put(wr, "SharedObj.Error, ");
      EVAL AtomListFuncs.DeleteD(exceptions, Atom.FromText("SharedObj.Error"));
      Br(wr);
      IF AtomListFuncs.DeleteD(exceptions, 
                               Atom.FromText("SharedObj.Fatal")) # NIL THEN
        Put(wr, "SharedObj.Fatal, ");
        Br(wr);
      END;
      Put(wr, "Event.Error, ");
      Br(wr);
      Put(wr, "Rd.Failure, ");
      Br(wr);
      EndLine(wr, "Thread.Alerted} =");

      Tab(wr, 2);
      PutLine(wr, "BEGIN");

      Tab(wr, 2);
      PutLine(wr, "IF ev.prot # SharedObj_Protocol THEN");
      EndLine(wr, "EventStubLib.RaiseUnmarshalFailure();");
      PutLine(wr, "END;");
      Tab(wr, 2);
      PutLine(wr, "WITH meth = SharedObjStubLib.InInt32(h) DO");
      Tab(wr, 2);
      
      IF exceptions # NIL THEN
        PutLine(wr, "TRY");
        Tab(wr, 2);
      END;
      PutLine(wr, "TRY");
      PutLine(wr, "SharedObjStubLib.AcquireWriteLock(self);");
      PutLine(wr, "self.updating := ThreadF.MyId();");
      PutLine(wr, "CASE meth OF");

      FOR i := 0 TO LAST(methods^) DO
        meth := methods[i].name;
        methTxt := Atom.ToText(meth);
        IF AtomList.Member(umethods, meth) THEN
          Put(wr, "| ");
          Tab(wr, 0);
          Put(wr, "ORD(" & identfTxt & "_SOMethods." & methTxt & ") => ");
          Br(wr);
          EndLine(wr, "Stub_" & methTxt & "_" & identfTxt & "(self, h);");
        END;
      END;
      Tab(wr, 2);
      PutLine(wr, "ELSE");
      EndLine(wr, "EventStubLib.RaiseUnmarshalFailure();");
      EndLine(wr, "END;");
      Tab(wr, 2);
      PutLine(wr, "FINALLY");
      PutLine(wr, "self.updating := -1;");
      EndLine(wr, "SharedObjStubLib.ReleaseWriteLock(self);");
      EndLine(wr, "END;");
      IF exceptions # NIL THEN
        Tab(wr, 2);
        Put(wr, "EXCEPT ");
        Br(wr);
        WHILE exceptions # NIL DO
          Put(wr, Atom.ToText(exceptions.head));
          IF exceptions.tail # NIL THEN
            Put(wr, ", ");
          END;
          Br(wr);
          exceptions := exceptions.tail;
        END;
        Put(wr, "=> ");
        Br(wr);
        EndLine(wr, "(* ignore these exceptions quietly *)");
        EndLine(wr, "END;");
      END;
      EndLine(wr, "END;");
      EndLine(wr, "END ApplyUpdate_" & identfTxt & ";");

      Nl(wr, freshLine := FALSE);
      Surrogates(self, wr, type, identfTxt, identfsTxt, methods, umethods);
      Stubs(self, wr, type, identfTxt, identfsTxt, methods, umethods);
      Callbacks(self, wr, type, identfTxt, identfsTxt, methods, umethods);
      Picklers(self, wr, type, identfTxt, identfsTxt);
    END;
  END main;

PROCEDURE bottom (self: T; wr: Formatter.T; <*UNUSED*> fname: TEXT) =
  BEGIN
    WITH Put    = Formatter.PutText,
         Nl = Formatter.NewLine DO
      Nl(wr, freshLine := FALSE);
      PutLine(wr, "BEGIN");
      Put(wr, self.initializers);
      PutLine(wr, "END " & self.fbasename & ".");
    END;
  END bottom;

TYPE Direction = {In, Out};

PROCEDURE PrintStubArgs (wr : Formatter.T;
                         sig: Type.Signature;
                         dir: Direction       ) =
  BEGIN
    FOR j := 0 TO LAST(sig.formals^) DO
      Formatter.PutText(wr, ", ");
      Formatter.Break(wr);
      Formatter.PutText(wr, Atom.ToText(sig.formals[j].name) & "_arg");
      IF dir = Direction.In THEN
        TYPECASE sig.formals[j].type OF
        | Type.OpenArray => Formatter.PutText(wr, "^");
        ELSE
        END;
      END;
    END;
  END PrintStubArgs;

PROCEDURE Surrogates (self: T; 
                      wr      : Formatter.T;
                      t       : Type.Object;
                      identfTxt, identfsTxt: TEXT;
                      methods : ImportList.MethodList;
                      umethods: AtomList.T           )
  RAISES {SOxCodeGenError.E} =
  VAR
    procedureName: TEXT;
    uMeth                     : BOOLEAN;
    methTxt      : TEXT;
    meth                 : Atom.T;
    SOexports := AtomList.List2(Atom.FromText(self.basename), 
                                Atom.FromText(SOxCodeUtils.FileName(
                                                  self.basename, 
                                                  SOxCodeFiles.T.PRX_I3)));
  BEGIN
    WITH Put    = Formatter.PutText,
         Nl     = Formatter.NewLine,
         Tab    = Formatter.Begin,
         EndTab = Formatter.End      DO
      FOR i := 0 TO LAST(methods^) DO
        meth := methods[i].name;
        methTxt := Atom.ToText(meth);

        uMeth := AtomList.Member(umethods, meth);
        procedureName := "Shared_" & Atom.ToText(methods[i].name) & 
                             "_" & identfTxt;

        Tab(wr, 2);
        CodeForType.ProcHeader(wr, t, procedureName, methods[i].sig,
                               suffix := "_arg", exports := SOexports); 
        PutLine(wr, " = ");

        IF uMeth THEN
          Tab(wr, 4);
          PutLine(wr, "VAR out: SharedObjStubLib.Handle;");
          PutLine(wr, "id := ThreadF.MyId();");
          EndLine(wr, "dataPresent: BOOLEAN; <* NOWARN *>");
          (* PutLine(wr, "stubProt: StubLib.StubProtocol;"); *)
          (*
            IF methods[i].sig.result # NIL THEN
              PutLine(wr, "res: " &
                CodeForType.ToText(methods[i].sig.result) & ";");
            END;
          *)
          (*
            IF StubUtils.perfMon THEN
              PutLine(wr, "wridx, rdidx: INTEGER;" & PerfComment);
            END;
          *)
        END;
        Tab(wr, 2);
        PutLine(wr, "BEGIN");
        IF Text.Equal("init", Atom.ToText(methods[i].name)) THEN
          PutLine(
            wr, "(**************************************************)");
          PutLine(
            wr, "(* This get's done once. After that, it's a noop. *)");
          PutLine(
            wr, "(**************************************************)");
          PutLine(wr, "self := NARROW(SharedObj.Init(self), " &
                                   identfTxt & ");");
          PutLine(wr, "self.makeProxy();");
          PutLine(
            wr, "(**************************************************)");
        END;
        PutLine(wr, "IF NOT self.ok THEN "
                      & "SharedObjError.RaiseDeadObject() END;");
        IF uMeth THEN
          Tab(wr, 2);
          PutLine(wr, "TRY");
          PutLine(wr, "SharedObjStubLib.AcquireReadLock(self);");
          Tab(wr, 2);
          PutLine(wr, "IF self.updating = id THEN ");
          PutLine(wr, "(* do a simple, non-update call to the method *)");
          IF methods[i].sig.result # NIL THEN
            Put(wr, "RETURN ");
          END;
          Put(wr, identfsTxt & "." & Atom.ToText(methods[i].name) & "(");
          Tab(wr, 0);
          Put(wr, "self");
          PrintStubArgs(wr, methods[i].sig, Direction.Out);
          EndTab(wr);
          IF methods[i].sig.result # NIL THEN
            EndLine(wr, ");");
          ELSE
            PutLine(wr, ");");
            EndLine(wr, "RETURN;");
          END;
          EndLine(wr, "END;");
          Tab(wr, 2);
          PutLine(wr, "FINALLY");
          EndLine(wr, "SharedObjStubLib.ReleaseReadLock(self);");
          PutLine(wr, "END;");
          (* "SharedObjError.RaiseRecursiveUpdate(); END;"); *)

          Tab(wr, 2);
          PutLine(wr, "TRY");
          (*
            IF StubUtils.perfMon THEN
            PutLine(wr, "IF NetObjPerf.enabled THEN" & PerfComment);
            PutLine(wr, "  NetObjPerf.StartCall(PerfUtil.ThreadId(), " &
              Fmt.Int(NUMBER(methods[i].sig.formals^)) & ");");
            PutLine(wr, "END;");
            END;
          *)
          PutLine(wr, "out := SharedObjStubLib.StartCall(self);");
          Tab(wr, 2);
          PutLine(wr, "IF SharedObjStubLib.MarshalArgs(out) THEN");
          PutLine(wr, "SharedObjStubLib.OutInt32(out, ORD(" &
            identfTxt & "_SOMethods." & Atom.ToText(methods[i].name) & "));");
          (*
            IF StubUtils.perfMon THEN
              PutLine(wr, "wridx := Wr.Index(c.wr);" & PerfComment);
            END;
          *)
          FOR j := 0 TO LAST(methods[i].sig.formals^) DO
            WITH f = methods[i].sig.formals[j] DO
              MarshalTypedVal(wr, Atom.ToText(f.name) & "_arg",
                              "SharedObjStubLib", f.type, Direction.Out,
                              calling := TRUE, maySuppress := TRUE);
            END;
          END;
          (*
            IF StubUtils.perfMon THEN
              PutLine(wr, "wridx := Wr.Index(c.wr) - wridx;" & PerfComment);
            END;
          *)
          EndTab(wr);
          PutLine(wr, "END;");
          PutLine(wr, "SharedObjStubLib.SequenceCall(out, SharedObj_Protocol);");
          Tab(wr, 2);
          PutLine(wr, "TRY");
          PutLine(wr, "SharedObjStubLib.AcquireWriteLock(self);");
          PutLine(wr, "self.updating := id;");
          Put(wr, "Callback_pre_" & Atom.ToText(methods[i].name) & 
            "_" & identfTxt & "(");
          Tab(wr, 0);
          Put(wr, "self");
          PrintStubArgs(wr, methods[i].sig, Direction.Out);
          EndTab(wr);
          PutLine(wr, ");");
          IF methods[i].sig.result # NIL THEN
            Tab(wr, 2); 
            Put(wr, "WITH res = ");
          END;
          Put(wr, identfsTxt & "." & Atom.ToText(methods[i].name) & "(");
          Tab(wr, 0);
          Put(wr, "self");
          PrintStubArgs(wr, methods[i].sig, Direction.Out);
          EndTab(wr);
          IF methods[i].sig.result # NIL THEN
            PutLine(wr, ") DO");
          ELSE
            PutLine(wr, ");");
          END;
          Put(wr, "Callback_post_" & Atom.ToText(methods[i].name) &  
            "_" & identfTxt & "(");
          Tab(wr, 0);
          Put(wr, "self");
          PrintStubArgs(wr, methods[i].sig, Direction.Out);
          EndTab(wr);
          IF methods[i].sig.result # NIL THEN
            PutLine(wr, ");");
            EndLine(wr, "RETURN res;");
            EndLine(wr, "END;");
          ELSE
            EndLine(wr, ");");
          END;
          Tab(wr, 2);
          PutLine(wr, "FINALLY");
          PutLine(wr, "self.updating := -1;");
          PutLine(wr, "SharedObjStubLib.ReleaseWriteLock(self);");
          EndLine(wr, "SharedObjStubLib.EndCall(out);");
          EndLine(wr, "END;");
          PutLine(wr, "EXCEPT");
          Put(wr, "| Wr.Failure (ec) => SharedObjError.RaiseCommFailure(ec)");
          IF methods[i].sig.result # NIL THEN
            PutLine(wr, "; <*ASSERT FALSE*>");
          ELSE
            PutLine(wr, ";");
          END;
          Put(wr, "| Thread.Alerted => SharedObjError.RaiseAlerted()");
          IF methods[i].sig.result # NIL THEN
            PutLine(wr, "; <*ASSERT FALSE*>");
          ELSE
            PutLine(wr, ";");
          END;
          EndLine(wr, "END;");

          (*
            IF StubUtils.perfMon THEN
              PutLine(wr, "rdidx := Rd.Index(c.rd);" & PerfComment);
            END;
          *)
        ELSE
          Tab(wr, 2);
          PutLine(wr, "TRY");
          PutLine(wr, "SharedObjStubLib.AcquireReadLock(self);");
          IF methods[i].sig.result # NIL THEN 
            Put(wr, "RETURN "); 
          END;
          Put(wr, identfsTxt & "." & Atom.ToText(methods[i].name) & "(");
          Tab(wr, 0);
          Put(wr, "self");
          PrintStubArgs(wr, methods[i].sig, Direction.Out);
          EndTab(wr);
          EndLine(wr, ");");
          Tab(wr, 2);
          PutLine(wr, "FINALLY");
          EndLine(wr, "SharedObjStubLib.ReleaseReadLock(self);");
          EndLine(wr, "END;");
        END;
        EndLine(wr, "END " & procedureName & ";");
        Nl(wr, freshLine := FALSE);
      END;
    END;
  END Surrogates;

PROCEDURE Stubs (self: T; 
                 wr      : Formatter.T;
                 t: Type.Object;
                 identfTxt, identfsTxt: TEXT;
                 methods : ImportList.MethodList;
                 umethods: AtomList.T           )
  RAISES {SOxCodeGenError.E} =
  VAR
    varType: Type.T;
    SOexports := AtomList.List2(Atom.FromText(self.basename), 
                                Atom.FromText(SOxCodeUtils.FileName(
                                                  self.basename, 
                                                  SOxCodeFiles.T.PRX_I3)));
  BEGIN
    FOR i := 0 TO LAST(methods^) DO
      WITH Put    = Formatter.PutText,
         Nl     = Formatter.NewLine,
         Tab    = Formatter.Begin,
         EndTab = Formatter.End,
(*
         Br     = Formatter.Break,
*)
         sig    = methods[i].sig,
         stubSig = StubCode.SigForStub(sig),
         meth = methods[i].name,
         methTxt = Atom.ToText(meth),
         procedureName = "Stub_" & methTxt & "_" & identfTxt
       DO
        IF AtomList.Member(umethods, meth) THEN
          Tab(wr, 2);
          CodeForType.ProcHeader(wr, t, procedureName, stubSig,
                                 StubCode.PragmasForStub(), 
                                 exports := SOexports); 
          PutLine(wr, " = ");
          IF NUMBER(sig.formals^) > 0 THEN
            Tab(wr, 6);
            Put(wr, "  VAR ");
            FOR j := 0 TO LAST(sig.formals^) DO
              WITH f = sig.formals[j] DO
                TYPECASE f.type OF
                | Type.OpenArray (oa) => varType := oa.refArray
                ELSE varType := f.type;
                END;
                WITH txt = Atom.ToText(f.name)& "_arg: " &
                     CodeForType.ToText(varType, exports := SOexports) & ";" DO
                  PutLine(wr, txt);
                END;
              END;
            END;
            EndLine(wr, "dataPresent: BOOLEAN <* NOWARN *>;");
          END;
          Tab(wr, 2);
          PutLine(wr, "BEGIN");

          FOR j := 0 TO LAST(sig.formals^) DO
            WITH f = sig.formals[j] DO
              MarshalTypedVal(wr, Atom.ToText(f.name) & "_arg",
                              "SharedObjStubLib", f.type, Direction.In,
                              calling := TRUE, maySuppress := TRUE);
            END;
          END;

          Put(wr, "Callback_pre_" & methTxt & "_" & identfTxt & "(");
          Tab(wr, 0);
          Put(wr, "self");
          PrintStubArgs(wr, sig, Direction.In);
          EndTab(wr);
          PutLine(wr, ");");
          IF sig.result # NIL THEN
            Put(wr, "EVAL ");
          END;
          Put(wr, identfsTxt & "." & methTxt & "(");
          Tab(wr, 0);
          Put(wr, "self");
          PrintStubArgs(wr, sig, Direction.In);
          EndTab(wr);
          PutLine(wr, ");");

          Put(wr, "Callback_post_" & methTxt &  "_" & identfTxt & "(");
          Tab(wr, 0);
          Put(wr, "self");
          PrintStubArgs(wr, sig, Direction.In);
          EndTab(wr);
          EndLine(wr, ");");
          EndLine(wr, "END " & procedureName & ";");
          Nl(wr, freshLine := FALSE);
        END;
      END;
    END;
  END Stubs;

VAR cbName := ARRAY [0..1] OF Text.T{ "pre", "post" };

PROCEDURE Callbacks (self: T; 
                     wr      : Formatter.T;
                     <*UNUSED*>t: Type.Object;
                     identfTxt: TEXT;
                     <*UNUSED*>identfsTxt: TEXT;
                     methods : ImportList.MethodList;
                     umethods: AtomList.T           ) =
  VAR
    SOexports := AtomList.List2(Atom.FromText(self.basename), 
                                Atom.FromText(SOxCodeUtils.FileName(
                                                  self.basename, 
                                                  SOxCodeFiles.T.PRX_I3)));
    procedureName: TEXT;
  BEGIN
    FOR i := 0 TO LAST(methods^) DO
      WITH Put    = Formatter.PutText,
         Nl     = Formatter.NewLine,
         EndTab = Formatter.End,
         Tab    = Formatter.Begin,
         sig = methods[i].sig,
         meth = methods[i].name,
         methTxt = Atom.ToText(meth)
       DO
        IF AtomList.Member(umethods, meth) THEN
          FOR j := FIRST(cbName) TO LAST(cbName) DO
            procedureName := "Callback_" & cbName[j] & "_" & methTxt & 
                                 "_" & identfTxt;

            Tab(wr, 2);
            Put(wr, "PROCEDURE " & procedureName & "(");
            Tab(wr,0);
            Put(wr, "self: " & identfTxt);
            CodeForType.PrintSig(wr, sig, NIL, suffix := "_arg",
                                exports := SOexports);
            EndLine(wr, ") =");
            (*
            CodeForType.ProcHeader(wr, t, procedureName, sig,
                                   suffix := "_arg", exports := SOexports); 
            *)
            PutLine(wr, "VAR cbs := self.callbacks;");
            Tab(wr, 2);
            PutLine(wr, "BEGIN");
            Tab(wr, 2);
            PutLine(wr, "WHILE cbs # NIL DO");
            Tab(wr, 2);
            PutLine(wr, "IF cbs.head.ready THEN");
            Tab(wr, 2);
            PutLine(wr, "WITH ref = WeakRef.ToRef(cbs.head.weakRef) DO");
            Tab(wr, 2);
            PutLine(wr, "IF ref # NIL THEN");
            Tab(wr, 2);
            PutLine(wr, "WITH cb = NARROW(ref, " & 
              SOxCodeUtils.FileName(self.basename,
                                    SOxCodeFiles.T.CB_I3) & "." &
                                    identfTxt & ") DO");
            Tab(wr, 2);
            Put(wr, "IF NOT cb." & cbName[j] & "_" & methTxt & "(");
            Tab(wr, 0);
            Put(wr, "self");
            PrintStubArgs(wr, sig, Direction.Out);
            EndTab(wr);
            PutLine(wr, ") THEN");
            EndLine(wr, "cb." & cbName[j] & "_anyChange(self);");
            EndLine(wr, "END;");
            EndLine(wr, "END;");
            EndLine(wr, "END;");
            EndLine(wr, "END;");
            PutLine(wr, "END;");
            EndLine(wr, "cbs := cbs.tail;");
            EndLine(wr, "END;");
            EndLine(wr, "END " & procedureName & ";");
            Nl(wr, freshLine := FALSE);
          END;
        END;
      END;
    END;
  END Callbacks; 

PROCEDURE Picklers (self: T; 
                 wr      : Formatter.T;
                 t: Type.Object;
                 identfTxt, identfsTxt: TEXT) RAISES {SOxCodeGenError.E} =
  VAR
    procedureName: Text.T;
  BEGIN
    WITH Put    = Formatter.PutText,
         Nl     = Formatter.NewLine,
         EndTab = Formatter.End,
         Br     = Formatter.Break,
         Tab    = Formatter.Begin,
         nodata = (NUMBER(t.fields^) = 0) DO

      PutLine(wr, "(* The pickling routine for this shared object." &
        " We will register a");
      PutLine(wr, "   pickler for " & self.basename & "." & 
        identfsTxt & ", and then handle both " & identfsTxt & " and " &
        identfTxt & ".");
      PutLine(wr, "   Pickling subtypes of " & identfTxt & " is illegal. *)");

      Tab(wr, 2);
      PutLine(wr, "REVEAL");
      Put(wr, identfTxt & "Special = ");
      Tab(wr, 0);
      PutLine(wr, "SharedObj.Special BRANDED \"" & self.basename & "." &
        identfTxt & "Special\" OBJECT");
      Tab(wr, 2);
      PutLine(wr, "OVERRIDES");
      PutLine(wr, "write := DefaultSpWrite_" & identfTxt & ";");
      EndLine(wr, "read := DefaultSpRead_" & identfTxt & ";");
      EndLine(wr, "END;");
      EndLine(wr, "");
      
      Tab(wr, 2);
      PutLine(wr, "TYPE");
      Put(wr, identfTxt & "_Special = ");
      Tab(wr, 0);
      Tab(wr, 2);
      PutLine(wr, "Pickle.Special OBJECT");
      PutLine(wr, "mu: MUTEX;");
      PutLine(wr, "sp: " & identfTxt & "Special;");
      EndLine(wr, "registered: BOOLEAN := FALSE;");
      
      Tab(wr, 2);
      PutLine(wr, "OVERRIDES");
      PutLine(wr, "write := Write_" & identfTxt & ";");
      EndLine(wr, "read := Read_" & identfTxt & ";");
      EndLine(wr, "END;");
      EndLine(wr, "");
      
      Tab(wr, 2);
      procedureName := "DefaultSpWrite_" & identfTxt;
      Put(wr, "PROCEDURE " & procedureName & " (");
      Tab(wr, 0);
      Put(wr, "<*UNUSED*>self: " & identfTxt & "Special; ");
      Br(wr);
      IF nodata THEN
        Put(wr, "<*UNUSED*>");
      END;
      Put(wr, "shobj: SharedObj.T; ");
      Br(wr);
      IF nodata THEN
        Put(wr, "<*UNUSED*>");
      END;
      PutLine(wr, "out: Pickle.Writer)");
      EndTab(wr);
      IF nodata THEN
        PutLine(wr, " =");
      ELSE
        PutLine(wr, "RAISES {Pickle.Error, Wr.Failure, Thread.Alerted} =");
      END;
      Tab(wr, 2);
      IF NOT nodata THEN
        PutLine(wr, "VAR");
        EndLine(wr, "obj := NARROW(shobj, " & identfsTxt & ");");
        Tab(wr, 2);
      END;
      PutLine(wr, "BEGIN");
      FOR j := 0 TO LAST(t.fields^) DO
        WITH f = t.fields[j] DO
          MarshalTypedVal(wr, "obj." & Atom.ToText(f.name),
                          "PickleStubs", f.type, Direction.Out,
                          calling := TRUE, maySuppress := TRUE);
        END;
      END;
      EndTab(wr);
      Nl(wr, freshLine := FALSE);
      EndLine(wr, "END " & procedureName & ";");
      Nl(wr, freshLine := FALSE);

      Tab(wr, 2);
      procedureName := "Write_" & identfTxt;
      Put(wr, "PROCEDURE " & procedureName & " (");
      Tab(wr, 0);
      Put(wr, "<*UNUSED*>ts: " & identfTxt & "_Special; ");
      Br(wr);
      Put(wr, "ref: REFANY; ");
      Br(wr);
      PutLine(wr, "out: Pickle.Writer)");
      EndTab(wr);
      PutLine(wr, "RAISES {Pickle.Error, Wr.Failure, Thread.Alerted} =");
      Tab(wr, 2);
      PutLine(wr, "VAR");
      PutLine(wr, "obj: " & identfsTxt & ";");
      PutLine(wr, "sp: " & identfTxt & "Special;");
      EndLine(wr, "tc := TYPECODE(ref);");
      Tab(wr, 2);
      PutLine(wr, "BEGIN");

      (*
      PutLine(wr, "IO.Put(\"Pickle.Special for " & 
        self.basename & "." & identfsTxt & "\\n\");");
      *)
      Tab(wr, 2);
      PutLine(wr, "IF tc # TYPECODE(" & identfsTxt & 
        ") AND tc # TYPECODE(" & identfTxt & ") THEN");
      EndLine(wr, "RAISE Pickle.Error(\"Can't pickle subtypes of " &
        self.basename & "." & identfTxt & "\");");
      PutLine(wr, "END;");
      PutLine(wr, "obj := NARROW(ref, " & identfsTxt & ");");
      PutLine(wr, "out.writeType(tc);");
      (*
      PutLine(wr, "IO.Put(\"Pickle.Special.wr for " & 
        self.basename & "." & identfsTxt & ": writing shared obj\\n\");");
      *)
      PutLine(wr, "SharedObjStubLib.StartWritePickle(obj, out);");
      Tab(wr,2);
      PutLine(wr, "LOCK sp" & identfTxt & ".mu DO");
      EndLine(wr, "sp := sp" & identfTxt & ".sp;");
      PutLine(wr, "END;");
      PutLine(wr, "sp.write(obj, out);");

      EndLine(wr, "SharedObjStubLib.EndWritePickle(obj, out);");
      (*
      EndLine(wr, "IO.Put(\"Pickle.Special.wr for " & 
        self.basename & "." & identfsTxt & ": wrote shared obj\\n\");");
      *)
      EndLine(wr, "END " & procedureName & ";");
      Nl(wr, freshLine := FALSE);

      Tab(wr, 2);
      procedureName := "DefaultSpRead_" & identfTxt;
      Put(wr, "PROCEDURE " & procedureName & " (");
      Tab(wr, 0);
      Put(wr, "<*UNUSED*>self: " & identfTxt & "Special; ");
      Br(wr);
      IF nodata THEN
        Put(wr, "<*UNUSED*>");
      END;
      Put(wr, "shobj: SharedObj.T; ");
      Br(wr);
      IF nodata THEN
        Put(wr, "<*UNUSED*>");
      END;
      Put(wr, "in: Pickle.Reader) ");
      Br(wr);
      EndTab(wr);
      IF nodata THEN
        PutLine(wr, " =");
      ELSE
        Put(wr, "RAISES {");
        Br(wr);
        Tab(wr, 0);
        Put(wr, "Pickle.Error, ");
        Br(wr);
        Put(wr, "Rd.EndOfFile, ");
        Br(wr);
        Put(wr, "Rd.Failure, ");
        Br(wr);
        EndLine(wr, "Thread.Alerted} =");
      END;
      Tab(wr, 2);
      IF NOT nodata THEN
        PutLine(wr, "VAR");
        EndLine(wr, "obj := NARROW(shobj, " & identfsTxt & ");");
        Tab(wr, 2);
      END;
      PutLine(wr, "BEGIN");
      FOR j := 0 TO LAST(t.fields^) DO
        WITH f = t.fields[j] DO
          MarshalTypedVal(wr, "obj." & Atom.ToText(f.name),
                          "PickleStubs", f.type, Direction.In,
                          calling := TRUE, maySuppress := TRUE);
        END;
      END;
      EndTab(wr);
      Nl(wr, freshLine := FALSE);
      EndLine(wr, "END " & procedureName & ";");
      Nl(wr, freshLine := FALSE);

      Tab(wr, 2);
      procedureName := "Read_" & identfTxt;
      Put(wr, "PROCEDURE " & procedureName & " (");
      Tab(wr, 0);
      Put(wr, "<*UNUSED*>ts: " & identfTxt & "_Special; in: Pickle.Reader; ");
      Br(wr);
      Put(wr, "id: Pickle.RefID):");
      Br(wr);
      EndTab(wr);
      Put(wr, "REFANY RAISES {");
      Br(wr);
      Tab(wr, 0);
      Put(wr, "Pickle.Error, ");
      Br(wr);
      Put(wr, "Rd.EndOfFile, ");
      Br(wr);
      Put(wr, "Rd.Failure, ");
      Br(wr);
      EndLine(wr, "Thread.Alerted} =");
      Tab(wr, 2);
      PutLine(wr, "VAR");
      PutLine(wr, "space: ObjectSpace.T;");
      PutLine(wr, "obj: " & identfsTxt & ";");
      PutLine(wr, "sp: " & identfTxt & "Special;");
      PutLine(wr, "proxy: EmbProxiedObj.Proxy;");
      EndLine(wr, "tc := in.readType();");
      Tab(wr, 2);
      PutLine(wr, "BEGIN");
      Tab(wr, 2);
      PutLine(wr, "IF tc = TYPECODE(" & identfTxt & ") THEN");
      (*
      PutLine(wr, "IO.Put(\"Pickle.Special.rd for " & 
        self.basename & "." & identfsTxt & ": shared obj\\n\");");
      *)
      EndLine(wr, "obj := NEW(" & identfTxt & ");");
      Tab(wr, 2);
      PutLine(wr, "ELSIF tc = TYPECODE(" & identfsTxt & ") THEN");
      (*
      PutLine(wr, "IO.Put(\"Pickle.Special.rd for " & 
        self.basename & "." & identfsTxt & ": non-shared obj\\n\");");
      *)
      EndLine(wr, "obj := NEW(" & identfsTxt & ");");
      Tab(wr, 2);
      PutLine(wr, "ELSE");
      EndLine(wr, "RAISE Pickle.Error(\"Can't unpickle subtypes of " &
        self.basename & "." & identfTxt & "\");");
      PutLine(wr, "END;");

      (*
      PutLine(wr, "IO.Put(\"Pickle.Special.rd for " & 
        self.basename & "." & identfsTxt & ": reading space\\n\");");
      *)
      PutLine(wr, "space := in.read();");
      (*
      PutLine(wr, "IO.Put(\"Pickle.Special.rd for " & 
        self.basename & "." & identfsTxt & ": reading shared obj\\n\");");
      *)
      PutLine(wr, "SharedObjStubLib.StartReadPickle(obj, in, space);");

      Tab(wr,2);
      PutLine(wr, "LOCK sp" & identfTxt & ".mu DO");
      EndLine(wr, "sp := sp" & identfTxt & ".sp;");
      PutLine(wr, "END;");
      PutLine(wr, "sp.read(obj, in);");

      Tab(wr, 2);
      PutLine(wr, "IF tc = TYPECODE(" & identfTxt & ") THEN");
      (*
      PutLine(wr, "IO.Put(\"Pickle.Special.rd for " & 
        self.basename & "." & identfsTxt & ": setup shared obj\\n\");");
      *)
      PutLine(wr, "obj := SharedObjStubLib.SetupNewCopy(obj, in, id, space);");
      PutLine(wr, "proxy := PickleStubs.InRef(in);");
      Tab(wr, 2);
      PutLine(wr, "IF obj.proxy = NIL THEN");
      EndLine(wr, "obj.proxy := proxy;");
      PutLine(wr, "END;");
      EndLine(wr, "obj.makeProxy();");
      Tab(wr, 2);
      PutLine(wr, "ELSE");
      PutLine(wr, "obj.proxy := NIL;");
      EndLine(wr, "obj.proxy := PickleStubs.InRef(in);");
      PutLine(wr, "END;");

      (* 
      PutLine(wr, "IO.Put(\"Pickle.Special.rd for " & 
        self.basename & "." & identfsTxt & ": done\\n\");");
      *)
      EndLine(wr, "RETURN obj;");
      EndLine(wr, "END " & procedureName & ";");
      Nl(wr, freshLine := FALSE);

      Tab(wr, 2);
      procedureName := "RegisterSpecial_" & identfTxt;
      Put(wr, "PROCEDURE " & procedureName & " (");
      PutLine(wr, "sp: " & identfTxt & "Special) =");
      PutLine(wr, "<* FATAL DuplicateSpecial *>");
      Tab(wr, 2);
      PutLine(wr, "BEGIN");
      PutLine(wr, "(* we will need to NEW it here if RegisterSpecial_" &
        identfTxt);
      PutLine(wr, "   is called from " & self.basename & " *)");
      Tab(wr, 2);
      PutLine(wr, "IF sp" & identfTxt & " = NIL THEN");
      Put(wr, "sp" & identfTxt & " := NEW(");
      Tab(wr, 0);
      Put(wr, identfTxt & "_Special, ");
      Br(wr);
      Put(wr, "sc := TYPECODE(" & identfsTxt & "), ");
      Br(wr);
      Put(wr, "mu := NEW(MUTEX)");
      EndTab(wr);
      EndLine(wr, ");");
      PutLine(wr, "END;");

      Tab(wr, 2);
      PutLine(wr, "LOCK sp" & identfTxt & ".mu DO");
      Tab(wr, 2);
      PutLine(wr, "IF sp" & identfTxt & ".registered THEN");
      EndLine(wr, "RAISE DuplicateSpecial;");
      PutLine(wr, "END;");
      PutLine(wr, "sp" & identfTxt & ".registered := TRUE;");
      EndLine(wr, "sp" & identfTxt & ".sp := sp;");
      EndLine(wr, "END;");
      EndLine(wr, "END " & procedureName & ";");
      Nl(wr, freshLine := FALSE);

      Tab(wr, 2);
      PutLine(wr, "VAR");
      EndLine(wr, "sp" & identfTxt & ": " & identfTxt & "_Special := NIL;");

      (* Add to the initializers *)
      self.initializers := self.initializers & 
         "  IF sp" & identfTxt & " = NIL THEN\n    sp" & identfTxt & 
         " := NEW(" & identfTxt & "_Special,\n       sc := TYPECODE(" & 
         identfsTxt & "),\n      mu := NEW(MUTEX),\n" &
         "      sp := NEW(" & identfTxt & 
         "Special));\n  END;\n  Pickle.RegisterSpecial(sp" & identfTxt & ");\n";
    END;
  END Picklers;

PROCEDURE MarshalTypedVal (fmtWr      : Formatter.T;
                           varName    : TEXT;
                           libName    : TEXT;
                           t          : Type.T;
                           d          : Direction;
                           calling    : BOOLEAN;
                           indexDepth                 := 0;
                           maySuppress                := FALSE)
  RAISES {SOxCodeGenError.E} =
  BEGIN
    TYPECASE t OF
    | Type.Char (ch) =>
        Enumeration(fmtWr, varName, libName, ch, d, 0, ORD(LAST(CHAR)));
    | Type.UserDefined (ud) =>
        Enumeration(fmtWr, varName, libName, t, d, 0, LAST(ud.elts^));
    | Type.Subrange (sub) =>
        IF t = Type.integer THEN
          StubLibCall(fmtWr, "Integer", varName, libName, d);
        ELSE
          SubRange(fmtWr, varName, libName, t, d,
                   NARROW(sub.min, Value.Ordinal).ord,
                   NARROW(sub.max, Value.Ordinal).ord);
        END;
    | Type.Real => StubLibCall(fmtWr, "Real", varName, libName, d);
    | Type.LongReal => StubLibCall(fmtWr, "Longreal", varName, libName, d);
    | Type.Extended => StubLibCall(fmtWr, "Extended", varName, libName, d);
    | Type.Reference (r) =>
        IF Type.MayBeRefAny(r) OR NOT Type.NamedType(r) THEN
          StubLibCall(fmtWr, "Ref", varName, libName, d, ", -1");
        ELSE
          StubLibCall(fmtWr, "Ref", varName, libName, d,
                      ", TYPECODE(" & CodeForType.ToText(r) & ")");
        END;
    | Type.Array (a) =>
        IF a.index = NIL THEN
          MarshalOpenArray(fmtWr, varName, libName, t, d, calling,
                           indexDepth, maySuppress);
        ELSE
          BeginOutOnly(fmtWr, libName, t, d, maySuppress);
          Formatter.Begin(fmtWr, 2);
          PutLine(fmtWr, "FOR i" & Fmt.Int(indexDepth) & " := FIRST("
                           & CodeForType.ToText(a.index) & ") TO LAST("
                           & CodeForType.ToText(a.index) & ") DO");
          MarshalTypedVal(
            fmtWr, varName & "[i" & Fmt.Int(indexDepth) & "]", libName,
            a.element, d, calling, indexDepth + 1);
          EndLine(fmtWr, "END;");
          EndOutOnly(fmtWr, maySuppress);
        END;
    | Type.Packed (p) =>
        BeginOutOnly(fmtWr, libName, t, d, maySuppress);
        MarshalTypedVal(
          fmtWr, varName, libName, p.base, d, calling, indexDepth);
        EndOutOnly(fmtWr, maySuppress);
    | Type.Record (rec) =>
        BeginOutOnly(fmtWr, libName, t, d, maySuppress);
        FOR i := 0 TO LAST(rec.fields^) DO
          MarshalTypedVal(
            fmtWr, varName & "." & Atom.ToText(rec.fields[i].name),
            libName, rec.fields[i].type, d, calling, indexDepth);
        END;
        EndOutOnly(fmtWr, maySuppress);
    | Type.Set (s) =>
        BeginOutOnly(fmtWr, libName, t, d, maySuppress);
        IF d = Direction.In THEN
          PutLine(fmtWr, varName & ":=" & CodeForType.ToText(s) & "{};");
          Formatter.Begin(fmtWr, 2);
          PutLine(fmtWr, "FOR i" & Fmt.Int(indexDepth) & " := FIRST("
                           & CodeForType.ToText(s.range) & ") TO LAST("
                           & CodeForType.ToText(s.range) & ") DO");
          Formatter.Begin(fmtWr, 2);
          PutLine(fmtWr, "IF " & libName & ".InBoolean(in) THEN");
          PutLine(fmtWr, varName & " := " & varName & " + "
                           & CodeForType.ToText(s) & "{i"
                           & Fmt.Int(indexDepth) & "};");
          EndLine(fmtWr, "END")
        ELSE
          Formatter.Begin(fmtWr, 2);
          PutLine(fmtWr, "FOR i" & Fmt.Int(indexDepth) & " := FIRST("
                           & CodeForType.ToText(s.range) & ") TO LAST("
                           & CodeForType.ToText(s.range) & ") DO");
          PutLine(fmtWr, libName & ".OutBoolean(out, i" & Fmt.Int(indexDepth)
                           & " IN " & varName & ");");
        END;
        EndLine(fmtWr, "END;");
        EndOutOnly(fmtWr, maySuppress);
    | Type.Procedure =>
        RAISE SOxCodeGenError.E("Can't have a procedure as argument or result "
                                & "of a network object method.");
    ELSE
      RAISE SOxCodeGenError.E("Run time error -- shouldn't occur");
    END;
  END MarshalTypedVal;

PROCEDURE SubRange (fmtWr   : Formatter.T;
                    varName : TEXT;
                    libName : TEXT;
                    t       : Type.Subrange;
                    d       : Direction;
                    min, max: INTEGER        ) RAISES {SOxCodeGenError.E}=
  BEGIN
    IF t.base = Type.integer OR t.base = t THEN
      StubLibCall(fmtWr, "Integer", varName, libName, d,
                  ", " & Fmt.Int(min) & ", " & Fmt.Int(max));
    ELSE
      TYPECASE t.base OF
      | Type.Enumeration =>
          Enumeration(fmtWr, varName, libName, t.base, d, min, max);
      | Type.Subrange =>
          SubRange(fmtWr, varName, libName, t.base, d, min, max);
      ELSE
        RAISE SOxCodeGenError.E("Run time error -- shouldn't occur");
      END;
    END;
  END SubRange;

PROCEDURE Enumeration (fmtWr   : Formatter.T;
                       varName : TEXT;
                       libName : TEXT;
                       t       : Type.Enumeration;
                       d       : Direction;
                       min, max: INTEGER           ) =
  BEGIN
    IF d = Direction.In THEN
      PutLine(fmtWr, varName & " := VAL(" & libName & ".InInteger(in, "
                       & Fmt.Int(min) & "," & Fmt.Int(max) & "), "
                       & CodeForType.ToText(t) & ");");
    ELSE
      PutLine(fmtWr, libName & ".OutInteger(out, ORD(" & varName & "));");
    END;
  END Enumeration;

PROCEDURE StubLibCall (fmtWr  : Formatter.T;
                       proc   : TEXT;
                       varName: TEXT;
                       libName: TEXT;
                       d      : Direction;
                       extra                  := "") =
  BEGIN
    IF d = Direction.In THEN
      PutLine(fmtWr, varName & " := " & libName & ".In" & proc & "(in"
                       & extra & ");");
    ELSE
      PutLine(fmtWr, libName & ".Out" & proc & "(out, " & varName & ");");
    END;
  END StubLibCall;

PROCEDURE StubLibCallNoRep (fmtWr  : Formatter.T;
                            proc   : TEXT;
                            varName: TEXT;
                            libName: TEXT;
                            d      : Direction;
                            extra                  := "") =
  BEGIN
    IF d = Direction.In THEN
      PutLine(fmtWr, varName & " := " & libName & ".In" & proc & "(in"
                       & extra & ");");
    ELSE
      PutLine(fmtWr, libName & ".Out" & proc & "(out, " & varName & ");");
    END;
  END StubLibCallNoRep;

PROCEDURE BeginOutOnly (           fmtWr      : Formatter.T;
                                   libName    : TEXT;
                        <*UNUSED*> t          : Type.T;
                                   d          : Direction;
                                   maySuppress: BOOLEAN      ) =
  VAR dataPresent := TRUE;       (* Could check for size *)
  (* When recognizing pragma, dataPresent determined by
     methods[i].formals[j].outOnly *)
  BEGIN
    IF maySuppress THEN
      IF d = Direction.Out THEN
        PutLine(fmtWr, "dataPresent := " & Fmt.Bool(dataPresent) & ";");
      END;
      StubLibCallNoRep(fmtWr, "Boolean", "dataPresent", libName, d);
      Formatter.Begin(fmtWr, 2);
      PutLine(fmtWr, "IF dataPresent THEN ");
    END;
  END BeginOutOnly;

PROCEDURE EndOutOnly (fmtWr: Formatter.T; maySuppress: BOOLEAN) =
  BEGIN
    IF maySuppress THEN PutLine(fmtWr, "END;"); Formatter.End(fmtWr); END;
  END EndOutOnly;

PROCEDURE MarshalOpenArray (fmtWr      : Formatter.T;
                            varName    : TEXT;
                            libName    : TEXT;
                            a          : Type.OpenArray;
                            d          : Direction;
                            calling    : BOOLEAN;
                            indexDepth : INTEGER;
                            maySuppress: BOOLEAN         )
  RAISES {SOxCodeGenError.E} =
  VAR
    nDimensions                        := a.openDimensions;
    aName, baseName, boundList: Text.T;
    component                 : Type.T;
  BEGIN
    WITH Put    = Formatter.PutText,
         Tab    = Formatter.Begin,
         EndTab = Formatter.End      DO
      IF calling THEN            (* Must marshal/unmarshal array bounds *)
        IF d = Direction.Out THEN
          StubLibCall(
            fmtWr, "Integer", "NUMBER(" & varName & ")", libName, d);
          aName := varName & "[0";
          FOR i := 2 TO nDimensions DO
            StubLibCall(
              fmtWr, "Integer", "NUMBER(" & aName & "])", libName, d);
            aName := aName & ", 0";
          END;
          baseName := varName;
        ELSE
          Put(fmtWr, "WITH n1 = " & libName & ".InInteger(in)");
          boundList := "n1";
          FOR i := 2 TO nDimensions DO
            PutLine(fmtWr, ",");
            Put(fmtWr, "    n" & Fmt.Int(i) & " = " & libName
                         & ".InInteger(in)");
            boundList := boundList & ", n" & Fmt.Int(i);
          END;
          PutLine(fmtWr, " DO");
          PutLine(fmtWr, "  " & varName & " := NEW("
                           & CodeForType.ToText(a.refArray) & ", "
                           & boundList & ");");
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
      BeginOutOnly(fmtWr, libName, a, d, maySuppress);

      Tab(fmtWr, 2);
      PutLine(fmtWr, "FOR n1 := 0 TO LAST(" & baseName & ") DO");
      aName := varName & "[n1";
      component := a.element;
      FOR i := 2 TO nDimensions DO
        Tab(fmtWr, 2);
        PutLine(
          fmtWr, "FOR n" & Fmt.Int(i) & " := 0 TO LAST(" & aName & "]) DO");
        aName := aName & ",  n" & Fmt.Int(i);
        component := NARROW(component, Type.OpenArray).element;
      END;
      MarshalTypedVal(
        fmtWr, aName & "]", libName, component, d, calling, indexDepth);
      FOR i := 1 TO nDimensions DO
        EndTab(fmtWr);
        PutLine(fmtWr, "END;");  (* End FOR Loop *)
      END;
      EndOutOnly(fmtWr, maySuppress);
    END;
  END MarshalOpenArray;

BEGIN
END SOxModuleSOCode.
