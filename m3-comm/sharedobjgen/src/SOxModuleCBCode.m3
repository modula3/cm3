(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * 
 * Author          : Tobias Hoellerer (htobias)
 * Created On      : Fri Nov 10 17:37:04 EST 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Sep 25 09:09:12 1997
 * Update Count    : 44
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.5  1997/10/22 14:45:12  bm
 * Bug fix.  Naming conflicts.
 *
 * Revision 1.4  1997/08/11 20:36:37  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

MODULE SOxModuleCBCode;

IMPORT SOxCodeUtils, SOxCoder, Formatter, ImportList,
       Type, SOxCodeFiles, Wr, CodeForType, Atom, AtomList; 

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
  extraImports := 
        ARRAY [1..6] OF Atom.T{
                        Atom.FromText("SharedObjRep"),
                        Atom.FromText("SharedObjStubLib"),
                        Atom.FromText("WeakRef"),
                        Atom.FromText("WeakRefList"),
                        Atom.FromText("WeakerRef"),
                        Atom.FromText("WeakRefListFuncs")};

PROCEDURE initImports (<*UNUSED*> self    : T;
                       <*UNUSED*> basename: TEXT;
                                  imports : ImportList.T) =
  VAR
    (*
    imp := ARRAY [1 .. 1] OF
             Atom.T{Atom.FromText(SOxCodeUtils.FileName(
                                    basename, SOxCodeFiles.T.CB_I3))};
    *)
  BEGIN
    CodeForType.AugmentImportList(imports, extraImports);
    (*
    CodeForType.AugmentImportList(imports, imp);
    *)
  END initImports;

PROCEDURE import (<*UNUSED*> self    : T;
                             type    : Type.Object;
                             methods : ImportList.MethodList;
                             umethods: AtomList.T;
                             imports : ImportList.T           ) =
  BEGIN
    CodeForType.ImportLst(type, imports, methods, umethods);
    CodeForType.ImportCBLst(type, imports, methods, umethods);
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
        SOxCodeUtils.FileName(basename, SOxCodeFiles.T.CB_M3);

      Put(wr, "MODULE " & self.fbasename & " EXPORTS ");
      Tab(wr, 0);
      EndLine(wr, self.fbasename & ", "
                    & SOxCodeUtils.FileName(
                        basename, SOxCodeFiles.T.CBPRX_I3) & ";");

      CodeForType.ProduceImports(wr, imports);

      Nl(wr, freshLine := FALSE);
    END;
  END head;

PROCEDURE decls (<*UNUSED*> self    : T;
                            wr      : Formatter.T;
                            typeID  : Type.Qid;
                 <*UNUSED*> stypeID : Type.Qid;  
                 <*UNUSED*> implName: TEXT;
                            methods : ImportList.MethodList;
                            umethods: AtomList.T             ) =
  BEGIN
    VAR
      typTxt   : TEXT;
      identfTxt: TEXT;
      meth     : Atom.T;
    BEGIN
      WITH Put    = Formatter.PutText,
           Nl     = Formatter.NewLine,
           Tab    = Formatter.Begin,
           Grp    = Formatter.Group,
           EndTab = Formatter.End      DO
        typTxt := CodeForType.QidToText(typeID);
        identfTxt := CodeForType.QidToIdentf(typeID);

        Tab(wr, 2);
        Put(wr, "REVEAL");
        Nl(wr, freshLine := FALSE);

        Tab(wr, 2);
        PutLine(
          wr, identfTxt & " = Public" & identfTxt & " BRANDED OBJECT");
        PutLine(wr, "obj: " & typTxt & ";");
        EndLine(wr, "wref: WeakerRef.T;");

        Tab(wr, 2);
        PutLine(wr, "OVERRIDES");

        PutLine(wr, "init := Init_" & identfTxt & ";");
        PutLine(wr, "cancel := Cancel_" & identfTxt & ";");
        PutLine(wr, "pre_anyChange := Pre_anyChange_" & identfTxt & ";");
        Put(wr, "post_anyChange := Post_anyChange_" & identfTxt & ";");
        FOR i := 0 TO LAST(methods^) DO
          meth := methods[i].name;
          IF AtomList.Member(umethods, meth) THEN
            Nl(wr, freshLine := FALSE);
            Grp(wr);
            Put(wr, "pre_" & Atom.ToText(meth) & " := ");
            Put(wr, "Pre_" & Atom.ToText(meth) & "_" & identfTxt & ";");
            EndTab(wr);

            Nl(wr, freshLine := FALSE);
            Grp(wr);
            Put(wr, "post_" & Atom.ToText(meth) & " := Post_");
            Put(wr, Atom.ToText(meth) & "_" & identfTxt & ";");
            EndTab(wr);
          END;
        END;
        EndTab(wr);
        Nl(wr, freshLine := FALSE);
        PutLine(wr, "END;");

        EndTab(wr);
        Nl(wr, freshLine := FALSE);
      END;
    END;
  END decls;


PROCEDURE main (           self    : T;
                           wr      : Formatter.T;
                           typeID  : Type.Qid;
                <*UNUSED*> type: Type.Object;  
                <*UNUSED*> stypeID : Type.Qid;  
                <*UNUSED*> implName: TEXT;
                           methods : ImportList.MethodList;
                           umethods: AtomList.T             ) =
  VAR
    typTxt   : TEXT;
    identfTxt: TEXT;
    meth     : Atom.T;
  BEGIN
    WITH Put    = Formatter.PutText,
         Nl     = Formatter.NewLine,
         Br     = Formatter.Break,
         Tab    = Formatter.Begin,
         EndTab = Formatter.End      DO
      typTxt := CodeForType.QidToText(typeID);
      identfTxt := CodeForType.QidToIdentf(typeID);

      Tab(wr, 2);
      Put(wr, "PROCEDURE Init_" & identfTxt & " (");
      Tab(wr, 0);
      Put(wr, "self: " & identfTxt & "; ");
      Br(wr);
      EndLine(wr, "obj: " & typTxt & "): " & identfTxt & " =");

      Tab(wr, 2);
      PutLine(wr, "VAR");
      Put(wr, "wref := NEW(");
      Tab(wr, 0);
      Put(wr, "WeakerRef.T, ");
      Br(wr);
      Put(wr, "weakRef := WeakRef.FromRef(self, ");
      Put(wr, "Cleanup_" & identfTxt & "_CB), ");
      Br(wr);
      Put(wr, "ready := TRUE);");
      EndTab(wr);
      EndTab(wr);
      Nl(wr, freshLine := FALSE);

      Tab(wr, 2);
      PutLine(wr, "BEGIN");

      PutLine(wr, "self.obj := obj;");
      PutLine(wr, "self.wref := wref;");

      Tab(wr, 2);
      Put(wr, "IF ");
      Tab(wr, 0);
      Put(wr, "MkProxy" & identfTxt & "CB # NIL AND self.proxy = NIL ");
      EndTab(wr);
      PutLine(wr, "THEN");
      EndLine(wr, "MkProxy" & identfTxt & "CB (self);");
      PutLine(wr, "END;");
      PutLine(wr, "SharedObjStubLib.AcquireWriteLock(obj);");

      Tab(wr, 2);
      PutLine(wr, "TRY");
      EndLine(
        wr, "obj.callbacks := WeakRefList.Cons(wref, obj.callbacks);");

      Tab(wr, 2);
      PutLine(wr, "FINALLY");
      EndLine(wr, "SharedObjStubLib.ReleaseWriteLock(obj);");
      PutLine(wr, "END;");

      EndLine(wr, "RETURN self;");
      EndLine(wr, "END Init_" & identfTxt & ";");

      Nl(wr, freshLine := FALSE);

      Tab(wr, 2);
      Put(wr, "PROCEDURE Cancel_" & identfTxt & " (");
      Tab(wr, 0);
      EndLine(wr, "self: " & identfTxt & ") =");

      Tab(wr, 2);
      PutLine(wr, "BEGIN");
      PutLine(wr, "SharedObjStubLib.AcquireWriteLock(self.obj);");

      Tab(wr, 2);
      PutLine(wr, "TRY");
      EndLine(wr, "EVAL WeakRefListFuncs.DeleteD(self.obj.callbacks, " &
        "self.wref);");

      Tab(wr, 2);
      PutLine(wr, "FINALLY");
      EndLine(wr, "SharedObjStubLib.ReleaseWriteLock(self.obj);");
      EndLine(wr, "END;");
      EndLine(wr, "END " & "Cancel_" & identfTxt & ";");
      Nl(wr, freshLine := FALSE);

      Tab(wr, 2);
      Put(wr, "PROCEDURE Cleanup_" & identfTxt & "_CB (");
      Tab(wr, 0);
      EndLine(wr, "READONLY wref: WeakRef.T; ref: REFANY) =");

      Tab(wr, 2);
      PutLine(wr, "VAR");
      PutLine(wr, "cb := NARROW(ref, " & identfTxt & ");");
      EndLine(wr, "weakerRef := NEW(WeakerRef.T, weakRef := wref);");

      Tab(wr, 2);
      PutLine(wr, "BEGIN");
      PutLine(wr, "SharedObjStubLib.AcquireWriteLock(cb.obj);");

      Tab(wr, 2);
      PutLine(wr, "TRY");
      PutLine(wr, "(* Callback is gone, so delete it *)");
      EndLine(
        wr, "EVAL WeakRefListFuncs.DeleteD(cb.obj.callbacks, weakerRef);");

      Tab(wr, 2);
      PutLine(wr, "FINALLY");
      EndLine(wr, "SharedObjStubLib.ReleaseWriteLock(cb.obj);");
      EndLine(wr, "END;");
      EndLine(wr, "END " & "Cleanup_" & identfTxt & "_CB;");
      Nl(wr, freshLine := FALSE);

      Tab(wr, 2);
      Put(wr, "PROCEDURE Pre_anyChange" & "_" & identfTxt & " (");
      Tab(wr, 0);
      EndLine(
        wr, "self: " & identfTxt & "; READONLY obj: " & typTxt & ") =");

      Tab(wr, 2);
      PutLine(wr, "BEGIN");
      PutLine(wr, "(* Default calls proxy or does nothing. *)");

      Tab(wr, 2);
      PutLine(wr, "IF self.proxy # NIL THEN");
      Put(wr, "NARROW (self.proxy, CBProxy" & identfTxt);
      EndLine(wr, ").pre_anyChange (obj);");
      EndLine(wr, "END;");
      EndLine(wr, "END Pre_anyChange" & "_" & identfTxt & ";");

      Nl(wr, freshLine := FALSE);
      Tab(wr, 2);
      Put(wr, "PROCEDURE Post_anyChange" & "_" & identfTxt & " (");
      Tab(wr, 0);
      EndLine(
        wr, "self: " & identfTxt & "; READONLY obj: " & typTxt & ") =");

      Tab(wr, 2);
      PutLine(wr, "BEGIN");
      PutLine(wr, "(* Default calls proxy or does nothing. *)");

      Tab(wr, 2);
      PutLine(wr, "IF self.proxy # NIL THEN");
      Put(wr, "NARROW (self.proxy, CBProxy" & identfTxt);
      EndLine(wr, ").post_anyChange (obj);");
      EndLine(wr, "END;");
      EndLine(wr, "END Post_anyChange" & "_" & identfTxt & ";");

      FOR i := 0 TO LAST(methods^) DO
        meth := methods[i].name;
        IF AtomList.Member(umethods, meth) THEN

          Nl(wr, freshLine := FALSE);
          Tab(wr, 2);
          Put(
            wr,
            "PROCEDURE Pre_" & Atom.ToText(meth) & "_" & identfTxt & " (");
          Tab(wr, 0);
          Put(wr, "self: " & identfTxt & "; READONLY obj: " & typTxt);
          CodeForType.PrintSig(wr, methods[i].sig);
          EndLine(wr, "): BOOLEAN =");

          Tab(wr, 2);
          PutLine(wr, "BEGIN");
          PutLine(wr, "(* Default calls proxy or does nothing. *)");

          Tab(wr, 2);
          PutLine(wr, "IF self.proxy # NIL THEN");
          Put(wr, "RETURN NARROW (self.proxy, CBProxy" & identfTxt);
          Put(wr, ").pre_" & Atom.ToText(meth) & " (");
          Tab(wr, 0);
          Put(wr, "obj");
          CodeForType.PrintArgs(wr, methods[i].sig);
          EndLine(wr, ");");
          EndTab(wr);
          PutLine(wr, "END;");
          EndLine(wr, "RETURN FALSE;");
          EndLine(
            wr, "END Pre_" & Atom.ToText(meth) & "_" & identfTxt & ";");

          Nl(wr, freshLine := FALSE);
          Tab(wr, 2);
          Put(wr, "PROCEDURE Post_" & Atom.ToText(meth) & "_" & identfTxt
                    & " (");
          Tab(wr, 0);
          Put(wr, "self: " & identfTxt & "; READONLY obj: " & typTxt);
          CodeForType.PrintSig(wr, methods[i].sig);
          EndLine(wr, "): BOOLEAN =");

          Tab(wr, 2);
          PutLine(wr, "BEGIN");
          PutLine(wr, "(* Default calls proxy or does nothing. *)");

          Tab(wr, 2);
          PutLine(wr, "IF self.proxy # NIL THEN");
          Put(wr, "RETURN NARROW (self.proxy, CBProxy" & identfTxt);
          Put(wr, ").post_" & Atom.ToText(meth) & " (");
          Tab(wr, 0);
          Put(wr, "obj");
          CodeForType.PrintArgs(wr, methods[i].sig);
          EndLine(wr, ");");
          EndTab(wr);
          PutLine(wr, "END;");
          EndLine(wr, "RETURN FALSE;");
          EndLine(
            wr, "END Post_" & Atom.ToText(meth) & "_" & identfTxt & ";");
        END;
      END;

      (* Add to the initializers *)
      self.initializers := self.initializers & 
        "SharedObjStubLib.InhibitTransmission(TYPECODE(" & identfTxt & 
        "), \"default " & identfTxt & 
        " callback cannot be transmitted/duplicated\");\n";
    END;
  END main;

PROCEDURE bottom (self: T; wr: Formatter.T; <*UNUSED*> fname: TEXT) =
  BEGIN
    WITH Put = Formatter.PutText, Nl = Formatter.NewLine DO
      Nl(wr, freshLine := FALSE);
      PutLine(wr, "BEGIN");
      Put(wr, self.initializers);
      PutLine(wr, "END " & self.fbasename & ".");
    END;
  END bottom;
  
BEGIN
END SOxModuleCBCode.
