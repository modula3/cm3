(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3DepCompile;

IMPORT
  Fmt, Err, TextExtras, Text, TextList, Pathname, Time,
  AST, M3AST_AS, ASTWalk, M3Args,
  M3CFETool, M3CImportS, M3CSpec, M3CTmpAtt,
  M3CUnit, M3CUnitRep, M3Context, M3ContextRemove, M3Conventions,
  M3FindFile, M3DirFindFile,
  M3DepDATool, M3DepFindFile, M3PathElem, M3PathElemList, M3Extension;


IMPORT M3AST_AS_F, M3AST_FE_F;


VAR
  verbose_g, filter_g, filterExact_g: BOOLEAN := FALSE;
  datool_g: M3Args.T;
  remove_notification_g: RemoveNotification := NIL;

CONST
  Changed = M3DepFindFile.Update.Changed;
  Deleted = M3DepFindFile.Update.Deleted;
  Added = M3DepFindFile.Update.Added;

TYPE
  RemoveNotification = M3Context.Closure OBJECT
    changes: ARRAY SUT OF TextList.T;
  OVERRIDES callback := Delete;
  END;

  SUT = {Int, Mod};
  Updates = ARRAY SUT OF M3DepFindFile.UpdateRec;
CONST
  Int = SUT.Int;
  Mod = SUT.Mod;

PROCEDURE CheckVerbose() RAISES {}=
  BEGIN
    verbose_g := M3Args.GetFlag(M3DepDATool.Get(), M3DepDATool.Verbose_Arg);
  END CheckVerbose;

(*PUBLIC*)
PROCEDURE Run(
    c: M3Context.T;
    prev, cur: M3DepFindFile.T;
    compile_dirs: M3PathElemList.T): INTEGER
    RAISES {}=
  VAR
    updates: Updates;
  BEGIN
    Clear(updates);
    datool_g := M3DepDATool.Get();
    IF NOT M3Args.Find(datool_g) THEN
      RETURN -1
    END;

    CheckVerbose();
    GatherUpdates(prev, cur, updates, compile_dirs);

    IF verbose_g THEN
      Err.Print("changes", Err.Severity.Comment);
      ListUnits(updates[Int], "interface");
      ListUnits(updates[Mod], "module");
    END;

    IF verbose_g THEN
      Err.Print("compiling", Err.Severity.Comment);
    END;

    RETURN FilteredCompile(c, cur, updates, CheckUpdateContext, compile_dirs);
  END Run;

PROCEDURE CheckFilterArg() RAISES {}=
  BEGIN
    filter_g := NOT M3Args.GetFlag(datool_g, M3DepDATool.NOFilterUnits_Arg);
    IF filter_g THEN
      filterExact_g := M3Args.GetFlag(datool_g,
          M3DepDATool.FilterUnitsExact_Arg);
    END;
  END CheckFilterArg;

TYPE
  UpdateContextProc = PROCEDURE(    
    context: M3Context.T; p: M3DepFindFile.T;
    updates: Updates;
    VAR (*out*) units: ARRAY SUT OF REF ARRAY OF TEXT
    ) RAISES {};

PROCEDURE FilteredCompile(
    c: M3Context.T;
    p: M3DepFindFile.T;
    VAR (*inout*) updates: Updates;
    up: UpdateContextProc;
    compile_dirs: M3PathElemList.T)
    : INTEGER
    RAISES {}=
  VAR f_updates: Updates;
    rc, trc := 0;
    units: ARRAY SUT OF REF ARRAY OF TEXT;
  BEGIN
    CheckFilterArg();
    WHILE Filter(c, p, updates, f_updates, compile_dirs) DO
      up(c, p, f_updates, units);
    
      trc := Compile(c, units);
      IF trc < 0 THEN rc := trc END;
    END;
    RETURN rc;
  END FilteredCompile;

PROCEDURE Filter(
    c: M3Context.T;
    p: M3DepFindFile.T;
    VAR (*inout*) updates: Updates;
    VAR (*out*) f_updates: Updates;
    compile_dirs: M3PathElemList.T
    ): BOOLEAN RAISES {}=
  VAR
    p_elems, elems: TextList.T;
    cu: M3AST_AS.Compilation_Unit;
    dir: M3PathElem.T;
    ext: M3Extension.T; exts: M3Extension.TSet;
  BEGIN
   IF filter_g OR filterExact_g THEN
    (* filter the set of units to those which are in the current directory
    or modules which "implement" interfaces in the context. We use a heuristic
    based on name for "implement" to avoid having to open the unit. 
    Units scheduled for compilation are taken off the 'updates' list and 
    placed on the 'f_updates' list 
    *)
    FOR sut := FIRST(SUT) TO LAST(SUT) DO
      (* Deleted units are transferred en mass to f_updates *)
      f_updates[sut, Deleted] := updates[sut, Deleted];
      updates[sut, Deleted] := NIL;

      f_updates[sut, Changed] := NIL;
      elems := updates[sut, Changed];
      (* Only keep these if we did when they were originally Added *)
      p_elems := NIL; 
      WHILE elems # NIL DO
        IF M3Context.Find(c, elems.head, UTFromSUT(sut), cu) THEN
	  f_updates[sut, Changed] := TextList.AppendD(f_updates[sut, Changed],
                                                    TextList.List1(elems.head));
          IF p_elems = NIL THEN
            updates[sut, Changed] := elems.tail
          ELSE p_elems.tail := elems.tail;
          END;
        ELSE
          IF verbose_g THEN Err.Print(Fmt.F("  filtering %s %s",
                                      M3CUnit.TypeName(UTFromSUT(sut)),
                                      elems.head),
                                      Err.Severity.Continue);
          END;
        END; (* if *)
        IF updates[sut, Changed] # elems.tail THEN p_elems := elems; END;
        elems := elems.tail;
      END;
      f_updates[sut, Added] := NIL;
      elems := updates[sut, Added];
      p_elems := NIL;
      exts := ExtsFromUt(sut);
      (* Keep these if they are local, or if they are modules and
      implement an interface *)
      WHILE elems # NIL DO
        dir := FindFromExts(p, elems.head, exts, ext);
	IF (compile_dirs = NIL OR M3PathElemList.Member(compile_dirs, dir)) OR 
           sut = Mod AND
	   ImplementsAnInterface(c, elems.head) THEN
	   f_updates[sut, Added] := TextList.AppendD(f_updates[sut, Added],
                                                   TextList.List1(elems.head));
          IF p_elems = NIL THEN
            updates[sut, Added] := elems.tail
          ELSE p_elems.tail := elems.tail;
          END;
        ELSE
          IF verbose_g THEN Err.Print(Fmt.F("  filtering %s %s",
	                                    M3CUnit.TypeName(UTFromSUT(sut)),
                                            elems.head),
                                      Err.Severity.Continue);
          END;
        END;
        IF updates[sut, Added] # elems.tail THEN p_elems := elems; END;
      	elems := elems.tail;
      END; (* while *)
    END; (* for *)
   ELSE
     FOR sut := FIRST(SUT) TO LAST(SUT) DO
       FOR a := FIRST(M3DepFindFile.Update) TO LAST(M3DepFindFile.Update) DO
         f_updates[sut, a] := updates[sut, a];
         updates[sut, a] := NIL;     	
       END; (* for *)
     END;
   END;
   RETURN NOT Empty(f_updates); 
  END Filter;


PROCEDURE Empty(updates: Updates): BOOLEAN RAISES {}=
  BEGIN
    FOR ut := FIRST(SUT) TO LAST(SUT) DO
      FOR a := FIRST(M3DepFindFile.Update) TO LAST(M3DepFindFile.Update) DO
        IF updates[ut, a] # NIL THEN RETURN FALSE END      	
      END; (* for *)
    END; (* for *)
    RETURN TRUE;
  END Empty;

PROCEDURE Clear(VAR (*inout*) updates: Updates) RAISES {}=
  BEGIN
    FOR ut := FIRST(SUT) TO LAST(SUT) DO
      FOR a := FIRST(M3DepFindFile.Update) TO LAST(M3DepFindFile.Update) DO
        updates[ut, a] := NIL;     	
      END; (* for *)
    END; (* for *)
  END Clear;

PROCEDURE ImplementsAnInterface (c: M3Context.T; name: TEXT): BOOLEAN
  RAISES {} =
  BEGIN
    FOR sut := FIRST(SUT) TO LAST(SUT) DO
      IF sut = Int THEN
        VAR
          iter := M3Context.NewIter(c, UTFromSUT(sut), FALSE);
          iname: TEXT;
          cu: M3AST_AS.Compilation_Unit;
        BEGIN
          WHILE M3Context.Next(iter, iname, cu) DO
            IF Implements(name, iname) THEN RETURN TRUE END;
          END;                  (* while *)
        END
      END;
    END;
    RETURN FALSE;
  END ImplementsAnInterface;

PROCEDURE Implements(module, interface: TEXT): BOOLEAN RAISES {}=
  VAR
    li := Text.Length(interface);
    lm := Text.Length(module);
    index: CARDINAL := 0;
  BEGIN
    (* if filterExact_g then module = interface, else
         *module = interface OR module* = interface *)
    IF filterExact_g THEN RETURN Text.Equal(module, interface)
    ELSE
      IF TextExtras.FindSub(module, interface, index) THEN
        RETURN index = 0 OR (index = lm-li);
      ELSE
      RETURN FALSE;
      END; (* if *)
    END;
  END Implements;

PROCEDURE Compile(
    c: M3Context.T;
    READONLY units: ARRAY SUT OF REF ARRAY OF TEXT;
    ): INTEGER RAISES {}=
  VAR
    phases: M3CUnit.Status;
    headerOnly: BOOLEAN;
    rc := 0;
 BEGIN
    CheckVerbose();
    <*FATAL ANY*> BEGIN
      M3Context.Apply(c, NEW(M3Context.Closure, callback := ClearPrimarySource));
    END;
    M3Args.SetStringList(M3CFETool.GetTool(), "Interfaces", units[Int]);
    M3Args.SetStringList(M3CFETool.GetTool(), "Modules", units[Mod]);
    IF M3Args.GetFlag(datool_g, M3DepDATool.CompileHeadersOnly_Arg) THEN
      phases := M3CUnit.Status{M3CUnit.State.Parsed,
	                       M3CUnit.State.ImportsResolved};
      headerOnly := TRUE;
    ELSE
      phases := M3CUnit.AllPhases;
      headerOnly := FALSE;
    END; (* if *)
    IF M3CFETool.CompileInContext(
      c,
      phases,
      headerOnly) < 0 THEN
      rc := -1;
    END;
    (* attribute seting, only needed if CompileHeadersOnly or
       semantic analysis didnt occur (which it might not if IMPORT errors) *)
    <*FATAL ANY*> BEGIN
      M3Context.Apply(c, NEW(M3Context.Closure, callback := SetTmpAttrs));
    END;
    RETURN rc;
  END Compile;

(* PUBLIC *)
PROCEDURE CompileUnits(c: M3Context.T; ut: M3CUnit.Type; 
    units: REF ARRAY OF TEXT): INTEGER RAISES {}=
  VAR
    updates: ARRAY SUT OF M3DepFindFile.UpdateRec;
    a_units: ARRAY SUT OF REF ARRAY OF TEXT;
    void: M3AST_AS.Compilation_Unit;
    sut := SUTFromUT(ut);
  BEGIN
    Clear(updates);
    FOR i := 0 TO NUMBER(units^)-1 DO
      IF M3Context.Find(c, units[i], ut, void) THEN
        updates[sut, Changed] := TextList.AppendD(updates[sut, Changed],
                                                TextList.List1(units[i]));
        ELSE
        updates[sut, Added] := TextList.AppendD(updates[sut, Added],
                                                TextList.List1(units[i]));
      END; (* if *)
    END; (* for *)
    UpdateContext(c, NIL, updates, a_units);
    RETURN Compile(c, a_units);
  END CompileUnits;

(* PUBLIC *)
PROCEDURE CompileAll(c: M3Context.T; p: M3DepFindFile.T;
                     compile_dirs: M3PathElemList.T): INTEGER RAISES {}=
  VAR
    updates: Updates;
  BEGIN
    CheckVerbose();
    (* for each unit in the set associated with 'p',
       compile it unless it is already compiled with no errors. *)
    GatherUpdates(NIL, p, updates, compile_dirs);
    CheckRemovedFromContextOrInError(c, Int,
        updates[Int, Added]);
    CheckRemovedFromContextOrInError(c, Mod,
        updates[Mod, Added]);

    RETURN FilteredCompile(c, p, updates, UpdateContext, compile_dirs);
  END CompileAll;

PROCEDURE CheckRemovedFromContextOrInError(
    c: M3Context.T; sut: SUT;
    VAR (*inout*) added: TextList.T)=
  VAR
    result: TextList.T := NIL;
    ut := UTFromSUT(sut);
  BEGIN
    WHILE added # NIL DO
      VAR cu: M3AST_AS.Compilation_Unit;
      BEGIN
        IF M3Context.Find(c, added.head, ut, cu) AND
	   cu.fe_status * M3CUnit.Errors = M3CUnit.Status{} THEN
	  (* ok *)
        ELSE
          result := TextList.AppendD(result, TextList.List1(added.head));
        END; (* if *)
      END;
      added := added.tail
    END; (* while *)
    added := result;
  END CheckRemovedFromContextOrInError;

PROCEDURE ListUnits(u: M3DepFindFile.UpdateRec; tn: TEXT) RAISES {}=
  VAR
    mu: TEXT;
    elems: TextList.T;
  BEGIN
    FOR a := FIRST(M3DepFindFile.Update) TO LAST(M3DepFindFile.Update) DO
      elems := u[a];
      WHILE elems # NIL DO
	IF a = Added THEN mu := " - added";
 	ELSIF a = Deleted THEN mu := " - deleted";
	ELSE mu := " - changed";
        END;
        Err.Print("  " & tn & " " & elems.head & mu, Err.Severity.Continue);
        elems := elems.tail;
      END; (* while *)        
    END; (* for *)
  END ListUnits;

PROCEDURE CheckUpdateContext(
    context: M3Context.T; p: M3DepFindFile.T;
    updates: Updates;
    VAR (*out*) units: ARRAY SUT OF REF ARRAY OF TEXT
    ) RAISES {}=
  BEGIN
    CheckContext(context, Int, p, updates[Int, Added]);
    CheckContext(context, Int, p, updates[Int, Changed]);
    CheckContext(context, Mod, p, updates[Mod, Added]);
    CheckContext(context, Mod, p, updates[Mod, Changed]);
    UpdateContext(context, NIL, updates, units);
  END CheckUpdateContext;

PROCEDURE UpdateContext(
    context: M3Context.T;
    <*UNUSED*> void: M3DepFindFile.T;
    updates: Updates;
    VAR (*out*) units: ARRAY SUT OF REF ARRAY OF TEXT
    ) RAISES {}=
  VAR
    cl := remove_notification_g;
    int_updates := updates[Int];
    mod_updates := updates[Mod];
  BEGIN
    cl.changes[Int] := NIL;
    cl.changes[Mod] := NIL;
    (* we want to end up with a new list to compile *)
    (* trash the deleted modules *)
    IF int_updates[Deleted] # NIL OR
       mod_updates[Deleted] # NIL THEN
      IF verbose_g THEN
        Err.Print("removing deleted units", Err.Severity.Comment);
      END;
      M3ContextRemove.Units(context,
          ArrayFromTextList(int_updates[Deleted])^,
	  M3CUnit.Type.Interface);
      M3ContextRemove.Units(context,
          ArrayFromTextList(mod_updates[Deleted])^,
	  M3CUnit.Type.Module);
    END;
    IF int_updates[Changed] # NIL OR
       mod_updates[Changed] # NIL THEN
      IF verbose_g THEN
        Err.Print("removing changed units", Err.Severity.Comment);
      END;
      M3ContextRemove.Units(context,
          ArrayFromTextList(int_updates[Changed])^,
	  M3CUnit.Type.Interface);
      M3ContextRemove.Units(context,
          ArrayFromTextList(mod_updates[Changed])^,
	  M3CUnit.Type.Module);
    END;
    MergeLists(int_updates[Added],
      cl.changes[Int]);
    MergeLists(mod_updates[Added],
      cl.changes[Mod]);
    units[Int] := ArrayFromTextList(cl.changes[Int]);
    units[Mod] := ArrayFromTextList(cl.changes[Mod]);
  END UpdateContext;

PROCEDURE CheckContext(
    c: M3Context.T; sut: SUT; p: M3DepFindFile.T; 
    VAR (*inout*) updates: TextList.T)=
  VAR p_updates: TextList.T := NIL;
      t := updates;
      ut := UTFromSUT(sut);
  BEGIN
    (* We may have explicitly compiled this already, check timestamps *)
    WHILE t # NIL DO
      VAR cu: M3AST_AS.Compilation_Unit;
      BEGIN
        IF M3Context.Find(c, t.head, ut, cu) AND
	   UidEqual(p, t.head, M3CUnit.ToType(cu.as_root), cu.fe_uid) THEN
          IF p_updates = NIL THEN updates := t.tail
          ELSE p_updates.tail := t.tail;
          END;
        END; (* if *)
      END;
      IF updates # t.tail THEN p_updates := t; END;
      t := t.tail;
    END; (* while *)
  END CheckContext;

PROCEDURE MergeLists(m: TextList.T; VAR (*inout*) l: TextList.T) RAISES {}=
  BEGIN
    WHILE m # NIL DO
      AddND(l, m.head);
      m := m.tail;
    END; (* for *)
  END MergeLists;

PROCEDURE GatherUpdates(p_old, p: M3DepFindFile.T;
                        VAR (*out*) updates: Updates;
                        compile_dirs: M3PathElemList.T)=
  VAR
    t_updates: Updates;
    dirs := p.dirs();
  BEGIN
    (* look for changed interfaces everywhere *)
    p.interfaces(p_old, updates[Int]);
    WHILE dirs # NIL DO
      Clear(t_updates);
      IF (compile_dirs = NIL) OR
         M3PathElemList.Member(compile_dirs, dirs.head) THEN
        p.modules(p_old, t_updates[Mod], dirs.head);
        FOR k := FIRST(M3DepFindFile.Update) TO LAST(M3DepFindFile.Update) DO
          JoinLists(t_updates[Mod, k], updates[Mod, k]);
        END; (* for *)
      END;
      dirs := dirs.tail;
    END;
  END GatherUpdates;

PROCEDURE JoinLists(l1: TextList.T; VAR (*inout*) l2: TextList.T) RAISES {}=
  BEGIN
    IF l1 = NIL THEN RETURN END;
    IF l2 = NIL THEN l2 := l1;
    ELSE 
      l2 := TextList.AppendD(l2, l1);
    END; (* if *)
  END JoinLists;


PROCEDURE Delete(
    cl: RemoveNotification;
    ut: M3CUnit.Type;
    name: TEXT;
    <*UNUSED*> cu: M3AST_AS.Compilation_Unit)
    RAISES {}=
  BEGIN
    IF verbose_g THEN
      Err.Print(Fmt.F("  %s %s removed from context",
          M3CUnit.TypeName(ut), name), Err.Severity.Continue);
    END;
    AddND(cl.changes[SUTFromUT(ut)], name); 
  END Delete;

PROCEDURE AddND(VAR l: TextList.T; name: TEXT) RAISES {}=
  VAR t := l;
  BEGIN
    WHILE t # NIL DO
      IF Text.Equal(t.head, name) THEN
      	RETURN
      END; (* if *)
      t := t.tail;
    END; (* while *)
    l := TextList.AppendD(l, TextList.List1(name));
  END AddND;

TYPE
  TmpAttClosure = ASTWalk.Closure OBJECT
    cu: M3AST_AS.Compilation_Unit;
  OVERRIDES
    callback := SetTmpAtt; (* new default method *)
  END;

(*PRIVATE*)
PROCEDURE ClearPrimarySource(
    <*UNUSED*> cl: M3Context.Closure;
    <*UNUSED*> ut: M3CUnit.Type;
    <*UNUSED*> name: Text.T; 
               cu: M3AST_AS.Compilation_Unit) RAISES {}=
  BEGIN
    M3CUnit.ExclState(cu.fe_status, M3Conventions.PrimarySource);
  END ClearPrimarySource;


(*PRIVATE*)
PROCEDURE SetTmpAttrs(
    <*UNUSED*> cl: M3Context.Closure;
    ut: M3CUnit.Type;
    <*UNUSED*> name: Text.T; 
    cu: M3AST_AS.Compilation_Unit) RAISES {}=
(* Do a tree walk, and call SetTmpAtt for every node. Also
   call M3CImportS.Set.
 *)
  BEGIN
    cu := M3CUnit.ToGenIns(cu, ut);
    IF cu # NIL AND NOT M3CUnit.State.SemChecked IN cu.fe_status THEN
      <*FATAL ANY*> BEGIN
        ASTWalk.VisitNodes(cu, NEW(TmpAttClosure, cu := cu));
      END;
      M3CImportS.Set(cu.as_root);
    END;
  END SetTmpAttrs;

(*PRIVATE*)
PROCEDURE SetTmpAtt(
    cl: TmpAttClosure;
    an: AST.NODE;
    <*UNUSED*> vm: ASTWalk.VisitMode) RAISES {}=
  BEGIN
    IF an = NIL THEN RETURN; END;
    M3CTmpAtt.SetTmpUnitId(an, cl.cu.as_root.as_id);
    M3CSpec.Set(an); (* sm_spec, sm_comp_unit *)
  END SetTmpAtt;

(*PRIVATE*)
PROCEDURE ArrayFromTextList(sl: TextList.T): REF ARRAY OF TEXT RAISES {} =
  VAR
    a := NEW(REF ARRAY OF TEXT, TextList.Length(sl));
  BEGIN
    FOR i := FIRST(a^) TO LAST(a^) DO
      a[i] := sl.head;
      sl := sl.tail;
    END;
    RETURN a;
  END ArrayFromTextList;

PROCEDURE UidEqual(t: M3DepFindFile.T; name: TEXT; ut: M3CUnit.Type; 
    uid: M3CUnit.Uid): BOOLEAN RAISES {}=
  <*FATAL M3FindFile.Failed*>
  VAR
    ext: M3Extension.T;
    dirElem := FindFromExts(t, name, ExtsFromUt(SUTFromUT(ut)), ext);
    fullName: TEXT := Pathname.Join(dirElem.text(),
                                    name, M3Extension.ToText(ext));
    fs: REF Time.T; 
  BEGIN
    fs := t.getProperty(name, ext);
    RETURN Text.Equal(fullName, uid.filename) AND
      fs^ = uid.stamp;
  END UidEqual;

PROCEDURE ExtsFromUt(sut: SUT): M3Extension.TSet RAISES {}=
  BEGIN
    CASE sut OF 
    | Int => RETURN M3Extension.TSet{M3Extension.T.Int, M3Extension.T.IntG};
    | Mod => RETURN M3Extension.TSet{M3Extension.T.Mod, M3Extension.T.ModG};
    END;
  END ExtsFromUt;

PROCEDURE FindFromExts(p: M3DirFindFile.Finder; name: TEXT;
                      exts: M3Extension.TSet;
                      VAR (*out*) ext: M3Extension.T): M3PathElem.T=
  BEGIN
    FOR e := FIRST(M3Extension.T) TO LAST(M3Extension.T) DO
      IF e IN exts THEN
        TRY
          ext := e;
          RETURN p.dirOf(name, ext)
        EXCEPT M3FindFile.Failed =>
        END
      END;
    END;
    RETURN NIL;
  END FindFromExts;

PROCEDURE UTFromSUT(sut: SUT): M3CUnit.Type=
  BEGIN
    IF sut = Int THEN RETURN M3CUnit.Type.Interface
    ELSE RETURN M3CUnit.Type.Module
    END
  END UTFromSUT;

PROCEDURE SUTFromUT(ut: M3CUnit.Type): SUT=
  BEGIN
    IF ut IN M3CUnit.Interfaces THEN RETURN Int
    ELSE RETURN Mod
    END
  END SUTFromUT;

BEGIN
  remove_notification_g := NEW(RemoveNotification);
  M3ContextRemove.AddNotification(remove_notification_g);
END M3DepCompile.
