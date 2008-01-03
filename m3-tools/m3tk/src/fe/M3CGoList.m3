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
(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3CGoList;

IMPORT Text, Rd, Err, RefList, TextList, Fmt, ASCII, OSError, Thread;
IMPORT M3AST_AS, M3AST_SC;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_SC_F;

IMPORT SeqM3AST_SC_Unit_stub;

IMPORT M3Error, M3Time, M3CId, M3FindFile;
IMPORT M3Context, M3ContextRemove, M3CUnit, M3CGo;
IMPORT M3CMkStd, M3CScope;
IMPORT M3Conventions, M3Extension;
IMPORT M3CConcTypeSpec;
FROM M3Conventions IMPORT IsStandard, ModuleName, Standard, 
    PrimarySource;
IMPORT M3ASTPickle, Pickle;

TYPE
  ContextData = BRANDED REF RECORD
    setCompTime: BOOLEAN := FALSE;
    compiledStandard: BOOLEAN := FALSE;
    compilees: CompileeArray;
    phases := M3CUnit.Status{};
    headerOnly, setPrimarySource: BOOLEAN := FALSE;
    en: ErrorNotification := NIL;
  END;

  Compilee = {Int, PnInt, Mod, PnMod};
  CompileeArray = ARRAY Compilee OF REF ARRAY OF TEXT;

(***************************************************************************)
(*                            Notify support                               *)
(***************************************************************************)

REVEAL Notification = Notification_public BRANDED OBJECT END;

TYPE NotificationElem = REF RECORD e: Notification END;

VAR notifications_g: RefList.T := NIL;
    notificationsMutex_g := NEW(MUTEX);

PROCEDURE AddNotification(e: Notification) RAISES {}=
  BEGIN
    LOCK notificationsMutex_g DO
      notifications_g := RefList.AppendD(notifications_g,
                             RefList.List1(NEW(NotificationElem, e := e)));
    END; (* lock *)
  END AddNotification;

PROCEDURE RemoveNotification(e: Notification) RAISES {}=
  VAR l: RefList.T := notifications_g; prev: RefList.T := NIL;
  BEGIN
    LOCK notificationsMutex_g DO
      WHILE l # NIL DO
        IF NARROW(l.head, NotificationElem).e = e THEN
          IF prev = NIL THEN notifications_g := l.tail
          ELSE prev.tail := l.tail;
          END;
          RETURN
        END;
        prev := l; l := l.tail;
      END;
    END; (* lock *)
  END RemoveNotification; 

PROCEDURE Notify(
      context: M3Context.T;
      nm: NotifyMode; 
      name: TEXT;
      ut: M3CUnit.Type;
      uf: M3CUnit.Form;
      cu: M3AST_AS.Compilation_Unit;
      compTime: M3Conventions.CompTime := NIL
      ) RAISES {}=
  BEGIN
    LOCK notificationsMutex_g DO
      VAR list: RefList.T := notifications_g;
      BEGIN
        WHILE list # NIL DO
          NARROW(list.head, NotificationElem).e.notify(context, nm, name,
                                                       ut, uf, cu, compTime);
          list := list.tail;
        END;
      END;
    END;
  END Notify;

(***************************************************************************)
(*                            Error handling                               *)
(***************************************************************************)

TYPE 
  ErrorNotification = M3Error.Notification OBJECT
    cd: ContextData;
    errCuList: RefList.T; (* OF ErrCu *)
  OVERRIDES
    notify := ErrorObserver;
  END;

  ErrCu = REF RECORD
    cu: M3AST_AS.Compilation_Unit;
  END;

PROCEDURE InitErrorObserver(
    en: ErrorNotification;
    cu: M3AST_AS.Compilation_Unit;
    ): RefList.T RAISES {} =
  VAR
    sErrCuList: RefList.T;
  BEGIN
    sErrCuList := en.errCuList;
    en.errCuList := NIL;    
    M3Error.SetCu(cu);
    RETURN sErrCuList;
  END InitErrorObserver;

PROCEDURE ErrorObserver(en: ErrorNotification;
    cu: M3AST_AS.Compilation_Unit; serious: BOOLEAN) RAISES {} =
  VAR 
    t: RefList.T;
    current: M3AST_AS.Compilation_Unit;
  CONST
    SErrors = M3CUnit.Status{M3CUnit.State.SErrors};
  BEGIN
    IF serious THEN
      cu.fe_status := cu.fe_status + SErrors;
      current := Current();
      IF current # NIL THEN
        current.fe_status := current.fe_status + SErrors;
      END;
      en.cd.phases := en.cd.phases + SErrors;
    END;
    t := en.errCuList;
    WHILE t # NIL DO
      IF NARROW(t.head, ErrCu).cu = cu THEN RETURN
      ELSE t := t.tail;
      END;
    END; (* while *)
    en.errCuList := RefList.AppendD(en.errCuList,
                                    RefList.List1(NEW(ErrCu, cu := cu)));
  END ErrorObserver;

PROCEDURE ShowErrors(en: ErrorNotification; sErrCuList: RefList.T) RAISES {} =
  VAR
    t: RefList.T;
    ecu: ErrCu;
  BEGIN
    t := en.errCuList;
    WHILE t # NIL DO
      ecu := NARROW(t.head, ErrCu);
      M3Error.SetCu(ecu.cu);
      M3Error.ShowAll(ecu.cu.as_root);
      t := t.tail;
    END; (* while *)
    en.errCuList := sErrCuList;
    M3Error.SetCu(M3CGo.Current());
  END ShowErrors;

PROCEDURE Current(): M3AST_AS.Compilation_Unit RAISES {} =
  BEGIN
    RETURN M3CGo.Current();
  END Current;

(*
TYPE
  TmpAttClosure = ASTWalk.Closure OBJECT id: M3AST_AS.UNIT_ID END;
*)

PROCEDURE Unpickle(context: M3Context.T; rd: Rd.T; pn: TEXT;
    ): M3AST_AS.Compilation_Unit RAISES {} =
  <*FATAL Thread.Alerted*>
  VAR
    icu, cu: M3AST_AS.Compilation_Unit := NIL;
    usb: M3AST_SC.Unit_stub;
    iter: SeqM3AST_SC_Unit_stub.Iter;
  BEGIN
    TRY
      M3ASTPickle.Read(context, rd, CheckFindUnpickleOrCompileSource, cu);
      Rd.Close(rd);

      iter := SeqM3AST_SC_Unit_stub.NewIter(cu.sc_unit_stub_s);
      WHILE SeqM3AST_SC_Unit_stub.Next(iter, usb) DO
        IF M3Context.FindFromId(context, usb.sc_unit_symrep, 
                                usb.sc_unit_type, icu) THEN
          IF NOT M3CUnit.Equal(usb.sc_unit_uid, icu.fe_uid) THEN
            WITH gcu = Current() DO
              M3Error.ReportWithId(gcu.as_root, 
                  "version conflict between interfaces '%s' and '%s'", 
                  cu.as_root.as_id.lx_symrep,
                  usb.sc_unit_symrep);
              M3CUnit.InclState(gcu.fe_status, M3CUnit.State.IErrors);
            END
          END
        END; (* if *)
      END; (* while *)
      (* temporary attributes are pickled for now 
      cl := NEW(TmpAttClosure, callback := TmpAttSet, id := cu.as_root.as_id);
      ASTWalk.VisitNodes(cu, cl); *)
    EXCEPT
    | Rd.Failure, Pickle.Error =>
        Err.Print(Fmt.F("unpickling from %s failed", pn), 
            Err.Severity.Error);
        cu := NIL;
    END;
    RETURN cu;
  END Unpickle;

(*********************************************
PROCEDURE TmpAttSet(cl: TmpAttClosure; n: AST.NODE; 
    <*UNUSED*>vm: ASTWalk.VisitMode) RAISES {}=
  BEGIN
    M3CTmpAtt.Set(n, cl.id);
  END TmpAttSet;
***********************************************)

PROCEDURE CheckFindUnpickleOrCompileSource(
    name: TEXT;
    unitType: M3CUnit.Type;
    sourceId: M3CUnit.Uid;
    context: M3Context.T;
    VAR (*out*) cu: M3AST_AS.Compilation_Unit): BOOLEAN RAISES {} =
  BEGIN
    IF FindUnpickleOrCompileSource(name, unitType, context, cu) THEN
      RETURN M3CUnit.Equal(sourceId, cu.fe_uid);
    ELSE
      M3CUnit.InclState(Current().fe_status, M3CUnit.State.IErrors);
      RETURN FALSE
    END;
  END CheckFindUnpickleOrCompileSource;

PROCEDURE FindUnpickleOrCompileSourceWithUF(
    name: TEXT;
    unitType: M3CUnit.Type;
    context: M3Context.T;
    VAR (*inout*) uf: M3CUnit.Form;
    VAR (*out*) cu: M3AST_AS.Compilation_Unit): BOOLEAN
    RAISES {} =
  BEGIN
    (* First see if its already in this context *)
    IF M3Context.Find(context, ModuleName(name), unitType, cu) THEN END;

    IF cu = NIL THEN
      IF UnpickleOrCompileSource(name, unitType, context, uf, cu) THEN END;
    END; (* if *)
    RETURN (cu # NIL) AND (cu.as_root # NIL);
  END FindUnpickleOrCompileSourceWithUF;

PROCEDURE FindUnpickleOrCompileSource(
    name: TEXT;
    unitType: M3CUnit.Type;
    context: M3Context.T;
    VAR (*out*) cu: M3AST_AS.Compilation_Unit): BOOLEAN
    RAISES {} =
  VAR
    uf: M3CUnit.Form;
    res: BOOLEAN;
  BEGIN
    uf := M3CUnit.Form.DontCare;
    res := FindUnpickleOrCompileSourceWithUF(
        name, unitType, context, uf, cu);
    IF res THEN
      TYPECASE cu.as_root OF
      | M3AST_AS.UNIT_GEN_INS(unit_ins) =>
          IF unit_ins.sm_ins_comp_unit # NIL THEN
            cu := unit_ins.sm_ins_comp_unit;
          ELSE
            res := FALSE;
            cu := NIL;
          END;
      ELSE
      END; (* typecase *)
    END;
    RETURN res;
  END FindUnpickleOrCompileSource;

PROCEDURE FindOrCompileSource(
    name: TEXT;
    unitType: M3CUnit.Type;
    context: M3Context.T;
    VAR (*out*) cu: M3AST_AS.Compilation_Unit): BOOLEAN 
    RAISES {} =
  VAR
    uf: M3CUnit.Form;
  BEGIN
    uf := M3CUnit.Form.Source;
    RETURN FindUnpickleOrCompileSourceWithUF(
        name, unitType, context, uf, cu);
  END FindOrCompileSource;

PROCEDURE UnpickleOrCompileSource(
    name: TEXT;
    unitType: M3CUnit.Type;
    context: M3Context.T;
    VAR (*inout*) uf: M3CUnit.Form;
    VAR (*out*) cu: M3AST_AS.Compilation_Unit): BOOLEAN
    RAISES {} =
  VAR
    isStandard: BOOLEAN;
    textForm: TEXT;
    stream: Rd.T;
    sourceId: M3CUnit.Uid;
    saveErrCuList: RefList.T;
    startTime: M3Time.T;
    compTime: M3Conventions.CompTime;
    openError := FALSE;
    cd: ContextData := GetData(context);

  PROCEDURE AddCu() RAISES {}=
    BEGIN
      IF isStandard THEN
        M3Context.SetStandard(cu);
      ELSE
        WITH m = ModuleName(name) DO
          CheckedAdd(context, m, unitType, cu);
        END;
      END; (* if *)
    END AddCu;

  BEGIN
    cu := NIL;
    isStandard := IsStandard(name);
    IF cd.setCompTime THEN compTime := NEW(M3Conventions.CompTime).init()
    ELSE compTime := NIL;
    END;

    IF cd.setCompTime THEN startTime := M3Time.Now() END;
    TRY
      VAR finder := context.getSub(TYPECODE(M3FindFile.T));
      BEGIN
        IF isStandard THEN
          <*FATAL M3FindFile.Failed*> BEGIN
            stream := M3CUnit.FindStandard(finder, uf, sourceId);
          END;
        ELSE
          stream := M3CUnit.FindUnit(finder, name, unitType, uf, sourceId);
        END;
      END
    EXCEPT
    | OSError.E =>
        openError := TRUE; stream := NIL;
    END;
    IF cd.setCompTime THEN compTime.open := M3Time.Interval(startTime) END;

    IF stream = NIL THEN
      VAR
        prefix: TEXT := "failed to ";
      BEGIN
        IF openError THEN prefix := prefix & "open";
        ELSE prefix := prefix & "find";
        END;
        IF isStandard THEN
          Err.Print(Fmt.F("%s standard interface", prefix), Err.Severity.Fatal);
        ELSE
          IF uf = M3CUnit.Form.Source THEN textForm := "source" 
          ELSIF uf = M3CUnit.Form.DontCare THEN textForm := "source or AST"
          ELSE textForm := "AST";
          END; (* if *)
          Err.Print(Fmt.F("%s %s for %s \'%s\'", prefix,
                        textForm, M3CUnit.TypeName(unitType), name), 
                  Err.Severity.Error);
        END; (* if *)
      END; (* begin *)
    ELSE
      IF uf = M3CUnit.Form.Source THEN 
        cu := NEW(M3AST_AS.Compilation_Unit).init();
        cu.fe_uid := sourceId;
        AddCu();
        CheckPrimarySource(unitType, name, cu, cd);
      END;

      Notify(context, NotifyMode.Before, name, unitType, uf, cu, compTime);

      IF uf = M3CUnit.Form.Source THEN        (* needs compiling from source *)
        saveErrCuList := InitErrorObserver(cd.en, cu);
        M3CGo.CompileUnit(cu, context, stream, FindUnpickleOrCompileSource,
            cd.phases, compTime, cd.headerOnly);
        <*FATAL Rd.Failure, Thread.Alerted*> BEGIN
          Rd.Close(stream);
        END;
        CheckUnitNameAndType(context, unitType, name, cu);
        ShowErrors(cd.en, saveErrCuList);
      ELSE
        IF cd.setCompTime THEN startTime := M3Time.Now() END;
        cu := Unpickle(context, stream, M3CUnit.TextName(sourceId));
        IF cd.setCompTime THEN compTime.parse := M3Time.Interval(startTime) END;
        AddCu();
      END; (* if *)

      Notify(context, NotifyMode.After, name, unitType, uf, cu, compTime);
    END; (* if *)

    RETURN cu # NIL;
  END UnpickleOrCompileSource;

PROCEDURE CompileStandard(context: M3Context.T) RAISES {} =
  VAR
    cu: M3AST_AS.Compilation_Unit;
    cd := GetData(context);
  BEGIN
   IF NOT cd.compiledStandard THEN
    (* Always compile the Standard identifiers, and introduce them into
    the scope permanently for all subsequent compilations. *)
    IF FindUnpickleOrCompileSource(Standard, M3CUnit.Type.Interface, context, cu) THEN
      M3Context.SetStandard(cu);
      (* If we actually compiled standard, this wont do anything.
         If we unpickled, it adds them to the scope.
      *)
      IF M3CUnit.State.SemChecked IN cd.phases THEN
        M3CScope.Standard(cu);
        M3CMkStd.RegisterBuiltIns(cu);
      END; (* if *)
      cd.compiledStandard := TRUE;
    END   
   END   
  END CompileStandard;

PROCEDURE CompileModulesOrInterfaces(
    units: REF ARRAY OF TEXT;
    unitType: M3CUnit.Type;
    context: M3Context.T
    ) RAISES {} =
  VAR
    cu: M3AST_AS.Compilation_Unit;
    cd := GetData(context);
  BEGIN
    IF NOT ParseOnly(cd.phases) THEN CompileStandard(context); END;
    FOR i := 0 TO NUMBER(units^)-1 DO
      IF FindOrCompileSource(units[i], unitType, context, cu) THEN 
      END;
    END; (* for *)
  END CompileModulesOrInterfaces;

PROCEDURE CompileUnitsInContext(
    VAR (*inout*) context: M3Context.T;
    READONLY interfaces, modules, pathNames: ARRAY OF TEXT;
    VAR (*inout*) phases: M3CUnit.Status;
    headerOnly := FALSE;
    setPrimarySource := TRUE;
    setCompTime: BOOLEAN := FALSE;
    ) RAISES {}=
  VAR
    compilees: CompileeArray;
    cd: ContextData;
  BEGIN
    IF context = NIL THEN
      context := M3Context.New();
    END;
    cd := GetData(context);
    M3Error.AddNotification(cd.en);
    cd.phases := phases; (* accumulate status in the global *)
    cd.headerOnly := headerOnly;
    cd.setPrimarySource := setPrimarySource;
    cd.setCompTime := setCompTime;
    CheckGarbageAndExtensions(interfaces, modules, pathNames, compilees);
    FOR i := FIRST(Compilee) TO LAST(Compilee) DO
      cd.compilees[i] := Remember(compilees[i]);
      M3ContextRemove.Units(context, compilees[i]^, ToUnitType(i));
    END;
    M3CConcTypeSpec.Validate(context);
    (* Now compile the requested units *)
    FOR i := FIRST(Compilee) TO LAST(Compilee) DO
      CompileModulesOrInterfaces(compilees[i], ToUnitType(i), context);
    END;
    phases := cd.phases;
    M3Error.RemoveNotification(cd.en); 
  END CompileUnitsInContext;

PROCEDURE ToUnitType(ce: Compilee): M3CUnit.Type=
  BEGIN
    IF ce = Compilee.Int OR ce = Compilee.PnInt THEN
      RETURN M3CUnit.Type.Interface
    ELSE
      RETURN M3CUnit.Type.Module
    END;
  END ToUnitType;
 
PROCEDURE CheckGarbageAndExtensions(
    READONLY interfaces, modules, pathNames: ARRAY OF TEXT;
    VAR (*out*) compilees: CompileeArray)=
  VAR
    n_interfaces, n_modules, n_pn_interfaces, n_pn_modules: INTEGER := 0;
    t: M3Extension.T;
    interfaces_l, modules_l, pn_interfaces_l, pn_modules_l: TextList.T;
  BEGIN
    FOR i := 0 TO NUMBER(interfaces)-1 DO
      IF GarbageModuleName(interfaces[i]) THEN
      ELSE
        interfaces_l := TextList.AppendD(interfaces_l,
                                        TextList.List1(interfaces[i]));
	INC(n_interfaces);
      END;
    END; (* for *)
    compilees[Compilee.Int] := ArrayFromList(interfaces_l);

    FOR i := 0 TO NUMBER(modules)-1 DO
      IF GarbageModuleName(modules[i]) THEN
      ELSE
        modules_l := TextList.AppendD(modules_l,
                                        TextList.List1(modules[i]));
	INC(n_modules);
      END;
    END; (* for *)
    compilees[Compilee.Mod] := ArrayFromList(modules_l);

    (* Now check the pathnames, splitting them into interfaces
       and modules, based on their extensions. *)
    FOR i := 0 TO NUMBER(pathNames)-1 DO
      IF GarbageModuleName(ModuleName(pathNames[i])) THEN
        (* ignore *)
      ELSIF M3Extension.Has(pathNames[i], t) AND t IN M3Extension.Ints THEN
        pn_interfaces_l := TextList.AppendD(pn_interfaces_l,
                      TextList.List1(pathNames[i]));
	INC(n_pn_interfaces);
      ELSIF M3Extension.Has(pathNames[i], t) AND t IN M3Extension.Mods THEN
        pn_modules_l := TextList.AppendD(pn_modules_l,
                      TextList.List1(pathNames[i]));
	INC(n_pn_modules);
      END;
    END;
    compilees[Compilee.PnInt] := ArrayFromList(pn_interfaces_l);
    compilees[Compilee.PnMod] := ArrayFromList(pn_modules_l);
    
  END CheckGarbageAndExtensions;

(*PRIVATE*)
PROCEDURE ArrayFromList(sl: TextList.T): REF ARRAY OF TEXT RAISES {} =
  VAR
    a := NEW(REF ARRAY OF TEXT, TextList.Length(sl));
  BEGIN
    FOR i := FIRST(a^) TO LAST(a^) DO
      a[i] := sl.head;
      sl := sl.tail;
    END;
    RETURN a;
  END ArrayFromList;

PROCEDURE GarbageModuleName(name: TEXT): BOOLEAN=
  BEGIN
    FOR i := 0 TO Text.Length(name)-1 DO
      VAR ch := ASCII.Upper[Text.GetChar(name, i)];
      BEGIN      
        IF ch >= 'A' AND ch <= 'Z' OR
           (i >= 1 AND
             (ch >= '0' AND ch <= '9' OR
             ch = '_')) THEN
          (* ok *)
        ELSE 
          Err.Print(Fmt.F("illegal unit name '%s'- ignored", 
                          name), Err.Severity.Warning);
          RETURN TRUE
        END;
      END;
    END;
    RETURN FALSE;
  END GarbageModuleName;
  
PROCEDURE Remember(units: REF ARRAY OF TEXT): REF ARRAY OF TEXT=
  VAR a := NEW(REF ARRAY OF TEXT, NUMBER(units^));
  BEGIN
    FOR i := 0 TO NUMBER(a^)-1 DO
      a[i] := ModuleName(units[i]);
    END; (* for *)
    RETURN a;
  END Remember;

PROCEDURE CheckPrimarySource(ut: M3CUnit.Type; name: TEXT;
    cu: M3AST_AS.Compilation_Unit;
    cd: ContextData) RAISES {} =
  VAR
    cut: M3CUnit.Type;
  BEGIN
    IF NOT cd.setPrimarySource THEN RETURN END;
    FOR c := FIRST(Compilee) TO LAST(Compilee) DO
      cut := ToUnitType(c);
      IF cut = ut THEN
        FOR i := 0 TO NUMBER(cd.compilees[c]^)-1 DO
          IF Text.Equal(cd.compilees[c][i], ModuleName(name)) THEN
           cu.fe_status := cu.fe_status + M3CUnit.Status{PrimarySource};
          END; (* if *)
        END; (* for *)
      END
    END
  END CheckPrimarySource;

PROCEDURE CheckUnitNameAndType(context: M3Context.T; unitType: M3CUnit.Type;
    name: TEXT; cu: M3AST_AS.Compilation_Unit) RAISES {}=
  VAR real_name: TEXT;
      real_type: M3CUnit.Type;
      change := FALSE;
  BEGIN
    WITH symrep = cu.as_root.as_id.lx_symrep DO
      IF symrep # NIL THEN
        real_name := M3CId.ToText(symrep);
        real_type := M3CUnit.ToType(cu.as_root);
        IF (real_type IN M3CUnit.Interfaces AND
           NOT (unitType IN M3CUnit.Interfaces)) OR 
           (real_type IN M3CUnit.Modules AND
           NOT (unitType IN M3CUnit.Modules)) THEN
          change := TRUE;
          M3Error.Warn(cu.as_root.as_id,
              "unit type does not match file extension");
        END; (* if *)
        IF NOT Text.Equal(real_name, ModuleName(name)) THEN
          change := TRUE;
          M3Error.WarnWithId(cu.as_root.as_id, 
              "unit name \'%s\' does not match filename", symrep);
        END; (* if *)
        IF change THEN
          M3Context.Remove(context, name, unitType);
          CheckedAdd(context, real_name, real_type, cu);
        END; (* if *)
      END; (* if *)
    END;
  END CheckUnitNameAndType;

PROCEDURE CheckedAdd(
    context: M3Context.T;
    name: TEXT;
    ut: M3CUnit.Type;
    cu: M3AST_AS.Compilation_Unit)
    RAISES {}=
  BEGIN
    TRY
      M3Context.Add(context, name, ut, cu);
    EXCEPT
    | M3Context.Duplicate =>
        Err.Print(Fmt.F("duplicate interface or module with name '%s'",
                        name), Err.Severity.Error);
    END;
  END CheckedAdd;


PROCEDURE ParseOnly(s: M3CUnit.Status): BOOLEAN RAISES {} =
  BEGIN
    RETURN (s * AllPhases) = M3CUnit.Status{M3CUnit.State.Parsed};
  END ParseOnly;

PROCEDURE GetData(c: M3Context.T): ContextData=
  VAR cd: ContextData := c.get(TYPECODE(ContextData));
  BEGIN
    IF cd = NIL THEN
      cd := NEW(ContextData);
      cd.en := NEW(ErrorNotification);
      cd.en.cd := cd;
      c.put(cd);
    END;
    RETURN cd;
  END GetData;


BEGIN

END M3CGoList.
