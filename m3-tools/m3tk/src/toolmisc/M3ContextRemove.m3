(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3ContextRemove;

IMPORT RefList;
IMPORT M3Context, M3CUnit, M3Conventions, M3AST_AS;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_FE_F;

IMPORT SeqM3AST_AS_Used_interface_id;

TYPE
  UnitsClosure = M3Context.Closure OBJECT
    unit_id_to_remove: M3AST_AS.UNIT_ID := NIL;
  OVERRIDES
    callback := RemoveInterfaceUsers;
  END;

  GenUnitsClosure = UnitsClosure OBJECT
  OVERRIDES
    callback := RemoveGenericUsers
  END;

  ErrorClosure =  M3Context.Closure OBJECT
  OVERRIDES
    callback := RemoveIfErrors;
  END;

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
    <*UNUSED*> context: M3Context.T;
    unitType: M3CUnit.Type; name: TEXT;     
    cu: M3AST_AS.Compilation_Unit)=
  <*FATAL ANY*>
  BEGIN
    LOCK notificationsMutex_g DO
      VAR list: RefList.T := notifications_g;
      BEGIN
        WHILE list # NIL DO
          NARROW(list.head, NotificationElem).e.callback(unitType, name, cu);
          list := list.tail;
        END;
      END;
    END;
  END Notify;

PROCEDURE SafeToUnit(cu: M3AST_AS.Compilation_Unit; ut: M3CUnit.Type):
  M3CUnit.Type =
  BEGIN
    IF cu.as_root = NIL THEN
      RETURN ut
    ELSE
      RETURN M3CUnit.ToType(cu.as_root)
    END;
  END SafeToUnit;

PROCEDURE Units(
    context: M3Context.T;
    READONLY units: ARRAY OF TEXT; 
    unitType: M3CUnit.Type;
    ) RAISES {}=
  VAR
    mname: TEXT;
    cu: M3AST_AS.Compilation_Unit;
  BEGIN
    (* Remove any old instances of the requested units from the context. *)
    FOR i := 0 TO NUMBER(units)-1 DO
      mname := M3Conventions.ModuleName(units[i]);
      IF M3Context.Find(context, mname, unitType, cu) THEN
        RemoveUnit(context, unitType, mname, cu);
      END;
    END; (* for *)
  END Units;

PROCEDURE UnitsWithErrors(context: M3Context.T)=
  <*FATAL ANY*>
  BEGIN
    M3Context.Apply(context,
      NEW(ErrorClosure), findStandard := FALSE); 
  END UnitsWithErrors;

PROCEDURE RemoveIfErrors(
    cl: ErrorClosure;
    unitType: M3CUnit.Type; name: TEXT; 
    cu: M3AST_AS.Compilation_Unit) RAISES {}=
  BEGIN
    IF (cu.fe_status * M3CUnit.Errors) # M3CUnit.Status{} THEN
      RemoveUnit(cl.context, unitType, name, cu);
    END; (* if *)    
  END RemoveIfErrors;

PROCEDURE RemoveUnit(
    context: M3Context.T;
    unitType: M3CUnit.Type; name: TEXT; 
    cu: M3AST_AS.Compilation_Unit) RAISES {}=

  BEGIN
    M3Context.Remove(context, name, unitType);
    Notify(context, SafeToUnit(cu, unitType), name, cu);
    
    IF cu.as_root = NIL THEN RETURN END;

    IF ISTYPE(cu.as_root, M3AST_AS.UNIT_GEN_DEF) THEN
      <*FATAL ANY*> BEGIN
        M3Context.ApplyToSet(context,
            NEW(GenUnitsClosure, unit_id_to_remove := cu.as_root.as_id),
            M3CUnit.TypeSet{M3CUnit.Type.Interface_gen_ins,
                            M3CUnit.Type.Module_gen_ins})
      END;
    END;

    IF (unitType = M3CUnit.Type.Interface) THEN
      (* remove everything that uses this interface *)
      TYPECASE cu.as_root OF
      | M3AST_AS.UNIT_GEN_INS(unit_ins) =>
          (* importers are bound to the generated UNIT_NORMAL *)
          cu := unit_ins.sm_ins_comp_unit;
          IF cu = NIL OR cu.as_root = NIL THEN RETURN END;
      ELSE
      END; (* typecase *)

      <*FATAL ANY*> BEGIN
        M3Context.Apply(context,
            NEW(UnitsClosure, unit_id_to_remove := cu.as_root.as_id),
                findStandard := FALSE);
      END;
    END; (* if *)
  END RemoveUnit;

PROCEDURE RemoveInterfaceUsers(
    cl: UnitsClosure;
    unitType: M3CUnit.Type; name: TEXT; 
    cu: M3AST_AS.Compilation_Unit) RAISES {}=
  VAR
    iter: SeqM3AST_AS_Used_interface_id.Iter;
    intf_id: M3AST_AS.Used_interface_id;
  BEGIN
    TYPECASE cu.as_root OF
    | M3AST_AS.UNIT_GEN_INS(unit_ins) =>
        cu := unit_ins.sm_ins_comp_unit;
        IF cu = NIL THEN RETURN END;
    ELSE
    END; (* typecase *)
    iter := SeqM3AST_AS_Used_interface_id.NewIter(
        NARROW(cu.as_root, M3AST_AS.UNIT_WITH_BODY).sm_import_s);
    WHILE SeqM3AST_AS_Used_interface_id.Next(iter, intf_id) DO
      IF (intf_id.sm_def # NIL) AND
         (NARROW(intf_id.sm_def, M3AST_AS.Interface_id) = cl.unit_id_to_remove) THEN
        M3Context.Remove(cl.context, name, unitType);
	Notify(cl.context, unitType, name, cu);
      END; (* if *)
    END; (* while *)
  END RemoveInterfaceUsers;

PROCEDURE RemoveGenericUsers(
    cl: GenUnitsClosure;
    unitType: M3CUnit.Type; name: TEXT; 
    cu: M3AST_AS.Compilation_Unit) RAISES {}=
  VAR gen_id := NARROW(cu.as_root, M3AST_AS.UNIT_GEN_INS).as_gen_id;
  BEGIN
    (* If this instantation is bound to the generic definition
    to be removed, remove it as well. *)
    IF gen_id.sm_def # NIL AND
       (NARROW(gen_id.sm_def, M3AST_AS.UNIT_ID) = cl.unit_id_to_remove) THEN
      RemoveUnit(cl.context, unitType, name, cu);
    END; (* if *)
  END RemoveGenericUsers;

BEGIN

END M3ContextRemove.
