MODULE M3CFETool;

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

IMPORT Err, Fmt;
IMPORT M3Args;
IMPORT M3CUnit;
IMPORT M3CGoList, M3Context, M3Time, M3Conventions;
IMPORT M3AST_AS;
IMPORT M3AST_FE_F;

CONST
  Version = "28-Sep-92";

VAR
  tool_g: M3Args.T;

TYPE
  NotificationClosure = M3CGoList.Notification OBJECT
    setCompTime := FALSE;
    printUnits := FALSE;
    totalCompTime: M3Conventions.CompTime := NIL;
    indent := 0;
  OVERRIDES notify := TimeAndPrintUnits;
  END;

PROCEDURE GetTool(): M3Args.T RAISES {} =
  BEGIN
    RETURN tool_g;
  END GetTool;

PROCEDURE CompileInContext(
    VAR (*inout*) context: M3Context.T;
    phases: M3CUnit.Status;
    headerOnly := FALSE;
    setPrimarySource := TRUE;
    ): INTEGER RAISES {} =
  VAR
    ml, il, pl: REF ARRAY OF TEXT;
    result: INTEGER := 0;
    notification := NEW(NotificationClosure);
    setCompTime: BOOLEAN;
  BEGIN
    IF NOT M3Args.Find(tool_g) THEN
      RETURN -1
    END;

    il := M3Args.GetStringList(tool_g, Interfaces_Arg);
    ml := M3Args.GetStringList(tool_g, Modules_Arg);
    pl := M3Args.GetStringList(tool_g, PathNames_Arg);

    setCompTime := M3Args.GetFlag(tool_g, Timings_Arg);
    notification.setCompTime := setCompTime;
    notification.printUnits := M3Args.GetFlag(tool_g, PrintUnits_Arg);
    IF setCompTime THEN
      notification.totalCompTime := NEW(M3Conventions.CompTime).init();
    END;
    IF setCompTime OR notification.printUnits THEN
      M3CGoList.AddNotification(notification);
    ELSE notification := NIL;
    END;
    IF il = NIL THEN il := NEW(REF ARRAY OF TEXT, 0); END;
    IF ml = NIL THEN ml := NEW(REF ARRAY OF TEXT, 0); END;
    IF pl = NIL THEN pl := NEW(REF ARRAY OF TEXT, 0); END;
    M3CGoList.CompileUnitsInContext(context, il^, ml^, pl^, phases,
        headerOnly, setPrimarySource, setCompTime);
    IF (phases * M3CUnit.Errors) # M3CUnit.Status{} THEN
      result := -1;
    END;

    IF notification # NIL THEN
      IF setCompTime THEN 
        Err.Print("total ", Err.Severity.Comment, newline := FALSE); 
        PrintTime(notification.totalCompTime); 
      END; (* if *)
      M3CGoList.RemoveNotification(notification);
    END;

    RETURN result;
  END CompileInContext;

PROCEDURE TimeAndPrintUnits(
    cl: NotificationClosure;
    context: M3Context.T;
    nm: M3CGoList.NotifyMode;
    name: TEXT;
    unitType: M3CUnit.Type;
    unitForm: M3CUnit.Form;
    cu: M3AST_AS.Compilation_Unit;
    compTime: M3Conventions.CompTime) RAISES {} =
  BEGIN
    IF cl.printUnits THEN
       PrintUnits(cl, context, nm, name, unitType, unitForm, cu, compTime);
    END;
    IF cl.totalCompTime # NIL AND nm = M3CGoList.NotifyMode.After THEN
      Err.Print(Fmt.F("%s \'%s\': ", M3CUnit.TypeName(unitType), name),
                Err.Severity.Comment, newline := FALSE);
      PrintTime(compTime);
      cl.totalCompTime.open := M3Time.Add(cl.totalCompTime.open,
                                           compTime.open);
      cl.totalCompTime.parse := M3Time.Add(cl.totalCompTime.parse,
                                            compTime.parse);
      cl.totalCompTime.semantic := M3Time.Add(cl.totalCompTime.semantic,
                                               compTime.semantic);
    END; (* if *)   
  END TimeAndPrintUnits;

PROCEDURE PrintTime(compTime: M3Conventions.CompTime) RAISES {} =
  BEGIN
    Err.Print(Fmt.F("time to open %s, parse %s, check semantics %s",
        M3Time.AsString(compTime.open),
        M3Time.AsString(compTime.parse),
        M3Time.AsString(compTime.semantic)), Err.Severity.Continue);    
  END PrintTime;

PROCEDURE PrintUnits(
    cl: NotificationClosure;
    <*UNUSED*> context: M3Context.T;
    nm: M3CGoList.NotifyMode;
    name: TEXT;
    unitType: M3CUnit.Type;
    unitForm: M3CUnit.Form;
    cu: M3AST_AS.Compilation_Unit;
    <*UNUSED*> compTime: M3Conventions.CompTime) RAISES {} =
  BEGIN
    IF nm = M3CGoList.NotifyMode.Before THEN
      PrintUnit(cl, unitType, name, unitForm, cu);
      INC(cl.indent, 2);
    ELSE
      DEC(cl.indent, 2);
    END; (* if *)
  END PrintUnits;

PROCEDURE PrintUnit(cl: NotificationClosure;
    unitType: M3CUnit.Type; unitName: TEXT; 
    uf: M3CUnit.Form; cu: M3AST_AS.Compilation_Unit) RAISES {} =
  BEGIN
    Err.Print("", Err.Severity.Comment, newline := FALSE);
    FOR i := 1 TO cl.indent DIV 2 DO
      Err.Print("  ", Err.Severity.Continue, newline := FALSE);
    END; (* for *)
    IF uf = M3CUnit.Form.Source THEN
      Err.Print(Fmt.F("compiling %s \'%s\'", M3CUnit.TypeName(unitType),
                   unitName), Err.Severity.Continue, newline := FALSE);
    ELSE
      Err.Print(Fmt.F("reading AST for %s \'%s\'", M3CUnit.TypeName(unitType), 
                   unitName), Err.Severity.Continue, newline := FALSE);
    END; (* if *)
    IF cu # NIL THEN
      Err.Print(Fmt.F(" from %s", M3CUnit.TextName(cu.fe_uid)), 
         Err.Severity.Continue, newline := FALSE);
    END; (* if *)
    Err.Print("", Err.Severity.Continue);
  END PrintUnit;

BEGIN
  tool_g := M3Args.New("m3cfe", "Modula-3 Compiler Front End", Version);
  M3Args.RegisterStringList(tool_g, PathNames_Arg, 
      "list of files to be compiled", M3Args.Opt.Positional);
  M3Args.RegisterStringList(tool_g, Modules_Arg, 
      "list of modules to be compiled");
  M3Args.RegisterStringList(tool_g, Interfaces_Arg, 
      "list of interfaces to be compiled");

  M3Args.RegisterFlag(tool_g, PrintUnits_Arg, 
      "print name of each unit compiled");
  M3Args.RegisterFlag(tool_g, Timings_Arg, "time compiler phases"); 
END M3CFETool.
