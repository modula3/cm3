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

MODULE M3LTool;

IMPORT Text, Err, Fmt, RefList;
IMPORT M3AST_AS;

IMPORT M3AST_AS_F, M3AST_PL_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_Proc_id;

IMPORT M3Args, M3Context;
IMPORT M3Error, M3CId;
IMPORT M3LExports, M3LDepends, M3LInitOrder, M3LOpaque;
IMPORT M3LBackEnd, M3LMain, M3LReveals;
IMPORT M3LTypeCodes, M3CUnit;

CONST
  DebugTypeCodes_Arg = "DebugTypeCodes";
  Main_Arg = "MainModule";
  MutualDependencies_Arg = "MutualDependencies";
  ForceInitOrder_Arg = "ForceInitOrder";

  Version = "Oct-2-92";

VAR 
  tool_g: M3Args.T;

TYPE CClosure = M3Context.Closure OBJECT OVERRIDES callback := ShowErrors END;

PROCEDURE Run(c: M3Context.T; do_depends := TRUE): INTEGER RAISES {}=
  <*FATAL ANY*>
  VAR 
    r := DoRun(c, do_depends);
  BEGIN
    M3Context.Apply(c, NEW(CClosure));
    RETURN r;
  END Run;

PROCEDURE ShowErrors(<*UNUSED*> cl: CClosure;
                     <*UNUSED*> ut: M3CUnit.Type; 
                     <*UNUSED*> name: TEXT;
                     cu: M3AST_AS.Compilation_Unit) RAISES {}=
  BEGIN
    M3Error.SetCu(cu); M3Error.ShowAll(cu.as_root);
  END ShowErrors;

PROCEDURE CheckResult(rc: INTEGER; VAR (*inout*) trc: INTEGER) RAISES {}=
  BEGIN
    IF rc < 0 THEN trc := rc; END;
  END CheckResult;

PROCEDURE DoRun(c: M3Context.T; do_depends: BOOLEAN): INTEGER RAISES {}=
  TYPE EClosure = M3LExports.Closure BRANDED OBJECT END;
  VAR
    main: M3AST_AS.Compilation_Unit;
    result: INTEGER := 0;
    e_closure: EClosure;
    t: M3LInitOrder.T;
  BEGIN
    IF M3Args.Find(tool_g) THEN
      e_closure := NEW(EClosure, report := ReportExports);
      M3LOpaque.SetReveal(c);
      M3LExports.Check(c, e_closure);
      (* the above call sets up info needed by this pass *)
      CheckResult(M3LReveals.Check(c), result);
      IF result < 0 THEN RETURN result END;
      (* no point in proceeding further *)
      IF do_depends THEN
        M3LDepends.Set(c, M3LDepends.Default());
      END;
      main := MainModule(c, result);
      IF result < 0 THEN RETURN result END;
      VAR cb: M3LInitOrder.MDCallback := NIL;
      BEGIN
        IF M3Args.GetFlag(tool_g, MutualDependencies_Arg) THEN
          cb := NEW(M3LInitOrder.MDCallback, callback := ReportMD);
        END;
        t := NEW(M3LInitOrder.T).init(c, main, cb);
      END;
      VAR 
        tcl: M3LTypeCodes.T;
      BEGIN
        tcl := M3LTypeCodes.Set(c, genTexts := TRUE, genFingerPrints := TRUE);
        RETURN M3LBackEnd.Run(c, t, tcl);
      END;
    ELSE
      RETURN -1
    END; (* if *)
  END DoRun;

PROCEDURE ReportMD(<*UNUSED*> cb: M3LInitOrder.MDCallback;
                   m1, m2: M3AST_AS.Module)=
  BEGIN
    Err.Print(Fmt.F("mutual dependency between %s and %s",
                    M3CId.ToText(m1.as_id.lx_symrep),
                    M3CId.ToText(m2.as_id.lx_symrep)),
              Err.Severity.Warning);
  END ReportMD;

PROCEDURE MainModule(c: M3Context.T;
    VAR (*inout*) result: INTEGER): M3AST_AS.Compilation_Unit RAISES {}=
  VAR
    name: Text.T;
    cu_s: RefList.T;
    cu: M3AST_AS.Compilation_Unit;
  BEGIN
    cu_s := M3LMain.Module(c);
    name := M3Args.GetString(tool_g, Main_Arg);
    IF cu_s = NIL THEN
      Err.Print("failed to find any main program modules", Err.Severity.Error);
      result := -1; RETURN NIL;              
    ELSE
      cu := cu_s.head;
      IF name = NIL THEN (* there had better be only one *)
        IF cu_s.tail # NIL THEN
          Err.Print(Fmt.F("more than one exporter of \'%s\' interface",
              M3LMain.Name),
              Err.Severity.Error);
          result := -1; RETURN NIL;  
        ELSE            
          RETURN cu (* take first *)
	END;
      END;

      (* take one which matches given name *)
      WHILE cu_s # NIL DO
        cu := cu_s.head;
      	IF Text.Equal(name, M3CId.ToText(cu.as_root.as_id.lx_symrep)) THEN
	  RETURN cu;
	END; (* if *)
        cu_s := cu_s.tail;
      END; (* while *)
      Err.Print(Fmt.F("no module named \'%s\' which exports \'%s\'",
                      name, M3LMain.Name), Err.Severity.Error);
      result := -1; RETURN NIL;
    END; (* if *)
  END MainModule;

PROCEDURE ReportExports(
    <*UNUSED*> cl: M3LExports.Closure;
    cu: M3AST_AS.Compilation_Unit; 
    an: M3AST_AS.SRC_NODE) RAISES {}=
  VAR
    en: M3Error.ERROR_NODE;
    id1, id2: M3CId.T;
  BEGIN
    id1 := NARROW(an, M3AST_AS.DEF_ID).lx_symrep;
    id2 := NARROW(cu.as_root, M3AST_AS.Interface).as_id.lx_symrep;
    en := an;
    M3Error.SetCu(cu);
    TYPECASE an OF <*NOWARN*>
    | M3AST_AS.Proc_id(proc_id) =>
       IF NOT M3LBackEnd.HardWired(M3CId.ToText(id2), M3CId.ToText(id1)) THEN
        M3Error.WarnWithId(en, 
            "procedure \'%s\' in interface \'%s\' is not implemented", id1, id2);
        SeqM3AST_AS_Proc_id.AddRear(
            NARROW(proc_id.tmp_unit_id, M3AST_AS.Interface_id).pl_missing_proc_s,
            an);
       END;

    | M3AST_AS.Type_id =>
        M3Error.WarnWithId(en, 
            "opaque type %s in interface %s is not implemented", id1, id2);
    END; (* case *)
  END ReportExports;

PROCEDURE Initialise(): M3Args.T RAISES {}=
  BEGIN
    tool_g := M3Args.New("m3l", "Modula-3 Pre-Linker", Version);
    M3Args.RegisterString(tool_g, Main_Arg, 
      "name of main program module (default \'Main\')");
    M3Args.RegisterString(tool_g, ForceInitOrder_Arg,
      "force initialisation order from given file");
    M3Args.RegisterFlag(tool_g, DebugTypeCodes_Arg, 
      "debug typecode generation");
    M3Args.RegisterFlag(tool_g, MutualDependencies_Arg, 
      "show mutual dependencies between modules");
    M3LBackEnd.RegisterArgs(tool_g);
    RETURN tool_g;
  END Initialise;

BEGIN
END M3LTool.
