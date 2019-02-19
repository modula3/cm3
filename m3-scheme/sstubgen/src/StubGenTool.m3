(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE StubGenTool;

IMPORT ASTWalk;
IMPORT M3Context, M3CId, M3CUnit, M3Conventions, M3CConcTypeSpec, M3AST_AS;
IMPORT M3CGo;
IMPORT M3Args, M3CFETool;
IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_FE_F;
IMPORT AstToType, StubUtils, SProtocol AS Protocol, Type;
IMPORT Atom, Stdio, Text;

CONST Version = "1-Oct-96";

TYPE
  Extension = M3CGo.Extension OBJECT
  OVERRIDES
    extend := StubGen;
  END;

PROCEDURE Init()=
  BEGIN
    M3CGo.AddExtension(NEW(Extension));
  END Init;

PROCEDURE N(x : REF ARRAY OF Type.Qid) : CARDINAL = 
  BEGIN
    IF x = NIL THEN RETURN 0 ELSE RETURN NUMBER(x^) END
  END N;

PROCEDURE GetArgs(tool: M3Args.T)=
  VAR strings, intfs, newarg: REF ARRAY OF TEXT;
      n, m, nextArg := 0;
      found: BOOLEAN;
      perfMon: BOOLEAN;
  BEGIN
    IF M3Args.GetFlag(tool, StubGen_V1_Arg) THEN Protocol.version := 1; END;
    IF M3Args.GetFlag(tool, StubGen_V2_Arg) THEN Protocol.version := 2; END;
    perfMon := M3Args.GetFlag(tool, StubGenPerf_Arg);
    StubUtils.SetPerfMon(perfMon);
    strings := M3Args.GetStringList(tool, StubGenTypes_Arg);
    stubTypes := GetQidList(strings);
    strings := M3Args.GetStringList(tool, StubGenExists_Arg);
    useTypes := GetQidList(strings);

    strings := M3Args.GetStringList(tool, StubGenInterfaces_Arg);
    stubInterfaces := GetIntfQidList(strings);

    IF stubTypes # NIL OR stubInterfaces # NIL THEN
      intfs := NEW(REF ARRAY OF TEXT, 
                   N(stubTypes) + N(stubInterfaces));
      IF stubTypes # NIL THEN
        FOR i := 0 TO LAST(stubTypes^) DO 
          IF stubTypes[i] # NIL AND stubTypes[i].intf # NIL THEN
            intfs[n] := Atom.ToText(stubTypes[i].intf);
            found := FALSE;
            FOR j := 0 TO n-1 DO
              IF Text.Equal(intfs[j], intfs[n]) THEN found := TRUE; END;
            END;
            IF NOT found THEN INC(n) END;
          END
        END
      END;

      IF stubInterfaces # NIL THEN
        FOR i := 0 TO LAST(stubInterfaces^) DO 
          IF stubInterfaces[i] # NIL AND stubInterfaces[i].intf # NIL THEN
            intfs[n] := Atom.ToText(stubInterfaces[i].intf);
            found := FALSE;
            FOR j := 0 TO n-1 DO
              IF Text.Equal(intfs[j], intfs[n]) THEN found := TRUE; END
            END;
            IF NOT found THEN INC(n) END
          END
        END
      END;

      m := n;
      strings := M3Args.GetStringList(tool, M3CFETool.Interfaces_Arg);
      IF strings = NIL THEN
        newarg := NEW(REF ARRAY OF TEXT, n);
        nextArg := 0;
      ELSE
        FOR i := 0 TO n-1 DO 
          found := FALSE;
          FOR j := 0 TO LAST(strings^) DO
            IF Text.Equal(intfs[i], strings[j]) THEN 
              found := TRUE;
            END;
          END;
          IF found THEN DEC(m); intfs[i] := NIL END;
        END;
        IF m > 0 THEN
          newarg := NEW(REF ARRAY OF TEXT, m+NUMBER(strings^));
          FOR i := 0 TO LAST(strings^) DO
            newarg[i] := strings[i];
          END;
          nextArg := LAST(strings^)+1;
        END;
      END;
      IF m > 0 THEN
        FOR i := 0 TO n-1 DO
          IF intfs[i] # NIL THEN
            newarg[nextArg] := intfs[i];
            INC(nextArg);
          END;
        END;
        M3Args.SetStringList(tool, M3CFETool.Interfaces_Arg, newarg);
      END;
    END;
    strings := M3Args.GetStringList(tool, M3CFETool.Interfaces_Arg);    
  END GetArgs;

PROCEDURE GetQidList(strings: REF ARRAY OF TEXT): REF ARRAY OF Type.Qid =
  VAR qids: REF ARRAY OF Type.Qid;
      dotpos: INTEGER;
  BEGIN
    IF strings = NIL THEN RETURN NIL END;
    qids := NEW(REF ARRAY OF Type.Qid, NUMBER(strings^));
    FOR i := 0 TO LAST(strings^) DO
      dotpos := Text.FindChar(strings[i], '.');
      IF dotpos < 1 THEN 
        StubUtils.Message(
            "Invalid argument \"" & strings[i] &"\".  " &
            "Should be qualified id.");
      ELSE
        qids[i] := NEW(Type.Qid, 
                       intf := Atom.FromText(Text.Sub(strings[i], 0, dotpos)),
                       item := Atom.FromText(Text.Sub(strings[i], dotpos+1, 
                                          LAST(INTEGER))));
      END
    END;
    RETURN qids;
  END GetQidList;

PROCEDURE GetIntfQidList(strings: REF ARRAY OF TEXT): REF ARRAY OF Type.Qid =
  VAR qids: REF ARRAY OF Type.Qid;
      dotpos: INTEGER;
  BEGIN
    IF strings = NIL THEN RETURN NIL END;
    qids := NEW(REF ARRAY OF Type.Qid, NUMBER(strings^));
    FOR i := 0 TO LAST(strings^) DO
      dotpos := Text.FindChar(strings[i], '.');
      IF dotpos >= 1 THEN 
        StubUtils.Message(
            "Invalid argument \"" & strings[i] &"\".  " &
            "Interface should not be qualified id.");
      ELSE
        qids[i] := NEW(Type.Qid, 
                       intf := Atom.FromText(strings[i]),
                       item := NIL);
      END
    END;
    RETURN qids;
  END GetIntfQidList;

PROCEDURE StubGen(<*UNUSED*> e      : Extension;
                             context: M3Context.T;
                             cu     : M3AST_AS.Compilation_Unit;
                  <*UNUSED*> VAR (*inout*) phases: M3CUnit.Status)=
  BEGIN
    IF M3Args.Find(tool_g) THEN
      IF M3Args.GetFlag(tool_g, StubGen_Arg) THEN
        IF M3Conventions.PrimarySource IN cu.fe_status AND
           (M3CUnit.Errors * cu.fe_status = M3CUnit.Status{}) THEN
          Set(context, cu);
        END;
      END;
    END;
  END StubGen;
  
PROCEDURE Set(context: M3Context.T; cu: M3AST_AS.Compilation_Unit)=
  VAR intfname := M3CId.ToText(cu.as_root.as_id.lx_symrep);
      handle: AstToType.Handle;
  BEGIN
    StubUtils.Message("Processing interface " & intfname);
    handle := AstToType.NewHandle(Stdio.stdout, intfname, context);
    IF handle=NIL THEN RETURN END; (* No NetObj.T in context *)
    (* some cases we dont want to handle, plus making sure we
       deal with generic instantiations. *)
    TYPECASE cu.as_root OF
    | NULL => RETURN
    | M3AST_AS.UNIT_GEN_DEF => 
        (* parsing is ok, but no semantic analysis *)
    | M3AST_AS.UNIT_GEN_INS(ui) =>
        WITH cu_ins = ui.sm_ins_comp_unit DO
          IF cu_ins # NIL THEN
            Set(context, cu_ins);
          END;
        END;
        RETURN
    | M3AST_AS.UNIT_NORMAL =>
        IF cu = M3Context.Standard() THEN RETURN END;
    ELSE StubUtils.Die("StubGenTool.Set: unknown AST root type");
    END;
 
    (* No reason to look at MODULEs *)
    TYPECASE cu.as_root OF
    | M3AST_AS.Module, M3AST_AS.Module_gen_ins, M3AST_AS.Module_gen_def =>
        RETURN
    ELSE (* continue *)
    END;
    M3CConcTypeSpec.SetCurrentReveal(cu, ASTWalk.VisitMode.Entry);
    ASTWalk.VisitNodes(cu, handle);   <* NOWARN *>
    M3CConcTypeSpec.SetCurrentReveal(cu, ASTWalk.VisitMode.Exit);
  END Set;

BEGIN
  tool_g := M3Args.New("stubs", 
               "Process M3 interfaces", Version,
               master := TRUE);
  M3Args.RegisterFlag(tool_g, StubGen_Arg, "stub generate");
  M3Args.RegisterFlag(tool_g, StubGen_V1_Arg, "protocol version 1");
  M3Args.RegisterFlag(tool_g, StubGen_V2_Arg, "protocol version 2");
  M3Args.RegisterStringList(tool_g, StubGenTypes_Arg, "list of types");
  M3Args.RegisterStringList(tool_g, StubGenInterfaces_Arg, "list of interfaces");
  M3Args.RegisterStringList(tool_g, StubGenExists_Arg,
                            "list of existing types");
  M3Args.RegisterFlag(tool_g, StubGenPerf_Arg, "performance monitoring");
END StubGenTool.


