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
 * Last Modified On: Thu Sep 25 09:42:30 1997
 * Update Count    : 15
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
 * Revision 1.4  1997/10/22 14:45:15  bm
 * Bug fix.  Naming conflicts.
 *
 * Revision 1.3  1997/08/11 20:36:43  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

(* Based on StubGenTool.m3 from the netobj stubgen package     *)
(*                                                             *)
(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE StubGenTool;

IMPORT ASTWalk;
IMPORT M3Context, M3CId, M3CUnit, M3Conventions, M3CConcTypeSpec, M3AST_AS;
IMPORT M3CGo;
IMPORT M3Args, M3CFETool;
IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_FE_F;
IMPORT AstToType, SOxCodeUtils, Type;
IMPORT Atom, Stdio, Text;

<* FATAL SOxCodeUtils.Error *>

CONST Version = "1-Jan-96";

TYPE
  Extension = M3CGo.Extension OBJECT
  OVERRIDES
    extend := StubGen;
  END;

PROCEDURE Init()=
  BEGIN
    M3CGo.AddExtension(NEW(Extension));
  END Init;

PROCEDURE GetArgs(tool: M3Args.T)=
  VAR strings, intfs, newarg: REF ARRAY OF TEXT;
      n, m, nextArg := 0;
      found: BOOLEAN;
      perfMon: BOOLEAN;
      obliqCode: BOOLEAN;
      proxyCode: BOOLEAN;
  BEGIN
    outputName := M3Args.GetString(tool, SOOutputName_Arg);
    perfMon := M3Args.GetFlag(tool, SOPerf_Arg);
    SOxCodeUtils.SetPerfMon(perfMon);
    obliqCode := M3Args.GetFlag(tool, SOObliq_Arg);
    SOxCodeUtils.SetObliqCode(obliqCode);
    proxyCode := M3Args.GetFlag(tool, SOProxy_Arg);
    SOxCodeUtils.SetProxyCode(proxyCode);
    strings := M3Args.GetStringList(tool, SOTypes_Arg);
    sharedTypes := GetQidList(strings);
    strings := M3Args.GetStringList(tool, SOExists_Arg);
    useTypes := GetQidList(strings);
    IF sharedTypes # NIL THEN
      intfs := NEW(REF ARRAY OF TEXT, NUMBER(sharedTypes^));
      FOR i := 0 TO LAST(sharedTypes^) DO 
        IF sharedTypes[i] # NIL AND sharedTypes[i].intf # NIL THEN
          intfs[n] := Atom.ToText(sharedTypes[i].intf);
          found := FALSE;
          FOR j := 0 TO n-1 DO
            IF Text.Equal(intfs[j], intfs[n]) THEN found := TRUE; END;
          END;
          IF NOT found THEN INC(n) END;
        END;
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
    interfaces := M3Args.GetStringList(tool, M3CFETool.Interfaces_Arg);    
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
        SOxCodeUtils.Message(
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

PROCEDURE StubGen(<*UNUSED*> e      : Extension;
                             context: M3Context.T;
                             cu     : M3AST_AS.Compilation_Unit;
                  <*UNUSED*> VAR (*inout*) phases: M3CUnit.Status)=
  BEGIN
    IF M3Args.Find(tool_g) THEN
      IF M3Conventions.PrimarySource IN cu.fe_status AND
        (M3CUnit.Errors * cu.fe_status = M3CUnit.Status{}) THEN
        Set(context, cu);
      END;
    END;
  END StubGen;
  
PROCEDURE Set(context: M3Context.T; cu: M3AST_AS.Compilation_Unit)=
  VAR intfname := M3CId.ToText(cu.as_root.as_id.lx_symrep);
      handle: AstToType.Handle;
  BEGIN
    SOxCodeUtils.Message("Processing interface " & intfname);
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
    ELSE RAISE SOxCodeUtils.Error("Run time error -- should not occur");
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
  tool_g := M3Args.New("shared", 
               "Generate code for shared objects", Version,
               master := TRUE);
  M3Args.RegisterString(tool_g, SOOutputName_Arg, 
                        "base file name for output", M3Args.Opt.Required);
  M3Args.RegisterStringList(tool_g, SOTypes_Arg, "list of types");
  M3Args.RegisterStringList(tool_g, SOExists_Arg,
                            "list of existing types");
  M3Args.RegisterFlag(tool_g, SOPerf_Arg, "performance monitoring");
  M3Args.RegisterFlag(tool_g, SOProxy_Arg, "generate proxy file");
  M3Args.RegisterFlag(tool_g, SOObliq_Arg, "generate obliq linkage code");
END StubGenTool.
