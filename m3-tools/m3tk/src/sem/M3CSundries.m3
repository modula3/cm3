MODULE M3CSundries;

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


IMPORT Text;

IMPORT AST, M3AST_AS;

IMPORT M3AST_AS_F, M3AST_SM_F;

IMPORT ASTWalk;

IMPORT M3Error, M3CDuplicate, M3CNameClash;


PROCEDURE Call(call: M3AST_AS.Call; isStatement: BOOLEAN) RAISES {}=
  VAR
    noResult: BOOLEAN;
  BEGIN
    TYPECASE call.as_callexp.sm_exp_type_spec OF
    | NULL =>
    | M3AST_AS.Procedure_type(procType) =>
        noResult := procType.as_result_type = NIL;
        IF noResult # isStatement THEN
          VAR
            e: Text.T;
          BEGIN
            IF isStatement THEN
              e := "call of function procedure cannot be used as a statement";
            ELSE
              e := "call of proper procedure cannot be used in an expression";
            END; (* if *)
            M3Error.Report(call, e);
          END;
        END; (* if *)
    ELSE
    END; (* typecase *)
  END Call;


PROCEDURE Procedure(p: M3AST_AS.Proc_decl; isModule: BOOLEAN) RAISES {}=
  VAR
    hasBody := p.as_body # NIL;
  BEGIN
    IF hasBody # isModule THEN
      VAR
        e: Text.T;
      BEGIN
        IF hasBody THEN
          e := "procedure in interface cannot have body";
        ELSE
          e := "procedure in module must have body";
        END; (* if *)
        M3Error.Report(p, e);
      END;
    END; (* if *)
  END Procedure;


REVEAL
  Handle = BRANDED OBJECT
    inModule, isProperCall: BOOLEAN;
    loopCount, procNesting: INTEGER := 0;
  END;


PROCEDURE Check(h: Handle; any: AST.NODE; v: ASTWalk.VisitMode) RAISES {}=
  BEGIN
    IF v = ASTWalk.VisitMode.Entry THEN
      TYPECASE any OF
      | M3AST_AS.Loop_st, M3AST_AS.While_st,
        M3AST_AS.For_st, M3AST_AS.Repeat_st =>
          INC(h.loopCount);
      | M3AST_AS.Call_st =>
          h.isProperCall := TRUE;
      | M3AST_AS.Call(call) =>
          Call(call, h.isProperCall);
          h.isProperCall := FALSE;
      | M3AST_AS.Proc_decl(proc_decl) =>
          INC(h.procNesting);
          Procedure(proc_decl, h.inModule);
      | M3AST_AS.Exit_st =>
          IF h.loopCount = 0 THEN
            M3Error.Report(any, "EXIT must be inside loop");
          END;
      | M3AST_AS.Case_st(case_st) =>
          M3CDuplicate.CaseLabels(case_st);
      | M3AST_AS.Try_except(try_except) =>
          M3CDuplicate.HandlerExceptions(try_except);
      | M3AST_AS.Enumeration_type(enumeration_type)=>
          M3CNameClash.Enumeration(enumeration_type);
      | M3AST_AS.Record_type(record_type) =>
          M3CNameClash.Record(record_type);
      | M3AST_AS.Object_type(object_type) =>
          M3CNameClash.Object(object_type);
      | M3AST_AS.Procedure_type(procedure_type) =>
          M3CNameClash.Procedure(procedure_type);
      | M3AST_AS.Exc_decl_s =>
          IF h.procNesting # 0 THEN
            M3Error.Report(any,
                "exception must be declared at top level");
          END;
      | M3AST_AS.Concrete_reveal, M3AST_AS.Subtype_reveal =>
          IF h.procNesting # 0 THEN
            M3Error.Report(any,
                "type can only be revealed at top level");
          END;
      ELSE
        (* nothing *)
      END; (* if *)
    ELSE
      TYPECASE any OF
      | M3AST_AS.Loop_st, M3AST_AS.While_st,
        M3AST_AS.For_st, M3AST_AS.Repeat_st =>
          DEC(h.loopCount);
      | M3AST_AS.Proc_decl =>
          DEC(h.procNesting);
      ELSE
        (* nothing *)
      END; (* case *)
    END; (* case *)
  END Check;


PROCEDURE NewHandle(
    inModule, isProperCall, inLoop, inProc: BOOLEAN)
    : Handle
    RAISES {}=
  VAR
    new := NEW(Handle, inModule := inModule, isProperCall := isProperCall);
  BEGIN
    IF inLoop THEN INC(new.loopCount) END;
    IF inProc THEN INC(new.procNesting) END;
    RETURN new;
  END NewHandle;


BEGIN
END M3CSundries.
