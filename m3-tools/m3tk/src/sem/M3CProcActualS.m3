MODULE M3CProcActualS;

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

IMPORT M3AST_LX, M3AST_AS, M3AST_SM, M3ASTNext;

IMPORT M3AST_AS_F, M3AST_SM_F;

IMPORT SeqM3AST_AS_EXP;

IMPORT M3Error, M3CActualUtil, M3CTypeRelation;

(*
   From the report (Sec. 3.2, Procedure call):

   The list of bindings is rewritten to fit the signature of P's type as
   follows:  First, each positional binding actual is converted into a
   keyword binding by supplying the name of the i'th formal parameter,
   where actual is the i'th binding in ActualS.  Second, for each
   parameter that has a default and is not bound after the first step, the
   binding name := default is added to the list of actuals, where name is
   the name of the parameter and default is its default value.

   It is a static error if the rewritten list of actuals binds any name
   more than once, binds any name that is not a formal parameter, or fails
   to bind any formal parameter.

   A procedure call can also have the form:

       o.m(ActualS)

   where o is an object and m names one of o's methods.  This is equivalent
   to:

       (o's m method)(o, ActualS)

   If the call is not a NEW, we'll build up the sm_actuals as follows:

   1. Map the positional binding actuals to their corresponding formals.

   2. Go over the remaining formals.  If there is a binding for it
      in the actuals, then use the binding.  Otherwise use the default.
*)


PROCEDURE NotProcedure(call: M3AST_AS.Call) RAISES {}=
  BEGIN
    M3Error.Report(call, "calling something that is not a procedure");
  END NotProcedure;


<*INLINE*> PROCEDURE ProcType(
    call: M3AST_AS.Call;
    complain: BOOLEAN)
    : M3AST_AS.Procedure_type
    RAISES {}=
  BEGIN
    TYPECASE call.as_callexp.sm_exp_type_spec OF
    | M3AST_AS.Procedure_type(p) =>
        RETURN p; (* NIL case ok *)
    ELSE
      IF complain THEN NotProcedure(call) END;
      RETURN NIL;
    END;
  END ProcType;


<*INLINE*> PROCEDURE AddParam(call: M3AST_AS.Call; exp: M3AST_AS.EXP) RAISES {}=
  BEGIN
    SeqM3AST_AS_EXP.AddRear(call.sm_actual_s, exp);
  END AddParam;


PROCEDURE Set(call: M3AST_AS.Call) RAISES {}=
  VAR
    procType := ProcType(call, TRUE);
  BEGIN
    (* If 'callExp' does not have its 'sm_exp_type_spec' set to a procedure
     type then there is nothing we can do (except complain, via 'ProcType') *)
    IF procType = NIL THEN RETURN END;

    (* Prepare to iterate formal parameters. Note that if this is a call
     of the form 'T.m(...)', where 'T' is an object type we have a hidden
     formal parameter, for which the actual must be specified positionally *)
    VAR
      iter := M3ASTNext.NewIterFormal(procType.as_formal_param_s);
      hiddenFormal := procType.sm_def_id # NIL AND
          ISTYPE(procType.sm_def_id, M3AST_AS.Type_id);
      (* Pre process the actuals *)
      actuals := M3CActualUtil.ActualList(call);
      formalP: M3AST_AS.Formal_param;
      formalId: M3AST_AS.FORMAL_ID;
      id: M3AST_LX.Symbol_rep;
      actual: M3AST_AS.EXP;
    BEGIN

      (* Find all of the positional actuals *)
      FOR pos := 0 TO M3CActualUtil.PositionalActuals(actuals) - 1 DO
        IF hiddenFormal THEN
          hiddenFormal := FALSE;
          id := NIL;
        ELSIF M3ASTNext.Formal(iter, formalP, formalId) THEN
          (* ok *)
          id := formalId.lx_symrep;
        ELSE
          M3CActualUtil.TooManyArguments(call);
          M3CActualUtil.FindUnmatched(actuals);
          RETURN;
        END;
        AddParam(call, M3CActualUtil.ActualAt(actuals, pos, id));
      END;

      (* If there was supposed to be an argument to match a hidden first formal
       did we get one? *)
      IF hiddenFormal THEN
        M3Error.Report(call, "object required as first positional argument");
      END;

      (* For the remaining FORMALS, see if there is a keyword actual. If there
       is, use its expression.  If not, use the default. *)
      WHILE M3ASTNext.Formal(iter, formalP, formalId) DO
        IF NOT M3CActualUtil.ActualByKeyword(actuals, formalId, actual) THEN
          actual := formalP.as_default;
          IF actual = NIL AND formalId.lx_symrep # NIL THEN
            M3Error.ReportWithId(call, "no argument for parameter \'%s\'",
                formalId.lx_symrep);
          END;
        END;
        AddParam(call, actual);
      END;

      M3CActualUtil.FindUnmatched(actuals);
    END;
  END Set;


PROCEDURE DefaultMethodCall(
    p: M3AST_AS.Procedure_type;
    VAR ts: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    TYPECASE p.sm_def_id OF
    | NULL =>
        RETURN FALSE;
    | M3AST_AS.Type_id(typeId) =>
        ts := typeId.sm_type_spec;
        RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END DefaultMethodCall;


PROCEDURE TypeCheck(call: M3AST_AS.Call; safe: BOOLEAN) RAISES {}=
  VAR
    procType := ProcType(call, FALSE);
  BEGIN
    IF procType = NIL THEN RETURN END;

    (* Prepare to iterate formal parameters. Note that if this is a call
     of the form 'T.m(...)', where 'T' is an object type we have a hidden
     formal parameter, for which the actual must be specified positionally *)
    VAR
      iterActuals := SeqM3AST_AS_EXP.NewIter(call.sm_actual_s);
      iterFormals := M3ASTNext.NewIterFormal(procType.as_formal_param_s);
      formalType: M3AST_SM.TYPE_SPEC_UNSET;
      hiddenFormal := DefaultMethodCall(procType, formalType);
      formalP: M3AST_AS.Formal_param;
      formalId: M3AST_AS.FORMAL_ID;
      actual: M3AST_AS.EXP;
      isVar: BOOLEAN;
    BEGIN
      WHILE SeqM3AST_AS_EXP.Next(iterActuals, actual) DO
        IF hiddenFormal THEN
          (* 'formalType' already set up *)
          isVar := FALSE;
          hiddenFormal := FALSE;
        ELSE
          IF NOT M3ASTNext.Formal(iterFormals, formalP, formalId) THEN
            EXIT;
          ELSE
            formalType := formalId.sm_type_spec;
            isVar := ISTYPE(formalId, M3AST_AS.F_Var_id);
          END;
        END;

        IF actual # NIL AND formalType # NIL THEN
          IF isVar THEN
            M3CActualUtil.CheckIsVARActual(actual);
            IF NOT M3CTypeRelation.VARPassable(formalType,
                actual.sm_exp_type_spec) THEN
              M3Error.Report(actual,
                  "actual parameter not passable to VAR formal");
            END (* if *)
          ELSE
            IF NOT M3CActualUtil.Passable(formalType, actual, safe) THEN
              M3Error.Report(
                  actual, "actual parameter not assignable to formal");
            END; (* if *)
          END;
        END;

      END;
    END;
  END TypeCheck;


BEGIN
END M3CProcActualS.
