(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

MODULE M3CharExprsToReplace;

IMPORT AST, ASTWalk;
IMPORT M3AST_AS;
IMPORT M3AST_AS_F, M3AST_SM_F;
IMPORT SeqM3AST_AS_EXP;
IMPORT M3Error;
IMPORT M3CharPreds;

REVEAL
  Handle = Public BRANDED OBJECT OVERRIDES callback := Node; END;

PROCEDURE NewHandle (): Handle RAISES {} =
  BEGIN
    RETURN NEW(Handle).init();
  END NewHandle;

PROCEDURE Node (<*UNUSED*> h : Handle;
                           n : AST.NODE;
                           vm: ASTWalk.VisitMode) RAISES {} =
  BEGIN
    IF vm = ASTWalk.VisitMode.Entry THEN
      TYPECASE n OF
      | M3AST_AS.Index (x) =>
          IF M3CharPreds.Tr(x.as_array.sm_exp_type_spec) THEN
            M3Error.Warn(n, "indexing into changing array"); END; (* if *)

      | M3AST_AS.Constructor (x) =>
          IF M3CharPreds.Tr(M3CharPreds.M3TYPE_To_TYPE_SPEC(x.as_type)) THEN
            M3Error.Warn(n, "cons of val in changing type"); END; (* if *)

      | M3AST_AS.NEWCall (call) =>
          WITH tae = SeqM3AST_AS_EXP.First(
                       call.sm_actual_s).sm_exp_type_spec DO
            IF (ISTYPE(tae, M3AST_AS.Ref_type)
                  AND M3CharPreds.TC(
                        M3CharPreds.M3TYPE_To_TYPE_SPEC(
                          NARROW(tae, M3AST_AS.Ref_type).as_type),
                        M3CharPreds.Tr)) OR M3CharPreds.TCO(tae, M3CharPreds.Tr) THEN
              M3Error.Warn(call, "NEW of container of changing type"); END; END;

      | M3AST_AS.Select =>

      | M3AST_AS.BINARY (x) =>
          IF M3CharPreds.Tr(x.as_exp1.sm_exp_type_spec)
               OR M3CharPreds.Tr(x.as_exp2.sm_exp_type_spec) THEN
            M3Error.Warn(
                x, "binary operation on val in changing type"); END; (* if *)

      ELSE END;                  (* typecase *)
      END;                       (* if *)
  END Node;

BEGIN

END M3CharExprsToReplace.
