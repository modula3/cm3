(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE AstToVal;

IMPORT AstToType, StubUtils, Value, M3AST_AS, M3CBackEnd_C;
IMPORT M3AST_SM_F;
IMPORT RTTypeSRC;

PROCEDURE ProcessExp(
  <*UNUSED*>h: AstToType.Handle; exp: M3AST_AS.EXP): Value.T =
  BEGIN
    TYPECASE exp.sm_exp_value OF
      |  M3CBackEnd_C.Integer_value (int) =>
           RETURN NEW(Value.Ordinal, ord := int.sm_value)
      |  M3CBackEnd_C.Text_value (txt) => 
           RETURN NEW(Value.Txt, val := txt.sm_value)
      |  M3CBackEnd_C.Real_value (real) => 
           RETURN NEW(Value.Float, val := real.sm_value);
      |  M3CBackEnd_C.LongReal_value (lreal) => 
           RETURN NEW(Value.LongFloat, val := lreal.sm_value);
      |  M3CBackEnd_C.Extended_value (ereal) => 
           RETURN NEW(Value.Extended, val := ereal.sm_value);
      (*
      |  M3CBackEnd_C.Set_constructor_value => 
      |  M3CBackEnd_C.Array_or_record_constructor_value => 
      |  M3CBackEnd_C.Proc_value => 
      *)
      ELSE StubUtils.Die("AstToVal.ProcessExp: unsupported value: "
                         & RTTypeSRC.TypeName (exp.sm_exp_value));
    END;
    RETURN NIL;
  END ProcessExp;

BEGIN
END AstToVal.
