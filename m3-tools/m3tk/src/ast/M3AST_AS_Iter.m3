(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Mon Aug  8 16:10:01 PDT 1994 by kalsow    *)
(*      modified on Fri Jan 28 10:57:13 PST 1994 by detlefs   *)

MODULE M3AST_AS_Iter;

IMPORT AST, M3AST_AS;
IMPORT M3AST_AS_F;
IMPORT AST_Iter;
IMPORT SeqM3AST_AS_IMPORTED, 
    SeqM3AST_AS_Used_interface_id, SeqM3AST_AS_Used_def_id,
    SeqM3AST_AS_Import_item, SeqM3AST_AS_F_Interface_id,
    SeqM3AST_AS_REVELATION, SeqM3AST_AS_DECL_REVL, SeqM3AST_AS_Const_decl, 
    SeqM3AST_AS_TYPE_DECL, SeqM3AST_AS_Var_decl, SeqM3AST_AS_Exc_decl, 
    SeqM3AST_AS_Var_id, SeqM3AST_AS_Enum_id, 
    SeqM3AST_AS_Field_id, SeqM3AST_AS_FORMAL_ID, SeqM3AST_AS_Qual_used_id, 
    SeqM3AST_AS_Fields, SeqM3AST_AS_Method, SeqM3AST_AS_M3TYPE, 
    SeqM3AST_AS_Formal_param, SeqM3AST_AS_CONS_ELEM, 
    SeqM3AST_AS_EXP, SeqM3AST_AS_Actual, SeqM3AST_AS_Case, SeqM3AST_AS_STM, 
    SeqM3AST_AS_Elsif, SeqM3AST_AS_Tcase, SeqM3AST_AS_Handler, 
    SeqM3AST_AS_Binding, SeqM3AST_AS_RANGE_EXP;

IMPORT M3AST_AS_Iter_rep;


PROCEDURE By_update(iter: By_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_exp := nn;

    END;
  END By_update;


PROCEDURE By_next(iter: By_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_exp;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END By_next;

TYPE
  By_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.By;
  OVERRIDES
    next := By_next;
    update := By_update;
  END;

PROCEDURE Binding_update(iter: Binding_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_id := nn;

    | 1 => 
        iter.n.as_exp := nn;

    END;
  END Binding_update;


PROCEDURE Binding_next(iter: Binding_iter; VAR r: AST.NODE): BOOLEAN RAISES {
    }=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_id;

    | 1 => 
        r := iter.n.as_exp;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Binding_next;

TYPE
  Binding_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Binding;
  OVERRIDES
    next := Binding_next;
    update := Binding_update;
  END;

PROCEDURE Handler_update(iter: Handler_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_Qual_used_id.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Qual_used_id.Update(iter.n.as_qual_id_s, iter.iter0, 
                nn);
            RETURN ;
          END;

      | 1 => 
          iter.n.as_id := nn;
          RETURN ;

      | 2 => 
          IF NOT SeqM3AST_AS_STM.Exhausted(iter.iter1) THEN
            SeqM3AST_AS_STM.Update(iter.n.as_stm_s, iter.iter1, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Handler_update;


PROCEDURE Handler_next(iter: Handler_iter; VAR r: AST.NODE): BOOLEAN RAISES {
    }=
  VAR
    node0: M3AST_AS.Qual_used_id;
    node1: M3AST_AS.STM;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_Qual_used_id.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 1 => 
          r := iter.n.as_id;
          EXIT;

      | 2 => 
          IF SeqM3AST_AS_STM.Next(iter.iter1, node1) THEN
            r := node1;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Handler_next;

TYPE
  Handler_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Handler;
    iter0: SeqM3AST_AS_Qual_used_id.Iter;
    iter1: SeqM3AST_AS_STM.Iter;
  OVERRIDES
    next := Handler_next;
    update := Handler_update;
  END;

PROCEDURE Tcase_update(iter: Tcase_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_M3TYPE.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_M3TYPE.Update(iter.n.as_type_s, iter.iter0, nn);
            RETURN ;
          END;

      | 1 => 
          iter.n.as_id := nn;
          RETURN ;

      | 2 => 
          IF NOT SeqM3AST_AS_STM.Exhausted(iter.iter1) THEN
            SeqM3AST_AS_STM.Update(iter.n.as_stm_s, iter.iter1, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Tcase_update;


PROCEDURE Tcase_next(iter: Tcase_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.M3TYPE;
    node1: M3AST_AS.STM;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_M3TYPE.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 1 => 
          r := iter.n.as_id;
          EXIT;

      | 2 => 
          IF SeqM3AST_AS_STM.Next(iter.iter1, node1) THEN
            r := node1;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Tcase_next;

TYPE
  Tcase_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Tcase;
    iter0: SeqM3AST_AS_M3TYPE.Iter;
    iter1: SeqM3AST_AS_STM.Iter;
  OVERRIDES
    next := Tcase_next;
    update := Tcase_update;
  END;

PROCEDURE Try_finally_update(iter: Try_finally_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_STM.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_STM.Update(iter.n.as_stm_s, iter.iter0, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Try_finally_update;


PROCEDURE Try_finally_next(iter: Try_finally_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  VAR
    node0: M3AST_AS.STM;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_STM.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
  END Try_finally_next;

TYPE
  Try_finally_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Try_finally;
    iter0: SeqM3AST_AS_STM.Iter;
  OVERRIDES
    next := Try_finally_next;
    update := Try_finally_update;
  END;

PROCEDURE Try_except_update(iter: Try_except_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_Handler.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Handler.Update(iter.n.as_handler_s, iter.iter0, nn);
            RETURN ;
          END;

      | 1 => 
          iter.n.as_else := nn;
          RETURN ;

      | 2 => 
          IF NOT SeqM3AST_AS_STM.Exhausted(iter.iter1) THEN
            SeqM3AST_AS_STM.Update(iter.n.as_stm_s, iter.iter1, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Try_except_update;


PROCEDURE Try_except_next(iter: Try_except_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  VAR
    node0: M3AST_AS.Handler;
    node1: M3AST_AS.STM;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_Handler.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 1 => 
          r := iter.n.as_else;
          EXIT;

      | 2 => 
          IF SeqM3AST_AS_STM.Next(iter.iter1, node1) THEN
            r := node1;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Try_except_next;

TYPE
  Try_except_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Try_except;
    iter0: SeqM3AST_AS_Handler.Iter;
    iter1: SeqM3AST_AS_STM.Iter;
  OVERRIDES
    next := Try_except_next;
    update := Try_except_update;
  END;

PROCEDURE Elsif_update(iter: Elsif_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_exp := nn;
          RETURN ;

      | 1 => 
          IF NOT SeqM3AST_AS_STM.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_STM.Update(iter.n.as_stm_s, iter.iter0, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Elsif_update;


PROCEDURE Elsif_next(iter: Elsif_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.STM;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_exp;
          EXIT;

      | 1 => 
          IF SeqM3AST_AS_STM.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Elsif_next;

TYPE
  Elsif_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Elsif;
    iter0: SeqM3AST_AS_STM.Iter;
  OVERRIDES
    next := Elsif_next;
    update := Elsif_update;
  END;

PROCEDURE Else_stm_update(iter: Else_stm_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_STM.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_STM.Update(iter.n.as_stm_s, iter.iter0, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Else_stm_update;


PROCEDURE Else_stm_next(iter: Else_stm_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  VAR
    node0: M3AST_AS.STM;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_STM.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
  END Else_stm_next;

TYPE
  Else_stm_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Else_stm;
    iter0: SeqM3AST_AS_STM.Iter;
  OVERRIDES
    next := Else_stm_next;
    update := Else_stm_update;
  END;

PROCEDURE Case_update(iter: Case_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_RANGE_EXP.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_RANGE_EXP.Update(iter.n.as_case_label_s, iter.iter0, 
                nn);
            RETURN ;
          END;

      | 1 => 
          IF NOT SeqM3AST_AS_STM.Exhausted(iter.iter1) THEN
            SeqM3AST_AS_STM.Update(iter.n.as_stm_s, iter.iter1, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Case_update;


PROCEDURE Case_next(iter: Case_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.RANGE_EXP;
    node1: M3AST_AS.STM;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_RANGE_EXP.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 1 => 
          IF SeqM3AST_AS_STM.Next(iter.iter1, node1) THEN
            r := node1;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
  END Case_next;

TYPE
  Case_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Case;
    iter0: SeqM3AST_AS_RANGE_EXP.Iter;
    iter1: SeqM3AST_AS_STM.Iter;
  OVERRIDES
    next := Case_next;
    update := Case_update;
  END;

PROCEDURE Block_update(iter: Block_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_DECL_REVL.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_DECL_REVL.Update(iter.n.as_decl_s, iter.iter0, nn);
            RETURN ;
          END;

      | 1 => 
          IF NOT SeqM3AST_AS_STM.Exhausted(iter.iter1) THEN
            SeqM3AST_AS_STM.Update(iter.n.as_stm_s, iter.iter1, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Block_update;


PROCEDURE Block_next(iter: Block_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.DECL_REVL;
    node1: M3AST_AS.STM;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_DECL_REVL.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 1 => 
          IF SeqM3AST_AS_STM.Next(iter.iter1, node1) THEN
            r := node1;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
  END Block_next;

TYPE
  Block_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Block;
    iter0: SeqM3AST_AS_DECL_REVL.Iter;
    iter1: SeqM3AST_AS_STM.Iter;
  OVERRIDES
    next := Block_next;
    update := Block_update;
  END;

PROCEDURE With_st_update(iter: With_st_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_Binding.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Binding.Update(iter.n.as_binding_s, iter.iter0, nn);
            RETURN ;
          END;

      | 1 => 
          IF NOT SeqM3AST_AS_STM.Exhausted(iter.iter1) THEN
            SeqM3AST_AS_STM.Update(iter.n.as_stm_s, iter.iter1, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END With_st_update;


PROCEDURE With_st_next(iter: With_st_iter; VAR r: AST.NODE): BOOLEAN RAISES {
    }=
  VAR
    node0: M3AST_AS.Binding;
    node1: M3AST_AS.STM;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_Binding.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 1 => 
          IF SeqM3AST_AS_STM.Next(iter.iter1, node1) THEN
            r := node1;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
  END With_st_next;

TYPE
  With_st_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.With_st;
    iter0: SeqM3AST_AS_Binding.Iter;
    iter1: SeqM3AST_AS_STM.Iter;
  OVERRIDES
    next := With_st_next;
    update := With_st_update;
  END;

PROCEDURE While_st_update(iter: While_st_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_exp := nn;
          RETURN ;

      | 1 => 
          IF NOT SeqM3AST_AS_STM.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_STM.Update(iter.n.as_stm_s, iter.iter0, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END While_st_update;


PROCEDURE While_st_next(iter: While_st_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  VAR
    node0: M3AST_AS.STM;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_exp;
          EXIT;

      | 1 => 
          IF SeqM3AST_AS_STM.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END While_st_next;

TYPE
  While_st_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.While_st;
    iter0: SeqM3AST_AS_STM.Iter;
  OVERRIDES
    next := While_st_next;
    update := While_st_update;
  END;

PROCEDURE Typecase_st_update(iter: Typecase_st_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_exp := nn;
          RETURN ;

      | 1 => 
          IF NOT SeqM3AST_AS_Tcase.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Tcase.Update(iter.n.as_tcase_s, iter.iter0, nn);
            RETURN ;
          END;

      | 2 => 
          iter.n.as_else := nn;
          RETURN ;

      END;
      INC(iter.slot);
    END;
  END Typecase_st_update;


PROCEDURE Typecase_st_next(iter: Typecase_st_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  VAR
    node0: M3AST_AS.Tcase;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_exp;
          EXIT;

      | 1 => 
          IF SeqM3AST_AS_Tcase.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 2 => 
          r := iter.n.as_else;
          EXIT;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Typecase_st_next;

TYPE
  Typecase_st_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Typecase_st;
    iter0: SeqM3AST_AS_Tcase.Iter;
  OVERRIDES
    next := Typecase_st_next;
    update := Typecase_st_update;
  END;

PROCEDURE Try_st_update(iter: Try_st_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_STM.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_STM.Update(iter.n.as_stm_s, iter.iter0, nn);
            RETURN ;
          END;

      | 1 => 
          iter.n.as_try_tail := nn;
          RETURN ;

      END;
      INC(iter.slot);
    END;
  END Try_st_update;


PROCEDURE Try_st_next(iter: Try_st_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.STM;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_STM.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 1 => 
          r := iter.n.as_try_tail;
          EXIT;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Try_st_next;

TYPE
  Try_st_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Try_st;
    iter0: SeqM3AST_AS_STM.Iter;
  OVERRIDES
    next := Try_st_next;
    update := Try_st_update;
  END;

PROCEDURE Return_st_update(iter: Return_st_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_exp := nn;

    END;
  END Return_st_update;


PROCEDURE Return_st_next(iter: Return_st_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_exp;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Return_st_next;

TYPE
  Return_st_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Return_st;
  OVERRIDES
    next := Return_st_next;
    update := Return_st_update;
  END;

PROCEDURE Repeat_st_update(iter: Repeat_st_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_STM.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_STM.Update(iter.n.as_stm_s, iter.iter0, nn);
            RETURN ;
          END;

      | 1 => 
          iter.n.as_exp := nn;
          RETURN ;

      END;
      INC(iter.slot);
    END;
  END Repeat_st_update;


PROCEDURE Repeat_st_next(iter: Repeat_st_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  VAR
    node0: M3AST_AS.STM;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_STM.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 1 => 
          r := iter.n.as_exp;
          EXIT;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Repeat_st_next;

TYPE
  Repeat_st_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Repeat_st;
    iter0: SeqM3AST_AS_STM.Iter;
  OVERRIDES
    next := Repeat_st_next;
    update := Repeat_st_update;
  END;

PROCEDURE Raise_st_update(iter: Raise_st_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_qual_id := nn;

    | 1 => 
        iter.n.as_exp_void := nn;

    END;
  END Raise_st_update;


PROCEDURE Raise_st_next(iter: Raise_st_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_qual_id;

    | 1 => 
        r := iter.n.as_exp_void;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Raise_st_next;

TYPE
  Raise_st_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Raise_st;
  OVERRIDES
    next := Raise_st_next;
    update := Raise_st_update;
  END;

PROCEDURE Loop_st_update(iter: Loop_st_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_STM.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_STM.Update(iter.n.as_stm_s, iter.iter0, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Loop_st_update;


PROCEDURE Loop_st_next(iter: Loop_st_iter; VAR r: AST.NODE): BOOLEAN RAISES {
    }=
  VAR
    node0: M3AST_AS.STM;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_STM.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
  END Loop_st_next;

TYPE
  Loop_st_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Loop_st;
    iter0: SeqM3AST_AS_STM.Iter;
  OVERRIDES
    next := Loop_st_next;
    update := Loop_st_update;
  END;

PROCEDURE Lock_st_update(iter: Lock_st_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_exp := nn;
          RETURN ;

      | 1 => 
          IF NOT SeqM3AST_AS_STM.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_STM.Update(iter.n.as_stm_s, iter.iter0, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Lock_st_update;


PROCEDURE Lock_st_next(iter: Lock_st_iter; VAR r: AST.NODE): BOOLEAN RAISES {
    }=
  VAR
    node0: M3AST_AS.STM;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_exp;
          EXIT;

      | 1 => 
          IF SeqM3AST_AS_STM.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Lock_st_next;

TYPE
  Lock_st_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Lock_st;
    iter0: SeqM3AST_AS_STM.Iter;
  OVERRIDES
    next := Lock_st_next;
    update := Lock_st_update;
  END;

PROCEDURE If_st_update(iter: If_st_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_exp := nn;
          RETURN ;

      | 1 => 
          IF NOT SeqM3AST_AS_STM.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_STM.Update(iter.n.as_stm_s, iter.iter0, nn);
            RETURN ;
          END;

      | 2 => 
          IF NOT SeqM3AST_AS_Elsif.Exhausted(iter.iter1) THEN
            SeqM3AST_AS_Elsif.Update(iter.n.as_elsif_s, iter.iter1, nn);
            RETURN ;
          END;

      | 3 => 
          iter.n.as_else := nn;
          RETURN ;

      END;
      INC(iter.slot);
    END;
  END If_st_update;


PROCEDURE If_st_next(iter: If_st_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.STM;
    node1: M3AST_AS.Elsif;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_exp;
          EXIT;

      | 1 => 
          IF SeqM3AST_AS_STM.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 2 => 
          IF SeqM3AST_AS_Elsif.Next(iter.iter1, node1) THEN
            r := node1;
            RETURN TRUE;
          END;

      | 3 => 
          r := iter.n.as_else;
          EXIT;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END If_st_next;

TYPE
  If_st_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.If_st;
    iter0: SeqM3AST_AS_STM.Iter;
    iter1: SeqM3AST_AS_Elsif.Iter;
  OVERRIDES
    next := If_st_next;
    update := If_st_update;
  END;

PROCEDURE For_st_update(iter: For_st_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_id := nn;
          RETURN ;

      | 1 => 
          iter.n.as_from := nn;
          RETURN ;

      | 2 => 
          iter.n.as_to := nn;
          RETURN ;

      | 3 => 
          iter.n.as_by := nn;
          RETURN ;

      | 4 => 
          IF NOT SeqM3AST_AS_STM.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_STM.Update(iter.n.as_stm_s, iter.iter0, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END For_st_update;


PROCEDURE For_st_next(iter: For_st_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.STM;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_id;
          EXIT;

      | 1 => 
          r := iter.n.as_from;
          EXIT;

      | 2 => 
          r := iter.n.as_to;
          EXIT;

      | 3 => 
          r := iter.n.as_by;
          EXIT;

      | 4 => 
          IF SeqM3AST_AS_STM.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END For_st_next;

TYPE
  For_st_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.For_st;
    iter0: SeqM3AST_AS_STM.Iter;
  OVERRIDES
    next := For_st_next;
    update := For_st_update;
  END;

PROCEDURE Exit_st_update(<*UNUSED*> iter: Exit_st_iter;
                         <*UNUSED*> nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    END;
  END Exit_st_update;


PROCEDURE Exit_st_next(<*UNUSED*>iter: Exit_st_iter;
                       <*UNUSED*>VAR r: AST.NODE): BOOLEAN RAISES {
    }=
  BEGIN
    RETURN FALSE;
  END Exit_st_next;

TYPE
  Exit_st_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Exit_st;
  OVERRIDES
    next := Exit_st_next;
    update := Exit_st_update;
  END;

PROCEDURE Eval_st_update(iter: Eval_st_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_exp := nn;

    END;
  END Eval_st_update;


PROCEDURE Eval_st_next(iter: Eval_st_iter; VAR r: AST.NODE): BOOLEAN RAISES {
    }=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_exp;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Eval_st_next;

TYPE
  Eval_st_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Eval_st;
  OVERRIDES
    next := Eval_st_next;
    update := Eval_st_update;
  END;

PROCEDURE Case_st_update(iter: Case_st_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_exp := nn;
          RETURN ;

      | 1 => 
          IF NOT SeqM3AST_AS_Case.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Case.Update(iter.n.as_case_s, iter.iter0, nn);
            RETURN ;
          END;

      | 2 => 
          iter.n.as_else := nn;
          RETURN ;

      END;
      INC(iter.slot);
    END;
  END Case_st_update;


PROCEDURE Case_st_next(iter: Case_st_iter; VAR r: AST.NODE): BOOLEAN RAISES {
    }=
  VAR
    node0: M3AST_AS.Case;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_exp;
          EXIT;

      | 1 => 
          IF SeqM3AST_AS_Case.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 2 => 
          r := iter.n.as_else;
          EXIT;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Case_st_next;

TYPE
  Case_st_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Case_st;
    iter0: SeqM3AST_AS_Case.Iter;
  OVERRIDES
    next := Case_st_next;
    update := Case_st_update;
  END;

PROCEDURE Call_st_update(iter: Call_st_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_call := nn;

    END;
  END Call_st_update;


PROCEDURE Call_st_next(iter: Call_st_iter; VAR r: AST.NODE): BOOLEAN RAISES {
    }=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_call;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Call_st_next;

TYPE
  Call_st_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Call_st;
  OVERRIDES
    next := Call_st_next;
    update := Call_st_update;
  END;

PROCEDURE Assign_st_update(iter: Assign_st_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_lhs_exp := nn;

    | 1 => 
        iter.n.as_rhs_exp := nn;

    END;
  END Assign_st_update;


PROCEDURE Assign_st_next(iter: Assign_st_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_lhs_exp;

    | 1 => 
        r := iter.n.as_rhs_exp;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Assign_st_next;

TYPE
  Assign_st_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Assign_st;
  OVERRIDES
    next := Assign_st_next;
    update := Assign_st_update;
  END;

PROCEDURE Actual_update(iter: Actual_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_id := nn;

    | 1 => 
        iter.n.as_exp_type := nn;

    END;
  END Actual_update;


PROCEDURE Actual_next(iter: Actual_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_id;

    | 1 => 
        r := iter.n.as_exp_type;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Actual_next;

TYPE
  Actual_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Actual;
  OVERRIDES
    next := Actual_next;
    update := Actual_update;
  END;

PROCEDURE Index_update(iter: Index_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_array := nn;
          RETURN ;

      | 1 => 
          IF NOT SeqM3AST_AS_EXP.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_EXP.Update(iter.n.as_exp_s, iter.iter0, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Index_update;


PROCEDURE Index_next(iter: Index_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.EXP;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_array;
          EXIT;

      | 1 => 
          IF SeqM3AST_AS_EXP.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Index_next;

TYPE
  Index_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Index;
    iter0: SeqM3AST_AS_EXP.Iter;
  OVERRIDES
    next := Index_next;
    update := Index_update;
  END;

PROCEDURE Call_update(iter: Call_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_callexp := nn;
          RETURN ;

      | 1 => 
          IF NOT SeqM3AST_AS_Actual.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Actual.Update(iter.n.as_param_s, iter.iter0, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Call_update;


PROCEDURE Call_next(iter: Call_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.Actual;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_callexp;
          EXIT;

      | 1 => 
          IF SeqM3AST_AS_Actual.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Call_next;

TYPE
  Call_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Call;
    iter0: SeqM3AST_AS_Actual.Iter;
  OVERRIDES
    next := Call_next;
    update := Call_update;
  END;

PROCEDURE Select_update(iter: Select_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_exp := nn;

    | 1 => 
        iter.n.as_id := nn;

    END;
  END Select_update;


PROCEDURE Select_next(iter: Select_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_exp;

    | 1 => 
        r := iter.n.as_id;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Select_next;

TYPE
  Select_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Select;
  OVERRIDES
    next := Select_next;
    update := Select_update;
  END;

PROCEDURE UNARY_update(iter: UNARY_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_exp := nn;

    END;
  END UNARY_update;


PROCEDURE UNARY_next(iter: UNARY_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_exp;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END UNARY_next;

TYPE
  UNARY_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.UNARY;
  OVERRIDES
    next := UNARY_next;
    update := UNARY_update;
  END;

PROCEDURE BINARY_update(iter: BINARY_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_exp1 := nn;

    | 1 => 
        iter.n.as_exp2 := nn;

    END;
  END BINARY_update;


PROCEDURE BINARY_next(iter: BINARY_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_exp1;

    | 1 => 
        r := iter.n.as_exp2;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END BINARY_next;

TYPE
  BINARY_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.BINARY;
  OVERRIDES
    next := BINARY_next;
    update := BINARY_update;
  END;

PROCEDURE Propagate_update(iter: Propagate_iter;
                           <*UNUSED*> nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    END;
  END Propagate_update;


PROCEDURE Propagate_next(<*UNUSED*> iter: Propagate_iter; <*UNUSED*> VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  BEGIN
    RETURN FALSE;
  END Propagate_next;

TYPE
  Propagate_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Propagate;
  OVERRIDES
    next := Propagate_next;
    update := Propagate_update;
  END;

PROCEDURE Actual_elem_update(iter: Actual_elem_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_actual := nn;

    END;
  END Actual_elem_update;


PROCEDURE Actual_elem_next(iter: Actual_elem_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_actual;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Actual_elem_next;

TYPE
  Actual_elem_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Actual_elem;
  OVERRIDES
    next := Actual_elem_next;
    update := Actual_elem_update;
  END;

PROCEDURE RANGE_EXP_elem_update(iter: RANGE_EXP_elem_iter; nn: AST.NODE)
    RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_range_exp := nn;

    END;
  END RANGE_EXP_elem_update;


PROCEDURE RANGE_EXP_elem_next(iter: RANGE_EXP_elem_iter; VAR r: AST.NODE): 
    BOOLEAN RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_range_exp;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END RANGE_EXP_elem_next;

TYPE
  RANGE_EXP_elem_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.RANGE_EXP_elem;
  OVERRIDES
    next := RANGE_EXP_elem_next;
    update := RANGE_EXP_elem_update;
  END;

PROCEDURE Constructor_update(iter: Constructor_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_type := nn;
          RETURN ;

      | 1 => 
          IF NOT SeqM3AST_AS_CONS_ELEM.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_CONS_ELEM.Update(iter.n.as_element_s, iter.iter0, nn);
            RETURN ;
          END;

      | 2 => 
          iter.n.as_propagate := nn;
          RETURN ;

      END;
      INC(iter.slot);
    END;
  END Constructor_update;


PROCEDURE Constructor_next(iter: Constructor_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  VAR
    node0: M3AST_AS.CONS_ELEM;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_type;
          EXIT;

      | 1 => 
          IF SeqM3AST_AS_CONS_ELEM.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 2 => 
          r := iter.n.as_propagate;
          EXIT;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Constructor_next;

TYPE
  Constructor_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Constructor;
    iter0: SeqM3AST_AS_CONS_ELEM.Iter;
  OVERRIDES
    next := Constructor_next;
    update := Constructor_update;
  END;

PROCEDURE Range_update(iter: Range_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_exp1 := nn;

    | 1 => 
        iter.n.as_exp2 := nn;

    END;
  END Range_update;


PROCEDURE Range_next(iter: Range_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_exp1;

    | 1 => 
        r := iter.n.as_exp2;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Range_next;

TYPE
  Range_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Range;
  OVERRIDES
    next := Range_next;
    update := Range_update;
  END;

PROCEDURE Range_EXP_update(iter: Range_EXP_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_exp := nn;

    END;
  END Range_EXP_update;


PROCEDURE Range_EXP_next(iter: Range_EXP_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_exp;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Range_EXP_next;

TYPE
  Range_EXP_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Range_EXP;
  OVERRIDES
    next := Range_EXP_next;
    update := Range_EXP_update;
  END;

PROCEDURE Raisees_some_update(iter: Raisees_some_iter; nn: AST.NODE) RAISES {
    }=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_Qual_used_id.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Qual_used_id.Update(iter.n.as_raisees_s, iter.iter0, 
                nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Raisees_some_update;


PROCEDURE Raisees_some_next(iter: Raisees_some_iter; VAR r: AST.NODE): 
    BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.Qual_used_id;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_Qual_used_id.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
  END Raisees_some_next;

TYPE
  Raisees_some_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Raisees_some;
    iter0: SeqM3AST_AS_Qual_used_id.Iter;
  OVERRIDES
    next := Raisees_some_next;
    update := Raisees_some_update;
  END;

PROCEDURE Formal_param_update(iter: Formal_param_iter; nn: AST.NODE) RAISES {
    }=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_FORMAL_ID.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_FORMAL_ID.Update(iter.n.as_id_s, iter.iter0, nn);
            RETURN ;
          END;

      | 1 => 
          iter.n.as_formal_type := nn;
          RETURN ;

      | 2 => 
          iter.n.as_default := nn;
          RETURN ;

      END;
      INC(iter.slot);
    END;
  END Formal_param_update;


PROCEDURE Formal_param_next(iter: Formal_param_iter; VAR r: AST.NODE): 
    BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.FORMAL_ID;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_FORMAL_ID.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 1 => 
          r := iter.n.as_formal_type;
          EXIT;

      | 2 => 
          r := iter.n.as_default;
          EXIT;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Formal_param_next;

TYPE
  Formal_param_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Formal_param;
    iter0: SeqM3AST_AS_FORMAL_ID.Iter;
  OVERRIDES
    next := Formal_param_next;
    update := Formal_param_update;
  END;

PROCEDURE Override_update(iter: Override_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_id := nn;

    | 1 => 
        iter.n.as_default := nn;

    END;
  END Override_update;


PROCEDURE Override_next(iter: Override_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_id;

    | 1 => 
        r := iter.n.as_default;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Override_next;

TYPE
  Override_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Override;
  OVERRIDES
    next := Override_next;
    update := Override_update;
  END;

PROCEDURE Method_update(iter: Method_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_type := nn;

    | 1 => 
        iter.n.as_id := nn;

    | 2 => 
        iter.n.as_default := nn;

    END;
  END Method_update;


PROCEDURE Method_next(iter: Method_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_type;

    | 1 => 
        r := iter.n.as_id;

    | 2 => 
        r := iter.n.as_default;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Method_next;

TYPE
  Method_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Method;
  OVERRIDES
    next := Method_next;
    update := Method_update;
  END;

PROCEDURE Fields_update(iter: Fields_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_Field_id.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Field_id.Update(iter.n.as_id_s, iter.iter0, nn);
            RETURN ;
          END;

      | 1 => 
          iter.n.as_type := nn;
          RETURN ;

      | 2 => 
          iter.n.as_default := nn;
          RETURN ;

      END;
      INC(iter.slot);
    END;
  END Fields_update;


PROCEDURE Fields_next(iter: Fields_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.Field_id;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_Field_id.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 1 => 
          r := iter.n.as_type;
          EXIT;

      | 2 => 
          r := iter.n.as_default;
          EXIT;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Fields_next;

TYPE
  Fields_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Fields;
    iter0: SeqM3AST_AS_Field_id.Iter;
  OVERRIDES
    next := Fields_next;
    update := Fields_update;
  END;

PROCEDURE Brand_update(iter: Brand_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_exp := nn;

    END;
  END Brand_update;


PROCEDURE Brand_next(iter: Brand_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_exp;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Brand_next;

TYPE
  Brand_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Brand;
  OVERRIDES
    next := Brand_next;
    update := Brand_update;
  END;

PROCEDURE Opaque_type_update(iter: Opaque_type_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_type := nn;

    END;
  END Opaque_type_update;


PROCEDURE Opaque_type_next(iter: Opaque_type_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_type;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Opaque_type_next;

TYPE
  Opaque_type_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Opaque_type;
  OVERRIDES
    next := Opaque_type_next;
    update := Opaque_type_update;
  END;

PROCEDURE Packed_type_update(iter: Packed_type_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_exp := nn;

    | 1 => 
        iter.n.as_type := nn;

    END;
  END Packed_type_update;


PROCEDURE Packed_type_next(iter: Packed_type_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_exp;

    | 1 => 
        r := iter.n.as_type;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Packed_type_next;

TYPE
  Packed_type_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Packed_type;
  OVERRIDES
    next := Packed_type_next;
    update := Packed_type_update;
  END;

PROCEDURE Ref_type_update(iter: Ref_type_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_trace_mode := nn;

    | 1 => 
        iter.n.as_brand := nn;

    | 2 => 
        iter.n.as_type := nn;

    END;
  END Ref_type_update;


PROCEDURE Ref_type_next(iter: Ref_type_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_trace_mode;

    | 1 => 
        r := iter.n.as_brand;

    | 2 => 
        r := iter.n.as_type;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Ref_type_next;

TYPE
  Ref_type_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Ref_type;
  OVERRIDES
    next := Ref_type_next;
    update := Ref_type_update;
  END;

PROCEDURE Procedure_type_update(iter: Procedure_type_iter; nn: AST.NODE)
    RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_Formal_param.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Formal_param.Update(iter.n.as_formal_param_s, iter.
                iter0, nn);
            RETURN ;
          END;

      | 1 => 
          iter.n.as_result_type := nn;
          RETURN ;

      | 2 => 
          iter.n.as_raises := nn;
          RETURN ;

      END;
      INC(iter.slot);
    END;
  END Procedure_type_update;


PROCEDURE Procedure_type_next(iter: Procedure_type_iter; VAR r: AST.NODE): 
    BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.Formal_param;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_Formal_param.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 1 => 
          r := iter.n.as_result_type;
          EXIT;

      | 2 => 
          r := iter.n.as_raises;
          EXIT;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Procedure_type_next;

TYPE
  Procedure_type_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Procedure_type;
    iter0: SeqM3AST_AS_Formal_param.Iter;
  OVERRIDES
    next := Procedure_type_next;
    update := Procedure_type_update;
  END;

PROCEDURE Set_type_update(iter: Set_type_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_type := nn;

    END;
  END Set_type_update;


PROCEDURE Set_type_next(iter: Set_type_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_type;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Set_type_next;

TYPE
  Set_type_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Set_type;
  OVERRIDES
    next := Set_type_next;
    update := Set_type_update;
  END;

PROCEDURE Object_type_update(iter: Object_type_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_ancestor := nn;
          RETURN ;

      | 1 => 
          iter.n.as_brand := nn;
          RETURN ;

      | 2 => 
          IF NOT SeqM3AST_AS_Fields.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Fields.Update(iter.n.as_fields_s, iter.iter0, nn);
            RETURN ;
          END;

      | 3 => 
          IF NOT SeqM3AST_AS_Method.Exhausted(iter.iter1) THEN
            SeqM3AST_AS_Method.Update(iter.n.as_method_s, iter.iter1, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Object_type_update;


PROCEDURE Object_type_next(iter: Object_type_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  VAR
    node0: M3AST_AS.Fields;
    node1: M3AST_AS.Method;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_ancestor;
          EXIT;

      | 1 => 
          r := iter.n.as_brand;
          EXIT;

      | 2 => 
          IF SeqM3AST_AS_Fields.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 3 => 
          IF SeqM3AST_AS_Method.Next(iter.iter1, node1) THEN
            r := node1;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Object_type_next;

TYPE
  Object_type_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Object_type;
    iter0: SeqM3AST_AS_Fields.Iter;
    iter1: SeqM3AST_AS_Method.Iter;
  OVERRIDES
    next := Object_type_next;
    update := Object_type_update;
  END;

PROCEDURE Record_type_update(iter: Record_type_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_Fields.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Fields.Update(iter.n.as_fields_s, iter.iter0, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Record_type_update;


PROCEDURE Record_type_next(iter: Record_type_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  VAR
    node0: M3AST_AS.Fields;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_Fields.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
  END Record_type_next;

TYPE
  Record_type_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Record_type;
    iter0: SeqM3AST_AS_Fields.Iter;
  OVERRIDES
    next := Record_type_next;
    update := Record_type_update;
  END;

PROCEDURE Array_type_update(iter: Array_type_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_M3TYPE.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_M3TYPE.Update(iter.n.as_indextype_s, iter.iter0, nn);
            RETURN ;
          END;

      | 1 => 
          iter.n.as_elementtype := nn;
          RETURN ;

      END;
      INC(iter.slot);
    END;
  END Array_type_update;


PROCEDURE Array_type_next(iter: Array_type_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  VAR
    node0: M3AST_AS.M3TYPE;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_M3TYPE.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 1 => 
          r := iter.n.as_elementtype;
          EXIT;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Array_type_next;

TYPE
  Array_type_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Array_type;
    iter0: SeqM3AST_AS_M3TYPE.Iter;
  OVERRIDES
    next := Array_type_next;
    update := Array_type_update;
  END;

PROCEDURE Subrange_type_update(iter: Subrange_type_iter; nn: AST.NODE)
    RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_range := nn;

    END;
  END Subrange_type_update;


PROCEDURE Subrange_type_next(iter: Subrange_type_iter; VAR r: AST.NODE): 
    BOOLEAN RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_range;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Subrange_type_next;

TYPE
  Subrange_type_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Subrange_type;
  OVERRIDES
    next := Subrange_type_next;
    update := Subrange_type_update;
  END;

PROCEDURE Enumeration_type_update(iter: Enumeration_type_iter; nn: AST.NODE)
    RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_Enum_id.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Enum_id.Update(iter.n.as_id_s, iter.iter0, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Enumeration_type_update;


PROCEDURE Enumeration_type_next(iter: Enumeration_type_iter; VAR r: AST.NODE)
    : BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.Enum_id;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_Enum_id.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
  END Enumeration_type_next;

TYPE
  Enumeration_type_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Enumeration_type;
    iter0: SeqM3AST_AS_Enum_id.Iter;
  OVERRIDES
    next := Enumeration_type_next;
    update := Enumeration_type_update;
  END;

PROCEDURE Root_type_update(iter: Root_type_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_trace_mode := nn;

    END;
  END Root_type_update;


PROCEDURE Root_type_next(iter: Root_type_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_trace_mode;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Root_type_next;

TYPE
  Root_type_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Root_type;
  OVERRIDES
    next := Root_type_next;
    update := Root_type_update;
  END;

PROCEDURE Named_type_update(iter: Named_type_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_qual_id := nn;

    END;
  END Named_type_update;


PROCEDURE Named_type_next(iter: Named_type_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_qual_id;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Named_type_next;

TYPE
  Named_type_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Named_type;
  OVERRIDES
    next := Named_type_next;
    update := Named_type_update;
  END;

PROCEDURE Concrete_reveal_update(iter: Concrete_reveal_iter; nn: AST.NODE)
    RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_qual_id := nn;

    | 1 => 
        iter.n.as_type := nn;

    END;
  END Concrete_reveal_update;


PROCEDURE Concrete_reveal_next(iter: Concrete_reveal_iter; VAR r: AST.NODE): 
    BOOLEAN RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_qual_id;

    | 1 => 
        r := iter.n.as_type;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Concrete_reveal_next;

TYPE
  Concrete_reveal_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Concrete_reveal;
  OVERRIDES
    next := Concrete_reveal_next;
    update := Concrete_reveal_update;
  END;

PROCEDURE Subtype_reveal_update(iter: Subtype_reveal_iter; nn: AST.NODE)
    RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_qual_id := nn;

    | 1 => 
        iter.n.as_type := nn;

    END;
  END Subtype_reveal_update;


PROCEDURE Subtype_reveal_next(iter: Subtype_reveal_iter; VAR r: AST.NODE): 
    BOOLEAN RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_qual_id;

    | 1 => 
        r := iter.n.as_type;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Subtype_reveal_next;

TYPE
  Subtype_reveal_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Subtype_reveal;
  OVERRIDES
    next := Subtype_reveal_next;
    update := Subtype_reveal_update;
  END;

PROCEDURE Concrete_decl_update(iter: Concrete_decl_iter; nn: AST.NODE)
    RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_id := nn;

    | 1 => 
        iter.n.as_type := nn;

    END;
  END Concrete_decl_update;


PROCEDURE Concrete_decl_next(iter: Concrete_decl_iter; VAR r: AST.NODE): 
    BOOLEAN RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_id;

    | 1 => 
        r := iter.n.as_type;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Concrete_decl_next;

TYPE
  Concrete_decl_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Concrete_decl;
  OVERRIDES
    next := Concrete_decl_next;
    update := Concrete_decl_update;
  END;

PROCEDURE Subtype_decl_update(iter: Subtype_decl_iter; nn: AST.NODE) RAISES {
    }=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_id := nn;

    | 1 => 
        iter.n.as_type := nn;

    END;
  END Subtype_decl_update;


PROCEDURE Subtype_decl_next(iter: Subtype_decl_iter; VAR r: AST.NODE): 
    BOOLEAN RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_id;

    | 1 => 
        r := iter.n.as_type;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Subtype_decl_next;

TYPE
  Subtype_decl_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Subtype_decl;
  OVERRIDES
    next := Subtype_decl_next;
    update := Subtype_decl_update;
  END;

PROCEDURE Exc_decl_update(iter: Exc_decl_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_id := nn;

    | 1 => 
        iter.n.as_type := nn;

    END;
  END Exc_decl_update;


PROCEDURE Exc_decl_next(iter: Exc_decl_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_id;

    | 1 => 
        r := iter.n.as_type;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Exc_decl_next;

TYPE
  Exc_decl_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Exc_decl;
  OVERRIDES
    next := Exc_decl_next;
    update := Exc_decl_update;
  END;

PROCEDURE Var_decl_update(iter: Var_decl_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_Var_id.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Var_id.Update(iter.n.as_id_s, iter.iter0, nn);
            RETURN ;
          END;

      | 1 => 
          iter.n.as_type := nn;
          RETURN ;

      | 2 => 
          iter.n.as_default := nn;
          RETURN ;

      END;
      INC(iter.slot);
    END;
  END Var_decl_update;


PROCEDURE Var_decl_next(iter: Var_decl_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  VAR
    node0: M3AST_AS.Var_id;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_Var_id.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 1 => 
          r := iter.n.as_type;
          EXIT;

      | 2 => 
          r := iter.n.as_default;
          EXIT;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Var_decl_next;

TYPE
  Var_decl_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Var_decl;
    iter0: SeqM3AST_AS_Var_id.Iter;
  OVERRIDES
    next := Var_decl_next;
    update := Var_decl_update;
  END;

PROCEDURE Const_decl_update(iter: Const_decl_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_id := nn;

    | 1 => 
        iter.n.as_type := nn;

    | 2 => 
        iter.n.as_exp := nn;

    END;
  END Const_decl_update;


PROCEDURE Const_decl_next(iter: Const_decl_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_id;

    | 1 => 
        r := iter.n.as_type;

    | 2 => 
        r := iter.n.as_exp;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Const_decl_next;

TYPE
  Const_decl_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Const_decl;
  OVERRIDES
    next := Const_decl_next;
    update := Const_decl_update;
  END;

PROCEDURE Proc_decl_update(iter: Proc_decl_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_id := nn;

    | 1 => 
        iter.n.as_type := nn;

    | 2 => 
        iter.n.as_body := nn;

    END;
  END Proc_decl_update;


PROCEDURE Proc_decl_next(iter: Proc_decl_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_id;

    | 1 => 
        r := iter.n.as_type;

    | 2 => 
        r := iter.n.as_body;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Proc_decl_next;

TYPE
  Proc_decl_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Proc_decl;
  OVERRIDES
    next := Proc_decl_next;
    update := Proc_decl_update;
  END;

PROCEDURE Exc_decl_s_update(iter: Exc_decl_s_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          IF NOT SeqM3AST_AS_Exc_decl.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Exc_decl.Update(iter.n.as_exc_decl_s, iter.iter0, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Exc_decl_s_update;


PROCEDURE Exc_decl_s_next(iter: Exc_decl_s_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  VAR
    node0: M3AST_AS.Exc_decl;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_Exc_decl.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
  END Exc_decl_s_next;

TYPE
  Exc_decl_s_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Exc_decl_s;
    iter0: SeqM3AST_AS_Exc_decl.Iter;
  OVERRIDES
    next := Exc_decl_s_next;
    update := Exc_decl_s_update;
  END;

PROCEDURE Var_decl_s_update(iter: Var_decl_s_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 =>
        IF NOT SeqM3AST_AS_Var_decl.Exhausted(iter.iter0) THEN
          SeqM3AST_AS_Var_decl.Update(iter.n.as_var_decl_s, iter.iter0, nn);
          RETURN ;
        END;
      END;
      INC(iter.slot);
    END;
  END Var_decl_s_update;


PROCEDURE Var_decl_s_next(iter: Var_decl_s_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  VAR
    node0: M3AST_AS.Var_decl;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_Var_decl.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
  END Var_decl_s_next;

TYPE
  Var_decl_s_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Var_decl_s;
    iter0: SeqM3AST_AS_Var_decl.Iter;
  OVERRIDES
    next := Var_decl_s_next;
    update := Var_decl_s_update;
  END;

PROCEDURE Type_decl_s_update(iter: Type_decl_s_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 =>
        IF NOT SeqM3AST_AS_TYPE_DECL.Exhausted(iter.iter0) THEN
          SeqM3AST_AS_TYPE_DECL.Update(iter.n.as_type_decl_s, iter.iter0, nn);
          RETURN ;
        END;
      END;
      INC(iter.slot);
    END;
  END Type_decl_s_update;


PROCEDURE Type_decl_s_next(iter: Type_decl_s_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  VAR
    node0: M3AST_AS.TYPE_DECL;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_TYPE_DECL.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
  END Type_decl_s_next;

TYPE
  Type_decl_s_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Type_decl_s;
    iter0: SeqM3AST_AS_TYPE_DECL.Iter;
  OVERRIDES
    next := Type_decl_s_next;
    update := Type_decl_s_update;
  END;

PROCEDURE Const_decl_s_update(iter: Const_decl_s_iter; nn: AST.NODE) RAISES {
    }=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
        IF NOT SeqM3AST_AS_Const_decl.Exhausted(iter.iter0) THEN
          SeqM3AST_AS_Const_decl.Update(iter.n.as_const_decl_s, iter.iter0,nn);
          RETURN ;
        END;
      END;
      INC(iter.slot);
    END;
  END Const_decl_s_update;


PROCEDURE Const_decl_s_next(iter: Const_decl_s_iter; VAR r: AST.NODE): 
    BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.Const_decl;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_Const_decl.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
  END Const_decl_s_next;

TYPE
  Const_decl_s_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Const_decl_s;
    iter0: SeqM3AST_AS_Const_decl.Iter;
  OVERRIDES
    next := Const_decl_s_next;
    update := Const_decl_s_update;
  END;

PROCEDURE Revelation_s_update(iter: Revelation_s_iter; nn: AST.NODE) RAISES {
    }=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 =>
        IF NOT SeqM3AST_AS_REVELATION.Exhausted(iter.iter0) THEN
          SeqM3AST_AS_REVELATION.Update(iter.n.as_reveal_s, iter.iter0, nn);
          RETURN ;
        END;
      END;
      INC(iter.slot);
    END;
  END Revelation_s_update;


PROCEDURE Revelation_s_next(iter: Revelation_s_iter; VAR r: AST.NODE): 
    BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.REVELATION;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_REVELATION.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
  END Revelation_s_next;

TYPE
  Revelation_s_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Revelation_s;
    iter0: SeqM3AST_AS_REVELATION.Iter;
  OVERRIDES
    next := Revelation_s_next;
    update := Revelation_s_update;
  END;

PROCEDURE From_import_update(iter: From_import_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_intf_id := nn;
          RETURN ;

      | 1 => 
          IF NOT SeqM3AST_AS_Used_def_id.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Used_def_id.Update(iter.n.as_id_s, iter.iter0, nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END From_import_update;


PROCEDURE From_import_next(iter: From_import_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  VAR
    node0: M3AST_AS.Used_def_id;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_intf_id;
          EXIT;

      | 1 => 
          IF SeqM3AST_AS_Used_def_id.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END From_import_next;

TYPE
  From_import_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.From_import;
    iter0: SeqM3AST_AS_Used_def_id.Iter;
  OVERRIDES
    next := From_import_next;
    update := From_import_update;
  END;

PROCEDURE Simple_import_update(iter: Simple_import_iter; nn: AST.NODE)
    RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 =>
        IF NOT SeqM3AST_AS_Import_item.Exhausted(iter.iter0) THEN
          SeqM3AST_AS_Import_item.Update(iter.n.as_import_item_s, iter.
              iter0, nn);
          RETURN ;
        END;
      END;
      INC(iter.slot);
    END;
  END Simple_import_update;


PROCEDURE Simple_import_next(iter: Simple_import_iter; VAR r: AST.NODE): 
    BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.Import_item;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          IF SeqM3AST_AS_Import_item.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
  END Simple_import_next;

TYPE
  Simple_import_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Simple_import;
    iter0: SeqM3AST_AS_Import_item.Iter;
  OVERRIDES
    next := Simple_import_next;
    update := Simple_import_update;
  END;

PROCEDURE Import_item_update(iter: Import_item_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_intf_id := nn;

    | 1 => 
        iter.n.as_id := nn;

    END;
  END Import_item_update;


PROCEDURE Import_item_next(iter: Import_item_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_intf_id;

    | 1 => 
        r := iter.n.as_id;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Import_item_next;

TYPE
  Import_item_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Import_item;
  OVERRIDES
    next := Import_item_next;
    update := Import_item_update;
  END;

PROCEDURE Module_update(iter: Module_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_unsafe := nn;
          RETURN ;

      | 1 => 
          iter.n.as_id := nn;
          RETURN ;

      | 2 => 
          IF NOT SeqM3AST_AS_Used_interface_id.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Used_interface_id.Update(iter.n.as_export_s, iter.
                iter0, nn);
            RETURN ;
          END;

      | 3 => 
          IF NOT SeqM3AST_AS_IMPORTED.Exhausted(iter.iter1) THEN
            SeqM3AST_AS_IMPORTED.Update(iter.n.as_import_s, iter.iter1, nn);
            RETURN ;
          END;

      | 4 => 
          iter.n.as_block := nn;
          RETURN ;

      END;
      INC(iter.slot);
    END;
  END Module_update;


PROCEDURE Module_next(iter: Module_iter; VAR r: AST.NODE): BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.Used_interface_id;
    node1: M3AST_AS.IMPORTED;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_unsafe;
          EXIT;

      | 1 => 
          r := iter.n.as_id;
          EXIT;

      | 2 => 
          IF SeqM3AST_AS_Used_interface_id.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 3 => 
          IF SeqM3AST_AS_IMPORTED.Next(iter.iter1, node1) THEN
            r := node1;
            RETURN TRUE;
          END;

      | 4 => 
          r := iter.n.as_block;
          EXIT;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Module_next;

TYPE
  Module_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Module;
    iter0: SeqM3AST_AS_Used_interface_id.Iter;
    iter1: SeqM3AST_AS_IMPORTED.Iter;
  OVERRIDES
    next := Module_next;
    update := Module_update;
  END;

PROCEDURE Interface_update(iter: Interface_iter; nn: AST.NODE) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_unsafe := nn;
          RETURN ;

      | 1 => 
          iter.n.as_id := nn;
          RETURN ;

      | 2 => 
          IF NOT SeqM3AST_AS_IMPORTED.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_IMPORTED.Update(iter.n.as_import_s, iter.iter0, nn);
            RETURN ;
          END;

      | 3 => 
          iter.n.as_block := nn;
          RETURN ;

      END;
      INC(iter.slot);
    END;
  END Interface_update;


PROCEDURE Interface_next(iter: Interface_iter; VAR r: AST.NODE): BOOLEAN
    RAISES {}=
  VAR
    node0: M3AST_AS.IMPORTED;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_unsafe;
          EXIT;

      | 1 => 
          r := iter.n.as_id;
          EXIT;

      | 2 => 
          IF SeqM3AST_AS_IMPORTED.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 3 => 
          r := iter.n.as_block;
          EXIT;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Interface_next;

TYPE
  Interface_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Interface;
    iter0: SeqM3AST_AS_IMPORTED.Iter;
  OVERRIDES
    next := Interface_next;
    update := Interface_update;
  END;

PROCEDURE Module_gen_ins_update(iter: Module_gen_ins_iter; nn: AST.NODE)
    RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_unsafe := nn;
          RETURN ;

      | 1 => 
          iter.n.as_id := nn;
          RETURN ;

      | 2 => 
          IF NOT SeqM3AST_AS_Used_interface_id.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Used_interface_id.Update(iter.n.as_export_s, iter.
                iter0, nn);
            RETURN ;
          END;

      | 3 => 
          iter.n.as_gen_id := nn;
          RETURN ;

      | 4 => 
          IF NOT SeqM3AST_AS_Used_interface_id.Exhausted(iter.iter1) THEN
            SeqM3AST_AS_Used_interface_id.Update(iter.n.as_id_s, iter.iter1, 
                nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Module_gen_ins_update;


PROCEDURE Module_gen_ins_next(iter: Module_gen_ins_iter; VAR r: AST.NODE): 
    BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.Used_interface_id;
    node1: M3AST_AS.Used_interface_id;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_unsafe;
          EXIT;

      | 1 => 
          r := iter.n.as_id;
          EXIT;

      | 2 => 
          IF SeqM3AST_AS_Used_interface_id.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 3 => 
          r := iter.n.as_gen_id;
          EXIT;

      | 4 => 
          IF SeqM3AST_AS_Used_interface_id.Next(iter.iter1, node1) THEN
            r := node1;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Module_gen_ins_next;

TYPE
  Module_gen_ins_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Module_gen_ins;
    iter0: SeqM3AST_AS_Used_interface_id.Iter;
    iter1: SeqM3AST_AS_Used_interface_id.Iter;
  OVERRIDES
    next := Module_gen_ins_next;
    update := Module_gen_ins_update;
  END;

PROCEDURE Interface_gen_ins_update(iter: Interface_gen_ins_iter; nn: AST.NODE
    ) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_unsafe := nn;
          RETURN ;

      | 1 => 
          iter.n.as_id := nn;
          RETURN ;

      | 2 => 
          iter.n.as_gen_id := nn;
          RETURN ;

      | 3 => 
          IF NOT SeqM3AST_AS_Used_interface_id.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_Used_interface_id.Update(iter.n.as_id_s, iter.iter0, 
                nn);
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Interface_gen_ins_update;


PROCEDURE Interface_gen_ins_next(iter: Interface_gen_ins_iter; 
    VAR r: AST.NODE): BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.Used_interface_id;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_unsafe;
          EXIT;

      | 1 => 
          r := iter.n.as_id;
          EXIT;

      | 2 => 
          r := iter.n.as_gen_id;
          EXIT;

      | 3 => 
          IF SeqM3AST_AS_Used_interface_id.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Interface_gen_ins_next;

TYPE
  Interface_gen_ins_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Interface_gen_ins;
    iter0: SeqM3AST_AS_Used_interface_id.Iter;
  OVERRIDES
    next := Interface_gen_ins_next;
    update := Interface_gen_ins_update;
  END;

PROCEDURE Module_gen_def_update(iter: Module_gen_def_iter; nn: AST.NODE)
    RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_id := nn;
          RETURN ;

      | 1 => 
          IF NOT SeqM3AST_AS_F_Interface_id.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_F_Interface_id.Update(iter.n.as_id_s, iter.iter0, nn);
            RETURN ;
          END;

      | 2 => 
          IF NOT SeqM3AST_AS_IMPORTED.Exhausted(iter.iter1) THEN
            SeqM3AST_AS_IMPORTED.Update(iter.n.as_import_s, iter.iter1, nn);
            RETURN ;
          END;

      | 3 => 
          iter.n.as_block := nn;
          RETURN ;

      END;
      INC(iter.slot);
    END;
  END Module_gen_def_update;


PROCEDURE Module_gen_def_next(iter: Module_gen_def_iter; VAR r: AST.NODE): 
    BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.F_Interface_id;
    node1: M3AST_AS.IMPORTED;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_id;
          EXIT;

      | 1 => 
          IF SeqM3AST_AS_F_Interface_id.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 2 => 
          IF SeqM3AST_AS_IMPORTED.Next(iter.iter1, node1) THEN
            r := node1;
            RETURN TRUE;
          END;

      | 3 => 
          r := iter.n.as_block;
          EXIT;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Module_gen_def_next;

TYPE
  Module_gen_def_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Module_gen_def;
    iter0: SeqM3AST_AS_F_Interface_id.Iter;
    iter1: SeqM3AST_AS_IMPORTED.Iter;
  OVERRIDES
    next := Module_gen_def_next;
    update := Module_gen_def_update;
  END;

PROCEDURE Interface_gen_def_update(iter: Interface_gen_def_iter; nn: AST.NODE
    ) RAISES {}=
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          iter.n.as_id := nn;
          RETURN ;

      | 1 => 
          IF NOT SeqM3AST_AS_F_Interface_id.Exhausted(iter.iter0) THEN
            SeqM3AST_AS_F_Interface_id.Update(iter.n.as_id_s, iter.iter0, nn);
            RETURN ;
          END;

      | 2 => 
          IF NOT SeqM3AST_AS_IMPORTED.Exhausted(iter.iter1) THEN
            SeqM3AST_AS_IMPORTED.Update(iter.n.as_import_s, iter.iter1, nn);
            RETURN ;
          END;

      | 3 => 
          iter.n.as_block := nn;
          RETURN ;

      END;
      INC(iter.slot);
    END;
  END Interface_gen_def_update;


PROCEDURE Interface_gen_def_next(iter: Interface_gen_def_iter; 
    VAR r: AST.NODE): BOOLEAN RAISES {}=
  VAR
    node0: M3AST_AS.F_Interface_id;
    node1: M3AST_AS.IMPORTED;
  BEGIN
    LOOP
      CASE iter.slot OF
      | 0 => 
          r := iter.n.as_id;
          EXIT;

      | 1 => 
          IF SeqM3AST_AS_F_Interface_id.Next(iter.iter0, node0) THEN
            r := node0;
            RETURN TRUE;
          END;

      | 2 => 
          IF SeqM3AST_AS_IMPORTED.Next(iter.iter1, node1) THEN
            r := node1;
            RETURN TRUE;
          END;

      | 3 => 
          r := iter.n.as_block;
          EXIT;

      ELSE
        RETURN FALSE;
      END;
      INC(iter.slot);
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Interface_gen_def_next;

TYPE
  Interface_gen_def_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Interface_gen_def;
    iter0: SeqM3AST_AS_F_Interface_id.Iter;
    iter1: SeqM3AST_AS_IMPORTED.Iter;
  OVERRIDES
    next := Interface_gen_def_next;
    update := Interface_gen_def_update;
  END;

PROCEDURE Compilation_Unit_update(iter: Compilation_Unit_iter; nn: AST.NODE)
    RAISES {}=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_root := nn;

    END;
  END Compilation_Unit_update;


PROCEDURE Compilation_Unit_next(iter: Compilation_Unit_iter; VAR r: AST.NODE)
    : BOOLEAN RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_root;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Compilation_Unit_next;

TYPE
  Compilation_Unit_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Compilation_Unit;
  OVERRIDES
    next := Compilation_Unit_next;
    update := Compilation_Unit_update;
  END;

PROCEDURE Qual_used_id_update(iter: Qual_used_id_iter; nn: AST.NODE) RAISES {
    }=
  BEGIN
    CASE iter.slot OF <*NOWARN*>
    | 0 => 
        iter.n.as_intf_id := nn;

    | 1 => 
        iter.n.as_id := nn;

    END;
  END Qual_used_id_update;


PROCEDURE Qual_used_id_next(iter: Qual_used_id_iter; VAR r: AST.NODE): 
    BOOLEAN RAISES {}=
  BEGIN
    CASE iter.slot OF
    | 0 => 
        r := iter.n.as_intf_id;

    | 1 => 
        r := iter.n.as_id;

    ELSE
      RETURN FALSE;
    END;
    INC(iter.slot);
    RETURN TRUE;
  END Qual_used_id_next;

TYPE
  Qual_used_id_iter = AST_Iter.T OBJECT
    n: M3AST_AS_F.Qual_used_id;
  OVERRIDES
    next := Qual_used_id_next;
    update := Qual_used_id_update;
  END;

PROCEDURE Qual_used_id_newIter(n: M3AST_AS_F.Qual_used_id): AST_Iter.T
    RAISES {}=
  BEGIN
    RETURN NEW(Qual_used_id_iter, n := n);
  END Qual_used_id_newIter;


PROCEDURE Compilation_Unit_newIter(n: M3AST_AS_F.Compilation_Unit): AST_Iter.
    T RAISES {}=
  BEGIN
    RETURN NEW(Compilation_Unit_iter, n := n);
  END Compilation_Unit_newIter;


PROCEDURE Interface_gen_def_newIter(n: M3AST_AS_F.Interface_gen_def): 
    AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Interface_gen_def_iter, n := n, iter0 := 
        SeqM3AST_AS_F_Interface_id.NewIter(n.as_id_s), iter1 := 
        SeqM3AST_AS_IMPORTED.NewIter(n.as_import_s));
  END Interface_gen_def_newIter;


PROCEDURE Module_gen_def_newIter(n: M3AST_AS_F.Module_gen_def): AST_Iter.T
    RAISES {}=
  BEGIN
    RETURN NEW(Module_gen_def_iter, n := n, iter0 := 
        SeqM3AST_AS_F_Interface_id.NewIter(n.as_id_s), iter1 := 
        SeqM3AST_AS_IMPORTED.NewIter(n.as_import_s));
  END Module_gen_def_newIter;


PROCEDURE Interface_gen_ins_newIter(n: M3AST_AS_F.Interface_gen_ins): 
    AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Interface_gen_ins_iter, n := n, iter0 := 
        SeqM3AST_AS_Used_interface_id.NewIter(n.as_id_s));
  END Interface_gen_ins_newIter;


PROCEDURE Module_gen_ins_newIter(n: M3AST_AS_F.Module_gen_ins): AST_Iter.T
    RAISES {}=
  BEGIN
    RETURN NEW(Module_gen_ins_iter, n := n, iter0 := 
        SeqM3AST_AS_Used_interface_id.NewIter(n.as_export_s), iter1 := 
        SeqM3AST_AS_Used_interface_id.NewIter(n.as_id_s));
  END Module_gen_ins_newIter;


PROCEDURE Interface_newIter(n: M3AST_AS_F.Interface): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Interface_iter, n := n, iter0 := SeqM3AST_AS_IMPORTED.NewIter(
        n.as_import_s));
  END Interface_newIter;


PROCEDURE Module_newIter(n: M3AST_AS_F.Module): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Module_iter, n := n, iter0 := SeqM3AST_AS_Used_interface_id.
        NewIter(n.as_export_s), iter1 := SeqM3AST_AS_IMPORTED.NewIter(n.
        as_import_s));
  END Module_newIter;


PROCEDURE Import_item_newIter(n: M3AST_AS_F.Import_item): AST_Iter.T RAISES {
    }=
  BEGIN
    RETURN NEW(Import_item_iter, n := n);
  END Import_item_newIter;


PROCEDURE Simple_import_newIter(n: M3AST_AS_F.Simple_import): AST_Iter.T
    RAISES {}=
  BEGIN
    RETURN NEW(Simple_import_iter, n := n, iter0 := SeqM3AST_AS_Import_item.
        NewIter(n.as_import_item_s));
  END Simple_import_newIter;


PROCEDURE From_import_newIter(n: M3AST_AS_F.From_import): AST_Iter.T RAISES {
    }=
  BEGIN
    RETURN NEW(From_import_iter, n := n, iter0 := SeqM3AST_AS_Used_def_id.
        NewIter(n.as_id_s));
  END From_import_newIter;


PROCEDURE Revelation_s_newIter(n: M3AST_AS_F.Revelation_s): AST_Iter.T
    RAISES {}=
  BEGIN
    RETURN NEW(Revelation_s_iter, n := n, iter0 := SeqM3AST_AS_REVELATION.
        NewIter(n.as_reveal_s));
  END Revelation_s_newIter;


PROCEDURE Const_decl_s_newIter(n: M3AST_AS_F.Const_decl_s): AST_Iter.T
    RAISES {}=
  BEGIN
    RETURN NEW(Const_decl_s_iter, n := n, iter0 := SeqM3AST_AS_Const_decl.
        NewIter(n.as_const_decl_s));
  END Const_decl_s_newIter;


PROCEDURE Type_decl_s_newIter(n: M3AST_AS_F.Type_decl_s): AST_Iter.T RAISES {
    }=
  BEGIN
    RETURN NEW(Type_decl_s_iter, n := n, iter0 := SeqM3AST_AS_TYPE_DECL.
        NewIter(n.as_type_decl_s));
  END Type_decl_s_newIter;


PROCEDURE Var_decl_s_newIter(n: M3AST_AS_F.Var_decl_s): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Var_decl_s_iter, n := n, iter0 := SeqM3AST_AS_Var_decl.NewIter
        (n.as_var_decl_s));
  END Var_decl_s_newIter;


PROCEDURE Exc_decl_s_newIter(n: M3AST_AS_F.Exc_decl_s): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Exc_decl_s_iter, n := n, iter0 := SeqM3AST_AS_Exc_decl.NewIter
        (n.as_exc_decl_s));
  END Exc_decl_s_newIter;


PROCEDURE Proc_decl_newIter(n: M3AST_AS_F.Proc_decl): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Proc_decl_iter, n := n);
  END Proc_decl_newIter;


PROCEDURE Const_decl_newIter(n: M3AST_AS_F.Const_decl): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Const_decl_iter, n := n);
  END Const_decl_newIter;


PROCEDURE Var_decl_newIter(n: M3AST_AS_F.Var_decl): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Var_decl_iter, n := n, iter0 := SeqM3AST_AS_Var_id.NewIter(n.
        as_id_s));
  END Var_decl_newIter;


PROCEDURE Exc_decl_newIter(n: M3AST_AS_F.Exc_decl): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Exc_decl_iter, n := n);
  END Exc_decl_newIter;


PROCEDURE Subtype_decl_newIter(n: M3AST_AS_F.Subtype_decl): AST_Iter.T
    RAISES {}=
  BEGIN
    RETURN NEW(Subtype_decl_iter, n := n);
  END Subtype_decl_newIter;


PROCEDURE Concrete_decl_newIter(n: M3AST_AS_F.Concrete_decl): AST_Iter.T
    RAISES {}=
  BEGIN
    RETURN NEW(Concrete_decl_iter, n := n);
  END Concrete_decl_newIter;


PROCEDURE Subtype_reveal_newIter(n: M3AST_AS_F.Subtype_reveal): AST_Iter.T
    RAISES {}=
  BEGIN
    RETURN NEW(Subtype_reveal_iter, n := n);
  END Subtype_reveal_newIter;


PROCEDURE Concrete_reveal_newIter(n: M3AST_AS_F.Concrete_reveal): AST_Iter.T
    RAISES {}=
  BEGIN
    RETURN NEW(Concrete_reveal_iter, n := n);
  END Concrete_reveal_newIter;


PROCEDURE Named_type_newIter(n: M3AST_AS_F.Named_type): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Named_type_iter, n := n);
  END Named_type_newIter;


PROCEDURE Root_type_newIter(n: M3AST_AS_F.Root_type): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Root_type_iter, n := n);
  END Root_type_newIter;


PROCEDURE Enumeration_type_newIter(n: M3AST_AS_F.Enumeration_type): AST_Iter.
    T RAISES {}=
  BEGIN
    RETURN NEW(Enumeration_type_iter, n := n, iter0 := SeqM3AST_AS_Enum_id.
        NewIter(n.as_id_s));
  END Enumeration_type_newIter;


PROCEDURE Subrange_type_newIter(n: M3AST_AS_F.Subrange_type): AST_Iter.T
    RAISES {}=
  BEGIN
    RETURN NEW(Subrange_type_iter, n := n);
  END Subrange_type_newIter;


PROCEDURE Array_type_newIter(n: M3AST_AS_F.Array_type): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Array_type_iter, n := n, iter0 := SeqM3AST_AS_M3TYPE.NewIter(n
        .as_indextype_s));
  END Array_type_newIter;


PROCEDURE Record_type_newIter(n: M3AST_AS_F.Record_type): AST_Iter.T RAISES {
    }=
  BEGIN
    RETURN NEW(Record_type_iter, n := n, iter0 := SeqM3AST_AS_Fields.NewIter(
        n.as_fields_s));
  END Record_type_newIter;


PROCEDURE Object_type_newIter(n: M3AST_AS_F.Object_type): AST_Iter.T RAISES {
    }=
  BEGIN
    RETURN NEW(Object_type_iter, n := n, iter0 := SeqM3AST_AS_Fields.NewIter(
        n.as_fields_s), iter1 := SeqM3AST_AS_Method.NewIter(n.as_method_s));
  END Object_type_newIter;


PROCEDURE Set_type_newIter(n: M3AST_AS_F.Set_type): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Set_type_iter, n := n);
  END Set_type_newIter;


PROCEDURE Procedure_type_newIter(n: M3AST_AS_F.Procedure_type): AST_Iter.T
    RAISES {}=
  BEGIN
    RETURN NEW(Procedure_type_iter, n := n, iter0 := SeqM3AST_AS_Formal_param
        .NewIter(n.as_formal_param_s));
  END Procedure_type_newIter;


PROCEDURE Ref_type_newIter(n: M3AST_AS_F.Ref_type): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Ref_type_iter, n := n);
  END Ref_type_newIter;


PROCEDURE Packed_type_newIter(n: M3AST_AS_F.Packed_type): AST_Iter.T RAISES {
    }=
  BEGIN
    RETURN NEW(Packed_type_iter, n := n);
  END Packed_type_newIter;


PROCEDURE Opaque_type_newIter(n: M3AST_AS_F.Opaque_type): AST_Iter.T RAISES {
    }=
  BEGIN
    RETURN NEW(Opaque_type_iter, n := n);
  END Opaque_type_newIter;


PROCEDURE Brand_newIter(n: M3AST_AS_F.Brand): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Brand_iter, n := n);
  END Brand_newIter;


PROCEDURE Fields_newIter(n: M3AST_AS_F.Fields): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Fields_iter, n := n, iter0 := SeqM3AST_AS_Field_id.NewIter(n.
        as_id_s));
  END Fields_newIter;


PROCEDURE Method_newIter(n: M3AST_AS_F.Method): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Method_iter, n := n);
  END Method_newIter;


PROCEDURE Override_newIter(n: M3AST_AS_F.Override): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Override_iter, n := n);
  END Override_newIter;


PROCEDURE Formal_param_newIter(n: M3AST_AS_F.Formal_param): AST_Iter.T
    RAISES {}=
  BEGIN
    RETURN NEW(Formal_param_iter, n := n, iter0 := SeqM3AST_AS_FORMAL_ID.
        NewIter(n.as_id_s));
  END Formal_param_newIter;


PROCEDURE Raisees_some_newIter(n: M3AST_AS_F.Raisees_some): AST_Iter.T
    RAISES {}=
  BEGIN
    RETURN NEW(Raisees_some_iter, n := n, iter0 := SeqM3AST_AS_Qual_used_id.
        NewIter(n.as_raisees_s));
  END Raisees_some_newIter;


PROCEDURE Range_EXP_newIter(n: M3AST_AS_F.Range_EXP): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Range_EXP_iter, n := n);
  END Range_EXP_newIter;


PROCEDURE Range_newIter(n: M3AST_AS_F.Range): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Range_iter, n := n);
  END Range_newIter;


PROCEDURE Constructor_newIter(n: M3AST_AS_F.Constructor): AST_Iter.T RAISES {
    }=
  BEGIN
    RETURN NEW(Constructor_iter, n := n, iter0 := SeqM3AST_AS_CONS_ELEM.
        NewIter(n.as_element_s));
  END Constructor_newIter;


PROCEDURE RANGE_EXP_elem_newIter(n: M3AST_AS_F.RANGE_EXP_elem): AST_Iter.T
    RAISES {}=
  BEGIN
    RETURN NEW(RANGE_EXP_elem_iter, n := n);
  END RANGE_EXP_elem_newIter;


PROCEDURE Actual_elem_newIter(n: M3AST_AS_F.Actual_elem): AST_Iter.T RAISES {
    }=
  BEGIN
    RETURN NEW(Actual_elem_iter, n := n);
  END Actual_elem_newIter;


PROCEDURE Propagate_newIter(n: M3AST_AS_F.Propagate): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Propagate_iter, n := n);
  END Propagate_newIter;


PROCEDURE BINARY_newIter(n: M3AST_AS_F.BINARY): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(BINARY_iter, n := n);
  END BINARY_newIter;


PROCEDURE UNARY_newIter(n: M3AST_AS_F.UNARY): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(UNARY_iter, n := n);
  END UNARY_newIter;


PROCEDURE Select_newIter(n: M3AST_AS_F.Select): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Select_iter, n := n);
  END Select_newIter;


PROCEDURE Call_newIter(n: M3AST_AS_F.Call): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Call_iter, n := n, iter0 := SeqM3AST_AS_Actual.NewIter(n.
        as_param_s));
  END Call_newIter;


PROCEDURE Index_newIter(n: M3AST_AS_F.Index): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Index_iter, n := n, iter0 := SeqM3AST_AS_EXP.NewIter(n.
        as_exp_s));
  END Index_newIter;


PROCEDURE Actual_newIter(n: M3AST_AS_F.Actual): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Actual_iter, n := n);
  END Actual_newIter;


PROCEDURE Assign_st_newIter(n: M3AST_AS_F.Assign_st): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Assign_st_iter, n := n);
  END Assign_st_newIter;


PROCEDURE Call_st_newIter(n: M3AST_AS_F.Call_st): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Call_st_iter, n := n);
  END Call_st_newIter;


PROCEDURE Case_st_newIter(n: M3AST_AS_F.Case_st): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Case_st_iter, n := n, iter0 := SeqM3AST_AS_Case.NewIter(n.
        as_case_s));
  END Case_st_newIter;


PROCEDURE Eval_st_newIter(n: M3AST_AS_F.Eval_st): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Eval_st_iter, n := n);
  END Eval_st_newIter;


PROCEDURE Exit_st_newIter(n: M3AST_AS_F.Exit_st): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Exit_st_iter, n := n);
  END Exit_st_newIter;


PROCEDURE For_st_newIter(n: M3AST_AS_F.For_st): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(For_st_iter, n := n, iter0 := SeqM3AST_AS_STM.NewIter(n.
        as_stm_s));
  END For_st_newIter;


PROCEDURE If_st_newIter(n: M3AST_AS_F.If_st): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(If_st_iter, n := n, iter0 := SeqM3AST_AS_STM.NewIter(n.
        as_stm_s), iter1 := SeqM3AST_AS_Elsif.NewIter(n.as_elsif_s));
  END If_st_newIter;


PROCEDURE Lock_st_newIter(n: M3AST_AS_F.Lock_st): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Lock_st_iter, n := n, iter0 := SeqM3AST_AS_STM.NewIter(n.
        as_stm_s));
  END Lock_st_newIter;


PROCEDURE Loop_st_newIter(n: M3AST_AS_F.Loop_st): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Loop_st_iter, n := n, iter0 := SeqM3AST_AS_STM.NewIter(n.
        as_stm_s));
  END Loop_st_newIter;


PROCEDURE Raise_st_newIter(n: M3AST_AS_F.Raise_st): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Raise_st_iter, n := n);
  END Raise_st_newIter;


PROCEDURE Repeat_st_newIter(n: M3AST_AS_F.Repeat_st): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Repeat_st_iter, n := n, iter0 := SeqM3AST_AS_STM.NewIter(n.
        as_stm_s));
  END Repeat_st_newIter;


PROCEDURE Return_st_newIter(n: M3AST_AS_F.Return_st): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Return_st_iter, n := n);
  END Return_st_newIter;


PROCEDURE Try_st_newIter(n: M3AST_AS_F.Try_st): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Try_st_iter, n := n, iter0 := SeqM3AST_AS_STM.NewIter(n.
        as_stm_s));
  END Try_st_newIter;


PROCEDURE Typecase_st_newIter(n: M3AST_AS_F.Typecase_st): AST_Iter.T RAISES {
    }=
  BEGIN
    RETURN NEW(Typecase_st_iter, n := n, iter0 := SeqM3AST_AS_Tcase.NewIter(n
        .as_tcase_s));
  END Typecase_st_newIter;


PROCEDURE While_st_newIter(n: M3AST_AS_F.While_st): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(While_st_iter, n := n, iter0 := SeqM3AST_AS_STM.NewIter(n.
        as_stm_s));
  END While_st_newIter;


PROCEDURE With_st_newIter(n: M3AST_AS_F.With_st): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(With_st_iter, n := n, iter0 := SeqM3AST_AS_Binding.NewIter(n.
        as_binding_s), iter1 := SeqM3AST_AS_STM.NewIter(n.as_stm_s));
  END With_st_newIter;


PROCEDURE Block_newIter(n: M3AST_AS_F.Block): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Block_iter, n := n, iter0 := SeqM3AST_AS_DECL_REVL.NewIter(n.
        as_decl_s), iter1 := SeqM3AST_AS_STM.NewIter(n.as_stm_s));
  END Block_newIter;


PROCEDURE Case_newIter(n: M3AST_AS_F.Case): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Case_iter, n := n, iter0 := SeqM3AST_AS_RANGE_EXP.NewIter(n.
        as_case_label_s), iter1 := SeqM3AST_AS_STM.NewIter(n.as_stm_s));
  END Case_newIter;


PROCEDURE Else_stm_newIter(n: M3AST_AS_F.Else_stm): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Else_stm_iter, n := n, iter0 := SeqM3AST_AS_STM.NewIter(n.
        as_stm_s));
  END Else_stm_newIter;


PROCEDURE Elsif_newIter(n: M3AST_AS_F.Elsif): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Elsif_iter, n := n, iter0 := SeqM3AST_AS_STM.NewIter(n.
        as_stm_s));
  END Elsif_newIter;


PROCEDURE Try_except_newIter(n: M3AST_AS_F.Try_except): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Try_except_iter, n := n, iter0 := SeqM3AST_AS_Handler.NewIter(
        n.as_handler_s), iter1 := SeqM3AST_AS_STM.NewIter(n.as_stm_s));
  END Try_except_newIter;


PROCEDURE Try_finally_newIter(n: M3AST_AS_F.Try_finally): AST_Iter.T RAISES {
    }=
  BEGIN
    RETURN NEW(Try_finally_iter, n := n, iter0 := SeqM3AST_AS_STM.NewIter(n.
        as_stm_s));
  END Try_finally_newIter;


PROCEDURE Tcase_newIter(n: M3AST_AS_F.Tcase): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Tcase_iter, n := n, iter0 := SeqM3AST_AS_M3TYPE.NewIter(n.
        as_type_s), iter1 := SeqM3AST_AS_STM.NewIter(n.as_stm_s));
  END Tcase_newIter;


PROCEDURE Handler_newIter(n: M3AST_AS_F.Handler): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Handler_iter, n := n, iter0 := SeqM3AST_AS_Qual_used_id.
        NewIter(n.as_qual_id_s), iter1 := SeqM3AST_AS_STM.NewIter(n.as_stm_s)
        );
  END Handler_newIter;


PROCEDURE Binding_newIter(n: M3AST_AS_F.Binding): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(Binding_iter, n := n);
  END Binding_newIter;


PROCEDURE By_newIter(n: M3AST_AS_F.By): AST_Iter.T RAISES {}=
  BEGIN
    RETURN NEW(By_iter, n := n);
  END By_newIter;

BEGIN
END M3AST_AS_Iter.
