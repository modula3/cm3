(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3AST_SM_Walk EXPORTS M3AST_SM_F;

IMPORT AST, M3AST_AS, M3AST_AS_F;
IMPORT AST_WalkRep, AST_Iter;
IMPORT SeqM3AST_AS_Actual;

IMPORT M3AST_AS_Iter_rep;

PROCEDURE Walk_NEWCall(n: NEWCall; h: AST_WalkRep.Handle) RAISES ANY=
  VAR
    m: M3AST_AS.Actual;
    iter: SeqM3AST_AS_Actual.Iter;
    s: SeqM3AST_AS_Actual.T;
  BEGIN
    h.Visit(n.as_callexp);
    IF n.sm_norm_actual_s = NIL THEN (* not set up yet! *)
      s := n.as_param_s;
    ELSE
      s := n.sm_norm_actual_s;
    END;
    iter := SeqM3AST_AS_Actual.NewIter(s);
    WHILE SeqM3AST_AS_Actual.Next(iter, m) DO h.Visit(m); END;
 END Walk_NEWCall;

TYPE
  Call_iter = AST_Iter.T OBJECT
    n: NEWCall;
    iter0: SeqM3AST_AS_Actual.Iter;
    norm := FALSE;
  OVERRIDES
    next := Call_next;
  END;

PROCEDURE NewIter_NEWCall(n: NEWCall): AST_Iter.T RAISES {}=
  VAR
    s: SeqM3AST_AS_Actual.T;
  BEGIN
    WITH iter = NEW(Call_iter, n := n) DO
      IF n.sm_norm_actual_s = NIL THEN (* not set up yet! *)
        s := n.as_param_s;
      ELSE
        s := n.sm_norm_actual_s;
        iter.norm := TRUE;
      END;
      iter.iter0 := SeqM3AST_AS_Actual.NewIter(s);
      RETURN iter;
    END;
  END NewIter_NEWCall;


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

(*******************************************************************
PROCEDURE Update_NEWCall(n: NEWCall; iter: AST_Iter.T; nn: AST.NODE)
    RAISES {}=
  VAR
    narrow_iter:= NARROW(iter, Call_iter);
  BEGIN
    LOOP
      CASE iter.slot OF <*NOWARN*>
      | 0 => 
          n.as_callexp := nn;
          RETURN ;

      | 1 => 
          IF NOT SeqM3AST_AS_Actual.Exhausted(narrow_iter.iter0) THEN
            IF narrow_iter.norm THEN
              SeqM3AST_AS_Actual.Update(n.sm_norm_actual_s,
                  narrow_iter.iter0, nn);
            ELSE
              SeqM3AST_AS_Actual.Update(n.as_param_s, narrow_iter.iter0, nn);
            END; (* if *)
            RETURN ;
          END;

      END;
      INC(iter.slot);
    END;
  END Update_NEWCall;
*****************************************************************************)


BEGIN

END M3AST_SM_Walk.
