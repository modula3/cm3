(* $Id$ *)

MODULE M3ASTScopeNames;
IMPORT M3AST_AS, M3AST_SM;
IMPORT SeqM3AST_AS_DEF_ID;
IMPORT M3AST_SM_F;
IMPORT RefSeq;

PROCEDURE Names(s: M3AST_SM.SCOPE): RefSeq.T =
  VAR ps: M3AST_SM.SCOPE;
      res := NEW(RefSeq.T).init();
  BEGIN
    WHILE s # NIL DO
      VAR
        iter := SeqM3AST_AS_DEF_ID.NewIter(s.sm_def_id_s);
        def_id: M3AST_AS.DEF_ID;
      BEGIN
        WHILE SeqM3AST_AS_DEF_ID.Next(iter, def_id) DO
          res.addhi(def_id)
        END;
        ps := s;
        s := s.sm_enc_scope;
        IF (s = NIL OR (s.sm_level # ps.sm_level)) THEN
          RETURN res
        END;
      END;
    END;
    <*ASSERT FALSE*>
  END Names;

BEGIN
END M3ASTScopeNames.
