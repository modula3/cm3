(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Mon Aug  8 16:05:15 PDT 1994 by kalsow    *)
(*      modified on Fri Jan 28 10:57:17 PST 1994 by detlefs   *)

MODULE M3ASTScope;

IMPORT ASTWalk, AST, M3AST_AS, M3AST_SM;
IMPORT SeqM3AST_AS_DEF_ID, SeqM3AST_AS_Binding;
IMPORT M3AST_AS_F, M3AST_SM_F;

REVEAL Closure = Closure_public BRANDED OBJECT
    for_scope: M3AST_SM.SCOPE := NIL;
    for_end: AST.NODE := NIL;
  END;

PROCEDURE Set(cl: Closure; n: AST.NODE; vm: ASTWalk.VisitMode)=
  BEGIN
    TYPECASE n OF
    | M3AST_AS.UNIT(unit) =>
        IF vm = ASTWalk.VisitMode.Entry THEN
          cl.scope := unit.as_id.vSCOPE
        ELSE
          cl.scope := NIL
        END;

    | M3AST_AS.Proc_decl(p) =>
        (* Only when we enter the Block, if any, do the
           formals come into scope, so we let the Block
           node do the work on entry. On exit, we reset
           the scope to the enclosing scope. *)
        IF vm = ASTWalk.VisitMode.Exit AND p.as_body # NIL THEN
          cl.scope := cl.scope.sm_enc_scope;
        END;

    | M3AST_AS.Block(block) =>
        IF vm = ASTWalk.VisitMode.Entry THEN
          cl.scope := block.vSCOPE
        ELSE
          cl.scope := cl.scope.sm_enc_scope;
        END;
        
    | M3AST_AS.Handler(h) =>
        IF vm = ASTWalk.VisitMode.Exit AND h.as_id # NIL THEN
          cl.scope := cl.scope.sm_enc_scope;
        END;

    | M3AST_AS.Handler_id(id) =>
        IF vm = ASTWalk.VisitMode.Entry THEN
          cl.scope := id.vSCOPE;
        END;
    
    | M3AST_AS.Tcase(h) =>
        IF vm = ASTWalk.VisitMode.Exit AND h.as_id # NIL THEN
          cl.scope := cl.scope.sm_enc_scope;
        END;

    | M3AST_AS.Tcase_id(id) =>
        IF vm = ASTWalk.VisitMode.Entry THEN
          cl.scope := id.vSCOPE;
        END;

    | M3AST_AS.Binding(b) =>
        (* As we exit a Binding, the With_id comes into
           scope (for the next Binding, for example). 
           They all go out of scope as we exit the With_st. *)
        IF vm = ASTWalk.VisitMode.Exit THEN
          cl.scope := b.as_id.vSCOPE;
        END;

    | M3AST_AS.With_st(w) =>
        (* scope reverts to encloser of that asssociated with
           first binding *)
        IF vm = ASTWalk.VisitMode.Exit THEN
          VAR iter := SeqM3AST_AS_Binding.NewIter(w.as_binding_s);
            b: M3AST_AS.Binding;
          BEGIN
            IF SeqM3AST_AS_Binding.Next(iter, b) THEN
              cl.scope := b.as_id.vSCOPE.sm_enc_scope;
            END;
          END;
        END;

    | M3AST_AS.For_st(for_st) =>
        (* Somewhat tricky; the For_id does not come into
           scope until the statement sequence, and we have
           an optional BY node. *)
        IF vm = ASTWalk.VisitMode.Entry THEN
          IF for_st.as_by # NIL THEN cl.for_end := for_st.as_by
          ELSE cl.for_end := for_st.as_from;
          END;
          cl.for_scope := for_st.as_id.vSCOPE;
        ELSE
          cl.scope := for_st.as_id.vSCOPE.sm_enc_scope;
        END;

    ELSE
      IF vm = ASTWalk.VisitMode.Exit THEN 
        IF n = cl.for_end THEN
          cl.scope := cl.for_scope;
          cl.for_end := NIL;
        END;
      END;
    END
  END Set;

PROCEDURE Lookup(s: M3AST_SM.SCOPE; 
    used_id: M3AST_AS.USED_ID): M3AST_AS.DEF_ID=
  VAR ps: M3AST_SM.SCOPE;
  BEGIN
    IF used_id.lx_symrep = NIL THEN RETURN NIL END;
    WHILE s # NIL DO
      VAR
        iter := SeqM3AST_AS_DEF_ID.NewIter(s.sm_def_id_s);
        def_id: M3AST_AS.DEF_ID;
      BEGIN
        WHILE SeqM3AST_AS_DEF_ID.Next(iter, def_id) DO
          IF used_id.lx_symrep = def_id.lx_symrep THEN
            RETURN def_id;
          END;
        END;
        ps := s;
        s := s.sm_enc_scope;
        IF (s = NIL OR (s.sm_level # ps.sm_level)) THEN
          RETURN NIL
        END;
      END;
    END;
    <*ASSERT FALSE*>
  END Lookup;

BEGIN
END M3ASTScope.
