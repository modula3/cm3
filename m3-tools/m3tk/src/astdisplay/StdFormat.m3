(************************************************************************
!		                                                        *
!*                                                                      *
!*         Copyright 1994 Sun Microsystems, Inc. All Rights Reserved.   *
!*                                                                      *
!*      Permission to use, copy, modify, and distribute this software   *
!*      and its documentation for any purpose and without fee is hereby *
!*      granted, provided that the above copyright notice appear in all *
!*      copies and that both that copyright notice and this permission  *
!*      notice appear in supporting documentation, and that the name of *
!*      Sun Microsystems, Inc. (SMI) not be used in advertising or      *
!*      publicity pertaining to distribution of the software without    *
!*      specific, written prior permission.                             *
!*                                                                      *
!*                                                                      *
!*      SMI DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,      *
!*      INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY,	        *
!*      FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT.           *
!*      IN NO EVENT SHALL SMI BE LIABLE FOR ANY SPECIAL, INCIDENTAL,    *
!*	INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER     *
!*      RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN      *
!*      ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,        *
!*      ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE     *
!*      OF THIS SOFTWARE.                                               *
!*                                                                      *
!***********************************************************************)

MODULE StdFormat;

IMPORT AST_Iter; <*NOWARN*>
IMPORT AST, M3AST_LX, M3AST_AS, M3AST_SM, M3AST_PG;
IMPORT SeqM3AST_LX_SRC_NODE;
IMPORT M3AST_LX_F, M3AST_AS_F, M3AST_PG_F;
IMPORT M3CId, M3Assert;
IMPORT M3CToken, M3CWhitespace, M3CLiteral, M3CComment;

IMPORT
    SeqM3AST_AS_IMPORTED,
    SeqM3AST_AS_Used_interface_id, SeqM3AST_AS_Used_def_id,
    SeqM3AST_AS_Import_item, SeqM3AST_AS_Override,
    SeqM3AST_AS_REVELATION, SeqM3AST_AS_DECL_REVL,
    SeqM3AST_AS_Const_decl, SeqM3AST_AS_TYPE_DECL,
    SeqM3AST_AS_Var_decl, SeqM3AST_AS_Exc_decl,
    SeqM3AST_AS_Var_id, 
    SeqM3AST_AS_Enum_id, SeqM3AST_AS_Field_id,
    SeqM3AST_AS_FORMAL_ID, SeqM3AST_AS_Qual_used_id,
    SeqM3AST_AS_Fields, SeqM3AST_AS_Method,
    SeqM3AST_AS_M3TYPE,
    SeqM3AST_AS_Formal_param, SeqM3AST_AS_CONS_ELEM,
    SeqM3AST_AS_EXP, SeqM3AST_AS_Actual,
    SeqM3AST_AS_Case, SeqM3AST_AS_STM,
    SeqM3AST_AS_Elsif, SeqM3AST_AS_Tcase,
    SeqM3AST_AS_Handler, SeqM3AST_AS_Binding,
    SeqM3AST_AS_RANGE_EXP, 
    <*NOWARN*> SeqM3AST_AS_F_Interface_id;

TYPE
  WS = {Space, CommaSpace, Newline, SemiNewline, SemiSpace};
  Handle = OBJECT
    isModule, suppressPROC := FALSE;
    indent := 0;
    comments: M3CComment.Iter;
    comment: M3CComment.T;
    this_unit_id: M3AST_AS.UNIT_ID;
  END;

CONST
  WSV = ARRAY WS OF TEXT {" ", ", ", "\n", ";\n", "; "};
  Indent_Size = 2;

VAR
  ws_g: ARRAY WS OF M3AST_LX.Whitespace_rep;
  indent_ws_g: ARRAY [0..15] OF M3AST_LX.Whitespace_rep;

PROCEDURE MkWS()=
  VAR indent := "";
  BEGIN
    FOR i := FIRST(WS) TO LAST(WS) DO 
      ws_g[i] := M3CWhitespace.Enter(WSV[i]);
    END;
    FOR i := 1 TO Indent_Size DO
      indent := indent & " ";
    END;
    FOR i := FIRST(indent_ws_g) TO LAST(indent_ws_g) DO
      VAR this_indent := "";
      BEGIN
        FOR j := 1 TO i DO
          this_indent := this_indent & indent;
        END;
        indent_ws_g[i] := M3CWhitespace.Enter(this_indent);
      END;
    END;
  END MkWS;
      
PROCEDURE NewToken(t: M3CToken.T): M3AST_LX.Token=
  BEGIN
    WITH n = NEW(M3AST_LX.Token) DO
      n.lx_token_rep := M3CToken.Token_rep(t);
      RETURN n;
    END
  END NewToken;

PROCEDURE NewWhitespace(ws: WS): M3AST_LX.Whitespace=
  BEGIN
    WITH n = NEW(M3AST_LX.Whitespace) DO
      n.lx_whitespace_rep := ws_g[ws];
      RETURN n;
    END
  END NewWhitespace;

PROCEDURE D(<*UNUSED*> h: Handle; n: M3AST_AS.SRC_NODE_C; t: TEXT)=
  BEGIN
    WITH ws = NEW(M3AST_LX.Whitespace) DO
      ws.lx_whitespace_rep := M3CWhitespace.Enter(t);
      SeqM3AST_LX_SRC_NODE.AddRear(n.lx_node_s, ws);
    END
  END D;

PROCEDURE Indent(h: Handle; n: M3AST_AS.SRC_NODE)=
  BEGIN
    IF h.indent # 0 THEN
     WITH ws = NEW(M3AST_LX.Whitespace) DO
       ws.lx_whitespace_rep := indent_ws_g[h.indent];
       Append(h, n, ws);
     END;
    END;
  END Indent;

PROCEDURE NLIncIndent(h: Handle; n: M3AST_AS.SRC_NODE)=
  BEGIN
    NL(h, n); IncIndent(h);
  END NLIncIndent;

PROCEDURE IncIndent(h: Handle)=
  BEGIN
    INC(h.indent);
  END IncIndent;

EXCEPTION IndentUnderflow;

PROCEDURE DecIndent(h: Handle)=
  BEGIN
    IF h.indent = 0 THEN 
      <*FATAL IndentUnderflow*>
      BEGIN RAISE IndentUnderflow; END;
    END;
    DEC(h.indent);
  END DecIndent;

PROCEDURE Space(h: Handle; n: M3AST_AS.SRC_NODE_C)=
  BEGIN
    Append(h, n, NewWhitespace(WS.Space));
  END Space;

PROCEDURE SCNL(h: Handle; n: M3AST_AS.SRC_NODE_C)=
  BEGIN
    Append(h, n, NewWhitespace(WS.SemiNewline));
  END SCNL;

PROCEDURE CS(h: Handle; n: M3AST_AS.SRC_NODE_C)=
  BEGIN
    Append(h, n, NewWhitespace(WS.CommaSpace));
  END CS;

PROCEDURE ScS(h: Handle; n: M3AST_AS.SRC_NODE_C)=
  BEGIN
    Append(h, n, NewWhitespace(WS.SemiSpace));
  END ScS;

PROCEDURE NL(h: Handle; n: M3AST_AS.SRC_NODE_C)=
  BEGIN
    Append(h, n, NewWhitespace(WS.Newline));
  END NL;

PROCEDURE CommaSpace(h: Handle; n: M3AST_AS.SRC_NODE_C)=
  BEGIN
    Append(h, n, NewWhitespace(WS.CommaSpace));
  END CommaSpace;

PROCEDURE Append(h: Handle; n: M3AST_LX.SRC_NODE_C; s: M3AST_LX.SRC_NODE)=
  BEGIN
    FlushComments(h, n, TRUE, s);
    SeqM3AST_LX_SRC_NODE.AddRear(n.lx_node_s, s);
  END Append;

PROCEDURE Between(
    h: Handle;
    n: M3AST_AS.SRC_NODE_C;
    VAR isFirst: BOOLEAN;
    p: PROCEDURE(h: Handle; n: M3AST_AS.SRC_NODE_C)) =
  BEGIN
    IF isFirst THEN isFirst := FALSE; RETURN; END;
    p(h, n);
  END Between;

PROCEDURE Set(n: M3AST_AS.SRC_NODE_C; <*UNUSED*> indent := 0)=
  VAR cu := NARROW(n, M3AST_AS.Compilation_Unit);
  BEGIN
    WITH h = NEW(Handle, comments := M3CComment.NewIter(cu.lx_comments),
                         this_unit_id := cu.as_root.as_id) DO
      EVAL M3CComment.Next(h.comments, h.comment);
      DoIt(h, NIL, n);
      WHILE h.comment # NIL DO
        D(h, n, M3CComment.Body(h.comment));
        IF M3CComment.Next(h.comments, h.comment) THEN
        ELSE h.comment := NIL;
        END;
      END;
    END;
  END Set;

PROCEDURE DoIt(h: Handle; parent, n: M3AST_AS.SRC_NODE_C)=
  BEGIN
    IF parent # NIL THEN Append(h, parent, n); END;
    n.lx_node_s := SeqM3AST_LX_SRC_NODE.Null;
    TYPECASE n OF
    | M3AST_AS.Compilation_Unit(q) => DoIt(h, n, q.as_root);
    | M3AST_AS.Interface(q) => Interface(h, q);
    | M3AST_AS.Qual_used_id(q) => Qual_used_id(h, q);
    | M3AST_AS.Module(q) => Module(h, q);
    | M3AST_AS.Unsafe(q) => Unsafe(h, q);
    | M3AST_AS.Import_item(q) => Import_item(h, q);
    | M3AST_AS.Simple_import(q) => Simple_import(h, q);
    | M3AST_AS.From_import(q) => From_import(h, q);
    | M3AST_AS.Revelation_s(q) => Revelation_s(h, q);
    | M3AST_AS.Const_decl_s(q) => Const_decl_s(h, q);
    | M3AST_AS.Type_decl_s(q) => Type_decl_s(h, q);
    | M3AST_AS.Var_decl_s(q) => Var_decl_s(h, q);
    | M3AST_AS.Exc_decl_s(q) => Exc_decl_s(h, q);
    | M3AST_AS.Proc_decl(q) => Proc_decl(h, q);
    | M3AST_PG.Inline(q) => Inline(h, q);
    | M3AST_PG.External(q) => External(h, q);
    | M3AST_AS.Const_decl(q) => Const_decl(h, q);
    | M3AST_AS.Var_decl(q) => Var_decl(h, q);
    | M3AST_AS.Exc_decl(q) => Exc_decl(h, q);
    | M3AST_AS.Subtype_decl(q) => Subtype_decl(h, q);
    | M3AST_AS.TYPE_DECL(q) => TYPE_DECL(h, q);
    | M3AST_AS.Concrete_decl(q) => Concrete_decl(h, q);
    | M3AST_AS.REVELATION(q) => REVELATION(h, q);
    | M3AST_AS.Concrete_reveal(q) => Concrete_reveal(h, q);
    | M3AST_AS.Subtype_reveal(q) => Subtype_reveal(h, q);
    | M3AST_AS.Named_type(q) => Named_type(h, q)
    | M3AST_AS.Integer_type(q) => Integer_type(h, q)
    | M3AST_AS.Real_type(q) => Real_type(h, q)
    | M3AST_AS.LongReal_type(q) => LongReal_type(h, q)
    | M3AST_AS.Extended_type(q) => Extended_type(h, q)
    | M3AST_AS.Null_type(q) => Null_type(h, q)
    | M3AST_AS.RefAny_type(q) => RefAny_type(h, q)
    | M3AST_AS.Address_type(q) => Address_type(h, q)
    | M3AST_AS.Root_type(q) => Root_type(h, q)
    | M3AST_AS.Array_type(q) => Array_type(h, q);
    | M3AST_AS.Enumeration_type(q) => Enumeration_type(h, q)
    | M3AST_AS.Subrange_type(q) => Subrange_type(h, q)
    | M3AST_AS.Record_type(q) => Record_type(h, q)
    | M3AST_AS.Object_type(q) => Object_type(h, q)
    | M3AST_AS.Set_type(q) => Set_type(h, q)
    | M3AST_AS.Procedure_type(q) => Procedure_type(h, q)
    | M3AST_AS.Ref_type(q) => Ref_type(h, q)
    | M3AST_AS.Packed_type(q) => Packed_type(h, q)
    | M3AST_AS.Opaque_type(q) => Opaque_type(h, q)
    | M3AST_SM.Type_type(q) => Type_type(h, q);
    | M3AST_SM.Any_type(q) => Any_type(h, q);
    | M3AST_AS.Brand(q) => Brand(h, q)
    | M3AST_AS.Untraced(q) => Untraced(h, q)
    | M3AST_AS.Fields(q) => Fields(h, q)
    | M3AST_AS.Method(q) => Method(h, q)
    | M3AST_AS.Override(q) => Override(h, q)
    | M3AST_AS.Formal_param(q) => Formal_param(h, q)
    | M3AST_AS.Raisees_any(q) => Raisees_any(h, q)
    | M3AST_AS.Raisees_some(q) => Raisees_some(h, q)
    | M3AST_AS.Range(q) => Range(h, q)
    | M3AST_AS.Range_EXP(q) => Range_EXP(h, q)
    | M3AST_AS.Constructor(q) => Constructor(h, q);
    | M3AST_AS.Propagate(q) => Propagate(h, q);
    | M3AST_AS.RANGE_EXP_elem(q) => RANGE_EXP_elem(h, q);
    | M3AST_AS.Actual_elem(q) => Actual_elem(h, q);
    | M3AST_AS.BINARY(q) => BINARY(h, q);
    | M3AST_AS.UNARY(q) => UNARY(h, q);
    | M3AST_AS.Select(q) => Select(h, q);
    | M3AST_AS.Call(q) => Call(h, q);
    | M3AST_AS.Index(q) => Index(h, q);
    | M3AST_AS.Actual(q) => Actual(h, q)
    | M3AST_AS.Exp_used_id(q) => Exp_used_id(h, q)
    | M3AST_AS.LITERAL => (* nothing more to do *)
    | M3AST_AS.Block(q) => Block(h, q)
    | M3AST_AS.Assign_st(q) => Assign_st(h, q);
    | M3AST_AS.Call_st(q) => Call_st(h, q);
    | M3AST_AS.Case_st(q) => Case_st(h, q);
    | M3AST_AS.Eval_st(q) => Eval_st(h, q);
    | M3AST_AS.Exit_st(q) => Exit_st(h, q);
    | M3AST_AS.For_st(q) => For_st(h, q);
    | M3AST_AS.If_st(q) => If_st(h, q);
    | M3AST_AS.Lock_st(q) => Lock_st(h, q);
    | M3AST_AS.Loop_st(q) => Loop_st(h, q);
    | M3AST_AS.Raise_st(q) => Raise_st(h, q);
    | M3AST_AS.Repeat_st(q) => Repeat_st(h, q);
    | M3AST_AS.Return_st(q) => Return_st(h, q);
    | M3AST_AS.Try_st(q) => Try_st(h, q);
    | M3AST_AS.Typecase_st(q) => Typecase_st(h, q);
    | M3AST_AS.While_st(q) => While_st(h, q);
    | M3AST_AS.With_st(q) => With_st(h, q);
    | M3AST_AS.Case(q) => Case(h, q);
    | M3AST_AS.Else_stm(q) => Else_stm(h, q);
    | M3AST_AS.By(q) => By(h, q);
    | M3AST_AS.Elsif(q) => Elsif(h, q);
    | M3AST_AS.Try_except(q) => Try_except(h, q);
    | M3AST_AS.Try_finally(q) => Try_finally(h, q);
    | M3AST_AS.Tcase(q) => Tcase(h, q);
    | M3AST_AS.Handler(q) => Handler(h, q);
    | M3AST_AS.Binding(q) => Binding(h, q);
    ELSE
      M3Assert.Check(FALSE);
    END
  END DoIt;

PROCEDURE DoUNIT_WITH_BODY(h: Handle; n: M3AST_AS.UNIT_WITH_BODY)=
  VAR
    m2: M3AST_AS.IMPORTED;
    iter2 := SeqM3AST_AS_IMPORTED.NewIter(n.as_import_s);
  BEGIN
    IF NOT SeqM3AST_AS_IMPORTED.Empty(n.as_import_s) THEN NL(h, n); END;
    WHILE SeqM3AST_AS_IMPORTED.Next(iter2, m2) DO DoIt(h, n, m2); END;
    NL(h, n); NL(h, n);
    DoIt(h, n, n.as_block);
  END DoUNIT_WITH_BODY;

PROCEDURE DECL_Prelude(h: Handle; n: M3AST_AS.SRC_NODE; s: M3CToken.T)=
  VAR
    e_decl: M3AST_PG.EXTERNAL_DECL;
  BEGIN
    IF h.indent = 0 THEN NL(h, n); END;
    Indent(h, n);
    IF M3AST_PG.IsA_EXTERNAL_DECL(n, e_decl) THEN
      WITH x = e_decl.pg_external DO
        IF x # NIL THEN DoIt(h, n, x); END;
      END;
    END; (* if *)
    Append(h, n, NewToken(s));
    NLIncIndent(h, n);
  END DECL_Prelude;

(*PRIVATE*)
PROCEDURE UnitPostlude(h: Handle; n: M3AST_AS.UNIT) RAISES {}=
  BEGIN
    D(h, n, " " & M3CId.ToText(n.as_id.lx_symrep) & "." & "\n");
  END UnitPostlude;

PROCEDURE Interface(h: Handle; n: M3AST_AS.Interface)=
  BEGIN
    h.isModule := FALSE;
    WITH x = n.vEXTERNAL_DECL.pg_external DO
      IF x # NIL THEN DoIt(h, n, x) END;
    END;
    IF n.as_unsafe # NIL THEN DoIt(h, n, n.as_unsafe); END;
    Append(h, n, NewToken(M3CToken.INTERFACE_)); Space(h, n);
    Append(h, n, n.as_id);
    SCNL(h, n);
    DoUNIT_WITH_BODY(h, n);
    UnitPostlude(h, n);
  END Interface;

PROCEDURE Qual_used_id(h: Handle; n: M3AST_AS.Qual_used_id)=
  BEGIN
    IF n.as_intf_id # NIL THEN 
      Append(h, n, n.as_intf_id); 
      Append(h, n, NewToken(M3CToken.Dot)); 
    END;
    Append(h, n, n.as_id);
  END Qual_used_id;

PROCEDURE Module(h: Handle; n: M3AST_AS.Module)=
  VAR
    m: M3AST_AS.Used_interface_id;
    iter := SeqM3AST_AS_Used_interface_id.NewIter(n.as_export_s);
    isFirst := TRUE;
  BEGIN
    h.isModule := TRUE;
    IF n.as_unsafe # NIL THEN DoIt(h, n, n.as_unsafe); END;
    Append(h, n, NewToken(M3CToken.MODULE_)); Space(h, n);
    Append(h, n, n.as_id);
    IF NOT SeqM3AST_AS_Used_interface_id.Empty(n.as_export_s) THEN
      Space(h, n); Append(h, n, NewToken(M3CToken.EXPORTS_)); Space(h, n);
    END;
    WHILE SeqM3AST_AS_Used_interface_id.Next(iter, m) DO
      Between(h, n, isFirst, CS); Append(h, n, m);
    END;
    SCNL(h, n);
    DoUNIT_WITH_BODY(h, n);
    UnitPostlude(h, n);
  END Module;

PROCEDURE Unsafe(h: Handle; n: M3AST_AS.Unsafe)=
  BEGIN
    Append(h, n, NewToken(M3CToken.UNSAFE_)); Space(h, n);
  END Unsafe;

PROCEDURE Import_item(h: Handle; n: M3AST_AS.Import_item)=
  BEGIN
    Append(h, n, n.as_intf_id);
    IF n.as_id # NIL THEN
      Space(h, n); Append(h, n, NewToken(M3CToken.AS_)); Space(h, n); 
      Append(h, n, n.as_id);
    END;
  END Import_item;

PROCEDURE Simple_import(h: Handle; n: M3AST_AS.Simple_import)=
  VAR
    m: M3AST_AS.Import_item;
    iter := SeqM3AST_AS_Import_item.NewIter(n.as_import_item_s);
    isFirst := TRUE;
  BEGIN
    Append(h, n, NewToken(M3CToken.IMPORT_)); Space(h, n);
    WHILE SeqM3AST_AS_Import_item.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    SCNL(h, n);
  END Simple_import;


PROCEDURE From_import(h: Handle; n: M3AST_AS.From_import)=
  VAR
    m: M3AST_AS.Used_def_id;
    iter := SeqM3AST_AS_Used_def_id.NewIter(n.as_id_s);
    isFirst := TRUE;
  BEGIN
    Append(h, n, NewToken(M3CToken.FROM_));
    Space(h, n); Append(h, n, n.as_intf_id);
    Space(h, n); Append(h, n, NewToken(M3CToken.IMPORT_));
    Space(h, n); 
    WHILE SeqM3AST_AS_Used_def_id.Next(iter, m) DO 
      Between(h, n, isFirst, CS); Append(h, n, m); 
    END;
    SCNL(h, n);
  END From_import;


PROCEDURE Revelation_s(h: Handle; n: M3AST_AS.Revelation_s)=
  VAR
    m: M3AST_AS.REVELATION;
    iter := SeqM3AST_AS_REVELATION.NewIter(n.as_reveal_s);
  BEGIN
    DECL_Prelude(h, n, M3CToken.REVEAL_);
    WHILE SeqM3AST_AS_REVELATION.Next(iter, m) DO DoIt(h, n, m); END;
    DecIndent(h);
  END Revelation_s;


PROCEDURE Const_decl_s(h: Handle; n: M3AST_AS.Const_decl_s)=
  VAR
    m: M3AST_AS.Const_decl;
    iter := SeqM3AST_AS_Const_decl.NewIter(n.as_const_decl_s);
  BEGIN
    DECL_Prelude(h, n, M3CToken.CONST_);
    WHILE SeqM3AST_AS_Const_decl.Next(iter, m) DO DoIt(h, n, m); END;
    DecIndent(h);
  END Const_decl_s;


PROCEDURE Type_decl_s(h: Handle; n: M3AST_AS.Type_decl_s)=
  VAR
    m: M3AST_AS.TYPE_DECL;
    iter := SeqM3AST_AS_TYPE_DECL.NewIter(n.as_type_decl_s);
  BEGIN
    DECL_Prelude(h, n, M3CToken.TYPE_);
    WHILE SeqM3AST_AS_TYPE_DECL.Next(iter, m) DO DoIt(h, n, m); END;
    DecIndent(h);
  END Type_decl_s;


PROCEDURE Var_decl_s(h: Handle; n: M3AST_AS.Var_decl_s)=
  VAR
    m: M3AST_AS.Var_decl;
    iter := SeqM3AST_AS_Var_decl.NewIter(n.as_var_decl_s);
  BEGIN
    DECL_Prelude(h, n, M3CToken.VAR_);
    WHILE SeqM3AST_AS_Var_decl.Next(iter, m) DO DoIt(h, n, m); END;
    DecIndent(h);
  END Var_decl_s;


PROCEDURE Exc_decl_s(h: Handle; n: M3AST_AS.Exc_decl_s)=
  VAR
    m: M3AST_AS.Exc_decl;
    iter := SeqM3AST_AS_Exc_decl.NewIter(n.as_exc_decl_s);
  BEGIN
    DECL_Prelude(h, n, M3CToken.EXCEPTION_);
    WHILE SeqM3AST_AS_Exc_decl.Next(iter, m) DO DoIt(h, n, m); END;
    DecIndent(h);
  END Exc_decl_s;


PROCEDURE Proc_decl(h: Handle; n: M3AST_AS.Proc_decl)=
  BEGIN
    NL(h, n); Indent(h, n);
    WITH x = n.vEXTERNAL_DECL.pg_external DO
      IF x # NIL THEN DoIt(h, n, x); END;
    END;
    IF n.pg_inline # NIL THEN DoIt(h, n, n.pg_inline); END;
    Append(h, n, NewToken(M3CToken.PROCEDURE_)); Space(h, n);
    Append(h, n, n.as_id);
    h.suppressPROC := TRUE; DoIt(h, n, n.as_type);
    IF n.as_body # NIL THEN 
      Append(h, n, NewToken(M3CToken.Equal)); 
      NLIncIndent(h, n);
      DoIt(h, n, n.as_body); 
      D(h, n, " " & M3CId.ToText(n.as_id.lx_symrep));
      DecIndent(h);
    END;
    SCNL(h, n);
  END Proc_decl;

PROCEDURE Inline(h: Handle; n: M3AST_PG.Inline)=
  BEGIN
    (* This ought to be a real pragma node *)
    D(h, n, "<*INLINE*> ");
  END Inline;

PROCEDURE External(h: Handle; n: M3AST_PG.External)=
  BEGIN
    (* This ought to be a real pragma node *)
    D(h, n, "<*EXTERNAL");
    IF n.lx_lang_spec # NIL THEN
      Space(h, n);
      D(h, n, M3CLiteral.ToText(n.lx_lang_spec));
      Space(h, n);
    END;
    D(h, n, "*> ");
  END External;

PROCEDURE Const_decl(h: Handle; n: M3AST_AS.Const_decl)=
  BEGIN
    Indent(h, n);
    Append(h, n, n.as_id);
    IF n.as_type # NIL THEN
      Append(h, n, NewToken(M3CToken.Colon));
      DoIt(h, n, n.as_type);
    END;
    Space(h, n); Append(h, n, NewToken(M3CToken.Equal));
    Space(h, n); DoIt(h, n, n.as_exp);
    SCNL(h, n);
  END Const_decl;


PROCEDURE Var_decl(h: Handle; n: M3AST_AS.Var_decl)=
  VAR
    m: M3AST_AS.Var_id;
    iter := SeqM3AST_AS_Var_id.NewIter(n.as_id_s);
    isFirst := TRUE;
  BEGIN
    Indent(h, n);
    WHILE SeqM3AST_AS_Var_id.Next(iter, m) DO 
      Between(h, n, isFirst, CS); Append(h, n , m);
    END;
    IF n.as_type # NIL THEN
      Append(h, n, NewToken(M3CToken.Colon)); Space(h, n);
      DoIt(h, n, n.as_type);
    END;
    IF n.as_default # NIL THEN
      Space(h, n); Append(h, n, NewToken(M3CToken.Becomes)); Space(h, n);
      DoIt(h, n, n.as_default);
    END;
    SCNL(h, n);
  END Var_decl;


PROCEDURE Exc_decl(h: Handle; n: M3AST_AS.Exc_decl)=
  BEGIN
    Indent(h, n);
    Append(h, n, n.as_id);
    IF n.as_type # NIL THEN 
      Append(h, n, NewToken(M3CToken.Bra));
      DoIt(h, n, n.as_type);
      Append(h, n, NewToken(M3CToken.Ket));
    END;
    SCNL(h, n);
  END Exc_decl;


PROCEDURE Subtype_decl(h: Handle; n: M3AST_AS.Subtype_decl)=
  BEGIN
    TYPE_DECL(h, n);
  END Subtype_decl;

PROCEDURE TYPE_DECL(h: Handle; n: M3AST_AS.TYPE_DECL)=
  BEGIN
    Indent(h, n);
    Append(h, n, n.as_id);
    Space(h, n); 
    IF ISTYPE(n, M3AST_AS.Subtype_decl) THEN
      Append(h, n, NewToken(M3CToken.Subtype))
    ELSE Append(h, n, NewToken(M3CToken.Equal));
    END;
    Space(h, n);
    DoIt(h, n, n.as_type);
    SCNL(h, n);
  END TYPE_DECL;


PROCEDURE Concrete_decl(h: Handle; n: M3AST_AS.Concrete_decl)=
  BEGIN
    TYPE_DECL(h, n);
  END Concrete_decl;


PROCEDURE Subtype_reveal(h: Handle; n: M3AST_AS.Subtype_reveal)=
  BEGIN
    REVELATION(h, n);
  END Subtype_reveal;


(* PRIVATE *)
PROCEDURE REVELATION(h: Handle; n: M3AST_AS.REVELATION)=
  BEGIN
    Indent(h, n);
    DoIt(h, n, n.as_qual_id);
    Space(h, n); 
    IF ISTYPE(n, M3AST_AS.Subtype_reveal) THEN
      Append(h, n, NewToken(M3CToken.Subtype))
    ELSE Append(h, n, NewToken(M3CToken.Equal));
    END;
    Space(h, n); 
    DoIt(h, n, n.as_type);
    SCNL(h, n);
  END REVELATION;

PROCEDURE Concrete_reveal(h: Handle; n: M3AST_AS.Concrete_reveal)=
  BEGIN
    REVELATION(h, n);
  END Concrete_reveal;


PROCEDURE Named_type(h: Handle; n: M3AST_AS.Named_type)=
  BEGIN
    DoIt(h, n, n.as_qual_id);
  END Named_type;


PROCEDURE Integer_type(
    h: Handle; n: M3AST_AS.Integer_type)=
  BEGIN
    Append(h, n, NewToken(M3CToken.INTEGER_));
  END Integer_type;

PROCEDURE Real_type(
    h: Handle; n: M3AST_AS.Real_type;)=
  BEGIN
    Append(h, n, NewToken(M3CToken.REAL_));
  END Real_type;

PROCEDURE LongReal_type(
    h: Handle; n: M3AST_AS.LongReal_type;)=
  BEGIN
    Append(h, n, NewToken(M3CToken.LONGREAL_));
  END LongReal_type;

PROCEDURE Extended_type(
    h: Handle; n: M3AST_AS.Extended_type;)=
  BEGIN
    Append(h, n, NewToken(M3CToken.EXTENDED_));
  END Extended_type;

PROCEDURE Null_type(
    h: Handle; n: M3AST_AS.Null_type;)=
  BEGIN
    Append(h, n, NewToken(M3CToken.NULL_));
  END Null_type;

PROCEDURE RefAny_type(
    h: Handle; n: M3AST_AS.RefAny_type;)=
  BEGIN
    Append(h, n, NewToken(M3CToken.REFANY_));
  END RefAny_type;

PROCEDURE Address_type(
    h: Handle; n: M3AST_AS.Address_type;)=
  BEGIN
    Append(h, n, NewToken(M3CToken.ADDRESS_));
  END Address_type;

PROCEDURE Root_type(
    h: Handle; n: M3AST_AS.Root_type)=
  BEGIN
    IF n.as_trace_mode # NIL THEN
      Append(h, n, NewToken(M3CToken.UNTRACED_));
      Append(h, n, NewToken(M3CToken.ROOT_));
    ELSE Append(h, n, NewToken(M3CToken.ROOT_));
    END;
  END Root_type;

PROCEDURE Array_type(h: Handle; n: M3AST_AS.Array_type)=
  VAR
    m: M3AST_AS.M3TYPE;
    iter := SeqM3AST_AS_M3TYPE.NewIter(n.as_indextype_s);
    isFirst := TRUE;
  BEGIN
    Append(h, n, NewToken(M3CToken.ARRAY_));
    Space(h, n);
    IF NOT SeqM3AST_AS_M3TYPE.Exhausted(iter) THEN
      WHILE SeqM3AST_AS_M3TYPE.Next(iter, m) DO 
        Between(h, n, isFirst, CommaSpace); DoIt(h, n, m); 
      END;
    Space(h, n);
    END;  
    Append(h, n, NewToken(M3CToken.OF_));
    Space(h, n);
    DoIt(h, n, n.as_elementtype);
  END Array_type;

PROCEDURE Enumeration_type(h: Handle; n: M3AST_AS.Enumeration_type)=
  VAR
    m: M3AST_AS.Enum_id;
    iter := SeqM3AST_AS_Enum_id.NewIter(n.as_id_s);
    isFirst := TRUE;
  BEGIN
    Append(h, n, NewToken(M3CToken.CurlyBra));
    WHILE SeqM3AST_AS_Enum_id.Next(iter, m) DO 
      Between(h, n, isFirst, CS);
      Append(h, n, m);
    END;
    Append(h, n, NewToken(M3CToken.CurlyKet));
  END Enumeration_type;


PROCEDURE Subrange_type(h: Handle; n: M3AST_AS.Subrange_type)=
  BEGIN
    Append(h, n, NewToken(M3CToken.SquareBra));
    DoIt(h, n, n.as_range);
    Append(h, n, NewToken(M3CToken.SquareKet));
  END Subrange_type;

PROCEDURE Record_type(h: Handle; n: M3AST_AS.Record_type)=
  VAR
    m: M3AST_AS.Fields;
    iter := SeqM3AST_AS_Fields.NewIter(n.as_fields_s);
  BEGIN
    Append(h, n, NewToken(M3CToken.RECORD_));
    NLIncIndent(h, n);
    WHILE SeqM3AST_AS_Fields.Next(iter, m) DO 
      DoIt(h, n, m); 
    END;
    DecIndent(h); Indent(h, n);
    Append(h, n, NewToken(M3CToken.END_));
  END Record_type;


PROCEDURE Object_type(h: Handle; n: M3AST_AS.Object_type)
     =
  VAR
    m: M3AST_AS.Fields;
    iter := SeqM3AST_AS_Fields.NewIter(n.as_fields_s);
    m2: M3AST_AS.Method;
    iter2 := SeqM3AST_AS_Method.NewIter(n.as_method_s);
    m3: M3AST_AS.Override;
    iter3 := SeqM3AST_AS_Override.NewIter(n.as_override_s);
  BEGIN
    IF n.as_ancestor # NIL THEN DoIt(h, n, n.as_ancestor); Space(h, n); END;
    IF n.as_brand # NIL THEN DoIt(h, n, n.as_brand); END;
    Append(h, n, NewToken(M3CToken.OBJECT_));
    NLIncIndent(h, n);
    WHILE SeqM3AST_AS_Fields.Next(iter, m) DO DoIt(h, n, m); END;
    IF NOT SeqM3AST_AS_Method.Empty(n.as_method_s) THEN
      DecIndent(h); Indent(h, n);
      Append(h, n, NewToken(M3CToken.METHODS_));
      NLIncIndent(h, n);
      WHILE SeqM3AST_AS_Method.Next(iter2, m2) DO DoIt(h, n, m2); END;
    END; (* if *)
    IF NOT SeqM3AST_AS_Override.Empty(n.as_override_s) THEN
      DecIndent(h); Indent(h, n);
      Append(h, n, NewToken(M3CToken.OVERRIDES_));
      NLIncIndent(h, n);
      WHILE SeqM3AST_AS_Override.Next(iter3, m3) DO DoIt(h, n, m3); END;
    END; (* if *)
    DecIndent(h); Indent(h, n);
    Append(h, n, NewToken(M3CToken.END_));
  END Object_type;


PROCEDURE Set_type(h: Handle; n: M3AST_AS.Set_type)=
  BEGIN
    Append(h, n, NewToken(M3CToken.SET_)); Space(h, n);
    Append(h, n, NewToken(M3CToken.OF_)); Space(h, n);
    DoIt(h, n, n.as_type);
  END Set_type;


PROCEDURE Procedure_type(h: Handle; n: M3AST_AS.Procedure_type)=
  VAR
    m: M3AST_AS.Formal_param;
    iter := SeqM3AST_AS_Formal_param.NewIter(n.as_formal_param_s);
    isFirst := TRUE;
  BEGIN
    IF NOT h.suppressPROC THEN 
      Append(h, n, NewToken(M3CToken.PROCEDURE_)); Space(h, n);
      Append(h, n, NewToken(M3CToken.Bra));
    ELSE Append(h, n, NewToken(M3CToken.Bra));
    END;
    h.suppressPROC := FALSE;
    WHILE SeqM3AST_AS_Formal_param.Next(iter, m) DO
      Between(h, n, isFirst, ScS); DoIt(h, n, m); 
    END;
    Append(h, n, NewToken(M3CToken.Ket));
    IF n.as_result_type # NIL THEN 
      Append(h, n, NewToken(M3CToken.Colon));
      Space(h, n);
      DoIt(h, n, n.as_result_type); 
    END;
    IF n.as_raises # NIL THEN DoIt(h, n, n.as_raises); END;
  END Procedure_type;


PROCEDURE Ref_type(h: Handle; n: M3AST_AS.Ref_type)=
  BEGIN
    IF n.as_trace_mode # NIL THEN DoIt(h, n, n.as_trace_mode); END;
    IF n.as_brand # NIL THEN DoIt(h, n, n.as_brand); END;
    Append(h, n, NewToken(M3CToken.REF_)); Space(h, n);
    DoIt(h, n, n.as_type);
  END Ref_type;

PROCEDURE Packed_type(h: Handle; n: M3AST_AS.Packed_type)=
  BEGIN
    Append(h, n, NewToken(M3CToken.BITS_)); Space(h, n);
    DoIt(h, n, n.as_exp);
    Space(h, n); Append(h, n, NewToken(M3CToken.FOR_)); Space(h, n);
    DoIt(h, n, n.as_type);
  END Packed_type;


PROCEDURE Opaque_type(h: Handle; n: M3AST_AS.Opaque_type)=
  BEGIN
    DoIt(h, n, n.as_type);
  END Opaque_type;


PROCEDURE Type_type(h: Handle; n: M3AST_SM.Type_type)=
  BEGIN
    D(h, n, "M3TYPE");
  END Type_type;


PROCEDURE Any_type(h: Handle; n: M3AST_SM.Any_type)=
  BEGIN
    D(h, n, "ANY");
  END Any_type;


PROCEDURE Brand(h: Handle; n: M3AST_AS.Brand)=
  BEGIN
    Append(h, n, NewToken(M3CToken.BRANDED_)); Space(h, n);
    IF n.as_exp # NIL THEN DoIt(h, n, n.as_exp) END;
  END Brand;


PROCEDURE Untraced(h: Handle; n: M3AST_AS.Untraced)=
  BEGIN
    Append(h, n, NewToken(M3CToken.UNTRACED_)); Space(h, n);
  END Untraced;


PROCEDURE Fields(h: Handle; n: M3AST_AS.Fields)=
  VAR
    m: M3AST_AS.Field_id;
    iter := SeqM3AST_AS_Field_id.NewIter(n.as_id_s);
    isFirst := TRUE;
  BEGIN
    Indent(h, n);
    WHILE SeqM3AST_AS_Field_id.Next(iter, m) DO 
      Between(h, n, isFirst, CS); Append(h, n, m); 
    END;
    IF n.as_type # NIL THEN
      Append(h, n, NewToken(M3CToken.Colon)); Space(h, n);
      DoIt(h, n, n.as_type); 
    END;
    IF n.as_default # NIL THEN 
      Space(h, n); Append(h, n, NewToken(M3CToken.Becomes)); Space(h, n);
      DoIt(h, n, n.as_default);
    END;
    SCNL(h, n);
  END Fields;


PROCEDURE Method(h: Handle; n: M3AST_AS.Method)=
  BEGIN
    Indent(h, n);
    Append(h, n, n.as_id);
    h.suppressPROC := TRUE; DoIt(h, n, n.as_type);
    IF n.as_default # NIL THEN
      Space(h, n); Append(h, n, NewToken(M3CToken.Becomes)); Space(h, n); 
      DoIt(h, n, n.as_default);
    END;
    SCNL(h, n);
  END Method;


PROCEDURE Override(h: Handle; n: M3AST_AS.Override)=
  BEGIN
    Indent(h, n);
    Append(h, n, n.as_id);
    Space(h, n); Append(h, n, NewToken(M3CToken.Becomes)); Space(h, n); 
    DoIt(h, n, n.as_default);
    SCNL(h, n);
  END Override;


PROCEDURE Formal_param(h: Handle; n: M3AST_AS.Formal_param)=
  VAR
    m: M3AST_AS.FORMAL_ID;
    iter := SeqM3AST_AS_FORMAL_ID.NewIter(n.as_id_s);
    isFirst := TRUE;
  BEGIN
    WHILE SeqM3AST_AS_FORMAL_ID.Next(iter, m) DO 
      IF isFirst THEN
        TYPECASE m OF
        | M3AST_AS.F_Var_id => 
            Append(h, n, NewToken(M3CToken.VAR_)); Space(h, n);
        | M3AST_AS.F_Readonly_id =>
            Append(h, n, NewToken(M3CToken.READONLY_)); Space(h, n);
        ELSE (* VALUE implied *)
        END;
      END; (* if *)
      Between(h, n, isFirst, CS); Append(h, n, m); 
    END;
    IF n.as_formal_type # NIL THEN
      Append(h, n, NewToken(M3CToken.Colon)); Space(h, n);
      DoIt(h, n, n.as_formal_type);
    END;
    IF n.as_default # NIL THEN
      Space(h, n); Append(h, n, NewToken(M3CToken.Becomes)); Space(h, n);
      DoIt(h, n, n.as_default);
    END;
  END Formal_param;


PROCEDURE Raisees_any(h: Handle; n: M3AST_AS.Raisees_any)=
  BEGIN
    Space(h, n);
    Append(h, n, NewToken(M3CToken.RAISES_));
    Space(h, n);
    Append(h, n, NewToken(M3CToken.ANY_));
  END Raisees_any;


PROCEDURE Raisees_some(h: Handle; n: M3AST_AS.Raisees_some)=
  VAR
    m: M3AST_AS.Qual_used_id;
    iter := SeqM3AST_AS_Qual_used_id.NewIter(n.as_raisees_s);
    isFirst := TRUE;
  BEGIN
    Space(h, n);
    Append(h, n, NewToken(M3CToken.RAISES_));
    Space(h, n);
    Append(h, n, NewToken(M3CToken.CurlyBra));
    WHILE SeqM3AST_AS_Qual_used_id.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    Append(h, n, NewToken(M3CToken.CurlyKet));
  END Raisees_some;


PROCEDURE Range(h: Handle; n: M3AST_AS.Range)=
  BEGIN
    DoIt(h, n, n.as_exp1);
    Space(h, n);
    Append(h, n, NewToken(M3CToken.Range));
    Space(h, n);
    DoIt(h, n, n.as_exp2);
  END Range;


PROCEDURE Range_EXP(h: Handle; n: M3AST_AS.Range_EXP)=
  BEGIN
    DoIt(h, n, n.as_exp);
  END Range_EXP;

PROCEDURE Constructor(h: Handle; n: M3AST_AS.Constructor)=
  VAR
    m: M3AST_AS.CONS_ELEM;
    iter := SeqM3AST_AS_CONS_ELEM.NewIter(n.as_element_s);
    isFirst := TRUE;
  BEGIN
    DoIt(h, n, n.as_type);
    Space(h, n);
    Append(h, n, NewToken(M3CToken.CurlyBra));
    WHILE SeqM3AST_AS_CONS_ELEM.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    IF n.as_propagate # NIL THEN DoIt(h, n, n.as_propagate); END;
    Append(h, n, NewToken(M3CToken.CurlyKet));
  END Constructor;


PROCEDURE Propagate(h: Handle; n: M3AST_AS.Propagate)=
  BEGIN
    Append(h, n, NewToken(M3CToken.Comma));
    Space(h, n);
    Append(h, n, NewToken(M3CToken.Range));
  END Propagate;

PROCEDURE RANGE_EXP_elem(h: Handle; n: M3AST_AS.RANGE_EXP_elem)=
  BEGIN
    DoIt(h, n, n.as_range_exp);
  END RANGE_EXP_elem;


PROCEDURE Actual_elem(h: Handle; n: M3AST_AS.Actual_elem)=
  BEGIN
    DoIt(h, n, n.as_actual);
  END Actual_elem;

PROCEDURE LowerPrec(child, parent: M3AST_AS.EXP; right: BOOLEAN
    ): BOOLEAN RAISES {}=
  VAR c_prec, p_prec: INTEGER;
  BEGIN
    (* Returns TRUE if child is lower precedence than parent 
      (and hence needs bracketing). 'right' denotes whether this
      is the right hand side, for which a bracket is needed
      if precedence is equal.
    *)

    (* parent is either a Binary or Unary; child may be any expression *)
    TYPECASE child OF
    | M3AST_AS.BINARY(b) =>
        c_prec := BPrec(b);
    | M3AST_AS.UNARY(u) =>
        c_prec := UPrec(u);
    ELSE
      RETURN FALSE
    END; (* if *)

    TYPECASE parent OF <*NOWARN*>
    | M3AST_AS.BINARY(b) =>
        p_prec := BPrec(b);
    | M3AST_AS.UNARY(u) =>
        p_prec := UPrec(u);
    END; (* if *)

    RETURN c_prec < p_prec OR (right AND c_prec = p_prec);
  END LowerPrec;

(*PRIVATE*)
PROCEDURE BPrec(op: M3AST_AS.BINARY): INTEGER RAISES {}=
  BEGIN
    TYPECASE op OF <*NOWARN*>
    | M3AST_AS.Times, M3AST_AS.Rdiv, 
      M3AST_AS.Div, M3AST_AS.Mod => RETURN 7;

    | M3AST_AS.Plus, M3AST_AS.Minus, M3AST_AS.Textcat => RETURN 6;

    | M3AST_AS.Eq, M3AST_AS.Ne, 
      M3AST_AS.Gt, M3AST_AS.Lt,
      M3AST_AS.Ge, M3AST_AS.Le, M3AST_AS.In => RETURN 5;
    
    | M3AST_AS.And => RETURN 3;

    | M3AST_AS.Or => RETURN 2;
    END; (* case *)
  END BPrec;

(*PRIVATE*)
PROCEDURE UPrec(op: M3AST_AS.UNARY): INTEGER RAISES {}=
  BEGIN
    TYPECASE op OF <*NOWARN*>
    | M3AST_AS.Deref => RETURN 9;

    | M3AST_AS.Unaryplus, M3AST_AS.Unaryminus => RETURN 8;

    | M3AST_AS.Not => RETURN 4;
    END; (* case *)
  END UPrec;


PROCEDURE BINARY(h: Handle; n: M3AST_AS.BINARY)=
  VAR bracket := FALSE;
  BEGIN
    IF LowerPrec(n.as_exp1, n, right := FALSE) THEN
      Append(h, n, NewToken(M3CToken.Bra)); bracket := TRUE;
    END; (* if *)
    DoIt(h, n, n.as_exp1);
    IF bracket THEN Append(h, n, NewToken(M3CToken.Ket)) END;
    Space(h, n); Append(h, n, NewToken(BTokenFor(n))); Space(h, n); 
    bracket := FALSE;
    IF LowerPrec(n.as_exp2, n, right := TRUE) THEN
       Append(h, n, NewToken(M3CToken.Bra)); bracket := TRUE;
    END; (* if *)
    DoIt(h, n, n.as_exp2);
    IF bracket THEN Append(h, n, NewToken(M3CToken.Ket)) END;
  END BINARY;

PROCEDURE BTokenFor(n: M3AST_AS.BINARY): M3CToken.T=
  BEGIN
    TYPECASE n OF <*NOWARN*>
    | M3AST_AS.Plus => RETURN M3CToken.Plus;
    | M3AST_AS.Minus => RETURN M3CToken.Minus;
    | M3AST_AS.Times => RETURN M3CToken.Times;
    | M3AST_AS.Rdiv => RETURN M3CToken.Divide;
    | M3AST_AS.Textcat => RETURN M3CToken.Ampersand;
    | M3AST_AS.Div => RETURN M3CToken.DIV_;
    | M3AST_AS.Mod => RETURN M3CToken.MOD_;
    | M3AST_AS.Eq => RETURN M3CToken.Equal;
    | M3AST_AS.Ne => RETURN M3CToken.NotEqual;
    | M3AST_AS.Gt => RETURN M3CToken.GreaterThan;
    | M3AST_AS.Lt => RETURN M3CToken.LessThan;
    | M3AST_AS.Ge => RETURN M3CToken.GreaterThanOrEqual;
    | M3AST_AS.Le => RETURN M3CToken.LessThanOrEqual;
    | M3AST_AS.And => RETURN M3CToken.AND_;
    | M3AST_AS.Or => RETURN M3CToken.OR_;
    | M3AST_AS.In => RETURN M3CToken.IN_;
    END;
  END BTokenFor;

PROCEDURE UNARY(h: Handle; n: M3AST_AS.UNARY)=
  VAR bracket := FALSE;
  BEGIN
    IF NOT ISTYPE(n, M3AST_AS.Deref) THEN 
      Append(h, n, NewToken(UTokenFor(n)));
      IF ISTYPE(n, M3AST_AS.Not) THEN Space(h, n); END;
    END;
    IF LowerPrec(n.as_exp, n, right := TRUE) THEN
      Append(h, n, NewToken(M3CToken.Bra)); bracket := TRUE;
    END; (* if *)
    DoIt(h, n, n.as_exp);
    IF ISTYPE(n, M3AST_AS.Deref) THEN 
      Append(h, n, NewToken(UTokenFor(n)));
    END;
    IF bracket THEN Append(h, n, NewToken(M3CToken.Ket)) END;
  END UNARY;

PROCEDURE UTokenFor(n: M3AST_AS.UNARY): M3CToken.T=
  BEGIN
    TYPECASE n OF <*NOWARN*>
    | M3AST_AS.Not => RETURN M3CToken.NOT_;
    | M3AST_AS.Unaryplus => RETURN M3CToken.Plus;
    | M3AST_AS.Unaryminus => RETURN M3CToken.Minus;
    | M3AST_AS.Deref => RETURN M3CToken.Dereference;
    END
  END UTokenFor;

PROCEDURE Select(h: Handle; n: M3AST_AS.Select)=
  BEGIN
    DoIt(h, n, n.as_exp);
    Append(h, n, NewToken(M3CToken.Dot));
    DoIt(h, n, n.as_id);
  END Select;

PROCEDURE Call(h: Handle; n: M3AST_AS.Call)=
  VAR
    m: M3AST_AS.Actual;
    iter := SeqM3AST_AS_Actual.NewIter(n.as_param_s);
    isFirst := TRUE;
  BEGIN
    DoIt(h, n, n.as_callexp);
    Append(h, n, NewToken(M3CToken.Bra));
    WHILE SeqM3AST_AS_Actual.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    Append(h, n, NewToken(M3CToken.Ket));
  END Call;


PROCEDURE Index(h: Handle; n: M3AST_AS.Index)=
  VAR
    m: M3AST_AS.EXP;
    iter := SeqM3AST_AS_EXP.NewIter(n.as_exp_s);
    isFirst := TRUE;
  BEGIN
    DoIt(h, n, n.as_array);
    Append(h, n, NewToken(M3CToken.SquareBra));
    WHILE SeqM3AST_AS_EXP.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    Append(h, n, NewToken(M3CToken.SquareKet));
  END Index;

PROCEDURE Actual(h: Handle; n: M3AST_AS_F.Actual)=
  BEGIN
    IF n.as_id # NIL THEN
      DoIt(h, n, n.as_id);
      Space(h, n);
      Append(h, n, NewToken(M3CToken.Becomes)); Space(h, n);
    END;
    DoIt(h, n, n.as_exp_type);
  END Actual;


PROCEDURE Exp_used_id(h: Handle; n: M3AST_AS.Exp_used_id)= 
  BEGIN
    Append(h, n, n.vUSED_ID);
  END Exp_used_id;

PROCEDURE Block(h: Handle; n: M3AST_AS.Block)=

  PROCEDURE LastNodeOf(n: M3AST_AS.SRC_NODE): M3AST_AS.SRC_NODE=
    VAR result: M3AST_AS.SRC_NODE := NIL;
    PROCEDURE Visit(n: M3AST_AS.SRC_NODE)=
       VAR iter := n.newIter(); child: AST.NODE;
       BEGIN
         result := n;
         WHILE iter.next(child) DO
           IF child # NIL THEN
             Visit(child) 
           END;
         END;
       END Visit;
    BEGIN
      Visit(n);
      RETURN result
    END LastNodeOf;

  VAR
    m: M3AST_AS.DECL_REVL;
    iter := SeqM3AST_AS_DECL_REVL.NewIter(n.as_decl_s);
  BEGIN
    WHILE SeqM3AST_AS_DECL_REVL.Next(iter, m) DO
      DoIt(h, n, m); 
      IF ISTYPE(h.this_unit_id, M3AST_AS.Interface_id) THEN
        FlushComments(h, n, FALSE, LastNodeOf(m));
      END;
    END;
    IF h.isModule THEN
      Indent(h, n);
      Append(h, n, NewToken(M3CToken.BEGIN_));
      NLIncIndent(h, n);
    END; (* if *) 
    VisitSeqStm(h, n, n.as_stm_s);
    IF h.isModule THEN DecIndent(h); END;
    Indent(h, n); 
    Append(h, n, NewToken(M3CToken.END_));
  END Block;

PROCEDURE Assign_st(h: Handle; n: M3AST_AS.Assign_st)=
  BEGIN
    Indent(h, n);
    DoIt(h, n, n.as_lhs_exp);
    Space(h, n); Append(h, n, NewToken(M3CToken.Becomes)); Space(h, n);
    DoIt(h, n, n.as_rhs_exp);
  END Assign_st;


PROCEDURE Call_st(h: Handle; n: M3AST_AS.Call_st)=
  BEGIN
    Indent(h, n);
    DoIt(h, n, n.as_call);
  END Call_st;


PROCEDURE Case_st(h: Handle; n: M3AST_AS.Case_st)=
  VAR
    m: M3AST_AS.Case;
    iter := SeqM3AST_AS_Case.NewIter(n.as_case_s);
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.CASE_)); Space(h, n);
    DoIt(h, n, n.as_exp);
    Space(h, n); Append(h, n, NewToken(M3CToken.OF_)); NL(h, n);
    WHILE SeqM3AST_AS_Case.Next(iter, m) DO DoIt(h, n, m); END;
    IF n.as_else # NIL THEN DoIt(h, n, n.as_else); END;
    Indent(h, n); 
    Append(h, n, NewToken(M3CToken.END_));
  END Case_st;


PROCEDURE Eval_st(h: Handle; n: M3AST_AS.Eval_st)=
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.EVAL_)); Space(h, n);
    DoIt(h, n, n.as_exp);
  END Eval_st;


PROCEDURE Exit_st(h: Handle; n: M3AST_AS.Exit_st)=
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.EXIT_));
  END Exit_st;


PROCEDURE For_st(h: Handle; n: M3AST_AS.For_st)=
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.FOR_)); Space(h, n);
    Append(h, n, n.as_id); Space(h, n); Append(h, n, NewToken(M3CToken.Becomes)); Space(h, n);
    DoIt(h, n, n.as_from); Space(h, n); Append(h, n, NewToken(M3CToken.TO_)); Space(h, n);
    DoIt(h, n, n.as_to);
    IF n.as_by # NIL THEN DoIt(h, n, n.as_by); END;    
    Space(h, n); Append(h, n, NewToken(M3CToken.DO_)); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h); Indent(h, n); 
    Append(h, n, NewToken(M3CToken.END_)); 
  END For_st;


PROCEDURE If_st(h: Handle; n: M3AST_AS.If_st)=
  VAR
    m: M3AST_AS.Elsif;
    iter := SeqM3AST_AS_Elsif.NewIter(n.as_elsif_s);
  BEGIN
    Indent(h, n); Append(h, n, NewToken(M3CToken.IF_)); Space(h, n);
    DoIt(h, n, n.as_exp);
    Space(h, n); Append(h, n, NewToken(M3CToken.THEN_)); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h);
    WHILE SeqM3AST_AS_Elsif.Next(iter, m) DO DoIt(h, n, m); END;
    IF n.as_else # NIL THEN DoIt(h, n, n.as_else); END;
    Indent(h, n); 
    Append(h, n, NewToken(M3CToken.END_)); 
  END If_st;


PROCEDURE Lock_st(h: Handle; n: M3AST_AS.Lock_st)=
  BEGIN
    Indent(h, n); Append(h, n, NewToken(M3CToken.LOCK_)); Space(h, n);
    DoIt(h, n, n.as_exp);
    Space(h, n); Append(h, n, NewToken(M3CToken.DO_)); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h); Indent(h, n);
    Append(h, n, NewToken(M3CToken.END_)); 
  END Lock_st;


PROCEDURE Loop_st(h: Handle; n: M3AST_AS.Loop_st)=
  BEGIN
    Indent(h, n); Append(h, n, NewToken(M3CToken.LOOP_));
    NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h); Indent(h, n);
    Append(h, n, NewToken(M3CToken.END_)); 
  END Loop_st;


PROCEDURE Raise_st(h: Handle; n: M3AST_AS.Raise_st)=
  BEGIN
    Indent(h, n); Append(h, n, NewToken(M3CToken.RAISE_)); Space(h, n);
    DoIt(h, n, n.as_qual_id);
    IF n.as_exp_void # NIL THEN 
      Space(h, n); 
      Append(h, n, NewToken(M3CToken.Bra)); 
      DoIt(h, n, n.as_exp_void); 
      Append(h, n, NewToken(M3CToken.Ket)); 
    END;
  END Raise_st;


PROCEDURE Repeat_st(h: Handle; n: M3AST_AS.Repeat_st)=
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.REPEAT_));
    NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h); Indent(h, n);
    Append(h, n, NewToken(M3CToken.UNTIL_)); Space(h, n);
    DoIt(h, n, n.as_exp);
  END Repeat_st;


PROCEDURE Return_st(h: Handle; n: M3AST_AS.Return_st)=
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.RETURN_)); Space(h, n);
    IF n.as_exp # NIL THEN DoIt(h, n, n.as_exp); END;
  END Return_st;


PROCEDURE Try_st(h: Handle; n: M3AST_AS.Try_st)=
  BEGIN
    Indent(h, n); Append(h, n, NewToken(M3CToken.TRY_));
    NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h);
    DoIt(h, n, n.as_try_tail);
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.END_)); 
  END Try_st;


PROCEDURE Typecase_st(h: Handle; n: M3AST_AS.Typecase_st)=
  VAR
    m: M3AST_AS.Tcase;
    iter := SeqM3AST_AS_Tcase.NewIter(n.as_tcase_s);
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.TYPECASE_)); Space(h, n);
    DoIt(h, n, n.as_exp);
    Space(h, n); Append(h, n, NewToken(M3CToken.OF_)); NL(h, n);
    WHILE SeqM3AST_AS_Tcase.Next(iter, m) DO DoIt(h, n, m); END;
    IF n.as_else # NIL THEN DoIt(h, n, n.as_else); END;
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.END_)); 
  END Typecase_st;


PROCEDURE While_st(h: Handle; n: M3AST_AS.While_st)=
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.WHILE_)); Space(h, n);
    DoIt(h, n, n.as_exp);
    Space(h, n); Append(h, n, NewToken(M3CToken.DO_)); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h); Indent(h, n);
    Append(h, n, NewToken(M3CToken.END_)); 
  END While_st;


PROCEDURE With_st(h: Handle; n: M3AST_AS.With_st)=
  VAR
    m: M3AST_AS.Binding;
    iter := SeqM3AST_AS_Binding.NewIter(n.as_binding_s);
    isFirst := TRUE;
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.WITH_)); Space(h, n);
    WHILE SeqM3AST_AS_Binding.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    Space(h, n); Append(h, n, NewToken(M3CToken.DO_)); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h); Indent(h, n);
    Append(h, n, NewToken(M3CToken.END_)); 
  END With_st;


PROCEDURE Case(h: Handle; n: M3AST_AS.Case)=
  VAR
    m: M3AST_AS.RANGE_EXP;
    iter := SeqM3AST_AS_RANGE_EXP.NewIter(n.as_case_label_s);
    isFirst := TRUE;
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.Bar)); Space(h, n);
    WHILE SeqM3AST_AS_RANGE_EXP.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    Space(h, n); 
    Append(h, n, NewToken(M3CToken.Implies)); Space(h, n);
    IncIndent(h); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h); DecIndent(h); NL(h, n);
  END Case;

PROCEDURE Else_stm(h: Handle; n: M3AST_AS.Else_stm)=
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.ELSE_));
    NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h);
  END Else_stm;


PROCEDURE By(h: Handle; n: M3AST_AS.By)=
  BEGIN
    Space(h, n); Append(h, n, NewToken(M3CToken.BY_)); Space(h, n);
    DoIt(h, n, n.as_exp);
  END By;


PROCEDURE Elsif(h: Handle; n: M3AST_AS.Elsif)=
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.ELSIF_)); Space(h, n);
    DoIt(h, n, n.as_exp);
    Space(h, n); Append(h, n, NewToken(M3CToken.THEN_)); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h);
  END Elsif;


PROCEDURE Try_except(h: Handle; n: M3AST_AS.Try_except)=
  VAR
    m: M3AST_AS.Handler;
    iter := SeqM3AST_AS_Handler.NewIter(n.as_handler_s);
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.EXCEPT_)); 
    NL(h, n);
    WHILE SeqM3AST_AS_Handler.Next(iter, m) DO DoIt(h, n, m); END;
    IF n.as_else # NIL THEN DoIt(h, n, n.as_else); END;
  END Try_except;


PROCEDURE Try_finally(h: Handle; n: M3AST_AS.Try_finally)=
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.FINALLY_));
    NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h);
  END Try_finally;


PROCEDURE Tcase(h: Handle; n: M3AST_AS.Tcase)=
  VAR
    m: M3AST_AS.M3TYPE;
    iter := SeqM3AST_AS_M3TYPE.NewIter(n.as_type_s);
    isFirst := TRUE;
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.Bar)); Space(h, n);
    WHILE SeqM3AST_AS_M3TYPE.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    IF n.as_id # NIL THEN 
      Append(h, n, NewToken(M3CToken.Bra));
      Append(h, n, n.as_id); 
      Append(h, n, NewToken(M3CToken.Ket));
    END;
    Space(h, n); 
    Append(h, n, NewToken(M3CToken.Implies)); Space(h, n);
    IncIndent(h); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h); DecIndent(h); NL(h, n);
  END Tcase;


PROCEDURE Handler(h: Handle; n: M3AST_AS.Handler)=
  VAR
    m: M3AST_AS.Qual_used_id;
    iter := SeqM3AST_AS_Qual_used_id.NewIter(n.as_qual_id_s);
    isFirst := TRUE;
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.Bar)); Space(h, n);
    WHILE SeqM3AST_AS_Qual_used_id.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    IF n.as_id # NIL THEN 
      Append(h, n, NewToken(M3CToken.Bra));
      Append(h, n, n.as_id); 
      Append(h, n, NewToken(M3CToken.Ket));
    END;
    Space(h, n); 
    Append(h, n, NewToken(M3CToken.Implies)); Space(h, n); 
    IncIndent(h); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h); DecIndent(h); NL(h, n);
  END Handler;


PROCEDURE Binding(h: Handle; n: M3AST_AS.Binding)=
  BEGIN
    Append(h, n, n.as_id);
    Space(h, n); Append(h, n, NewToken(M3CToken.Equal)); Space(h, n);
    DoIt(h, n, n.as_exp);
  END Binding;

PROCEDURE VisitSeqStm(h: Handle; n: M3AST_AS.SRC_NODE_C; ss: SeqM3AST_AS_STM.T)=
  VAR
    m: M3AST_AS.STM;
    iter := SeqM3AST_AS_STM.NewIter(ss);
  BEGIN
    WHILE SeqM3AST_AS_STM.Next(iter, m) DO 
      DoIt(h, n, m);
      SCNL(h, n);
    END;
  END VisitSeqStm;

PROCEDURE FlushComments(h: Handle; n: M3AST_AS.SRC_NODE_C; follow: BOOLEAN;
    m: M3AST_AS.SRC_NODE)=
  VAR p: PROCEDURE(t: M3CComment.T): M3AST_AS.SRC_NODE;
  BEGIN
     IF follow THEN p := M3CComment.FollowingNode
     ELSE p := M3CComment.PrecedingNode;
     END;
     WHILE h.comment # NIL AND p(h.comment) = m DO
       D(h, n, M3CComment.Body(h.comment));
       NL(h, n);
       IF M3CComment.Next(h.comments, h.comment) THEN
       ELSE h.comment := NIL;
       END;
     END;
  END FlushComments;



BEGIN
  MkWS();
END StdFormat.
