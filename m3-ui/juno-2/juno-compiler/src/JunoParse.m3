(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Dec 29 11:16:34 PST 1995 by heydon                   *)
(*      modified on Sat Feb 18 19:57:03 PST 1995 by gnelson                  *)
(*      modified on Fri Aug  7 21:54:03 PDT 1992 by myers                    *)

MODULE JunoParse;

IMPORT JunoValue;
IMPORT JunoAST, JunoLex, JunoToken, JunoASTUtils;
IMPORT Rd, Text;

(* The procedures in this module are structured as follows. Not counting the
   public procedures Unit(), Command(), and Expression(), there is roughly a
   one-to-one correspondence between procedures in this module and
   non-terminals in the LL(1) Juno grammar described in the file "Juno.bnf".
   To make it easier to check that any given procedure correctly parses its
   specified non-terminal, each procedure includes the relevant productions
   from the grammar in its comment.

   Each parsing procedure may raise one of two errors: Rd.Failure and Error.
   Rd.Failure is raised when there is a problem encountered reading from their
   argument stream. Error is raised when either a lex- or parse-error occurs.

   In the event that Error is raised, we need to "return" as much of the
   partial AST constructed up to the point where the error occurred. An
   overriding concern of this implementation to guarantee that any token that
   has been successfully parsed is incorporated in the result AST, even in the
   event that Error is raised. In this way, the JunoUnparse procedures will be
   able to pretty-print all of the tokens that were read successfully. To
   facilitate error recovery, most procedures in this module return their
   result as a VAR (*OUT*) parameter. In some cases, the result is returned as
   a VAR (*INOUT*) (or VAR (*IO*)) parameter; in these cases, the parameter is
   presumed to contain a non-NIL object, some of whose fields may have already
   been filled in at the time of the call.

   Some procedures also take additional READONLY parameters. The tokens
   represented by such parameters have not been incorporated into the result
   AST at the time the procedure is called, so if some parse error occurs, it
   is the responsibility of the procedure to incorporate such READONLY
   parameters into the result AST before raising Error so as to conform to the
   rule described in the previous paragraph. For example, see the procedure
   PH2().

   The grammar contains two versions of most of the Formula/Expression
   nonterminals. The "normal" nonterminals have names like "Formula", "Form1",
   "Expr", and "Expr1". The mirrors of these nonterminals are special because
   they apply only in the case where a QId (qualified identifier) has already
   been parsed and so the formula is known to start with a QId. The
   nonterminals in this case have names formed by appending the "normal"
   formula/expression nonterminal names with "QId". Rather than implementing
   separate procedures for these nonterminals, each of the formula/expression
   procedures takes an optional READONLY QId argument. If the argument is NIL,
   then parsing occurs as for the "normal" nonterminal. Otherwise, parsing
   occurs as for the "QId" version of the nonterminal. Both productions are
   listed in the comment for these procedures. Note also that "Form2" and
   "Expr3" have no "QId" counterparts, so they don't take an optional READONLY
   argument.

   There are two procedures provided for matching the current token: Match()
   and MatchKind(). The former should only be used when the type of the
   current token is known. The latter is used when the type of the current
   token is not known, but is expected to have a particular type. *)

TYPE
  LookAhead = RECORD
    s: JunoLex.Stream;			 (* token stream *)
    t: JunoToken.T;			 (* most recently read token *)
    cnt: CARDINAL := 0;                  (* count of number of tokens parsed *)
  END;

VAR
  NilRef: REFANY := NIL;		 (* for use as arg to MatchKind() *)
  End: JunoAST.T;			 (* = JunoAST.End *)

(* ========================= TOP-LEVEL PROCEDURES ========================== *)

REVEAL
  IterativeParse = BRANDED "JunoParse.IterativeParse" OBJECT
    la: LookAhead
  END;

PROCEDURE StartIterativeParse(READONLY rd: Rd.T): IterativeParse
  RAISES { Rd.Failure, JunoLex.Error } =
  VAR
    lookAhead := LookAhead{s := JunoLex.New(rd), t := NIL};
    ip := NEW(IterativeParse, la := lookAhead);
  BEGIN
    ip.la.t := ip.la.s.next();
    INC(ip.la.cnt);
    RETURN ip;
  END StartIterativeParse;

PROCEDURE FinishIterativeParse(ip: IterativeParse) =
  BEGIN
    EVAL JunoLex.Close(ip.la.s);
  END FinishIterativeParse;

PROCEDURE GetIndex(ip: IterativeParse): INTEGER =
  BEGIN
    RETURN ip.la.s.lastPos
  END GetIndex;
  
CONST BlockSet = SET OF JunoToken.Kind
  { JunoToken.Kind.Module, JunoToken.Kind.UI, JunoToken.Kind.Private,
    JunoToken.Kind.Import, JunoToken.Kind.Comment,
    JunoToken.Kind.Const, JunoToken.Kind.Var,
    JunoToken.Kind.Pred, JunoToken.Kind.Func, JunoToken.Kind.Proc };

PROCEDURE Block(
    ip: IterativeParse;
    VAR (*OUT*) ast: JunoAST.Block;
    VAR (*OUT*) tokenCnt: CARDINAL)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
  BEGIN
    TRY
      ip.la.cnt := 0;
      IF ip.la.t.kind = JunoToken.Kind.EndMarker THEN
        ast := NIL; RETURN
      END;
      INC(ip.la.cnt);
      IF ip.la.t.kind IN BlockSet
        THEN Block2(ip.la, ast)
        ELSE RaiseError(ip.la)
      END
    FINALLY
      tokenCnt := ip.la.cnt
    END;
  END Block;

PROCEDURE Command(
    READONLY rd: Rd.T;
    VAR (*OUT*) ast: JunoAST.Cmd;
    VAR (*OUT*) tokenCnt: CARDINAL)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
  VAR la: LookAhead; BEGIN
    TRY
      la.s := JunoLex.New(rd);
      TRY la.t := la.s.next() FINALLY ast := NIL END; (* prime the stream *)
      INC(la.cnt);
      Cmd(la, ast);
      IF la.t.kind = JunoToken.Kind.EndMarker
        THEN DEC(la.cnt)
        ELSE RaiseError(la)
      END;
      EVAL JunoLex.Close(la.s)
    FINALLY
      tokenCnt := la.cnt
    END;
  END Command;

PROCEDURE Expression(
    READONLY rd: Rd.T;
    VAR (*OUT*) ast: JunoAST.Expr;
    VAR (*OUT*) tokenCnt: CARDINAL)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
  VAR la: LookAhead; BEGIN
    TRY
      la.s := JunoLex.New(rd);
      TRY la.t := la.s.next() FINALLY ast := NIL END; (* prime the stream *)
      INC(la.cnt);
      Expr(la, ast);
      IF la.t.kind = JunoToken.Kind.EndMarker
        THEN DEC(la.cnt)
        ELSE RaiseError(la)
      END;
      EVAL JunoLex.Close(la.s)
    FINALLY
      tokenCnt := la.cnt
    END;
  END Expression;

PROCEDURE FoldHeader(
    READONLY rd: Rd.T;
    VAR (*OUT*) ast: JunoAST.PredHeader;
    VAR (*OUT*) tokenCnt: CARDINAL)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
  VAR la: LookAhead; BEGIN
    TRY
      la.s := JunoLex.New(rd);
      TRY la.t := la.s.next() FINALLY ast := NIL END; (* prime the stream *)
      INC(la.cnt);
      FoldHeader2(la, ast);
      IF la.t.kind = JunoToken.Kind.EndMarker
        THEN DEC(la.cnt)
        ELSE RaiseError(la)
      END;
      EVAL JunoLex.Close(la.s)
    FINALLY
      tokenCnt := la.cnt
    END;
  END FoldHeader;

PROCEDURE IdList(
    READONLY rd: Rd.T;
    VAR (*OUT*) ast: JunoAST.IdList;
    VAR (*OUT*) tokenCnt: CARDINAL)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
  VAR la: LookAhead; BEGIN
    TRY
      la.s := JunoLex.New(rd);
      TRY la.t := la.s.next() FINALLY ast := NIL END; (* prime the stream *)
      IF la.t.kind = JunoToken.Kind.EndMarker THEN
        ast := JunoAST.EmptyIdList;
      ELSE
      	INC(la.cnt);
      	IdList0(la, ast);
      	IF la.t.kind = JunoToken.Kind.EndMarker
    	  THEN DEC(la.cnt)
    	  ELSE RaiseError(la)
      	END
      END;
      EVAL JunoLex.Close(la.s)
    FINALLY
      tokenCnt := la.cnt
    END
  END IdList;

(* =============================== BLOCKS ================================== *)

PROCEDURE Block2(VAR (*IO*) la: LookAhead; VAR (*OUT*) block: JunoAST.Block)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Assumes "la.t.kind IN BlockSet".

| Block = MODULE Id ";"
|       | [ FROM Id ] IMPORT IDList ";"
|       | Comment
|       | UI UIDecl ";"
|       | [ PRIVATE ] Decl ";".
*)
  BEGIN
    CASE la.t.kind OF
    | JunoToken.Kind.Module    => Module(la, block)
    | JunoToken.Kind.Import    => Import(la, block)
    | JunoToken.Kind.UI        => UIDecl(la, block)
    | JunoToken.Kind.Comment   => Comment(la, block)
    ELSE
      VAR private := la.t.kind = JunoToken.Kind.Private; BEGIN
        IF private THEN
          VAR raised := TRUE; BEGIN
            TRY Match(la); raised := FALSE FINALLY (* skip "PRIVATE" *)
              IF raised THEN
                (* use arbitrary JunoAST.Decl in case of error *)
                block := NEW(JunoAST.ConstDecl, private := private, bp := End)
              END
            END
          END
        END;
        Decl(la, block, private);	           (* skip decl *)
        MatchKind(la, JunoToken.Kind.Semi, NilRef) (* skip ";" *)
      END
    END
  END Block2;

PROCEDURE Module(VAR (*IO*) la: LookAhead; VAR (*OUT*) block: JunoAST.Block)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Assumes "la.t.kind = JunoToken.Kind.Module".

|  Block = MODULE Id ";".
*)
  VAR module := NEW(JunoAST.Module, bp := End); id: REFANY; BEGIN
    block := module;
    Match(la);                                       (* skip "MODULE" *)
    TRY MatchKind(la, JunoToken.Kind.Id, id) FINALLY (* skip name *)
      module.name := NARROW(id, JunoAST.Id)
    END;
    MatchKind(la, JunoToken.Kind.Semi, NilRef);      (* skip ";" *)
  END Module;

PROCEDURE Import(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) block: JunoAST.Block)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Assumes "la.t.kind = JunoToken.Kind.Import".

|  Block = IMPORT IDList ";".
*)
  VAR import := NEW(JunoAST.Import, bp := End); BEGIN
    block := import;
    Match(la);                                       (* skip "IMPORT" *)
    IdList0(la, import.idList);                      (* skip Id's *)
    MatchKind(la, JunoToken.Kind.Semi, NilRef);      (* skip ";" *)
  END Import;

PROCEDURE UIDecl(VAR (*IO*) la: LookAhead; VAR (*OUT*) block: JunoAST.Block)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Assumes "la.t.kind = JunoToken.Kind.UI".

|  Block  = UI UIDecl ";"
|  UIDecl = Id "(" [ ExprList ] ")".
*)
  VAR ui := NEW(JunoAST.UIDecl, bp := End); id: REFANY; BEGIN
    block := ui;
    Match(la);				             (* skip "UI" *)
    TRY MatchKind(la, JunoToken.Kind.Id, id) FINALLY (* skip Id *)
      ui.name := NARROW(id, JunoAST.Id)
    END;
    MatchKind(la, JunoToken.Kind.LPren, NilRef);     (* skip "(" *)
    IF la.t.kind # JunoToken.Kind.RPren
      THEN ExprList(la, ui.args)                     (* parse args *)
      ELSE ui.args := JunoAST.EmptyExprList          (* make args empty *)
    END;
    MatchKind(la, JunoToken.Kind.RPren, NilRef);     (* skip ")" *)
    MatchKind(la, JunoToken.Kind.Semi, NilRef);      (* skip ";" *)
  END UIDecl;

PROCEDURE FoldHeader2(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) res: JunoAST.PredHeader)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Parse something like a "PredHeader", but the trailing parentheses are
   optional if there are no arguments.

|  Id [ "(" [ IDList ] ")" ].

   If only "Id" is read, then the "ins" field of the result will be
   "NIL". If "Id()" is read, then the "ins" field of the result will be
   "JunoAST.EmptyIdList".
*)
  VAR id: REFANY; BEGIN
    res := NEW(JunoAST.PredHeader, bp := End);
    TRY MatchKind(la, JunoToken.Kind.Id, id) FINALLY (* read Id *)
      res.name := NARROW(id, JunoAST.Id)
    END;
    IF la.t.kind = JunoToken.Kind.EndMarker THEN
      res.ins := NIL;
      RETURN
    END;
    MatchKind(la, JunoToken.Kind.LPren, NilRef);     (* skip "(" *)
    IF la.t.kind # JunoToken.Kind.RPren
      THEN IdList0(la, res.ins)                      (* parse args *)
      ELSE res.ins := JunoAST.EmptyIdList            (* make args empty *)
    END;
    MatchKind(la, JunoToken.Kind.RPren, NilRef)      (* skip ")" *)
  END FoldHeader2;

PROCEDURE Comment(VAR (*IO*) la: LookAhead; VAR (*OUT*) block: JunoAST.Block)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Requires "la.t.kind = JunoToken.Kind.Comment". *)
  VAR comment := NEW(JunoAST.Comment, bp := End); txt: REFANY; BEGIN
    block := comment;
    TRY MatchKind(la, JunoToken.Kind.Comment, txt) FINALLY
      comment.txt := NARROW(txt, TEXT);
      comment.private := (Text.GetChar(comment.txt, 0) = '/')
    END
  END Comment;

PROCEDURE Decl(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) decl: JunoAST.Block;
    private: BOOLEAN)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  Decl	= CONST ConstDecl
| 	| VAR VarDecl
| 	| PRED PredDecl
| 	| FUNC FuncDecl
| 	| PROC ProcDecl.
*)
  BEGIN
    CASE la.t.kind OF
      JunoToken.Kind.Const => ConstDecl(la, decl, private)
    | JunoToken.Kind.Var   => VarDecl(la, decl, private)
    | JunoToken.Kind.Pred  => PredDecl(la, decl, private)
    | JunoToken.Kind.Func  => FuncDecl(la, decl, private)
    | JunoToken.Kind.Proc  => ProcDecl(la, decl, private)
    ELSE RaiseError(la)
    END
  END Decl;

PROCEDURE ConstDecl(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) decl: JunoAST.Block;
    priv: BOOLEAN)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Assumes "la.t.kind = JunoToken.Kind.Const".

|  Decl      = CONST ConstDecl.
|  ConstDecl = ConstDeclItem { "," ConstDeclItem }.
*)
  VAR constant := NEW(JunoAST.ConstDecl, private := priv, bp := End); BEGIN
    decl := constant;
    Match(la);                                       (* skip "CONST" *)
    INC(constant.size);
    ConstDeclItem(la, constant.head);	             (* skip ConstDeclItem *)
    VAR curr := constant.head; BEGIN
      WHILE la.t.kind = JunoToken.Kind.Comma DO
        INC(constant.size);
        Match(la);			             (* skip "," *)
        ConstDeclItem(la, curr.next);	             (* skip ConstDeclItem *)
        curr := curr.next
      END
    END
  END ConstDecl;

PROCEDURE ConstDeclItem(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) item: JunoAST.ConstDeclItem)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* 
|  ConstDeclItem = Id "=" ConstExpr.
*)
  VAR id: REFANY; BEGIN
    item := NEW(JunoAST.ConstDeclItem);
    TRY MatchKind(la, JunoToken.Kind.Id, id) FINALLY (* skip Id *)
      item.name := NARROW(id, JunoAST.Id)
    END;
    MatchKind(la, JunoToken.Kind.Equals, NilRef);    (* skip "=" *)
    Expr(la, item.value);                            (* skip Expr *)
  END ConstDeclItem;

PROCEDURE VarDecl(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) decl: JunoAST.Block;
    priv: BOOLEAN)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Assumes "la.t.kind = JunoToken.Kind.Var".

|  Decl    = VAR VarDecl.
|  VarDecl = VarDeclItem { "," VarDeclItem }.
*)
  VAR var := NEW(JunoAST.VarDecl, private := priv, bp := End); BEGIN
    decl := var;
    Match(la);                                       (* skip "VAR" *)
    INC(var.size);
    VarDeclItem(la, var.head);	                     (* skip VarDeclItem *)
    VAR curr := var.head; BEGIN
      WHILE la.t.kind = JunoToken.Kind.Comma DO
        INC(var.size);
        Match(la);			             (* skip "," *)
        VarDeclItem(la, curr.next);	             (* skip VarDeclItem *)
        curr := curr.next
      END
    END
  END VarDecl;

PROCEDURE VarDeclItem(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) item: JunoAST.VarDeclItem)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  VarDeclItem = Id [ ":=" Expr ].
*)
  VAR id: REFANY; BEGIN
    item := NEW(JunoAST.VarDeclItem);
    TRY MatchKind(la, JunoToken.Kind.Id, id) FINALLY (* skip Id *)
      item.name := NARROW(id, JunoAST.Id)
    END;
    IF la.t.kind = JunoToken.Kind.Assign THEN
      MatchKind(la, JunoToken.Kind.Assign, NilRef);  (* skip ":=" *)
      Expr(la, item.value)                           (* skip Expr *)
    ELSE
      item.value := JunoAST.NilExpr
    END
  END VarDeclItem;

PROCEDURE PredDecl(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) decl: JunoAST.Block;
    priv: BOOLEAN)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Assumes "la.t.kind = JunoToken.Kind.Pred".

|  Decl     = PRED PredDecl
|  PredDecl = PredHead IS Formula END.
*)
  VAR pred := NEW(JunoAST.PredDecl, private := priv, bp := End); BEGIN
    decl := pred;
    Match(la);                                       (* skip "PRED" *)
    PredHead(la, pred.header);			     (* skip header *)
    Match(la);                                       (* skip "IS" *)
    Formula(la, pred.body);                          (* skip body *)
    MatchKind(la, JunoToken.Kind.End, NilRef)        (* skip "END" *)
  END PredDecl;

PROCEDURE PredHead(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) header: JunoAST.PredHeader)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  PredHead = Id "(" [ IDList ] ")".
*)
  VAR id: REFANY; BEGIN
    header := NEW(JunoAST.PredHeader, bp := End);
    TRY MatchKind(la, JunoToken.Kind.Id, id) FINALLY (* skip pred name *)
      header.name := NARROW(id, JunoAST.Id)
    END;
    MatchKind(la, JunoToken.Kind.LPren, NilRef);     (* skip "(" *)
    IF la.t.kind # JunoToken.Kind.RPren
      THEN IdList0(la, header.ins)	             (* skip in parameters *)
      ELSE header.ins := JunoAST.EmptyIdList	     (* make IN params empty *)
    END;
    MatchKind(la, JunoToken.Kind.RPren, NilRef)      (* skip ")" *)
  END PredHead;

PROCEDURE FuncDecl(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) decl: JunoAST.Block;
    priv: BOOLEAN)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Assumes "la.t.kind = JunoToken.Kind.Func".

|  Decl     = FUNC FuncDecl.
|  FuncDecl = FuncHead IS Constraint END.
*)
  VAR func := NEW(JunoAST.FuncDecl, private := priv, bp := End); BEGIN
    decl := func;
    Match(la);                                       (* skip "FUNC" *)
    FuncHead(la, func.header);			     (* skip header *)
    Match(la);                                       (* skip "IS" *)
    Formula(la, func.body);                          (* skip body *)
    MatchKind(la, JunoToken.Kind.End, NilRef)        (* skip "END" *)
  END FuncDecl;

PROCEDURE FuncHead(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) header: JunoAST.FuncHeader)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  FuncHead = Id "(" [ IDList ] ")" "=" Id.
*)
  VAR id: REFANY; BEGIN
    header := NEW(JunoAST.FuncHeader, bp := End);
    TRY MatchKind(la, JunoToken.Kind.Id, id) FINALLY (* skip result name *)
      header.result := NARROW(id, JunoAST.Id)
    END;
    MatchKind(la, JunoToken.Kind.Equals, NilRef);    (* skip "=" *)
    TRY MatchKind(la, JunoToken.Kind.Id, id) FINALLY (* skip func name *)
      header.name := NARROW(id, JunoAST.Id)
    END;
    MatchKind(la, JunoToken.Kind.LPren, NilRef);     (* skip "(" *)
    IF la.t.kind # JunoToken.Kind.RPren
      THEN IdList0(la, header.ins)	             (* skip in parameters *)
      ELSE header.ins := JunoAST.EmptyIdList	     (* make IN params empty *)
    END;
    MatchKind(la, JunoToken.Kind.RPren, NilRef);     (* skip ")" *)
  END FuncHead;

PROCEDURE ProcDecl(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) decl: JunoAST.Block;
    priv: BOOLEAN)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Assumes "la.t.kind = JunoToken.Kind.Proc".

|  Decl     = PROC ProcDecl.
|  ProcDecl = ProcHead IS Cmd END.
*)
  VAR proc := NEW(JunoAST.ProcDecl, private := priv, bp := End); BEGIN
    decl := proc;
    Match(la);                                       (* skip "PROC" *)
    ProcHead(la, proc.header);		             (* skip header *)
    Match(la);                                       (* skip "IS" *)
    Cmd(la, proc.body);                              (* skip body *)
    MatchKind(la, JunoToken.Kind.End, NilRef)        (* skip "END" *)
  END ProcDecl;

PROCEDURE ProcHead(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) header: JunoAST.ProcHeader)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  ProcHead = Id PH2 | PH4.
|  PH4      = "(" IDList ")" PH5.
*)
  BEGIN
    header := NEW(JunoAST.ProcHeader, bp := End);
    CASE la.t.kind OF
    | JunoToken.Kind.Id =>
        VAR newId: JunoAST.Id; id: REFANY; raised := TRUE; BEGIN
          TRY MatchKind(la, JunoToken.Kind.Id, id); raised := FALSE FINALLY
            newId := NARROW(id, JunoAST.Id);
            IF raised THEN header.outs := NewIdList(newId) END
          END;
          PH2(la, newId, header)
        END
    | JunoToken.Kind.LPren =>
        header.outs := JunoAST.EmptyIdList; (* make OUT params an empty list *)
        PH4(la, header)                     (* skip inouts and prochead *)
    ELSE RaiseError(la)
    END
  END ProcHead;

PROCEDURE PH2(
    VAR (*IO*) la: LookAhead;
    READONLY id: JunoAST.Id;
    VAR (*IO*) header: JunoAST.ProcHeader)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Assumes "header # NIL", so if we don't find a token we expect, we can
   simply raise "Error" after saving the value "id" in "header".

   "Id" is the most recently parsed token. It has a different meaning
   depending on the PH2 alternative taken. In the case of the first
   alternative, "Id" is the first of a sequence of OUT parameters. In the
   case of the second alternative, "Id" is a single INOUT parameter. In the
   case of the third alternative, "Id" is the procedure name.

|  PH2 = { "," Id } ":=" PH3 | PH5 | PH6.
|  PH5 = ":" Id PH6
|  PH6 = "(" [ IDList ] ")".
*)
  BEGIN
    CASE la.t.kind OF
    | JunoToken.Kind.Comma, JunoToken.Kind.Assign =>
        IdList0(la, header.outs, id);	              (* skip out params *)
        MatchKind(la, JunoToken.Kind.Assign, NilRef); (* skip ":=" *)
        PH3(la, header);
    | JunoToken.Kind.Colon =>
        header.outs := JunoAST.EmptyIdList;         (* make OUT list empty *)
        header.inouts := NewIdList(id);	            (* initialize INOUTs *)
        PH5(la, id, header)		            (* parse proc name *)
    | JunoToken.Kind.LPren =>
        header.outs := JunoAST.EmptyIdList;         (* make OUT list empty *)
        header.inouts := JunoAST.EmptyIdList;	    (* make INOUT list empty *)
        header.name := id;
        PH6(la, id, header)
    ELSE
        header.outs := NewIdList(id);
        RaiseError(la)
    END
  END PH2;

PROCEDURE PH3(
    VAR (*IO*) la: LookAhead;
    VAR (*IO*) header: JunoAST.ProcHeader)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Assumes "header # NIL", so if we don't find a token we expect, we can
   simply raise "Error". Also assumes "header.outs # NIL", i.e., the OUT
   parameters slot has of "header" has already been filled in.

|  PH3 = Id (PH5 | PH6) | PH4.
|  PH4 = "(" IDList ")" PH5.
|  PH5 = ":" Id PH6.
|  PH6 = "(" [ IDList ] ")".
*)
  BEGIN
    CASE la.t.kind OF
    | JunoToken.Kind.Id =>
        VAR newId: JunoAST.Id; id: REFANY; BEGIN
          TRY MatchKind(la, JunoToken.Kind.Id, id) FINALLY
             header.name := NARROW(id, JunoAST.Id)
          END;
          newId := NARROW(id, JunoAST.Id);
          CASE la.t.kind OF
          | JunoToken.Kind.Colon =>
              PH5(la, newId, header)	    (* skip proc name and args *)
          | JunoToken.Kind.LPren =>
              header.inouts := JunoAST.EmptyIdList; (* make INOUTs empty *)
              PH6(la, newId, header)	    (* skip proc name and args *)
          ELSE RaiseError(la)
          END
        END
    | JunoToken.Kind.LPren => PH4(la, header)
    ELSE RaiseError(la)
    END
  END PH3;

PROCEDURE PH4(
    VAR (*IO*) la: LookAhead;
    VAR (*IO*) header: JunoAST.ProcHeader)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Assumes "header # NIL", so if we don't find a token we expect, we can
   simply raise "Error". This happens implicitly in the calls to
   "MatchKind()".

|  PH4 = "(" IDList ")" PH5.
*)
  BEGIN
    header.inout_prens := TRUE;
    MatchKind(la, JunoToken.Kind.LPren, NilRef); (* skip "(" *)
    IdList0(la, header.inouts);		         (* skip inout params *)
    MatchKind(la, JunoToken.Kind.RPren, NilRef); (* skip ")" *)
    PH5(la, NIL, header);
  END PH4;

PROCEDURE PH5(
    VAR (*IO*) la: LookAhead;
    READONLY inout: JunoAST.Id;
    VAR (*IO*) header: JunoAST.ProcHeader)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Assumes "header # NIL". If "inout # NIL", it is installed as the (1-item)
   list of in-out parameters in "header.inouts". Otherwise, it is assumed that
   "header.inouts # NIL", i.e., it has already been filled in.

|  PH5 = ":" Id PH6.
*)
  VAR id: REFANY; BEGIN
    IF inout # NIL THEN header.inouts := NewIdList(inout) END;
    MatchKind(la, JunoToken.Kind.Colon, NilRef);     (* skip ":" *)
    TRY MatchKind(la, JunoToken.Kind.Id, id) FINALLY (* skip proc name *)
      header.name := NARROW(id, JunoAST.Id)
    END;
    PH6(la, NIL, header)
  END PH5;

PROCEDURE PH6(
    VAR (*IO*) la: LookAhead;
    READONLY name: JunoAST.Id;
    VAR (*IO*) header: JunoAST.ProcHeader)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Assumes "header # NIL". If "name # NIL", it is installed as the procedure
   name in "header.name". Otherwise, it is assumed that the procedure name has
   already been filled in.

|  PH6 = "(" [ IDList ] ")".
*)
  BEGIN
    IF name # NIL THEN header.name := name END;
    MatchKind(la, JunoToken.Kind.LPren, NilRef); (* skip "(" *)
    IF la.t.kind # JunoToken.Kind.RPren
      THEN IdList0(la, header.ins)	         (* skip in parameters *)
      ELSE header.ins := JunoAST.EmptyIdList	 (* make IN params empty *)
    END;
    MatchKind(la, JunoToken.Kind.RPren, NilRef); (* skip ")" *)
  END PH6;

(* =============================== COMMANDS ================================ *)

PROCEDURE Cmd(VAR (*IO*) la: LookAhead; VAR (*OUT*) ast: JunoAST.Cmd)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  Cmd = Cmd2 [ "|" Cmd ].
*)
  BEGIN
    Cmd2(la, ast);
    IF la.t.kind = JunoToken.Kind.Else THEN
      VAR elseCmd := NEW(JunoAST.Else, c1 := ast, bp := End); BEGIN
        ast := elseCmd;
        Match(la);			 (* skip "|" *)
        Cmd(la, elseCmd.c2)
      END
    END
  END Cmd;

PROCEDURE Cmd2(VAR (*IO*) la: LookAhead; VAR (*OUT*) ast: JunoAST.Cmd)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  Cmd2 = Cmd3 | QId QIdCmdTail.
*)
  BEGIN
    IF la.t.kind = JunoToken.Kind.Id THEN
      VAR qid: JunoAST.QId := NIL; raised := TRUE; BEGIN
        TRY QID(la, qid); raised := FALSE FINALLY
          IF raised THEN
            ast := NEW(JunoAST.Assign, bp := End,
              vars := JunoASTUtils.NewQIdList(qid, bp := End));
          END
        END;
        QIdCmdTail(la, qid, ast)
      END
    ELSE
      Cmd3(la, ast)
    END
  END Cmd2;

PROCEDURE Cmd3(VAR (*IO*) la: LookAhead; VAR (*OUT*) ast: JunoAST.Cmd)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* The possible tokens with which the non-terminal "FormulaX" may begin are
   given by the constant "FormulaFirstSet" below.

|  Cmd3 = Cmd4 | FormulaX FormTail.
*)
  CONST FormulaFirstSet = SET OF JunoToken.Kind
    { JunoToken.Kind.LitReal, JunoToken.Kind.LitText, JunoToken.Kind.LPren,
      JunoToken.Kind.LBracket, JunoToken.Kind.Minus, JunoToken.Kind.Nil,
      JunoToken.Kind.True, JunoToken.Kind.False, JunoToken.Kind.Not,
      JunoToken.Kind.Real..JunoToken.Kind.Min };
  BEGIN
    IF la.t.kind IN FormulaFirstSet THEN
      (* In this case, we must be able to parse a Formula *)
      VAR f: JunoAST.Expr; raised := TRUE; BEGIN
        TRY Formula(la, f); raised := FALSE FINALLY
          IF raised THEN ast := NEW(JunoAST.Guard, grd := f, bp := End) END
        END;
        FormTail(la, f, ast);
      END
    ELSE
      Cmd4(la, ast)
    END
  END Cmd3;

PROCEDURE Cmd4(VAR (*IO*) la: LookAhead; VAR (*OUT*) ast: JunoAST.Cmd)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  Cmd4 = Cmd6 Cmd5.
*)
  BEGIN
    Cmd6(la, ast);
    Cmd5(la, ast, ast)
  END Cmd4;

PROCEDURE Cmd5(
    VAR (*IO*) la: LookAhead;
    VALUE cmd: JunoAST.Cmd;
    VAR (*OUT*) ast: JunoAST.Cmd)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* "Cmd" is the previously parsed command. If the current token is ";", then
   "cmd" is concatenated with the next command in the stream to form a command
   sequence. Otherwise, "ast" is set to "cmd".

|  Cmd5 = [ ";" Cmd2 ].
*)
  BEGIN
    IF la.t.kind = JunoToken.Kind.Semi THEN
      VAR seq := NEW(JunoAST.Seq, c1 := cmd, bp := End); BEGIN
        ast := seq;
        Match(la);			 (* skip ";" *)
        Cmd2(la, seq.c2)
      END
    ELSE
      ast := cmd
    END
  END Cmd5;

PROCEDURE Cmd6(VAR (*IO*) la: LookAhead; VAR (*OUT*) ast: JunoAST.Cmd)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  Cmd6 = SKIP | ABORT
|       | VAR NearVarList IN Cmd END
|       | DO Cmd OD | IF Cmd FI
|       | SAVE Id IN Cmd END | "{" Cmd "}".
*)
  BEGIN
    CASE la.t.kind OF
    | JunoToken.Kind.Skip =>
        ast := JunoAST.SkipVal;
        Match(la)					   (* skip "SKIP" *)
    | JunoToken.Kind.Abort =>
        ast := JunoAST.AbortVal;
        Match(la)					   (* skip "ABORT" *)
    | JunoToken.Kind.Var =>
        VAR proj := NEW(JunoAST.Proj, bp := End); BEGIN
          ast := proj;
          Match(la);			                   (* skip "VAR" *)
          NearVarList(la, proj.vars);	                   (* skip variables *)
          MatchKind(la, JunoToken.Kind.In, NilRef);        (* skip "IN" *)
          Cmd(la, proj.body);				   (* skip body *)
          MatchKind(la, JunoToken.Kind.End, NilRef)	   (* skip "END" *)
        END
    | JunoToken.Kind.Do =>
        VAR doCmd := NEW(JunoAST.Do, bp := End); BEGIN
          ast := doCmd;
          Match(la);					   (* skip "DO" *)
          Cmd(la, doCmd.body);				   (* skip body *)
          MatchKind(la, JunoToken.Kind.Od, NilRef)	   (* skip "OD" *)
        END
    | JunoToken.Kind.If =>
        VAR ifCmd := NEW(JunoAST.If, bp := End); BEGIN
          ast := ifCmd;
          Match(la);					   (* skip "IF" *)
          Cmd(la, ifCmd.body);				   (* skip body *)
          MatchKind(la, JunoToken.Kind.Fi, NilRef)	   (* skip "FI" *)
        END
    | JunoToken.Kind.Save =>
        VAR save := NEW(JunoAST.Save, bp := End); id: REFANY; BEGIN
          ast := save;
          Match(la);			                   (* skip "SAVE" *)
          TRY MatchKind(la, JunoToken.Kind.Id, id) FINALLY (* skip Id *)
            save.nm := NEW(JunoAST.QId, bp := End,
              id0 := JunoAST.NilId, id1 := id)
          END;
          MatchKind(la, JunoToken.Kind.In, NilRef);	   (* skip "IN" *)
          Cmd(la, save.body);				   (* skip body *)
          MatchKind(la, JunoToken.Kind.End, NilRef)	   (* skip "END" *)
        END
    | JunoToken.Kind.LBrace =>
        VAR grp := NEW(JunoAST.GroupedCmd, bp := End); BEGIN
          ast := grp;
          Match(la);					   (* skip "{" *)
          Cmd(la, grp.body);				   (* skip body *)
          MatchKind(la, JunoToken.Kind.RBrace, NilRef)	   (* skip "}" *)
        END
    ELSE
        ast := NIL;
        RaiseError(la)
    END
  END Cmd6;

PROCEDURE QIdCmdTail(
    VAR (*IO*) la: LookAhead;
    READONLY qid: JunoAST.QId;
    VAR (*OUT*) ast: JunoAST.Cmd)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* "QId" is the most recently parsed qualified identifier.

|  QIdCmdTail = { "," QId } QIdCT2
|             | FormulaQId FormTail.
|  QIdCT2     = ":=" ExprList Cmd5.
*)
  BEGIN
    IF la.t.kind=JunoToken.Kind.Comma OR la.t.kind=JunoToken.Kind.Assign THEN
      VAR qids: JunoAST.QIdList; raised := TRUE; BEGIN
        TRY QIdList(la, qid, qids); raised := FALSE FINALLY
          IF raised THEN
            ast := NEW(JunoAST.Assign, bp := End, vars := qids)
          END
        END;
        QIdCT2(la, qids, ast)
      END
    ELSE
      VAR f: JunoAST.Expr; raised := TRUE; BEGIN
        TRY Formula(la, f, qid); raised := FALSE FINALLY
          IF raised THEN ast := NEW(JunoAST.Guard, grd := f, bp := End) END
        END;
        FormTail(la, f, ast);
      END
    END
  END QIdCmdTail;

PROCEDURE QIdCT2(
    VAR (*IO*) la: LookAhead;
    READONLY qids: JunoAST.QIdList;
    VAR (*OUT*) ast: JunoAST.Cmd)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* "Qids" is the most recently parsed list of qualified identifiers. Raises
   Error if "la.t.kind # JunoToken.Kind.Assign".

|  IDCT9 = ":=" ExprList Cmd5.
*)
   VAR assign := NEW(JunoAST.Assign, vars := qids, bp := End); BEGIN
     ast := assign;
     MatchKind(la, JunoToken.Kind.Assign, NilRef);   (* skip ":=" *)
     ExprList(la, assign.exprs);	             (* skip terms *)
     (*
      * Convert assignment to procedure call if we are certain it must be a
      * procedure call, namely, if there is more than one variable on the left
      * and exactly one call on the right. In the absence of semantic
      * information, we can't be guaranteed it's a procedure call otherwise, so
      * be conservative and treat it as an assignment. Note that an expression
      * of the form: "var := Proc(args)" can be treated *either* as an
      * assignment or as a procedure call. *)
     IF assign.vars.size > 1 AND assign.exprs.size = 1 THEN
       TYPECASE assign.exprs.head.expr OF
         JunoAST.Call(c) =>
           VAR proc := CallToProcCall(c); BEGIN
             proc.outs := assign.vars;
             ast := proc
           END
       ELSE (* SKIP *)
       END
     END;
     Cmd5(la, ast, ast);
   END QIdCT2;

PROCEDURE FormTail(
    VAR (*IO*) la: LookAhead;
    READONLY f: JunoAST.Expr;
    VAR (*OUT*) ast: JunoAST.Cmd)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* "F" is the most recently parsed formula. If it is followed by "->", then
   "ast" is set to a guarded command with guard "f". Otherwise, "f" must be a
   call expression (if not, Error is raised), and "ast" is set to the
   procedure call command equivalent to "f".

|  FormTail = Cmd5			 (* proc call *)
|           | "->" Cmd2.		 (* guard *)
*)
  BEGIN
    IF la.t.kind = JunoToken.Kind.Guard THEN
      VAR grd := NEW(JunoAST.Guard, grd := f, bp := End); BEGIN
        ast := grd;
        Match(la);			 (* skip "->" *)
        Cmd2(la, grd.body)		 (* skip guard body *)
      END
    ELSE
      TYPECASE f OF
      | JunoAST.Call(c) =>
          Cmd5(la, CallToProcCall(c), ast);
      ELSE
          ast := NEW(JunoAST.Guard, grd := f, bp := End);
          (* If the error message produced by the "expected '->' token" is too
	     specific, we can change the following to simply RAISE Error with
             argument ParseError(la). *)
          MatchKind(la, JunoToken.Kind.Guard, NilRef); (* signal error *)
      END
    END
  END FormTail;

PROCEDURE CallToProcCall(call: JunoAST.Call): JunoAST.ProcCall =
(* Convert a call expression into a procedure call command. *)
  BEGIN
    RETURN NEW(JunoAST.ProcCall, outs := JunoAST.EmptyQIdList,
      inouts := call.inouts, inout_parens := call.inout_parens,
      name := call.name, ins := call.ins, bp := End)
  END CallToProcCall;

(* =============================== FORMULAS ================================ *)

PROCEDURE Formula(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) ast: JunoAST.Expr;
    READONLY qid: JunoAST.QId := NIL)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  Formula    = Form1 [ OR Formula ].
|  FormulaQId = Form1QId [ OR Formula ].
*)
  BEGIN
    Form1(la, ast, qid);
    IF la.t.kind = JunoToken.Kind.Or THEN
      VAR or := NEW(JunoAST.Or, f1 := ast, bp := End); BEGIN
        ast := or;
        Match(la);			 (* skip "OR" *)
        Formula(la, or.f2)
      END
    END
  END Formula;

PROCEDURE Form1(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) ast: JunoAST.Expr;
    READONLY qid: JunoAST.QId := NIL)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  Form1    = Form2 [ AND Form1 ].
|  Form1QId = Form3QId [ AND Form1 ].
*)
  BEGIN
    IF qid = NIL THEN Form2(la, ast) ELSE Form3(la, ast, qid) END;
    IF la.t.kind = JunoToken.Kind.And THEN
      VAR and := NEW(JunoAST.And, f1 := ast, bp := End); BEGIN
        ast := and;
        Match(la);			 (* skip "AND" *)
        Form1(la, and.f2)
      END
    END
  END Form1;

PROCEDURE Form2(VAR (*IO*) la: LookAhead; VAR (*OUT*) ast: JunoAST.Expr)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  Form2 = Form3 | NOT Form2.
*)
  BEGIN
    IF la.t.kind = JunoToken.Kind.Not THEN
      VAR not := NEW(JunoAST.Not, bp := End); BEGIN
        ast := not;
        Match(la);			 (* skip "NOT" *)
        Form2(la, not.f)
      END
    ELSE
      Form3(la, ast)
    END
  END Form2;

PROCEDURE Form3(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) ast: JunoAST.Expr;
    READONLY qid: JunoAST.QId := NIL)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* This procedure actually binds "ast" to an object that is a proper subtype
   of JunoAST.Relation.

|  Form3      = Expr [ RelationOp Expr ].
|  Form3QId   = ExprQId  [ RelationOp Expr ].
|  RelationOp = "=" | "#" | "<" | ">" | "<=" | ">=" | CONG | PARA | HOR | VER.
*)
  VAR rel: JunoAST.Relation; BEGIN
    Expr(la, ast, qid);
    CASE la.t.kind OF
    | JunoToken.Kind.Equals, JunoToken.Kind.Near =>
  	  rel := NEW(JunoAST.Equals, bp := End, e1 := ast,
  	    near := (la.t.kind = JunoToken.Kind.Near));
  	  ast := rel;
  	  Match(la);			              (* skip "=" or "~" *)
  	  Expr(la, rel.e2)
    | JunoToken.Kind.Differs =>
  	  rel := NEW(JunoAST.Differs, e1 := ast, bp := End);
  	  ast := rel;
  	  Match(la);			              (* skip "#" *)
  	  Expr(la, rel.e2)
    | JunoToken.Kind.Less =>
  	  rel := NEW(JunoAST.Less, e1 := ast, bp := End);
  	  ast := rel;
  	  Match(la);			              (* skip "<" *)
  	  Expr(la, rel.e2)
    | JunoToken.Kind.Greater =>
  	  rel := NEW(JunoAST.Greater, e1 := ast, bp := End);
  	  ast := rel;
  	  Match(la);			              (* skip ">" *)
  	  Expr(la, rel.e2)
    | JunoToken.Kind.AtMost =>
  	  rel := NEW(JunoAST.AtMost, e1 := ast, bp := End);
  	  ast := rel;
  	  Match(la);			              (* skip "<=" *)
  	  Expr(la, rel.e2)
    | JunoToken.Kind.AtLeast =>
  	  rel := NEW(JunoAST.AtLeast, e1 := ast, bp := End);
  	  ast := rel;
  	  Match(la);			              (* skip ">=" *)
  	  Expr(la, rel.e2)
    | JunoToken.Kind.Cong =>
  	  rel := NEW(JunoAST.Cong, e1 := ast, bp := End);
  	  ast := rel;
  	  Match(la);			              (* skip "CONG" *)
  	  Expr(la, rel.e2)
    | JunoToken.Kind.Para =>
  	  rel := NEW(JunoAST.Para, e1 := ast, bp := End);
  	  ast := rel;
  	  Match(la);			              (* skip "PARA" *)
  	  Expr(la, rel.e2)
    | JunoToken.Kind.Hor =>
  	  rel := NEW(JunoAST.Hor, e1 := ast, bp := End);
  	  ast := rel;
  	  Match(la);			              (* skip "HOR" *)
  	  Expr(la, rel.e2)
    | JunoToken.Kind.Ver =>
  	  rel := NEW(JunoAST.Ver, e1 := ast, bp := End);
  	  ast := rel;
  	  Match(la);			              (* skip "VER" *)
  	  Expr(la, rel.e2)
    ELSE (* SKIP *)
    END
  END Form3;

(* ============================== EXPRESSIONS ============================== *)

PROCEDURE Expr(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) ast: JunoAST.Expr;
    READONLY qid: JunoAST.QId := NIL)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  Expr    = Expr1 [ REL Expr1 ].
|  ExprQId = Expr1QId [ REL Expr1 ].
*)
  BEGIN
    Expr1(la, ast, qid);
    IF la.t.kind = JunoToken.Kind.Rel THEN
      VAR rel := NEW(JunoAST.Rel, e1 := ast, bp := End); BEGIN
        ast := rel;
        Match(la);			 (* skip "REL" *)
        Expr1(la, rel.e2);
      END
    END
  END Expr;

PROCEDURE Expr1(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) ast: JunoAST.Expr;
    READONLY qid: JunoAST.QId := NIL)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  Expr1    = Expr2 [ Expr1Tail ].
|  Expr1QId = Expr2QId [ Expr1Tail ].
*)
  BEGIN
    Expr2(la, ast, qid);
    Expr1Tail(la, ast)
  END Expr1;

PROCEDURE Expr1Tail(
    VAR (*IO*) la: LookAhead;
    VAR (*INOUT*) ast: JunoAST.Expr)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* On entry, "ast" is the expression parsed so far. If the next token is NOT
   an "AddOp", then this procedure consumes no tokens and leaves "ast"
   unchanged.

|  Expr1Tail = AddOp Expr2 [ Expr1Tail ].
|  AddOp     = "+" | "-" | "&".
*)
  VAR add: JunoAST.BuiltInAddFunc; BEGIN
    CASE la.t.kind OF
    | JunoToken.Kind.Plus =>   add := NEW(JunoAST.Plus,   e1 := ast, bp :=End);
    | JunoToken.Kind.Minus =>  add := NEW(JunoAST.Minus,  e1 := ast, bp :=End);
    | JunoToken.Kind.Concat => add := NEW(JunoAST.Concat, e1 := ast, bp :=End);
    ELSE RETURN
    END;
    ast := add;
    Match(la);				 (* skip "+", "-", or "&" *)
    Expr2(la, add.e2);
    Expr1Tail(la, ast)
  END Expr1Tail;

PROCEDURE Expr2(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) ast: JunoAST.Expr;
    READONLY qid: JunoAST.QId := NIL)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* 
|  Expr2    = Expr3 [ Expr2Tail ].
|  Expr2QId = Expr4QId [ Expr2Tail ].
*)
  BEGIN
    IF qid = NIL THEN Expr3(la, ast) ELSE Expr4(la, ast, qid) END;
    Expr2Tail(la, ast)
  END Expr2;

PROCEDURE Expr2Tail(
    VAR (*IO*) la: LookAhead;
    VAR (*INOUT*) ast: JunoAST.Expr)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* On entry, "ast" is the expression parsed so far. If the next token is NOT
   a "MulOp", then this procedure consumes no tokens and leaves "ast"
   unchanged.

|  Expr2Tail = MulOp Expr3 [ Expr2Tail ].
|  MulOp     = "*" | "/" | DIV | MOD.
*)
  VAR mul: JunoAST.BuiltInMulFunc; BEGIN
    CASE la.t.kind OF
    | JunoToken.Kind.Times =>  mul := NEW(JunoAST.Times,  e1 := ast, bp :=End);
    | JunoToken.Kind.Divide => mul := NEW(JunoAST.Divide, e1 := ast, bp :=End);
    | JunoToken.Kind.Div =>    mul := NEW(JunoAST.Div,    e1 := ast, bp :=End);
    | JunoToken.Kind.Mod =>    mul := NEW(JunoAST.Mod,    e1 := ast, bp :=End);
    ELSE RETURN
    END;
    ast := mul;
    Match(la);				 (* skip "*", "/", "DIV", or "MOD" *)
    Expr3(la, mul.e2);
    Expr2Tail(la, ast)
  END Expr2Tail;

PROCEDURE Expr3(VAR (*IO*) la: LookAhead; VAR (*OUT*) ast: JunoAST.Expr)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  Expr3 = Expr4 | "-" Expr3.
*)
  BEGIN
    IF la.t.kind = JunoToken.Kind.Minus THEN
      VAR minus := NEW(JunoAST.UMinus, bp := End); BEGIN
        ast := minus;
        Match(la);                         (* skip "-" *)
        Expr3(la, minus.e)
      END
    ELSE
      Expr4(la, ast)
    END
  END Expr3;

PROCEDURE Expr4(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) ast: JunoAST.Expr;
    READONLY qid: JunoAST.QId := NIL)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  Expr4       = NIL | TRUE | FALSE | Literal | BuiltIn
|              | QID [ QIDExprTail ]
|              | "(" LPExprTail
|              | "[" ExprList "]".
|  Expr4QId    = [ QIDExprTail ].
|  QIDExprTail = [ ":" QID ] LPET7.
|  LPET7       = "(" [ ExprList ] ")".
*)
  CONST FirstQIDExprTail = SET OF JunoToken.Kind{
    JunoToken.Kind.Colon, JunoToken.Kind.LPren};
  BEGIN
    IF qid = NIL THEN
      CASE la.t.kind OF
      | JunoToken.Kind.Nil =>   ast := JunoAST.NilVal;   Match(la)
      | JunoToken.Kind.True =>  ast := JunoAST.TrueVal;  Match(la)
      | JunoToken.Kind.False => ast := JunoAST.FalseVal; Match(la)
      | JunoToken.Kind.LitReal =>
          VAR num := NEW(JunoAST.Number, bp := End); BEGIN
            ast := num;
            MatchReal(la, JunoToken.Kind.LitReal, num.val)  (* skip real *)
          END
      | JunoToken.Kind.LitText =>
          VAR txt := NEW(JunoAST.Text, bp := End); t: REFANY; BEGIN
            ast := txt;
            TRY MatchKind(la, JunoToken.Kind.LitText, t) FINALLY
              txt.val := NARROW(t, TEXT)	            (* skip text *)
            END
          END
      | FIRST(JunoToken.ResvdId)..LAST(JunoToken.ResvdId) =>
          BuiltIn(la, ast)
      | JunoToken.Kind.Id =>				       (* Id of QID *)
          VAR qid := NEW(JunoAST.QId, bp := End); id: REFANY; BEGIN
            TRY
              TRY MatchKind(la, JunoToken.Kind.Id, id) FINALLY (* skip ID *)
                qid.id0 := NARROW(id, JunoAST.Id)
              END;
              QID(la, qid);				       (* parse QID *)
            FINALLY
              ast := qid
            END;
            IF la.t.kind IN FirstQIDExprTail THEN
              QIDExprTail(la, qid, ast)
            END
          END
      | JunoToken.Kind.LPren =>
          VAR raised := TRUE; BEGIN
            TRY Match(la); raised := FALSE FINALLY
              IF raised THEN ast := NEW(JunoAST.GroupedExpr, bp := End) END
            END
          END;
          LPExprTail(la, ast)				       (* skip tail *)
      | JunoToken.Kind.LBracket =>
          VAR lst := NEW(JunoAST.List, bp := End); BEGIN
            ast := lst;
            Match(la);					       (* skip "[" *)
            ExprList(la, lst.elts);		               (* skip list *)
            MatchKind(la, JunoToken.Kind.RBracket, NilRef);    (* skip "]" *)
          END
      ELSE
          ast := NIL;
          RaiseError(la)
      END
    ELSE
      IF la.t.kind IN FirstQIDExprTail THEN
        QIDExprTail(la, qid, ast)
      ELSE
        ast := qid
      END
    END
  END Expr4;

PROCEDURE BuiltIn(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) ast: JunoAST.Expr)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Assumes "la.t.kind" is in "JunoToken.ResvdId". *)
  VAR
    up: JunoAST.BIUPred;
    uf: JunoAST.BIUFunc;
    bf: JunoAST.BIBFunc;
  BEGIN
    CASE la.t.kind OF <* NOWARN *>
      JunoToken.Kind.Real    => up := NEW(JunoAST.IsReal,  bp := End);
    | JunoToken.Kind.Text    => up := NEW(JunoAST.IsText,  bp := End);
    | JunoToken.Kind.Pair    => up := NEW(JunoAST.IsPair,  bp := End);
    | JunoToken.Kind.Int     => up := NEW(JunoAST.IsInt,   bp := End);
    | JunoToken.Kind.Floor   => uf := NEW(JunoAST.Floor,   bp := End);
    | JunoToken.Kind.Ceiling => uf := NEW(JunoAST.Ceiling, bp := End);
    | JunoToken.Kind.Round   => uf := NEW(JunoAST.Round,   bp := End);
    | JunoToken.Kind.Abs     => uf := NEW(JunoAST.Abs,     bp := End);
    | JunoToken.Kind.Sin     => uf := NEW(JunoAST.Sin,     bp := End);
    | JunoToken.Kind.Cos     => uf := NEW(JunoAST.Cos,     bp := End);
    | JunoToken.Kind.Ln      => uf := NEW(JunoAST.Ln,      bp := End);
    | JunoToken.Kind.Exp     => uf := NEW(JunoAST.Exp,     bp := End);
    | JunoToken.Kind.Car     => uf := NEW(JunoAST.Car,     bp := End);
    | JunoToken.Kind.Cdr     => uf := NEW(JunoAST.Cdr,     bp := End);
    | JunoToken.Kind.Max     => bf := NEW(JunoAST.Max,     bp := End);
    | JunoToken.Kind.Min     => bf := NEW(JunoAST.Min,     bp := End);
    | JunoToken.Kind.Atan    => bf := NEW(JunoAST.Atan,    bp := End);
    END;
    CASE la.t.kind OF <* NOWARN *>
      JunoToken.Kind.Real..JunoToken.Kind.Int =>
        ast := up;
        Match(la);			             (* skip predicate name *)
        MatchKind(la, JunoToken.Kind.LPren, NilRef); (* skip "(" *)
        Expr(la, up.e);			             (* skip argument *)
        MatchKind(la, JunoToken.Kind.RPren, NilRef); (* skip ")" *)
    | JunoToken.Kind.Floor..JunoToken.Kind.Cdr =>
        ast := uf;
        Match(la);			             (* skip function name *)
        MatchKind(la, JunoToken.Kind.LPren, NilRef); (* skip "(" *)
        Expr(la, uf.e);			             (* skip argument *)
        MatchKind(la, JunoToken.Kind.RPren, NilRef); (* skip ")" *)
    | JunoToken.Kind.Max..JunoToken.Kind.Atan =>
        ast := bf;
        Match(la);			             (* skip function name *)
        MatchKind(la, JunoToken.Kind.LPren, NilRef); (* skip "(" *)
        Expr(la, bf.e1);		             (* skip 1st argument *)
        MatchKind(la, JunoToken.Kind.Comma, NilRef); (* skip "," *)
        Expr(la, bf.e2);		             (* skip 2nd argument *)
        MatchKind(la, JunoToken.Kind.RPren, NilRef); (* skip ")" *)
    END
  END BuiltIn;

PROCEDURE QIDExprTail(
    VAR (*IO*) la: LookAhead;
    READONLY qid: JunoAST.QId;
    VAR (*OUT*) ast: JunoAST.Expr)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Assumes "la.t.kind = JunoToken.Kind.Colon or "la.t.kind =
   JunoToken.Kind.LPren".

   "Qid" is the most recently parsed qualified identifier. Its meaning depends
   on the next token. If the next token is ":", then "qid" is the name of an
   INOUT parameter to a procedure call. Otherwise, it is the name of the
   procedure, predicate, or function being called. In either case, "ast" is
   actually bound to an object of type JunoAST.Call.

|  QIDExprTail = [ ":" QID ] LPET7.
|  LPET7       = "(" [ ExprList ] ")".
*)
  VAR call := NEW(JunoAST.Call, bp := End); BEGIN
    ast := call;
    IF la.t.kind = JunoToken.Kind.Colon THEN
      call.inouts := JunoASTUtils.NewQIdList(qid, bp := End);
      Match(la);			           (* skip ":" *)
      QID(la, call.name)		           (* read QID call name *)
    ELSE
      call.inouts := JunoAST.EmptyExprList;      (* make INOUT params empty *)
      call.name := qid;
    END;
    MatchKind(la, JunoToken.Kind.LPren, NilRef); (* skip "(" *)
    IF la.t.kind # JunoToken.Kind.RPren
      THEN ExprList(la, call.ins)		 (* skip expr list *)
      ELSE call.ins := JunoAST.EmptyExprList     (* make IN params empty *)
    END;
    MatchKind(la, JunoToken.Kind.RPren, NilRef); (* skip ")" *)
  END QIDExprTail;

PROCEDURE LPExprTail(VAR (*IO*) la: LookAhead; VAR (*OUT*) ast: JunoAST.Expr)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  LPExprTail = LPET2 | LPET3.
|  LPET2      = E NearVarList "::" Constraint ")".      (* existential quant *)
|  LPET3      = Formula LPET4.                (* paren formula, pair, inouts *)
*)
  BEGIN
    IF la.t.kind = JunoToken.Kind.Exists THEN
      VAR ex := NEW(JunoAST.Exists, bp := End); BEGIN
        ast := ex;
        Match(la);					  (* skip "E" *)
        NearVarList(la, ex.vars);			  (* skip var list *)
        MatchKind(la, JunoToken.Kind.SuchThat, NilRef);	  (* skip "::" *)
        Formula(la, ex.f);				  (* skip constraint *)
        MatchKind(la, JunoToken.Kind.RPren, NilRef);	  (* skip final ")" *)
      END
    ELSE
      VAR first: JunoAST.Expr; raised := TRUE; BEGIN
        TRY Formula(la, first); raised := FALSE FINALLY	  (* skip first expr *)
          IF raised THEN
            ast := NEW(JunoAST.GroupedExpr, expr := first, bp := End)
          END
        END;
        LPET4(la, first, ast)
      END
    END
  END LPExprTail;

PROCEDURE LPET4(
    VAR (*IO*) la: LookAhead;
    READONLY f1: JunoAST.Expr;
    VAR (*OUT*) ast: JunoAST.Expr)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* "F1" is the most recently parsed formula. Its meaning depends on which
   LPET4 alternative is taken. If the first alternative is taken, then "ast"
   will be a grouped expression containing "f1" or a procedure call expression
   with "f1" as the single INOUT parameter to the call. If the second
   alternative is taken, then "f1" is considered the first expression in an
   expression list forming a pair of the INOUT parameters to a procedure call.

|  LPET4 = ")" [ LPET6 ]
|        | "," Expr LPET5.
|  LPET6 = ":" QID LPET7.
*)
  BEGIN
    CASE la.t.kind OF
    | JunoToken.Kind.RPren =>
        VAR raised := TRUE; BEGIN
          TRY Match(la); raised := FALSE FINALLY
            IF raised THEN
              ast := NEW(JunoAST.GroupedExpr, expr := f1, bp := End)
            END
          END
        END;
        IF la.t.kind = JunoToken.Kind.Colon THEN
          VAR call := NEW(JunoAST.Call, inout_parens := TRUE,
            inouts := JunoASTUtils.NewExprList(f1, bp := End), bp := End);
          BEGIN
            ast := call;
            LPET6(la, call)
          END
        ELSE
          ast := NEW(JunoAST.GroupedExpr, expr := f1, bp := End)
        END
    | JunoToken.Kind.Comma =>
        VAR f2: JunoAST.Expr := NIL; raised := TRUE; BEGIN
          TRY
            Match(la);                     (* skip "," *)
            Expr(la, f2);                  (* skip second expr *)
            raised := FALSE
          FINALLY
            IF raised THEN
              ast := NEW(JunoAST.Pair, e1 := f1, e2 := f2, bp := End)
            END
          END;
          LPET5(la, f1, f2, ast)
        END
    ELSE
        ast := NEW(JunoAST.GroupedExpr, expr := f1, bp := End);
        RaiseError(la)
    END
  END LPET4;

PROCEDURE LPET5(
    VAR (*IO*) la: LookAhead;
    READONLY f1, f2: JunoAST.Expr;
    VAR (*OUT*) ast: JunoAST.Expr)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* "F1" and "f2" are the two most recently parsed expressions of an expression
   list. Their meaning depends on which LPET5 alternative is taken. If the
   first alternative is taken, then they are either the two elements of a pair
   or the two INOUT parameters to a procedure. If the second alternative is
   taken, then they are the first two of many INOUT parameters to a procedure.

|  LPET5 = ")" [ LPET6 ]
|        | "," QIDList ")" LPET6.
|  LPET6 = ":" QID LPET7.
*)
  BEGIN
    CASE la.t.kind OF
    | JunoToken.Kind.RPren =>
        VAR raised := TRUE; BEGIN
          TRY Match(la); raised := FALSE FINALLY
            IF raised THEN
              ast := NEW(JunoAST.Pair, e1 := f1, e2 := f2, bp := End)
            END
          END
        END;
        IF la.t.kind = JunoToken.Kind.Colon THEN
          VAR call := NEW(JunoAST.Call, inout_parens := TRUE,
            inouts := NewExprList2(f1, f2), bp := End);
          BEGIN
            ast := call;
            LPET6(la, call)
          END
        ELSE
          ast := NEW(JunoAST.Pair, e1 := f1, e2 := f2, bp := End)
        END
    | JunoToken.Kind.Comma =>
        VAR
          exprs := NewExprList2(f1, f2);
          call := NEW(JunoAST.Call, bp := End,
            inout_parens := TRUE, inouts := exprs);
          qid_list: JunoAST.QIdList;
        BEGIN
          ast := call;
          VAR raised := TRUE; BEGIN
            TRY Match(la); raised := FALSE FINALLY     (* skip "," *)
              IF raised THEN
                (* fix "call" so it will seem to contain > 2 inouts *)
                INC(call.inouts.size);
                exprs.head.next.next := exprs.head
              END
            END
          END;
          TRY QIdList(la, NIL, qid_list) FINALLY       (* skip QID's *)
            INC(exprs.size, qid_list.size);
            exprs.head.next.next := qid_list.head
          END;
          MatchKind(la, JunoToken.Kind.RPren, NilRef); (* skip ")" *)
          LPET6(la, call)                              (* skip rest of proc *)
        END
    ELSE
        ast := NEW(JunoAST.Pair, e1 := f1, e2 := f2, bp := End);
        RaiseError(la)
    END;
  END LPET5;

PROCEDURE LPET6(
    VAR (*IO*) la: LookAhead;
    VAR (*IO*) ast: JunoAST.Call)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Assumes "ast" is initially a JunoAST.Call whose "inouts" field has been
   filled in. Expects JunoToken.Kind.Colon as next token.

|  LPET6 = ":" QID LPET7.
|  LPET7 = "(" [ ExprList ] ")".
*)
  BEGIN
    MatchKind(la, JunoToken.Kind.Colon, NilRef); (* skip ":" *)
    QID(la, ast.name);			         (* skip proc name *)
    MatchKind(la, JunoToken.Kind.LPren, NilRef); (* skip "(" *)
    IF la.t.kind # JunoToken.Kind.RPren
      THEN ExprList(la, ast.ins)		 (* skip expr list *)
      ELSE ast.ins := JunoAST.EmptyExprList      (* make IN params empty *)
    END;
    MatchKind(la, JunoToken.Kind.RPren, NilRef); (* skip ")" *)
  END LPET6;

(* ============================= MISCELLANEOUS ============================= *)

PROCEDURE ExprList(
    VAR (*IO*) la: LookAhead;
    VAR (*IO*) elist: JunoAST.ExprList)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* If "elist = NIL", then this procedure parses the next expression as the
   first element of a list of expressions, and initializes "elist" to a new
   JunoAST.ExprList whose first element is that expression. Otherwise, it
   assumes "elist" is already initialized to a JunoAST.ExprList with a single
   element. In either case, this procedure parses the rest of the expression
   list.

|  ExprList = Expr { "," Expr }.
*)
  VAR curr: JunoAST.ExprLink; BEGIN
    IF elist = NIL THEN
      curr := NEW(JunoAST.ExprLink);
      elist := NEW(JunoAST.ExprList, size := 1, head := curr, bp := End);
      Expr(la, curr.expr)
    ELSE
      curr := elist.head
    END;
    WHILE la.t.kind = JunoToken.Kind.Comma DO
      INC(elist.size);
      curr.next := NEW(JunoAST.ExprLink);
      curr := curr.next;
      Match(la);			 (* skip "," *)
      Expr(la, curr.expr)		 (* skip Expr *)
    END
  END ExprList;

PROCEDURE NewExprList2(READONLY e1, e2: JunoAST.Expr): JunoAST.ExprList =
(* Return an expression list of length 2 containing "e1" and "e2". *)
  VAR
    curr := NEW(JunoAST.ExprLink, expr := e1);
    result := NEW(JunoAST.ExprList, size := 2, head := curr, bp := End);
  BEGIN
    curr.next := NEW(JunoAST.ExprLink, expr := e2);
    RETURN result
  END NewExprList2;

PROCEDURE QID(VAR (*IO*) la: LookAhead; VAR (*IO*) qid: JunoAST.QId)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* If "qid = NIL", "qid" is first bound to a new JunoAST.QId. In this case,
   "la.t.kind" must be "JunoToken.Kind.Id", or else the Error exception is
   raised with "qid # NIL" and "qid.id0 = NIL". In the error-free case when
   the current token is an identifier, the procedure stores the current token
   in "qid.id0". If "qid # NIL", it is assumed that "qid" contains the first
   part of a (potentially) qualified identifier in "qid.id0".

   If the identifier is not qualified (i.e., no JunoToken.Kind.Dot appears in
   the token stream after the first identifier), then the single identifier is
   stored in "qid.id1", and "qid.id0" is set to the special value
   "JunoAST.NilId". 

|  QID = Id [ "." Id ].
*)
  VAR id: REFANY; BEGIN
    IF qid = NIL THEN
      qid := NEW(JunoAST.QId, bp := End);
      TRY MatchKind(la, JunoToken.Kind.Id, id) FINALLY
        qid.id0 := NARROW(id, JunoAST.Id)
      END
    END;
    IF la.t.kind = JunoToken.Kind.Dot THEN
      Match(la);					  (* skip "." *)
      TRY MatchKind(la, JunoToken.Kind.Id, id) FINALLY
        qid.id1 := NARROW(id, JunoAST.Id)
      END
    ELSE
      qid.id1 := qid.id0;
      qid.id0 := JunoAST.NilId;
    END
  END QID;

PROCEDURE QIdList(
    VAR (*IO*) la: LookAhead;
    VALUE qid: JunoAST.QId;
    VAR (*OUT*) qids: JunoAST.QIdList)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Read a QIDList from "la", storing the result in "qids". If "qid # NIL", it
   is expected to contain the first QID of the list. Otherwise, the first
   element of the list is read from "la"; in this case, if the current
   token is not "JunoToken.Kind.Id", this procedure raises Error.

|  QIDList = QID { "," QID }.
*)
  VAR curr: JunoAST.ExprLink; BEGIN
    TRY IF qid = NIL THEN QID(la, qid) END FINALLY
      qids := JunoASTUtils.NewQIdList(qid, bp := End)
    END;
    curr := qids.head;
    WHILE la.t.kind = JunoToken.Kind.Comma DO
      INC(qids.size);
      curr.next := NEW(JunoAST.ExprLink);
      curr := curr.next;
      Match(la);			   (* skip "," *)
      TRY qid := NIL; QID(la, qid) FINALLY (* skip QId *)
        curr.expr := qid
      END
    END
  END QIdList;

PROCEDURE NearVar(
    VAR (*IO*) la: LookAhead;
    VAR (*IO*) nv: JunoAST.NearVarLink)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(* If "nv" = NIL, then a new "NearVarLink" is allocated for "nv"; otherwise,
   the information in "nv" is overwritten.

|  NearVar = Id [ ("~" | "=") Expr ].
*)
  BEGIN
    IF nv = NIL THEN nv := NEW(JunoAST.NearVarLink) END;
    VAR id: REFANY; BEGIN
      TRY MatchKind(la, JunoToken.Kind.Id, id) FINALLY
        nv.id := NARROW(id, JunoAST.Id)
      END
    END;
    IF la.t.kind = JunoToken.Kind.Near THEN
      Match(la);			 (* skip "~" *)
      Expr(la, nv.hint)			 (* skip expression *)
    ELSIF la.t.kind = JunoToken.Kind.Equals THEN
      nv.frozen := TRUE;
      Match(la);			 (* skip "=" *)
      Expr(la, nv.hint)			 (* skip expression *)
    ELSE
      nv.hint := JunoAST.NilExpr
    END
  END NearVar;

PROCEDURE NearVarList(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) nv: JunoAST.NearVarList)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  NearVarList = NearVar { "," NearVar }.
*)
  VAR last: JunoAST.NearVarLink; BEGIN
    nv := NEW(JunoAST.NearVarList, bp := End);
    NearVar(la, nv.head);
    last := nv.head;
    INC(nv.size);
    WHILE la.t.kind = JunoToken.Kind.Comma DO
      INC(nv.size);
      last.next := NEW(JunoAST.NearVarLink);
      last := last.next;
      Match(la);					  (* skip "," *)
      NearVar(la, last)			                  (* skip NearVar *)
    END
  END NearVarList;

PROCEDURE IdList0(
    VAR (*IO*) la: LookAhead;
    VAR (*OUT*) ids: JunoAST.IdList;
    READONLY first: JunoAST.Id := NIL)
  RAISES {Error, JunoLex.Error, Rd.Failure} =
(*
|  IDList = Id { "," Id }.
*)
  VAR id: REFANY; curr := NEW(JunoAST.IdLink); BEGIN
    ids := NEW(JunoAST.IdList, size := 1, head := curr, bp := End);
    IF first = NIL THEN
      TRY MatchKind(la, JunoToken.Kind.Id, id) FINALLY
        curr.id := NARROW(id, JunoAST.Id)
      END
    ELSE
      curr.id := first
    END;
    WHILE la.t.kind = JunoToken.Kind.Comma DO
      INC(ids.size);
      curr.next := NEW(JunoAST.IdLink);
      curr := curr.next;
      Match(la);					  (* skip "," *)
      TRY MatchKind(la, JunoToken.Kind.Id, id) FINALLY    (* skip Id *)
        curr.id := NARROW(id, JunoAST.Id)
      END
    END
  END IdList0;

PROCEDURE NewIdList(READONLY id: JunoAST.Id := NIL): JunoAST.IdList =
(* Create and return a new JunoAST.IdList. If "id # NIL", then the
   returned list has size 1 and the single value "id". Otherwise, the list
   has size 0. *)
  VAR result := NEW(JunoAST.IdList, bp := End); BEGIN
    IF id # NIL THEN
      result.size := 1;
      result.head := NEW(JunoAST.IdLink, id := id)
    END;
    RETURN result
  END NewIdList;

PROCEDURE MatchKind(
    VAR (*IO*) la: LookAhead;
    READONLY kind: JunoToken.Kind;
    VAR (*OUT*) val: REFANY
  ) RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Like Match() below, only raises "Error" with the appropriate "ErrorRec"
   if "la.t.kind # kind". "Val" is set to the value corresponding to the
   current token; this only has meaning if "kind" is one of:
   "JunoToken.Kind.LitReal", "JunoToken.Kind.LitText", "JunoToken.Kind.Id",
   or "JunoToken.Kind.Comment". *)
  BEGIN
    IF la.t.kind # kind THEN
      val := NIL;
      RaiseError(la, kind)
    END;
    val := la.t.val;			 (* do assignment in all cases *)
    Match(la)
  END MatchKind;

PROCEDURE MatchReal(
    VAR (*IO*) la: LookAhead;
    READONLY kind: JunoToken.Kind;
    VAR (*OUT*) num: JunoValue.Real
  ) RAISES {Error, JunoLex.Error, Rd.Failure} =
(* Like Match() below, only raises "Error" with the appropriate "ErrorRec"
   if "la.t.kind # kind". "Val" is set to the real value corresponding to
   the current token, namely "la.t.num". This operation is meaningful only
   if "kind" is "JunoToken.Kind.LitReal". *)
  BEGIN
    IF la.t.kind # kind THEN RaiseError(la, kind) END;
    num := la.t.num;
    Match(la)
  END MatchReal;

PROCEDURE Match(VAR (*IO*) la: LookAhead) RAISES {JunoLex.Error, Rd.Failure} =
(* Reads the next token into "la.t". If there is a lex error reading the next
   token, it is converted into the appropriate "JunoLex.ErrorRec" and
   "JunoLex.Error" is raised. *)
  BEGIN
    la.t := la.s.next();
    INC(la.cnt)
  END Match;

PROCEDURE RaiseError(
    VAR (*IO*) la: LookAhead;
    READONLY kind := JunoToken.Kind.Unknown)
  RAISES {Error} =
(* Raises "Error" with an argument "ErrorRec" corresponding to the current
   token, and the expected kind of token "kind". This procedure also has the
   side-effect of decrementing "la.cnt", since we don't want to count the
   token that caused the parse error as having been parsed. *)
  BEGIN
    DEC(la.cnt);
    RAISE Error(NEW(ErrorRec, found := JunoToken.Copy(la.t),
      additional := JunoLex.Close(la.s), expected := kind))
  END RaiseError;

BEGIN
  End := JunoAST.End
END JunoParse.
