(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Oct 31 10:06:18 PST 1994 by heydon                   *)
(*      modified on Fri Aug  7 21:54:02 PDT 1992 by myers                    *)

MODULE JunoAST;

IMPORT JunoASTUtils;
IMPORT Atom;

TYPE (* declare special Nil types *)
  NilExprT = Expr BRANDED "JunoAST.NilExprT" OBJECT END;

REVEAL
  T = TPub BRANDED "JunoAST.T" OBJECT OVERRIDES
    iterator := EmptyIt
  END;
  Assign = AssignPub BRANDED "JunoAST.Assign" OBJECT OVERRIDES
    iterator := AssignIt
  END;
  ProcCall = ProcCallPub BRANDED "JunoAST.ProcCall" OBJECT OVERRIDES
    iterator := ProcCallIt
  END;
  BodyCmd = BodyCmdPub BRANDED "JunoAST.BodyCmd" OBJECT OVERRIDES
    iterator := BodyCmdIt
  END;
  Proj = ProjPub BRANDED "JunoAST.Proj" OBJECT OVERRIDES
    iterator := ProjIt
  END;
  Guard = GuardPub BRANDED "JunoAST.Guard" OBJECT OVERRIDES
    iterator := GuardIt
  END;
  TwoCmd = TwoCmdPub BRANDED "JunoAST.TwoCmd" OBJECT OVERRIDES
    iterator := TwoCmdIt
  END;
  Query = QueryPub BRANDED "JunoAST.Query" OBJECT OVERRIDES
    iterator := QueryIt
  END;
  TwoForm = TwoFormPub BRANDED "JunoAST.TwoForm" OBJECT OVERRIDES
    iterator := TwoFormIt
  END;
  Not = NotPub BRANDED "JunoAST.Not" OBJECT OVERRIDES
    iterator := NotIt
  END;
  Exists = ExistsPub BRANDED "JunoAST.Exists" OBJECT OVERRIDES
    iterator := ExistsIt
  END;
  BIUPred = BIUPredPub BRANDED "JunoAST.BIUPred" OBJECT OVERRIDES
    iterator := BIUPredIt
  END;
  Relation = RelationPub BRANDED "JunoAST.Relation" OBJECT OVERRIDES
    iterator := RelationIt
  END;
  BIUFunc = BIUFuncPub BRANDED "JunoAST.BIUFunc" OBJECT OVERRIDES
    iterator := BIUFuncIt
  END;
  BIBFunc = BIBFuncPub BRANDED "JunoAST.BIBFunc" OBJECT OVERRIDES
    iterator := BIBFuncIt
  END;
  List = ListPub BRANDED "JunoAST.List" OBJECT OVERRIDES
    iterator := ListIt
  END;
  Call = CallPub BRANDED "JunoAST.Call" OBJECT OVERRIDES
    iterator := CallIt
  END;
  ExprList = ExprListPub  BRANDED "JunoAST.ExprList" OBJECT OVERRIDES
    iterator := ExprListIt
  END;
  ConjQuery = ConjQueryPub BRANDED "JunoAST.ConjQuery" OBJECT OVERRIDES
    iterator := ConjQueryIt
  END;
  GroupedExpr = GroupedExprPub BRANDED "JunoAST.GroupedExpr" OBJECT OVERRIDES
    iterator := GroupedExprIt
  END;
  NormalForm = NormalFormPub BRANDED "JunoAST.NormalForm" OBJECT OVERRIDES
    iterator := NormalFormIt
  END;
  NearVarList = NearVarListPub BRANDED "JunoAST.NearVarList" OBJECT OVERRIDES
    iterator := NearVarListIt
  END;

TYPE
  FieldIter = Iterator BRANDED "JunoAST.FieldIter" OBJECT
    field := 0
  END;
  AssignIter = FieldIter BRANDED "JunoAST.AssignIter" OBJECT
    ast: Assign
  END; 
  ProcCallIter = FieldIter BRANDED "JunoAST.ProcCallIter" OBJECT
    ast: ProcCall
  END;
  BodyCmdIter = FieldIter BRANDED "JunoAST.BodyCmdIter" OBJECT
    ast: BodyCmd
  END;
  ProjIter = FieldIter BRANDED "JunoAST.ProjIter" OBJECT
    ast: Proj
  END;
  GuardIter = FieldIter BRANDED "JunoAST.GuardIter" OBJECT
    ast: Guard
  END;
  TwoCmdIter = FieldIter BRANDED "JunoAST.TwoCmdIter" OBJECT
    ast: TwoCmd
  END;
  QueryIter = FieldIter BRANDED "JunoAST.QueryIter" OBJECT
    ast: Query
  END;
  TwoFormIter = FieldIter BRANDED "JunoAST.TwoFormIter" OBJECT
    ast: TwoForm
  END;
  NotIter = FieldIter BRANDED "JunoAST.NotIter" OBJECT
    ast: Not
  END;
  ExistsIter = FieldIter BRANDED "JunoAST.ExistsIter" OBJECT
    ast: Exists
  END;
  BIUPredIter = FieldIter BRANDED "JunoAST.BIUPredIter" OBJECT
    ast: BIUPred
  END;
  RelationIter = FieldIter BRANDED "JunoAST.RelationIter" OBJECT
    ast: Relation
  END;
  BIUFuncIter = FieldIter BRANDED "JunoAST.BIUFuncIter" OBJECT
    ast: BIUFunc
  END;
  BIBFuncIter = FieldIter BRANDED "JunoAST.BIBFuncIter" OBJECT
    ast: BIBFunc
  END;
  ExprListIter = Iterator BRANDED "JunoAST.ExprListIter" OBJECT
    curr: ExprLink
  END;
  CallIter = FieldIter BRANDED "JunoAST.CallIter" OBJECT
    ast: Call
  END;
  GrpExprIter = FieldIter BRANDED "JunoAST.GrpExprIter" OBJECT
    ast: GroupedExpr
  END;
  ConjQueryIter = Iterator BRANDED "JunoAST.ConjQueryIter" OBJECT
    forms: REF Formulas;
    curr := 0
  END;
  NormalFormIter = Iterator BRANDED "JunoAST.NormalFormIter" OBJECT
    forms: REF Formulas;
    curr := 0
  END;
  NVListIter = Iterator BRANDED "JunoAST.NVListIter" OBJECT
    curr: NearVarLink
  END;

PROCEDURE EmptyIt(<*UNUSED*> ast: T): Iterator =
  BEGIN RETURN NEW(Iterator, next := NoNext) END EmptyIt;

PROCEDURE NoNext(
    <*UNUSED*> it: Iterator;
    <*UNUSED*> VAR (*OUT*) child: T):
    BOOLEAN =
  BEGIN RETURN FALSE END NoNext;

PROCEDURE AssignIt(ast: Assign): Iterator =
  BEGIN RETURN NEW(AssignIter, ast := ast, next := AssignNext) END AssignIt;

PROCEDURE AssignNext(it: AssignIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    CASE it.field OF <* NOWARN *>
      0 => child := it.ast.vars
    | 1 => child := it.ast.exprs
    | 2 => RETURN FALSE
    END;
    INC(it.field);
    RETURN TRUE
  END AssignNext;

PROCEDURE ProcCallIt(ast: ProcCall): Iterator =
  BEGIN
    RETURN NEW(ProcCallIter, ast := ast, next := ProcCallNext)
  END ProcCallIt;

PROCEDURE ProcCallNext(it: ProcCallIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    CASE it.field OF <* NOWARN *>
      0 => child := it.ast.outs
    | 1 => child := it.ast.inouts
    | 2 => child := it.ast.name
    | 3 => child := it.ast.ins
    | 4 => RETURN FALSE
    END;
    INC(it.field);
    RETURN TRUE
  END ProcCallNext;

PROCEDURE BodyCmdIt(ast: BodyCmd): Iterator =
  BEGIN RETURN NEW(BodyCmdIter, ast := ast, next := BodyCmdNext) END BodyCmdIt;

PROCEDURE BodyCmdNext(it: BodyCmdIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    CASE it.field OF <* NOWARN *>
      0 => child := it.ast.body
    | 1 => RETURN FALSE
    END;
    INC(it.field);
    RETURN TRUE
  END BodyCmdNext;

PROCEDURE ProjIt(ast: Proj): Iterator =
  BEGIN RETURN NEW(ProjIter, ast := ast, next := ProjNext) END ProjIt;

PROCEDURE ProjNext(it: ProjIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    CASE it.field OF <* NOWARN *>
      0 => child := it.ast.vars
    | 1 => child := it.ast.body
    | 2 => RETURN FALSE
    END;
    INC(it.field);
    RETURN TRUE
  END ProjNext;

PROCEDURE GuardIt(ast: Guard): Iterator =
  BEGIN RETURN NEW(GuardIter, ast := ast, next := GuardNext) END GuardIt;

PROCEDURE GuardNext(it: GuardIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    CASE it.field OF <* NOWARN *>
      0 => child := it.ast.grd
    | 1 => child := it.ast.body
    | 2 => RETURN FALSE
    END;
    INC(it.field);
    RETURN TRUE
  END GuardNext;

PROCEDURE TwoCmdIt(ast: TwoCmd): Iterator =
  BEGIN RETURN NEW(TwoCmdIter, ast := ast, next := TwoCmdNext) END TwoCmdIt;

PROCEDURE TwoCmdNext(it: TwoCmdIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    CASE it.field OF <* NOWARN *>
      0 => child := it.ast.c1
    | 1 => child := it.ast.c2
    | 2 => RETURN FALSE
    END;
    INC(it.field);
    RETURN TRUE
  END TwoCmdNext;

PROCEDURE QueryIt(ast: Query): Iterator =
  BEGIN RETURN NEW(QueryIter, ast := ast, next := QueryNext) END QueryIt;

PROCEDURE QueryNext(it: QueryIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    CASE it.field OF <* NOWARN *>
      0 => child := it.ast.f
    | 1 => child := it.ast.vars
    | 2 => RETURN FALSE
    END;
    INC(it.field);
    RETURN TRUE
  END QueryNext;

PROCEDURE ConjQueryIt(ast: ConjQuery): Iterator =
  BEGIN
    RETURN NEW(ConjQueryIter, forms := ast.conj, next := ConjQueryNext)
  END ConjQueryIt;

PROCEDURE ConjQueryNext(it: ConjQueryIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    IF it.curr > LAST(it.forms^) THEN RETURN FALSE END;
    child := it.forms[it.curr];
    INC(it.curr);
    RETURN TRUE
  END ConjQueryNext;

PROCEDURE TwoFormIt(ast: TwoForm): Iterator =
  BEGIN RETURN NEW(TwoFormIter, ast := ast, next := TwoFormNext) END TwoFormIt;

PROCEDURE TwoFormNext(it: TwoFormIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    CASE it.field OF <* NOWARN *>
      0 => child := it.ast.f1
    | 1 => child := it.ast.f2
    | 2 => RETURN FALSE
    END;
    INC(it.field);
    RETURN TRUE
  END TwoFormNext;

PROCEDURE NotIt(ast: Not): Iterator =
  BEGIN RETURN NEW(NotIter, ast := ast, next := NotNext) END NotIt;

PROCEDURE NotNext(it: NotIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    CASE it.field OF <* NOWARN *>
      0 => child := it.ast.f
    | 1 => RETURN FALSE
    END;
    INC(it.field);
    RETURN TRUE
  END NotNext;

PROCEDURE ExistsIt(ast: Exists): Iterator =
  BEGIN RETURN NEW(ExistsIter, ast := ast, next := ExistsNext) END ExistsIt;

PROCEDURE ExistsNext(it: ExistsIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    CASE it.field OF <* NOWARN *>
      0 => child := it.ast.vars
    | 1 => child := it.ast.f
    | 2 => RETURN FALSE
    END;
    INC(it.field);
    RETURN TRUE
  END ExistsNext;

PROCEDURE BIUPredIt(ast: BIUPred): Iterator =
  BEGIN RETURN NEW(BIUPredIter, ast := ast, next := BIUPredNext) END BIUPredIt;

PROCEDURE BIUPredNext(it: BIUPredIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    CASE it.field OF <* NOWARN *>
      0 => child := it.ast.e
    | 1 => RETURN FALSE
    END;
    INC(it.field);
    RETURN TRUE
  END BIUPredNext;

PROCEDURE RelationIt(ast: Relation): Iterator =
  BEGIN
    RETURN NEW(RelationIter, ast := ast, next := RelationNext)
  END RelationIt;

PROCEDURE RelationNext(it: RelationIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    CASE it.field OF <* NOWARN *>
      0 => child := it.ast.e1
    | 1 => child := it.ast.e2
    | 2 => RETURN FALSE
    END;
    INC(it.field);
    RETURN TRUE
  END RelationNext;

PROCEDURE BIUFuncIt(ast: BIUFunc): Iterator =
  BEGIN RETURN NEW(BIUFuncIter, ast := ast, next := BIUFuncNext) END BIUFuncIt;

PROCEDURE BIUFuncNext(it: BIUFuncIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    CASE it.field OF <* NOWARN *>
      0 => child := it.ast.e
    | 1 => RETURN FALSE
    END;
    INC(it.field);
    RETURN TRUE
  END BIUFuncNext;

PROCEDURE BIBFuncIt(ast: BIBFunc): Iterator =
  BEGIN RETURN NEW(BIBFuncIter, ast := ast, next := BIBFuncNext) END BIBFuncIt;

PROCEDURE BIBFuncNext(it: BIBFuncIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    CASE it.field OF <* NOWARN *>
      0 => child := it.ast.e1
    | 1 => child := it.ast.e2
    | 2 => RETURN FALSE
    END;
    INC(it.field);
    RETURN TRUE
  END BIBFuncNext;

PROCEDURE ListIt(ast: List): Iterator =
  BEGIN
    RETURN NEW(ExprListIter, curr := ast.elts.head, next := ExprListNext)
  END ListIt;

PROCEDURE ExprListIt(ast: ExprList): Iterator =
  BEGIN
    RETURN NEW(ExprListIter, curr := ast.head, next := ExprListNext)
  END ExprListIt;

PROCEDURE ExprListNext(it: ExprListIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    IF it.curr = NIL THEN RETURN FALSE END;
    child := it.curr.expr;
    it.curr := it.curr.next;
    RETURN TRUE
  END ExprListNext;

PROCEDURE CallIt(ast: Call): Iterator =
  BEGIN RETURN NEW(CallIter, ast := ast, next := CallNext) END CallIt;

PROCEDURE CallNext(it: CallIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    CASE it.field OF <* NOWARN *>
      0 => child := it.ast.inouts
    | 1 => child := it.ast.name
    | 2 => child := it.ast.ins
    | 3 => RETURN FALSE
    END;
    INC(it.field);
    RETURN TRUE
  END CallNext;

PROCEDURE GroupedExprIt(ast: GroupedExpr): Iterator =
  BEGIN
    RETURN NEW(GrpExprIter, ast := ast, next := GroupedExprNext)
  END GroupedExprIt;

PROCEDURE GroupedExprNext(it: GrpExprIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    CASE it.field OF <* NOWARN *>
      0 => child := it.ast.expr
    | 1 => RETURN FALSE
    END;
    INC(it.field);
    RETURN TRUE
  END GroupedExprNext;

PROCEDURE NormalFormIt(ast: NormalForm): Iterator =
  BEGIN
    RETURN NEW(NormalFormIter, forms := ast.conj, next := NormalFormNext)
  END NormalFormIt;

PROCEDURE NormalFormNext(it: NormalFormIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    IF it.curr > LAST(it.forms^) THEN RETURN FALSE END;
    child := it.forms[it.curr];
    INC(it.curr);
    RETURN TRUE
  END NormalFormNext;

PROCEDURE NearVarListIt(ast: NearVarList): Iterator =
  BEGIN
    RETURN NEW(NVListIter, curr := ast.head, next := NearVarListNext)
  END NearVarListIt;

PROCEDURE NearVarListNext(it: NVListIter; VAR (*OUT*) child: T): BOOLEAN =
  BEGIN
    IF it.curr = NIL THEN RETURN FALSE END;
    child := it.curr.hint;
    it.curr := it.curr.next;
    RETURN TRUE
  END NearVarListNext;
      
REVEAL
  Block = T BRANDED "JunoAST.Block" OBJECT END;
  Decl = DeclPublic BRANDED "JunoAST.Decl" OBJECT END;
  Cmd = T BRANDED "JunoAST.Cmd" OBJECT END;
  LitPred = Formula BRANDED "JunoAST.LitPred" OBJECT END;
  AtomicExpr = Expr BRANDED "JunoAST.AtomicExpr" OBJECT END;
  BuiltInPred = Expr BRANDED "JunoAST.BuiltInPred" OBJECT END;
  BuiltInFunc = Expr BRANDED "JunoAST.BuiltInFunc" OBJECT END;

PROCEDURE Predecessor(ast: T): T =
  VAR node: T := ast; BEGIN
    WHILE node # NIL AND node.bp # End DO node := node.bp END; 
    <* ASSERT node = NIL OR node.bp = End *>
    RETURN node
  END Predecessor;

BEGIN
  (* Initialize special Nil variables *)
  NilId   := Atom.FromText("*NilId*");
  NilExpr := NEW(NilExprT);

  (* Initialize empty lists *)
  EmptyIdList := NEW(IdList);
  EmptyNVList := NEW(NearVarList);
  EmptyQIdList := NEW(QIdList);
  EmptyExprList := EmptyQIdList;

  (* Initialize constant structures *)
  End := NEW(T);
  SkipVal  := NEW(Skip,  bp := End);
  AbortVal := NEW(Abort, bp := End);
  HaltVal  := NEW(Halt,  bp := End);
  FailVal  := NEW(Fail,  bp := End);
  TrueVal  := NEW(True,  bp := End);
  FalseVal := NEW(False, bp := End);
  NilVal   := NEW(Nil,   bp := End);

  (* Initialize pred and func names *)
  CongName   := JunoASTUtils.QIdFromText("_CONG");
  ParaName   := JunoASTUtils.QIdFromText("_PARA");
  HorName    := JunoASTUtils.QIdFromText("_HOR");
  VerName    := JunoASTUtils.QIdFromText("_VER");
  UMinusName := JunoASTUtils.QIdFromText("_UMINUS");
  CarName    := JunoASTUtils.QIdFromText("_CAR");
  CdrName    := JunoASTUtils.QIdFromText("_CDR");
  MinusName  := JunoASTUtils.QIdFromText("_MINUS");
  DivideName := JunoASTUtils.QIdFromText("_DIVIDE");
  RelName    := JunoASTUtils.QIdFromText("_REL");
END JunoAST.
