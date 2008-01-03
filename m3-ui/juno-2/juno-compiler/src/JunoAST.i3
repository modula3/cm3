(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Dec 27 09:56:25 PST 1995 by heydon                   *)
(*      modified on Fri Aug  7 21:53:56 PDT 1992 by myers                    *)

INTERFACE JunoAST;

(* Juno Abstract Syntax Trees

   This interface defines a data structure for Juno parse trees. 

   An AST.T (or "node") represents a syntactically valid sequence of tokens
   from a Juno program or program fragment.  This interface reveals enough
   information to reconstruct the sequence of tokens in the concrete syntax
   that corresponds to an abstract tree, except that the tree reflects only
   the values of literals; not the source that produced them. 

   The remainder of this interface consists mostly of declarations for various
   node types (see the end of this file for a summary of the type hierarchy).
   Roughly speaking, there is a node type defined for each non-terminal in the
   Juno grammar. However, to accomodate the Juno parser, the node types
   defined here correspond to a less restrictive version of the grammar
   than the one given in the language definition. In particular:

|    - JunoAST.i3 makes no distinction between total and partial commands
|    - JunoAST.i3 makes no distinction between formulas and expressions
|    - JunoAST.i3 allows procedure InOut parameters to be expressions
|    - JunoAST.QIdList's are only subtypes of JunoAST.ExprList's

   On the other hand, the following JunoAST types conform to the grammar:

|    - JunoAST.Skip, JunoAST.Abort
|    - JunoAST.Literal (includes True, False, Number, Text, QId, Nil)
|    - JunoAST.NearVarList
|    - JunoAST.IdList
|    - JunoAST.Id

   Each declaration is followed by the syntax for the token sequences that a
   node of that type can represent.  We use the following notation for syntax:

|      A B        A followed by B
|      A | B      A or B
|      [A]        A or empty
|      {A}        A possibly empty list of A's.

   The following conventions are used to declare the node types:

   1. If syntax class X is defined as the EBNF alternative Y | Z, then node
   types Y and Z are subtypes of X.  For example,

|      Import = Decl BRANDED OBJECT END;
|      (* CompleteImport | SelectiveImport *)

   This means that a node of type Import represents a string in one of the
   syntactic classes CompleteImport or SelectiveImport, and implies that the
   node types CompleteImport and SelectiveImport are subtypes of Import, and
   that they are its only direct subtypes. Moreover, the node type Import is
   defined for purposes of sub-typing only; clients should never NEW such
   "class" nodes.

   2. If a syntax class X is defined as an EBNF sequence Y1 ... Yn, then the
   node type X contains a field for each of the Y's, excluding operators and
   keywords.  The fields are in the same order as the Y's.  For example,

|      SelectiveImport <: Import OBJECT id: Id; idList: IdList END;
|      (* FROM Id IMPORT IdList *)

   This means that a node p of type SelectiveImport represents a string with
   the syntax FROM Id IMPORT IdList. The field p.id represents the Id part of
   the string and p.idList represents the IdList part of the string.  There
   is no need to represent the FROM and IMPORT keywords explicitly, since
   they are implied by the node type.

   3. If any of the Y's in the above rule are surrounded by [], then if Y is
   omitted, the correspnding fields of the node will be an empty list for
   each field that is a "list", or one of the "Nil" global variables for each
   field that is not a list. If any of the Y's is surrounded by {}, the
   corresponding part of the node type is a "list".

   A list is an object with a "size" field and a "head" field. The "size"
   field contains the number of entries in the list, and the "head" field
   points to the head of the linked list for the entries. For example,
   consider

|      QId = LitValue BRANDED OBJECT
|        id0: Id := NIL;		 (* may be the special value NilId *)
|        id1: Id := NIL;
|      END;
|      (* [ Id . ] Id *)
|       IdList = T BRANDED OBJECT
|        size: CARDINAL := 0;
|        head: IdLink := NIL
|      END;
|      IdLink = REF RECORD
|        id: Id := NIL;
|        next: IdLink := NIL
|      END;
|      (* Id { Id } *)

   If p is a node of type QId, then p.id0 = NilId means the [Id .] clause is
   absent in the concrete syntax. If p is a node of type IdList, then
   p.size denotes the size of the list, and p.head points to the linked list
   of identifiers. For example, the 3rd identifier in the list is designated
   by p.head.next.next.id.

   The mnemonic names for the fields help you to tell which Id in the object
   goes with which Id in the syntax; if there is any doubt, the rule is that
   the object fields are in the same order as the syntax non-terminals.

   4. Parse errors produce partial AST's. In the event of a parse error, any
   unfilled fields will be NIL. (However, a parse error does not necessarily
   produce an AST containing at least one NIL field. For example, the text
   "x + y ->", when parsed as an expression, produces a valid Plus object
   whose two expressions are "x" and "y".) Conversely, fields of an AST are
   NIL only in the event of a parse error. For example, consider this
   declaration:

|      ProcCall = Cmd BRANDED OBJECT
|        outs: QIdList := NIL;
|        inouts: ExprList := NIL;
|        inout_parens := FALSE;
|        name: QId := NIL;
|        ins: ExprList := NIL
|      END;
|      (* [ QIdList := ] [ ( ExprList ) : ] QId ( [ ExprList ] ) *)

   If p is a node of type ProcCall, then p.outs is the list of OUT parameters
   in the call, p.inouts is the list of INOUT parameters in the call, p.name
   is the name of the procedure, and p.ins is the list of IN parameters. The
   field p.inout_parens represents whether or not the INOUT parameters were
   surrounded by parentheses (for the purposes of unparsing in case there is a
   single INOUT parameter. If parsing is successful, the fields p.outs,
   p.inouts, and p.ins will never be NIL; if the procedure call omits any of
   these lists, their corresponding field will contain an list of "size" 0.

   To indicate the absense of some optional fields, certain AST nodes refer to
   the global constants "NilId" and "NilExpr". For example:

|      VarDeclItem = T BRANDED OBJECT
|        name: Id := NIL;
|        value: Expr := NIL;			 (* may be NilExpr *)
|        next: VarDeclItem := NIL;
|      END;
|      (* Id [ := Expr ] *)

   The initial value assigned to the global variable is optional. On a
   successful parse, the parser sets the "value" field to the distinguished
   value "NilExpr" to indicate that the optional expression was not supplied.
*)

IMPORT JunoValue, Atom;

VAR (* READONLY *)
  NilId: Id;				 (* value when optional Id omitted *)
  NilExpr: Expr;			 (* value when optional Expr omitted *)

VAR (* READONLY *)
  EmptyIdList: IdList;			 (* empty list of identifiers *)
  EmptyNVList: NearVarList;		 (* empty list of near variables *)
  EmptyQIdList: QIdList;		 (* empty list of qualified Id's *)
  EmptyExprList: ExprList;		 (* empty list of expressions *)

VAR (* READONLY *)
  End: T;				 (* back pointer sentinel *)
  SkipVal: Skip;			 (* canonical "SKIP"  *)
  AbortVal: Abort;			 (* canonical "ABORT" *)
  HaltVal: Halt;			 (* canonical "HALT"  *)
  FailVal: Fail;			 (* canonical "FAIL"  *)
  TrueVal: True;			 (* canonical "TRUE"  *)
  FalseVal: False;			 (* canonical "FALSE" *)
  NilVal: Nil;				 (* canonical "NIL    *)

VAR (* READONLY *)
  (* Names of the built-in predicates and functions allowed in constraints;
     their definitions must be installed in a top-level scope. *)
  CongName, ParaName, HorName, VerName, UMinusName, CarName, CdrName,
    MinusName, DivideName, RelName: QId;

TYPE
  IdType = { None, Local, Var, Const, Pred, Func, Proc, ExtProc };

TYPE
  Vars = ARRAY OF NearVarLink;
  Formulas = ARRAY OF Formula;

TYPE
  T <: TPub;				 (* class node only; do not NEW *)
  TPub = BRANDED "JunoAST.TPub" OBJECT
    bp: T := NIL;			 (* back pointer *)
    start, end: CARDINAL := 0;		 (* start <= end *)
  METHODS
    iterator(): Iterator;		 (* an iterator for the children *)
  END;
  (* Unit | Decl | Cmd | Expr | ExprList | NearVarList | IdList *)

  Iterator = BRANDED "JunoAST.Iterator" OBJECT METHODS
    next(VAR (*OUT*) child: T): BOOLEAN
  END;

  (* The parser produces an abstract syntax tree each of whose nodes "n" has
     the property that "n.bp = JunoAST.End". The compiler transforms this
     input AST into a result AST. In the result AST, each node is either a
     node from the original AST (in which case it will have a back pointer to
     "JunoAST.End"), or it is a new node created by the compiler. The
     "predecessor" of a new node "n" is defined to be the first node reached
     with a back pointer to "JunoAST.End" by following the "bp" links from
     "n". If one of the "bp" links on this path is "NIL", then "n"'s
     predecessor is undefined.

     The assembler annotates each node in the AST produced by the compiler by
     setting the node's "start" and "end" fields. For each node "n", these
     fields describe the half-open interval of locations "[n.start, n.end)" in
     the bytestream produced by the assembler corresponding to execution of
     the node "n".

     Each AST node has 0 or more logical children. Abstract iterators provide
     a facility for walking over any AST tree rooted at a JunoAST.Cmd. The
     method call "ast.iterate()" returns a new abstract iterator "it" on the
     logical children of "ast". Subsequent invocations of "it.next(ch)"
     iterate over those children. The "next" method either sets its argument
     to the next child and returns TRUE, or returns FALSE if there are no more
     children. It is an error to invoke the "next" method of an iterator once
     it has returned FALSE. The default "iterator" method returns an iterator
     whose "next" method immediately returns FALSE. *)

PROCEDURE Predecessor(ast: T): T;
(* Returns the predecessor of "ast"; returns NIL if the predecessor is
   undefined. *)

  (* ============================  DECLARATIONS ============================ *)

TYPE
  Unit = T BRANDED "JunoAST.Unit" OBJECT
    size: CARDINAL := 0;
    head: BlockLink := NIL
  END;
  BlockLink = REF RECORD
    block: Block := NIL;
    next: BlockLink := NIL
  END;
  (* Block { Block } *)

  Block <: T;				 (* class node only; do not NEW *)
  (* Module | Import | Comment | UIDecl | Decl *)

  Module = Block BRANDED "JunoAST.Module" OBJECT
    name: Id := NIL
  END;
  (* MODULE Id; *)

  Import = Block BRANDED "JunoAST.Import" OBJECT
    idList: IdList := NIL
  END;
  (* IMPORT IdList; *)

  Comment = Block BRANDED "JunoAST.Comment" OBJECT
    txt: TEXT := NIL;
    private: BOOLEAN := FALSE
  END;
  (* "(* ... *)" (public) or "/* ... */" (private) comment *)

  UIDecl = Block BRANDED "JunoAST.UIDecl" OBJECT
    name: Id := NIL;
    args: ExprList := NIL
  END;
  (* Id "(" [ ExprList ] ")" *)
    
  Decl <: DeclPublic;			 (* class node only; do not NEW *)
  DeclPublic = Block BRANDED "JunoAST.DeclPublic" OBJECT
    private: BOOLEAN := FALSE
  END;
  (* [ PRIVATE ] ( ConstDecl | VarDecl | PredDecl | FuncDecl | ProcDecl ) *)

  ConstDecl = Decl BRANDED "JunoAST.ConstDecl" OBJECT
    size: CARDINAL := 0;
    head: ConstDeclItem := NIL;
  END;
  ConstDeclItem = REF RECORD
    name: Id := NIL;
    value: Expr := NIL;
    next: ConstDeclItem := NIL;
  END;
  (* CONST Id = Expr { , Id = Expr } ; *)

  VarDecl = Decl BRANDED "JunoAST.VarDecl" OBJECT
    size: CARDINAL := 0;
    head: VarDeclItem := NIL;
  END;
  VarDeclItem = REF RECORD
    name: Id := NIL;
    value: Expr := NIL;			 (* may be NilExpr *)
    next: VarDeclItem := NIL;
  END;
  (* VAR Id [ := Expr ] { Id [ := Expr ] } ; *)

  Header = T BRANDED "JunoAST.Header" OBJECT (* class node only; do not NEW *)
    name: Id := NIL;
    ins: IdList := NIL;
  END;
  (* PredHeader | FuncHeader | ProcHeader *)

  PredHeader = Header BRANDED "JunoAST.PredHeader" OBJECT END;
  (* Id(IdList) *)

  FuncHeader = Header BRANDED "JunoAST.FuncHeader" OBJECT
    result: Id := NIL;
  END;
  (* Id = Id(IdList) *)

  ProcHeader = Header BRANDED "JunoAST.ProcHeader" OBJECT
    outs: IdList := NIL;
    inouts: IdList := NIL;
    inout_prens := FALSE;
  END;
  (* [ IdList := ] [ ( IdList ): ] Id(IdList) *)

  PredDecl = Decl BRANDED "JunoAST.PredDecl" OBJECT
    header: PredHeader := NIL;
    body: Formula := NIL;
  END;
  (* PRED PredHeader IS Formula END; *)

  FuncDecl = Decl BRANDED "JunoAST.FuncDecl" OBJECT
    header: FuncHeader := NIL;
    body: Constraint := NIL;
  END;
  (* FUNC FuncHeader IS Constraint END; *)

  ProcDecl = Decl BRANDED "JunoAST.ProcDecl" OBJECT
    header: ProcHeader := NIL;
    body: TotalCmd := NIL;
  END;
  (* PROC ProcHeader IS TotalCmd END; *)

  (* ==============================  COMMANDS ============================== *)

  Cmd <: T;				 (* class node only; do not NEW *)
  TotalCmd = Cmd;			 (* class node only; do not NEW *)
  (* Skip | Abort | Halt | Fail | Assign | ProcCall | If | Do | Save | Proj |
     Seq | Guard | Else | GroupedCmd | Query | ConjQuery | Flip | Safe *)

  Skip =  Cmd BRANDED "JunoAST.Skip"  OBJECT END;
  Abort = Cmd BRANDED "JunoAST.Abort" OBJECT END;
  Halt =  Cmd BRANDED "JunoAST.Halt"  OBJECT END;
  Fail =  Cmd BRANDED "JunoAST.Fail"  OBJECT END;
  (* SKIP, ABORT, HALT, FAIL *)

  Assign <: AssignPub;
  AssignPub = Cmd BRANDED "JunoAST.AssignPub" OBJECT
    vars: QIdList := NIL;
    exprs: ExprList := NIL;
  END;
  (* QIdList := ExprList *)

  ProcCall <: ProcCallPub;
  ProcCallPub = Cmd BRANDED "JunoAST.ProcCallPub" OBJECT
    outs: QIdList := NIL;
    inouts: ExprList := NIL;
    inout_parens := FALSE;
    name: QId := NIL;
    ins: ExprList := NIL
  END;
  (* [ QIdList := ] [ ( ExprList ) : ] QId ( [ ExprList ] ) *)
  (* A ProcCall node is only created if the corresponding stream of tokens is
     syntactically guaranteed to represent a procedure call (as opposed to an
     assignment). This is the case if:
|
|    1) the leading "QIdList :=" is omitted,
|    2) the option "( Exprlist ) :" does appear, or
|    3) the initial QIdList does appear and contains at least 2 QId's.
|
     Otherwise, the token stream is parsed as an assignment. Hence, a stream
     of tokens of the form:
|
|      QId := QId ( [ ExprList ] )
|
     is parsed as an assignment, since it cannot be guaranteed (in the absence
     of semantic information) that the second QId names a procedure rather
     than a function. *)

  BodyCmd <: BodyCmdPub;		 (* class node only; do not NEW *)
  BodyCmdPub = Cmd BRANDED "JunoAST.BodyCmdPub" OBJECT
    body: Cmd := NIL
  END;
  (* If | Do | GroupedCmd | Flip | Safe | Save *)

  If =   BodyCmd BRANDED "JunoAST.If"   OBJECT END;	   (* IF Cmd FI *)
  Do =   BodyCmd BRANDED "JunoAST.Do"   OBJECT END;	   (* DO Cmd OD *)
  Flip = BodyCmd BRANDED "JunoAST.Flip" OBJECT END;	   (* FLIP(cmd) *)
  Safe = BodyCmd BRANDED "JunoAST.Safe" OBJECT END;	   (* SAFE(cmd) *)

  GroupedCmd = BodyCmd BRANDED "JunoAST.GroupedCmd" OBJECT END; (* { Cmd } *)

  Save = BodyCmd BRANDED "JunoAST.Save" OBJECT
    nm: QId := NIL;
    save, restore: QId := NIL;		 (* set by JunoCompile.AnnotateAtoms *)
  END;
  (* SAVE Id IN Cmd END *)
  (* The "nm" will always be unqualified, but it is a "QId" instead of a
     simple "Id" so that it can be supplied as an error AST in the event that
     the named interface is unknown. *)
  (* "save" and "restore" are the annotated QId's of the save and restore
     procedures for interface "Id". *)

  Proj <: ProjPub;
  ProjPub = Cmd BRANDED "JunoAST.ProjPub" OBJECT
    vars: NearVarList := NIL;
    body: Cmd := NIL
  END;
  (* VAR NearVarList IN Cmd END *)

  Guard <: GuardPub;
  GuardPub = Cmd BRANDED "JunoAST.GuardPub" OBJECT
    grd: Formula := NIL;
    body: Cmd := NIL
  END;
  (* Formula -> Cmd *)

  TwoCmd <: TwoCmdPub;			 (* class node only; do not NEW *)
  TwoCmdPub = Cmd BRANDED "JunoAST.TwoCmdPub" OBJECT
    c1: Cmd := NIL;
    c2: Cmd := NIL
  END;
  (* Seq | Else *)

  Seq =  TwoCmd BRANDED "JunoAST.Seq"  OBJECT END;	 (* Cmd ; Cmd *)
  Else = TwoCmd BRANDED "JunoAST.Else" OBJECT END;	 (* Cmd | Cmd *)

  Query <: QueryPub;
  QueryPub = Cmd BRANDED "JunoAST.QueryPub" OBJECT
    f: Formula;
    vars: NearVarList;
  END;
  (* f ? (vars) *)

  ConjQuery <: ConjQueryPub;
  ConjQueryPub = Cmd BRANDED "JunoAST.CongQueryPub" OBJECT
    conj: REF Formulas;
    var: REF Vars
  END;
  (* (conj[0] AND ... AND conj[LAST(conj)]) ? (var[0], ... ,var[LAST(var)]) *)
  (* See "NearVarLink" for a description of the "frozen" and "hint" fields of
     the variables in a "ConjQuery". *)
  (* The logical children of a "ConjQuery" node are the formulas in its "conj"
     array. *)

  (* ======================  EXPRESSIONS / FORMULAS ======================== *)

  Expr = T BRANDED "JunoAST.Expr" OBJECT (* class node only; do not NEW *)
    b3cnt: CARDINAL := 0		 (* used in JunoCompileNF.ToCmd *)
  END;
  (* LitPred | BuiltInPred | AtomicExpr | BuiltInFunc |
     Call | GroupedExpr | NormalForm *)

  (* Expr synonyms *)
  Formula = Expr;
  Constraint = Formula;

  LitPred <: Formula;			 (* class node only; do not NEW *)
  (* True | False *)

  True  = LitPred BRANDED "JunoAST.True"  OBJECT END;	 (* TRUE *)
  False = LitPred BRANDED "JunoAST.False" OBJECT END;	 (* FALSE *)

  BuiltInPred <: Formula;		 (* class node only; do not NEW *)
  (* And | Or | Not | Exists | BIUPred | Relation *)

  TwoForm <: TwoFormPub;		 (* class node only; do not NEW *)
  TwoFormPub = BuiltInPred BRANDED "JunoAST.TwoFormPub" OBJECT
    f1: Formula := NIL;
    f2: Formula := NIL
  END;
  (* And | Or *)

  And = TwoForm BRANDED "JunoAST.And" OBJECT END; (* Formula AND Formula *)
  Or  = TwoForm BRANDED "JunoAST.Or"  OBJECT END; (* Formula OR Formula  *)

  Not <: NotPub;
  NotPub = BuiltInPred BRANDED "JunoAST.NotPub" OBJECT
    f: Formula := NIL
  END;
  (* NOT Formula *)

  Exists <: ExistsPub;
  ExistsPub = BuiltInPred BRANDED "JunoAST.ExistsPub" OBJECT
    vars: NearVarList := NIL;
    f: Constraint := NIL;
  END;
  (* E NearVarList :: Constraint *)

  BIUPred <: BIUPredPub;	 (* class node only; do not NEW *)
  BIUPredPub = BuiltInPred BRANDED "JunoAST.BIUPredPub" OBJECT
    e: Expr := NIL
  END;
  (* IsReal | IsText | IsPair | IsInt *)

  IsReal = BIUPred BRANDED "JunoAST.IsReal" OBJECT END;	 (* REAL(Expr) *)
  IsText = BIUPred BRANDED "JunoAST.IsText" OBJECT END;	 (* TEXT(Expr) *)
  IsPair = BIUPred BRANDED "JunoAST.IsPair" OBJECT END;	 (* PAIR(Expr) *)
  IsInt  = BIUPred BRANDED "JunoAST.IsInt"  OBJECT END;	 (* INT(Expr)  *)

  Relation <: RelationPub;		 (* class node only; do not NEW *)
  RelationPub = BuiltInPred BRANDED "JunoAST.RelationPub" OBJECT
    e1: Expr := NIL;
    e2: Expr := NIL
  END;
  (* Equals | Differs | Less | Greater | AtMost | AtLeast
     | Cong | Para | Hor | Ver *)

  Equals = Relation BRANDED "JunoAST.Equals" OBJECT
    near: BOOLEAN := FALSE
  END;
  (* Expr = Expr OR Expr ~ Expr *)

  Differs  = Relation BRANDED "JunoAST.Differs" OBJECT END; (* Expr # Expr  *)
  Less     = Relation BRANDED "JunoAST.Less"    OBJECT END; (* Expr < Expr  *)
  Greater  = Relation BRANDED "JunoAST.Greater" OBJECT END; (* Expr > Expr  *)
  AtMost   = Relation BRANDED "JunoAST.AtMost"  OBJECT END; (* Expr <= Expr *)
  AtLeast  = Relation BRANDED "JunoAST.AtLeast" OBJECT END; (* Expr >= Expr *)

  Cong = Relation BRANDED "JunoAST.Cong" OBJECT END; (* Expr CONG Expr *)
  Para = Relation BRANDED "JunoAST.Para" OBJECT END; (* Expr PARA Expr *)
  Hor  = Relation BRANDED "JunoAST.Hor"  OBJECT END; (* Expr HOR Expr  *)
  Ver  = Relation BRANDED "JunoAST.Ver"  OBJECT END; (* Expr VER Expr  *)

  (* --------------------------- EXPRESSIONS ------------------------------- *)

  AtomicExpr <: Expr;			 (* class node only; do not NEW *)
  (* LitValue | QId *)

  (* class node only; do not NEW *)
  LitValue = AtomicExpr BRANDED "JunoAST.LitValue" OBJECT END;
  (* Number | Text | Nil *)

  Number = LitValue BRANDED "JunoAST.Number" OBJECT
    val: JunoValue.Real                  (* always positive *)
  END;
  (* Digit {Digit} [ "." Digit {Digit} ] [ (e|E) [+|-] Digit {Digit} ] *)

  Text = LitValue BRANDED "JunoAST.Text" OBJECT
    val: TEXT := NIL;
    index: INTEGER := -1		 (* set by JunoCompile.AnnotateAtoms *)
  END;
  (* "Text String" *)
  (* The "index" is an index in "JunoRT.value_tbl". *)

  Nil = LitValue BRANDED "JunoAST.Nil" OBJECT END;
  (* NIL *)

  QId = AtomicExpr BRANDED "JunoAST.QId" OBJECT
    id0: Id := NIL;			 (* may be NilId *)
    id1: Id := NIL;			 
    type := IdType.None;		 (* set by JunoCompile.AnnotateAtoms *)
    index: INTEGER := 0;		 (* set by JunoCompile.AnnotateAtoms *)
  END;
  (* [ Id . ] Id *)
  (* If "id0 # NilId", then this represents the qualified identifier
     "id0.id1". Otherwise, it represents the unqualified identifier "id1".
     The "type" represents the type of the QId. The interpretation of "index"
     depends on type: If "type = Local", then "index" is an index in the local
     frame; if "type = Const" or "Var", "index" is an index in
     "JunoRT.value_tbl". *)

  BuiltInFunc <: Expr;			 (* class node only; do not NEW *)
  (* BIUFunc | BIBFunc | List *)

  BIUFunc <: BIUFuncPub;		 (* class node only; do not NEW *)
  BIUFuncPub = BuiltInFunc BRANDED "JunoAST.BIUFuncPub" OBJECT
    e: Expr := NIL;
  END;
  (* UMinus | Floor | Ceiling | Round | Abs |
     Sin | Cos | Exp | Ln | Car | Cdr *)

  UMinus  = BIUFunc BRANDED "JunoAST.UMinus"  OBJECT END; (* - Expr        *)
  Floor   = BIUFunc BRANDED "JunoAST.Floor"   OBJECT END; (* FLOOR(Expr)   *)
  Ceiling = BIUFunc BRANDED "JunoAST.Ceiling" OBJECT END; (* CEILING(Expr) *)
  Round   = BIUFunc BRANDED "JunoAST.Round"   OBJECT END; (* ROUND(Expr)   *)
  Abs     = BIUFunc BRANDED "JunoAST.Abs"     OBJECT END; (* ABS(Expr)     *)
  Sin     = BIUFunc BRANDED "JunoAST.Sin"     OBJECT END; (* SIN(Expr)     *)
  Cos     = BIUFunc BRANDED "JunoAST.Cos"     OBJECT END; (* COS(Expr)     *)
  Exp     = BIUFunc BRANDED "JunoAST.Exp"     OBJECT END; (* EXP(Expr)     *)
  Ln      = BIUFunc BRANDED "JunoAST.Ln"      OBJECT END; (* LN(Expr)      *)
  Car     = BIUFunc BRANDED "JunoAST.Car"     OBJECT END; (* CAR(Expr)     *)
  Cdr     = BIUFunc BRANDED "JunoAST.Cdr"     OBJECT END; (* CDR(Expr)     *)

  BIBFunc <: BIBFuncPub;		 (* class node only; do not NEW *)
  BIBFuncPub = BuiltInFunc BRANDED "JunoAST.BIBFuncPub" OBJECT
    e1: Expr := NIL;
    e2: Expr := NIL
  END;
  (* BuiltInAddFunc | BuiltInMulFunc | Pair | Rel | Max | Min | Atan *)

  BuiltInAddFunc = BIBFunc BRANDED "JunoAST.BuiltInAddFunc" OBJECT END;
  (* Plus | Minus | Concat *)

  Plus   = BuiltInAddFunc BRANDED "JunoAST.Plus"   OBJECT END;(* Expr + Expr *)
  Minus  = BuiltInAddFunc BRANDED "JunoAST.Minus"  OBJECT END;(* Expr - Expr *)
  Concat = BuiltInAddFunc BRANDED "JunoAST.Concat" OBJECT END;(* Expr & Expr *)

  BuiltInMulFunc = BIBFunc BRANDED "JunoAST.BuiltInMulFunc" OBJECT END;
  (* Times | Divide | Div | Mod *)

  Times  = BuiltInMulFunc BRANDED "JunoAST.Times"  OBJECT END;(* Expr * Expr *)
  Divide = BuiltInMulFunc BRANDED "JunoAST.Divide" OBJECT END;(* Expr / Expr *)
  Div    = BuiltInMulFunc BRANDED "JunoAST.Div" OBJECT END; (* Expr DIV Expr *)
  Mod    = BuiltInMulFunc BRANDED "JunoAST.Mod" OBJECT END; (* Expr MOD Expr *)

  Pair = BIBFunc BRANDED "JunoAST.Pair" OBJECT END; (* ( Expr , Expr )  *)
  Rel  = BIBFunc BRANDED "JunoAST.Rel"  OBJECT END; (* Expr REL Expr    *)
  Max  = BIBFunc BRANDED "JunoAST.Max"  OBJECT END; (* MAX(Expr, Expr)  *)
  Min  = BIBFunc BRANDED "JunoAST.Min"  OBJECT END; (* MIN(Expr, Expr)  *)
  Atan = BIBFunc BRANDED "JunoAST.Atan" OBJECT END; (* ATAN(Expr, Expr) *)

  List <: ListPub;			 (* class node only; do not NEW *)
  ListPub = BuiltInFunc BRANDED "JunoAST.ListPub" OBJECT
    elts: ExprList := NIL
  END;
  (* [ ExprList ] *)

  Call <: CallPub;
  CallPub = Expr BRANDED "JunoAST.CallPub" OBJECT
    inouts: ExprList := NIL;
    inout_parens := FALSE;
    name: QId := NIL;
    ins: ExprList := NIL;
    normal_form: Formula := NIL;	 (* set by JunoCompile.AnnotateAtoms *)
  END;
  (* [ ( ExprList ): ] QId( [ ExprList] ) *)
  (* If "c" is of type "Call" and represents a call to a user-defined
     predicate or function, then "c.normal_form" is annotated to contain the
     normal form of the predicate body associated with the predicate or
     function. *)

  GroupedExpr <: GroupedExprPub;
  GroupedExprPub = Expr BRANDED "JunoAST.GroupedExprPub" OBJECT
    expr: Expr := NIL
  END;
  (* ( Expr ) *)

  NormalForm <: NormalFormPub;
  NormalFormPub = Expr BRANDED "JunoAST.NormalFormPub" OBJECT
    conj: REF Formulas;
    var: REF Vars;
  END;
  (* (E var[0], ... ,var[LAST(var)] :: conj[0] AND ... AND conj[LAST(conj)]) *)
  (* See "NearVarLink" for a description of the "frozen" and "hint" fields of
     the variables in a "NormalForm". *)

  (* ===========================  MISCELLANY =============================== *)

  ExprList <: ExprListPub;
  ExprListPub = T BRANDED "JunoAST.ExprListPub" OBJECT
    size: CARDINAL := 0;
    head: ExprLink := NIL
  END;
  ExprLink = REF RECORD
    expr: Expr := NIL;
    next: ExprLink := NIL
  END;
  (* Expr { , Expr } *)

  QIdList = ExprList BRANDED "JunoAST.QIdList" OBJECT END;
  (* QId { , QId } *)

  NearVarList <: NearVarListPub;
  NearVarListPub = T BRANDED "JunoAST.NearVarListPub" OBJECT
    size: CARDINAL := 0;
    head: NearVarLink := NIL;
  END;
  NearVarLink = REF RECORD
    id: Id := NIL;
    evar, frozen := FALSE;
    hint: Expr := NIL;			 (* may be "NilExpr" *)
    index: INTEGER := 0;		 (* set by JunoCompile.AnnotateAtoms *)
    next: NearVarLink := NIL;
  END;
  (* Id [ (~|=) Expr ] { Id [ (~|=) Expr ] } *)
  (* In "Proj" and "Exists" nodes, the "frozen" and "hint" fields indicate
     whether the variable is frozen, hinted, or unhinted. If "frozen" is set,
     then the variable is frozen, and "hint # NilExpr". Otherwise, if "hint #
     NilExpr", the variable is hinted, and "hint" is the value of its hint. If
     "hint = NilExpr", the variable is said to be unhinted. In this case, the
     value of the "frozen" bit may have other meanings, as described below.

     In "ConjQuery" and "NormalForm" nodes, the "frozen" and "hint" fields
     have a different meaning. The "hint" field is ignored (it is typically
     "NilExpr"). The "frozen" bit indicates whether the variable has a valid
     value prior to the query.

     The "evar" bit indicates whether this is an existentially quantified (and
     hence, temporary) variable. The "index" is an index in the local frame.

     The logicial children of a NearVarList are the "hint" fields of the
     elements of the list. *)

  IdList = T BRANDED "JunoAST.IdList" OBJECT
    size: CARDINAL := 0;
    head: IdLink := NIL
  END;
  IdLink = REF RECORD
    id: Id := NIL;
    index: INTEGER := 0;		 (* set by JunoScope *)
    next: IdLink := NIL
  END;
  (* Id { , Id } *)
  (* The "index" is an index in the local frame. *)

  Id = Atom.T;

END JunoAST.

(* Type Hierarchy (indentation indicates subtyping):

|  NAME                       DESCRIPTION
|  ---------------------      ------------------------
|  T                          Top-level AST type
|    Unit                     Compilation unit (a list of Blocks)
|    Block                    Top-level block of a unit
|      Module                 Module
|      Import                 Import statement
|      Comment                Comment (top-level only)
|      UIDecl                 User-interface declaration
|      Decl                   Declaration
|        ConstDecl            Constant declaration
|        VarDecl              Variable declaration
|        PredDecl             Predicate declaration
|        FuncDecl             Function declaration
|        ProcDecl             Procedure declaration
|    ValueDeclItem            Const/Var initializer supertype
|      ConstDeclItem          Single constant declaration
|      VarDeclItem            Single variable declaration
|    Header                   Declaration header
|      PredHeader             Predicate header
|      FuncHeader             Function header
|      ProcHeader             Procedure header
|    Cmd                      Command
|      Skip                   SKIP command
|      Abort                  ABORT command
|      Halt                   HALT command
|      Fail                   FAIL command (added by compiler only)
|      Assign                 Assignment command
|      ProcCall               Procedure call command (no outs)
|      If                     IF..FI command
|      Do                     DO..OD command
|      Save                   SAVE..IN..END command
|      Proj                   Projection command
|      Seq                    Sequence command
|      Guard                  Guard command
|      Else                   Else command
|      GroupedCmd             Command grouped with {..}
|      Query                  P?(vlist) (added by compiler only)
|      ConjQuery              Conjunction Query (added by compiler only)
|      Flip                   FLIP(S) (added by compiler only)
|      Safe                   SAFE(S) (added by compiler only)
|    Expr (= Formula)         Expression (includes formulas)
|      LitPred                Literal predicate
|        True                 TRUE
|        False                FALSE
|      BuiltInPred            Built-in predicate expression
|        And                  AND
|        Or                   OR
|        Not                  NOT
|        Exists               E quantification
|        BIUPred              Built-in unary predicate on expressions
|          IsReal             REAL
|          IsText             TEXT
|          IsPair             PAIR
|          IsInt              INT
|        Relation             Built-in binary predicate expression
|          Equals             =
|          Differs            #
|          Less               <
|          Greater            >
|          AtMost             <=
|          AtLeast            >=
|          Cong               CONG
|          Para               PARA
|          Hor                HOR
|          Ver                VER
|      AtomicExpr             Literal or QID
|        LitValue             Literal Value
|          Number             Real literal
|          Text               Text literal
|          Nil                NIL
|        QId                  QID expression
|      BuiltInFunc            Built-in function expression
|        BIUFunc              Built-in unary expressions
|          UMinus             Unary -
|          Floor              FLOOR
|          Ceiling            CEILING
|          Round              ROUND
|          Abs                ABS
|          Sin                SIN
|          Cos                COS
|          Exp                EXP
|          Ln                 LN
|          Car                CAR
|          Cdr                CDR
|        BIBFunc              Built-in binary expressions
|          BuiltInAddFunc     Built-in function expression using a "AddOp"
|            Plus             +
|            Minus            -
|            Concat           &
|          BuiltInMulFunc     Built-in function expression using a "MulOp"
|            Times            *
|            Divide           /
|            Div              DIV
|            Mod              MOD
|          Pair               Ordered Pair ()
|          Rel                REL
|          Max                MAX
|          Min                MIN
|          Atan               ATAN
|        List                 List Introduction []
|      Call                   User pred/func/proc call
|      GroupedExpr            Expression grouped with (..)
|      NormalForm             Normal form constraint (added by compiler only)
|    ExprList                 List of Expr's
|      QIdList                List of QId's
|    NearVarList              List of QId's with optional initializations
|    IdList                   List of Id's
|  Id                         An identifier

*)
