(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun  3 20:42:32 PDT 1993 by meehan                   *)

MODULE Macro;

(* In this module, we handle the reading of S-expressions, as well as
   the implementation of macros. *)

IMPORT ASCII, Atom, AtomRefTbl, Fmt, FormsVBT, FVRuntime, Rd, RefList, Sx,
       Text, Thread;

FROM FVRuntime IMPORT FVSyntax, ToText;
FROM RefListUtils IMPORT AssocQ, Equal, NthTail, Pop, Push, SetNth;

REVEAL
  T = Public BRANDED OBJECT
        name    : Atom.T;
        formals : RefList.T := NIL;
        expander: Op;            (* compiled object *)
        boa     : BOOLEAN;       (* actuals are not named *)
      OVERRIDES
        apply := Apply
      END;

TYPE ReadMacro = Sx.ReadMacro OBJECT bqLevel: REF CARDINAL END;

VAR                              (* CONST *)
  qAppend     := Atom.FromText ("Append");
  qCons       := Atom.FromText ("Cons");
  qLength     := Atom.FromText ("Length");
  qList       := Atom.FromText ("List");
  qListStar   := Atom.FromText ("List*");
  qNth        := Atom.FromText ("Nth");
  qNthTail    := Atom.FromText ("NthTail");
  qEqual      := Atom.FromText ("Equal");
  qIf         := Atom.FromText ("IF");
  qAnd        := Atom.FromText ("AND");
  qNot        := Atom.FromText ("NOT");
  qOr         := Atom.FromText ("OR");
  qEQ         := Atom.FromText ("=");
  qGE         := Atom.FromText (">=");
  qGT         := Atom.FromText (">");
  qLE         := Atom.FromText ("<=");
  qLT         := Atom.FromText ("<");
  qNIL        := Atom.FromText ("NIL");
  qMinus      := Atom.FromText ("-");
  qPlus       := Atom.FromText ("+");
  qCat        := Atom.FromText ("Cat");
  qTextEmpty  := Atom.FromText ("Empty");
  qTextSub    := Atom.FromText ("Sub");
  qFromName   := Atom.FromText ("Intern");
  qSymbolName := Atom.FromText ("SymbolName");

PROCEDURE Parse (list: RefList.T): T RAISES {FormsVBT.Error} =
  (* list = (name [BOA] formals bqexp). *)
  VAR
    formals: RefList.T;
    res             := NEW (T);
    n               := RefList.Length (list);
  PROCEDURE err (msg: TEXT; x: REFANY := "") RAISES {FormsVBT.Error} =
    BEGIN
      RAISE
        FormsVBT.Error (Fmt.F ("Illegal Macro form: %s %s", msg, ToText (x)))
    END err;
  BEGIN
    res.boa := n = 4 AND list.tail.head = FVRuntime.qBOA;
    IF NOT res.boa AND NOT n = 3 THEN err ("Syntax error") END;
    TYPECASE Pop (list) OF
    | NULL => err ("Macro name is NIL")
    | Atom.T (s) => res.name := s
    | REFANY (r) => err ("Macro name isn't a symbol: ", r)
    END;
    IF res.boa THEN list := list.tail END;
    TYPECASE Pop (list) OF
    | RefList.T (x) => formals := x
    | REFANY (x) => err ("Bad list of formals: ", x)
    END;
    WHILE formals # NIL DO
      TYPECASE Pop (formals) OF
      | NULL => err ("Null formal")
      | Atom.T (s) =>
          IF AssocQ (res.formals, s) # NIL THEN
            err ("Duplicate formal: ", s)
          ELSE
            Push (res.formals, RefList.List2 (s, NoDefault))
          END
      | RefList.T (pair) =>
          IF RefList.Length (pair) # 2 THEN
            err ("Bad formal", pair)
          ELSE
            TYPECASE pair.head OF
            | Atom.T (s) =>
                IF AssocQ (res.formals, s) # NIL THEN
                  err ("Duplicate formal: ", s)
                ELSE
                  Push (res.formals, RefList.List2 (s, pair.tail.head))
                END
            ELSE
              err ("Bad formal", pair)
            END
          END
      | REFANY (r) => err ("Formals must be symbols: ", r)
      END
    END;
    res.formals := RefList.ReverseD (res.formals);
    res.expander := Compile (list.head, res.formals, RefanyTC);
    RETURN res
  END Parse;

CONST RefanyTC = -1;
VAR
  TextTC    := TYPECODE (TEXT);
  ListTC    := TYPECODE (RefList.T);
  IntegerTC := TYPECODE (REF INTEGER);
  RealTC    := TYPECODE (REF REAL);
  NullTC    := TYPECODE (NULL);
  BooleanTC := TYPECODE (REF BOOLEAN);
  SymbolTC  := TYPECODE (Atom.T);

VAR
  NullOp := NEW (
              Op, args := RefList.List1 (NIL), tc := NullTC, eval := EvalQuote);

(*
CONST LastTypeIndex = 7;

TYPE TypeIndex = [0 .. LastTypeIndex];

VAR TypeCodes: ARRAY TypeIndex OF INTEGER;

PROCEDURE TypeCodeIndex (tc: INTEGER): TypeIndex =
  BEGIN
    FOR i := FIRST (TypeIndex) TO LAST (TypeIndex) DO
      IF tc = TypeCodes [i] THEN RETURN i END
    END;
    <* ASSERT FALSE *>
    END TypeCodeIndex;

PROCEDURE InitTypeCodes () =
  PROCEDURE OK (a, b: TypeIndex) =
    BEGIN
      ComparableTypes [a, b] := TRUE;
      ComparableTypes [b, a] := TRUE
    END OK;
  BEGIN
    TypeCodes := ARRAY TypeIndex OF
                   INTEGER {RefanyTC, TextTC, ListTC, IntegerTC, RealTC,
                            NullTC, BooleanTC, SymbolTC};
    FOR i := FIRST (TypeIndex) TO LAST (TypeIndex) DO
      FOR j := FIRST (TypeIndex) TO LAST (TypeIndex) DO
        ComparableTypes [i, j] := i = j
      END
    END;
    WITH ref     = TypeCodeIndex (RefanyTC),
         text    = TypeCodeIndex (TextTC),
         list    = TypeCodeIndex (ListTC),
         integer = TypeCodeIndex (IntegerTC),
         real    = TypeCodeIndex (RealTC),
         null    = TypeCodeIndex (NullTC),
         boolean = TypeCodeIndex (BooleanTC),
         symbol  = TypeCodeIndex (SymbolTC)  DO
      OK (ref, text);
      OK (ref, list);
      OK (ref, null);
      OK (ref, symbol);
      OK (text, null);
      OK (list, null);
    END;
  END InitTypeCodes;

VAR ComparableTypes: ARRAY TypeIndex, TypeIndex OF BOOLEAN;

<* UNUSED *> PROCEDURE Comparable (a, b: INTEGER): BOOLEAN =
  BEGIN
    RETURN ComparableTypes [TypeCodeIndex (a), TypeCodeIndex (b)]
  END Comparable;
*)

VAR VarOps := ARRAY [0 .. 5] OF Op {NIL, ..};

PROCEDURE Compile (exp: REFANY; formals: RefList.T; tc := RefanyTC): Op
  RAISES {FormsVBT.Error} =
  VAR
    value: REFANY;
    c    : Compiler;
  BEGIN
    TYPECASE exp OF
    | NULL => Check (tc, NullTC); RETURN NullOp
    | Atom.T (s) =>
        IF s = qNIL THEN Check (tc, NullTC); RETURN NullOp END;
        WITH p = Position (formals, s) DO
          IF p = -1 THEN
            RAISE FormsVBT.Error ("Unbound variable: " & Atom.ToText (s))
          ELSIF p < NUMBER (VarOps) THEN
            RETURN VarOps [p]
          ELSE
            RETURN NEW (Op, tc := p, eval := EvalVar)
          END
        END
    | TEXT =>
        Check (tc, TextTC);
        RETURN
          NEW (Op, args := RefList.List1 (exp), tc := TextTC, eval := EvalQuote)
    | REF INTEGER =>
        Check (tc, IntegerTC);
        RETURN NEW (Op, args := RefList.List1 (exp), tc := IntegerTC,
                    eval := EvalQuote)
    | REF REAL =>
        Check (tc, RealTC);
        RETURN
          NEW (Op, args := RefList.List1 (exp), tc := RealTC, eval := EvalQuote)
    | REF BOOLEAN =>
        Check (tc, BooleanTC);
        RETURN NEW (Op, args := RefList.List1 (exp), tc := BooleanTC,
                    eval := EvalQuote)
    | RefList.T (x) =>
        WITH f    = x.head,
             args = x.tail,
             n    = RefList.Length (args) DO
          TYPECASE f OF
          | Atom.T (s) =>
              IF Ctable.get (s, value) THEN
                c := value;
                Check (tc, c.tc, c.n, n);
                RETURN c.compile (c, args, formals, tc)
              END
          ELSE
          END
        END
    ELSE
    END;
    RAISE FormsVBT.Error (
            "Illegal expression in macro definition:" & ToText (exp))
  END Compile;

TYPE Display = REF ARRAY OF REFANY;

PROCEDURE Fault (typeName: TEXT; arg: REFANY): REFANY RAISES {FormsVBT.Error} =
  BEGIN
    RAISE FormsVBT.Error (
            Fmt.F ("A %s was required here: %s", typeName, ToText (arg)))
  END Fault;
  
PROCEDURE Apply (m: T; actuals: RefList.T): REFANY RAISES {FormsVBT.Error} =
  PROCEDURE err (msg: TEXT; actuals: REFANY := "") RAISES {FormsVBT.Error} =
    BEGIN
      RAISE FormsVBT.Error (Fmt.F ("Error in call to macro %s: %s %s",
                                   Atom.ToText (m.name), msg, ToText (actuals)))
    END err;
  VAR
    ac              := RefList.Length (actuals);
    fc              := RefList.Length (m.formals);
    d               := NEW (Display, fc);
    vars: RefList.T := NIL;
    pair: RefList.T;
  BEGIN
    IF ac > fc THEN err ("Too many arguments: ", Sx.FromInt (ac)) END;
    IF m.boa THEN
      FOR i := 0 TO ac - 1 DO d [i] := Pop (actuals) END;
      FOR i := ac TO fc - 1 DO
        pair := RefList.Nth (m.formals, i);
        IF pair.tail.head = NoDefault THEN
          err ("Argument has no default: ", pair.head)
        ELSE
          d [i] := pair.tail.head
        END
      END
    ELSE
      IF ac # fc THEN
        FOR i := 0 TO fc - 1 DO
          pair := RefList.Nth (m.formals, i);
          d [i] := pair.tail.head
        END
      END;
      WHILE actuals # NIL DO
        TYPECASE Pop (actuals) OF
        | NULL => err ("NIL argument")
        | RefList.T (y) =>
            IF RefList.Length (y) # 2 THEN
              err ("Illegal argument: ", y)
            ELSE
              WITH p = Position (m.formals, y.head) DO
                IF p = -1 THEN
                  err ("Unknown variable: ", y.head)
                ELSIF RefList.Member (vars, y.head) THEN
                  err ("Argument passed twice: ", y.head)
                ELSE
                  d [p] := y.tail.head;
                  Push (vars, y.head)
                END
              END
            END
        | REFANY (r) => err ("Illegal argument: ", r)
        END
      END;
      IF fc # ac THEN
        FOR i := 0 TO fc - 1 DO
          IF d [i] = NoDefault THEN
            pair := RefList.Nth (m.formals, i);
            err ("No value was supplied for ", pair.head)
          END
        END
      END
    END;
    RETURN m.expander.eval (m.expander, d)
  END Apply;

PROCEDURE Eval (op: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  BEGIN
    RETURN op.eval (op, d)
  END Eval;

TYPE
  Compiler = OBJECT
               tc: INTEGER;      (* the typecode of the result *)
               n : CARDINAL;     (* the number of parameters *)
               compile: CProc    (* the compilation "method" *)
             END;
  CProc =
    PROCEDURE (c: Compiler; args: RefList.T; formals: RefList.T; tc: INTEGER):
      Op RAISES {FormsVBT.Error};
  Test = {GE, GT, LE, LT, EQ};
  ComparisonCompiler = Compiler OBJECT test: Test END;
  Op = OBJECT
         tc                := RefanyTC;
         args: RefList.T;
         eval: PROCEDURE (op: Op; d: Display): REFANY RAISES {FormsVBT.Error}
       END;
  ComparisonOp = Op OBJECT test: Test END;

VAR Ctable := NEW (AtomRefTbl.Default).init (20); (* Maps symbol -> compiler *)

PROCEDURE InitCompilers () =
  PROCEDURE f (s: Atom.T; tc: INTEGER; n: CARDINAL; compile: CProc) =
    VAR c := NEW (Compiler, tc := tc, n := n, compile := compile);
    BEGIN
      EVAL Ctable.put (s, c)
    END f;
  PROCEDURE g (s: Atom.T; test: Test) =
    BEGIN
      EVAL Ctable.put (
             s,
             NEW (ComparisonCompiler, tc := BooleanTC, n := LAST (CARDINAL),
                  compile := CompileComparison, test := test))
    END g;
  BEGIN
    f (qAnd, BooleanTC, LAST (CARDINAL), CompileAnd);
    f (qAppend, ListTC, 2, CompileAppend);
    f (FVRuntime.qBackquote, RefanyTC, 1, CompileBackquote);
    f (qCat, TextTC, LAST (CARDINAL), CompileCat);
    f (qCons, ListTC, 2, CompileCons);
    f (qFromName, SymbolTC, 1, CompileFromName);
    f (qIf, RefanyTC, 3, CompileIf);
    f (qList, ListTC, LAST (CARDINAL), CompileList);
    f (qEqual, BooleanTC, 2, CompileEqual);
    f (qLength, IntegerTC, 1, CompileLength);
    f (qListStar, ListTC, LAST (CARDINAL), CompileListStar);
    f (qMinus, RefanyTC, LAST (CARDINAL), CompileMinus);
    f (qNot, BooleanTC, 1, CompileNot);
    f (qNth, RefanyTC, 2, CompileNth);
    f (qNthTail, ListTC, 2, CompileNthTail);
    f (qOr, BooleanTC, LAST (CARDINAL), CompileOr);
    f (qPlus, RefanyTC, LAST (CARDINAL), CompilePlus);
    f (FVRuntime.qQuote, RefanyTC, 1, CompileQuote);
    f (qSymbolName, TextTC, 1, CompileSymbolName);
    f (qTextEmpty, BooleanTC, 1, CompileEmpty);
    (* f (qTextEqual, BooleanTC, 2, CompileTextEqual); *)
    (* f (qTextLength, IntegerTC, 1, CompileTextLength); *)
    f (qTextSub, TextTC, 3, CompileSub);
    g (qEQ, Test.EQ);
    g (qGE, Test.GE);
    g (qGT, Test.GT);
    g (qLE, Test.LE);
    g (qLT, Test.LT)
  END InitCompilers;

PROCEDURE Check (TCwanted, TCgonnaGet       : INTEGER;
                 argCountWanted, argCountGot: CARDINAL  := 0)
  RAISES {FormsVBT.Error} =
  BEGIN
    IF argCountWanted # argCountGot AND argCountWanted # LAST (CARDINAL) THEN
      RAISE FormsVBT.Error (
              Fmt.F ("Wrong number of args: %s instead of %s",
                     Fmt.Int (argCountGot), Fmt.Int (argCountWanted)))
    ELSIF TCwanted # RefanyTC AND TCgonnaGet # NullTC
            AND TCgonnaGet # TCwanted
            AND TCgonnaGet # RefanyTC (* NARROW at runtime *)
      THEN
      RAISE FormsVBT.Error ("Invalid type")
    END
  END Check;
  
PROCEDURE CompileQuote (<* UNUSED *> self   : Compiler;
                                     args   : RefList.T;
                        <* UNUSED *> formals: RefList.T;
                                     tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  VAR actualTC := TYPECODE (args.head);
  BEGIN
    Check (tc, actualTC);
    RETURN NEW (Op, args := args, tc := actualTC, eval := EvalQuote)
  END CompileQuote;
    
PROCEDURE EvalQuote (x: Op; <* UNUSED *> d: Display): REFANY =
  BEGIN
    RETURN x.args.head
  END EvalQuote;

PROCEDURE CompileCons (<* UNUSED *> self   : Compiler;
                                    args   : RefList.T;
                                    formals: RefList.T;
                                    tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  BEGIN
    args.head := Compile (args.head, formals, RefanyTC);
    args.tail.head := Compile (args.tail.head, formals, ListTC);
    RETURN NEW (Op, args := args, tc := tc, eval := EvalCons)
  END CompileCons;

PROCEDURE EvalCons (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  BEGIN
    RETURN
      RefList.Cons (Eval (x.args.head, d), GetList (Eval (x.args.tail.head, d)))
  END EvalCons;

PROCEDURE CompileLength (<* UNUSED *> self   : Compiler;
                                          args   : RefList.T;
                                          formals: RefList.T;
                                          tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  BEGIN
    args.head := Compile (args.head, formals, ListTC);
    RETURN NEW (Op, args := args, tc := tc, eval := EvalLength)
  END CompileLength;

PROCEDURE EvalLength (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  BEGIN
    TYPECASE Eval (x.args.head, d) OF
    | RefList.T (list) => RETURN Sx.FromInt (RefList.Length (list))
    | TEXT (t) => RETURN Sx.FromInt (Text.Length (t))
    | REFANY (ref) => RETURN Fault ("list or text", ref)
    END
  END EvalLength;

PROCEDURE CompileEqual (<* UNUSED *> self   : Compiler;
                                         args   : RefList.T;
                                         formals: RefList.T;
                                         tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  BEGIN
    args.head := Compile (args.head, formals, ListTC);
    args.tail.head := Compile (args.tail.head, formals, ListTC);
    RETURN NEW (Op, args := args, tc := tc, eval := EvalEqual)
  END CompileEqual;

PROCEDURE EvalEqual (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  BEGIN
    RETURN
      Sx.FromBool (Equal (Eval (x.args.head, d), Eval (x.args.tail.head, d)))
  END EvalEqual;


PROCEDURE CompileNth (<* UNUSED *> self   : Compiler;
                                   args   : RefList.T;
                                   formals: RefList.T;
                                   tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  BEGIN
    args.head := Compile (args.head, formals, ListTC);
    args.tail.head := Compile (args.tail.head, formals, IntegerTC);
    RETURN NEW (Op, args := args, tc := tc, eval := EvalNth)
  END CompileNth;

PROCEDURE EvalNth (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  VAR
    list := GetList (Eval (x.args.head, d));
    n    := GetRefCardinal (Eval (x.args.tail.head, d))^;
  BEGIN
    IF n < RefList.Length (list) THEN
      RETURN RefList.Nth (list, n)
    ELSE
      RAISE FormsVBT.Error (
              Fmt.F ("RefList.Nth (..., %s): range error", Fmt.Int (n)))
    END
  END EvalNth;

PROCEDURE CompileNthTail (<* UNUSED *> self   : Compiler;
                                       args   : RefList.T;
                                       formals: RefList.T;
                                       tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  BEGIN
    args.head := Compile (args.head, formals, ListTC);
    args.tail.head := Compile (args.tail.head, formals, IntegerTC);
    RETURN NEW (Op, args := args, tc := tc, eval := EvalNthTail)
  END CompileNthTail;

PROCEDURE EvalNthTail (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  VAR
    list := GetList (Eval (x.args.head, d));
    n    := GetRefCardinal (Eval (x.args.tail.head, d))^;
  BEGIN
    IF n <= RefList.Length (list) THEN
      RETURN NthTail (list, n)
    ELSE
      RAISE FormsVBT.Error (
              Fmt.F ("RefList.NthTail (..., %s): range error", Fmt.Int (n)))
    END
  END EvalNthTail;

PROCEDURE CompileList (<* UNUSED *> self   : Compiler;
                                    args   : RefList.T;
                                    formals: RefList.T;
                                    tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  VAR res := NEW (Op, args := args, tc := tc, eval := EvalList);
  BEGIN
    WHILE args # NIL DO
      args.head := Compile (args.head, formals);
      args := args.tail
    END;
    RETURN res
  END CompileList;

PROCEDURE EvalList (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  VAR
    res: RefList.T := NIL;
    ops         := x.args;
  BEGIN
    WHILE ops # NIL DO Push (res, Eval (Pop (ops), d)) END;
    RETURN RefList.ReverseD (res)
  END EvalList;

PROCEDURE CompileListStar (<* UNUSED *> self   : Compiler;
                                        args   : RefList.T;
                                        formals: RefList.T;
                                        tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  VAR res := NEW (Op, args := args, tc := tc, eval := EvalListStar);
  BEGIN
    WHILE args # NIL DO
      args.head := Compile (args.head, formals);
      args := args.tail
    END;
    RETURN res
  END CompileListStar;

PROCEDURE EvalListStar (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  VAR
    ops       := x.args;
    op   : Op := Pop (ops);
    first     := RefList.List1 (Eval (op, d));
    last      := first;
  BEGIN
    WHILE ops.tail # NIL DO
      op := Pop (ops);
      Push (last.tail, Eval (op, d));
      last := last.tail
    END;
    op := ops.head;
    last.tail := GetList (Eval (op, d));
    RETURN first
  END EvalListStar;

PROCEDURE CompileAppend (<* UNUSED *> self   : Compiler;
                                      args   : RefList.T;
                                      formals: RefList.T;
                                      tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  VAR res := NEW (Op, args := args, tc := tc, eval := EvalAppend);
  BEGIN
    WHILE args # NIL DO
      args.head := Compile (args.head, formals, ListTC);
      args := args.tail
    END;
    RETURN res
  END CompileAppend;

PROCEDURE EvalAppend (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  VAR
    res : RefList.T := NIL;
    args         := RefList.Reverse (x.args);
  BEGIN
    WHILE args # NIL DO
      res := RefList.Append (GetList (Eval (args.head, d)), res);
      args := args.tail
    END;
    RETURN res
  END EvalAppend;

PROCEDURE CompileIf (<* UNUSED *> self   : Compiler;
                                  args   : RefList.T;
                                  formals: RefList.T;
                                  tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  BEGIN
    SetNth (args, 0, Compile (RefList.Nth (args, 0), formals, BooleanTC));
    SetNth (args, 1, Compile (RefList.Nth (args, 1), formals, RefanyTC));
    SetNth (args, 2, Compile (RefList.Nth (args, 2), formals, RefanyTC));
    RETURN NEW (Op, args := args, tc := tc, eval := EvalIf)
  END CompileIf;

PROCEDURE EvalIf (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  BEGIN
    IF GetBoolean (Eval (RefList.Nth (x.args, 0), d)) THEN
      RETURN Eval (RefList.Nth (x.args, 1), d)
    ELSE
      RETURN Eval (RefList.Nth (x.args, 2), d)
    END
  END EvalIf;

PROCEDURE CompileAnd (<* UNUSED *> self   : Compiler;
                                   args   : RefList.T;
                                   formals: RefList.T;
                                   tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  VAR res := NEW (Op, args := args, tc := tc, eval := EvalAnd);
  BEGIN
    WHILE args # NIL DO
      args.head := Compile (args.head, formals, BooleanTC);
      args := args.tail
    END;
    RETURN res
  END CompileAnd;

PROCEDURE EvalAnd (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  BEGIN
    WHILE x.args # NIL DO
      IF NOT GetBoolean (Eval (Pop (x.args), d)) THEN
        RETURN Sx.False
      END
    END;
    RETURN Sx.True
  END EvalAnd;

PROCEDURE CompileOr (<* UNUSED *> self   : Compiler;
                                   args   : RefList.T;
                                   formals: RefList.T;
                                   tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  VAR res := NEW (Op, args := args, tc := tc, eval := EvalOr);
  BEGIN
    WHILE args # NIL DO
      args.head := Compile (args.head, formals, BooleanTC);
      args := args.tail
    END;
    RETURN res
  END CompileOr; 

PROCEDURE EvalOr (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  BEGIN
    WHILE x.args # NIL DO
      IF GetBoolean (Eval (Pop (x.args), d)) THEN RETURN Sx.True END
    END;
    RETURN Sx.False
  END EvalOr;

PROCEDURE CompileNot (<* UNUSED *> self   : Compiler;
                                   args   : RefList.T;
                                   formals: RefList.T;
                                   tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  VAR res := NEW (Op, args := args, tc := tc, eval := EvalNot);
  BEGIN
    args.head := Compile (args.head, formals, BooleanTC);
    RETURN res
  END CompileNot; 

PROCEDURE EvalNot (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  BEGIN
    IF GetBoolean (Eval (x.args.head, d)) THEN
      RETURN Sx.False
    ELSE
      RETURN Sx.True
    END
  END EvalNot;

PROCEDURE CompileBackquote (<* UNUSED *> self   : Compiler;
                                         args   : RefList.T;
                                         formals: RefList.T;
                            <* UNUSED *> tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  BEGIN
    (* There is no EvalBackquote.  Backquoted S-expressions simply expand into
       other S-expressions, which are in turn compiled. *)
    RETURN Compile (Backquote (RefList.Nth (args, 0)), formals)
  END CompileBackquote;

PROCEDURE Backquote (exp: REFANY): REFANY RAISES {FormsVBT.Error} =
  (* This returns a Lisp-like S-expression that can be passed to Eval to
     produce a new FormsVBT expression.  The only operators are QUOTE,
     LIST, LIST*, and APPEND. *)
  BEGIN
    TYPECASE exp OF
    | NULL => RETURN NIL
    | RefList.T (list) =>
        IF list.head = FVRuntime.qComma THEN
          RETURN list.tail.head
        ELSIF list.head = FVRuntime.qBackquote THEN
          RETURN Backquote (Backquote (list.tail.head))
        ELSE
          TYPECASE list.head OF
          | NULL =>
          | RefList.T (sublist) =>
              IF sublist.head = FVRuntime.qCommaAtsign THEN
                RETURN RefList.List3 (qAppend, sublist.tail.head,
                                   Backquote (list.tail))
              END
          ELSE
          END;
          RETURN Combine (Backquote (list.head), Backquote (list.tail))
        END
    ELSE
    END;
    RETURN RefList.List2 (FVRuntime.qQuote, exp)
  END Backquote;

PROCEDURE Combine (car, cdr: REFANY): REFANY =
  BEGIN
    (* This implementation attempts to recycle cons-cells wherever possible. *)
    TYPECASE car OF
    | NULL =>
        TYPECASE cdr OF
        | NULL =>
            (* (cons NIL NIL) -> (QUOTE (NIL)) *)
            RETURN RefList.List2 (FVRuntime.qQuote, RefList.List1 (NIL))
        | RefList.T (cdr) =>
            IF cdr.head = FVRuntime.qQuote THEN
              (* (cons NIL (QUOTE x)) -> (QUOTE (NIL .  x)) *)
              cdr.tail.head := RefList.Cons (NIL, cdr.tail.head);
              RETURN cdr
            END
        ELSE
        END
    | RefList.T (car) =>
        IF car.head = FVRuntime.qQuote THEN
          TYPECASE cdr OF
          | NULL =>
              (* (cons (QUOTE x) NIL) -> (QUOTE (x)) *)
              car.tail := RefList.List1 (car.tail);
              RETURN car
          | RefList.T (cdr) =>
              IF cdr.head = FVRuntime.qQuote THEN
                (* (cons (QUOTE x) (QUOTE y)) -> (QUOTE (x .  y)) *)
                car.tail.tail := cdr.tail.head;
                cdr.head := car.tail;
                cdr.tail := NIL;
                car.tail := cdr;
                RETURN car
                (* RETURN RefList.List2 ( qQuote, RefList.New (car.tail.head,
                   cdr.tail.head)) *)
              ELSIF cdr.head = qList OR cdr.head = qListStar THEN
                Push (cdr.tail, car);
                RETURN cdr
              END
          ELSE
          END
        ELSE
          TYPECASE cdr OF
          | NULL =>
              (* (cons x NIL) -> (LIST x) *)
              RETURN RefList.List2 (qList, car)
          | RefList.T (cdr) =>
              IF cdr.head = qList OR cdr.head = qListStar THEN
                (* (cons x (LIST .  y)) -> (LIST x .  y) *)
                Push (cdr.tail, car);
                RETURN cdr
              END
          ELSE <* ASSERT FALSE *>    
          END
        END
    ELSE
    END;
    RETURN RefList.List3 (qListStar, car, cdr)
  END Combine;

(*
PROCEDURE CompileEquals (<* UNUSED *> self   : Compiler;
                                      args   : RefList.T;
                                      formals: RefList.T;
                                      tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  VAR
    res     := NEW (Op, args := args, tc := tc, eval := EvalEquals);
    op : Op;
  BEGIN
    args.head := Compile (args.head, formals);
    op := args.head;
    args.tail.head := Compile (args.tail.head, formals, op.tc);
    RETURN res
  END CompileEquals;

PROCEDURE EvalEquals (x: Op; d: Display): REFANY
  RAISES {FormsVBT.Error} =
  VAR
    op1: Op := x.args.head;
    op2: Op := x.args.tail.head;
    a       := Eval (op1, d);
    b       := Eval (op2, d);
  BEGIN
    IF a = b THEN
      RETURN Sx.True
    ELSIF NOT Comparable (op1.tc, op2.tc) THEN
      RAISE FormsVBT.Error ("Invalid comparison")
    ELSIF x.tc = IntegerTC THEN
      RETURN BooleanRefs [GetRefInteger (a)^ = GetRefInteger (b)^]
    ELSIF x.tc = RealTC THEN
      RETURN BooleanRefs [GetRefReal (a)^ = GetRefReal (b)^]
    ELSE
      (* If a and b are non-numeric refs, and we got here, then a # b. *)
      RETURN Sx.False
    END
  END EvalEquals;
*)

PROCEDURE CompilePlus (<* UNUSED *> self   : Compiler;
                                    args   : RefList.T;
                                    formals: RefList.T;
                       <* UNUSED *> tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  VAR
    foundType     := FALSE;
    type          := RefanyTC;
    res           := NEW (Op, args := args, eval := EvalPlus);
    op       : Op;
  BEGIN
    IF args = NIL THEN RAISE FormsVBT.Error ("(+) isn't defined.") END;
    REPEAT
      op := Compile (args.head, formals);
      args.head := op;
      args := args.tail;
      IF foundType THEN
        IF (op.tc = IntegerTC OR op.tc = RealTC) AND op.tc # type THEN
          RAISE FormsVBT.Error ("Invalid argument to +")
        END
      ELSIF op.tc = IntegerTC OR op.tc = RealTC THEN
        foundType := TRUE;
        type := op.tc
      ELSIF op.tc # RefanyTC THEN
        RAISE FormsVBT.Error ("Invalid argument to +")
      END
    UNTIL args = NIL;
    res.tc := type;
    RETURN res
  END CompilePlus;

PROCEDURE EvalPlus (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  VAR
    ops     := x.args;
    op : Op;
  PROCEDURE AddIntegers (isum: INTEGER): REFANY RAISES {FormsVBT.Error} =
    BEGIN
      WHILE ops # NIL DO
        op := Pop (ops);
        isum := isum + GetRefInteger (Eval (op, d))^
      END;
      RETURN Sx.FromInt (isum)
    END AddIntegers;
  PROCEDURE AddReals (rsum: REAL): REFANY RAISES {FormsVBT.Error} =
    BEGIN
      WHILE ops # NIL DO
        op := Pop (ops);
        rsum := rsum + GetRefReal (Eval (op, d))^
      END;
      RETURN Sx.FromReal (rsum)
    END AddReals;
  BEGIN
    IF x.tc = IntegerTC THEN
      RETURN AddIntegers (0)
    ELSIF x.tc = RealTC THEN
      RETURN AddReals (0.0)
    ELSE
      op := Pop (ops);
      TYPECASE Eval (op, d) OF
      | NULL => RETURN Fault ("number", NIL)
      | REF INTEGER (ri) => RETURN AddIntegers (ri^)
      | REF REAL (rr) => RETURN AddReals (rr^)
      | REFANY (ref) => RETURN Fault ("number", ref)
      END
    END
  END EvalPlus;

PROCEDURE CompileMinus (<* UNUSED *> self   : Compiler;
                                     args   : RefList.T;
                                     formals: RefList.T;
                        <* UNUSED *> tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  VAR
    foundType         := FALSE;
    type              := RefanyTC;
    res               := NEW (Op, args := args, eval := EvalMinus);
    op       : Op;
  BEGIN
    IF args = NIL THEN RAISE FormsVBT.Error ("(-) isn't defined.") END;
    REPEAT
      op := Compile (args.head, formals);
      args.head := op;
      args := args.tail;
      IF foundType THEN
        IF (op.tc = IntegerTC OR op.tc = RealTC) AND op.tc # type THEN
          RAISE FormsVBT.Error ("Invalid argument to -")
        END
      ELSIF op.tc = IntegerTC OR op.tc = RealTC THEN
        foundType := TRUE;
        type := op.tc
      ELSIF op.tc # RefanyTC THEN
        RAISE FormsVBT.Error ("Invalid argument to -")
      END
    UNTIL args = NIL;
    res.tc := type;
    RETURN res
  END CompileMinus;

PROCEDURE EvalMinus (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  VAR
    ops     := x.args;
    op : Op;
  PROCEDURE SubIntegers (isum: INTEGER): REFANY RAISES {FormsVBT.Error} =
    BEGIN
      WHILE ops # NIL DO
        op := Pop (ops);
        isum := isum - GetRefInteger (op.eval (op, d))^
      END;
      RETURN Sx.FromInt (isum)
    END SubIntegers;
  PROCEDURE SubReals (rsum: REAL): REFANY RAISES {FormsVBT.Error} =
    BEGIN
      WHILE ops # NIL DO
        op := Pop (ops);
        rsum := rsum - GetRefReal (op.eval (op, d))^
      END;
      RETURN Sx.FromReal (rsum)
    END SubReals;
  BEGIN
    IF x.tc = IntegerTC THEN
      RETURN SubIntegers (0)
    ELSIF x.tc = RealTC THEN
      RETURN SubReals (0.0)
    ELSE
      op := Pop (ops);
      TYPECASE op.eval (op, d) OF
      | NULL => RETURN Fault ("number", NIL)
      | REF INTEGER (ri) => RETURN SubIntegers (ri^)
      | REF REAL (rr) => RETURN SubReals (rr^)
      | REFANY (ref) => RETURN Fault ("number", ref)
      END
    END
  END EvalMinus;

PROCEDURE CompileComparison (             self   : Compiler;
                                          args   : RefList.T;
                                          formals: RefList.T;
                             <* UNUSED *> tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  VAR
    c: ComparisonCompiler := self;
    res := NEW (ComparisonOp, args := args, eval := EvalComparison,
                test := c.test);
  BEGIN
    IF RefList.Length (args) < 2 THEN
      RAISE FormsVBT.Error ("Too few arguments")
    END;
    WHILE args # NIL DO
      args.head := Compile (args.head, formals);
      args := args.tail
    END;
    RETURN res
  END CompileComparison;

PROCEDURE EvalComparison (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  VAR
    xc: ComparisonOp := x;
    a, args : RefList.T := NIL;
  PROCEDURE compareIntegers (base: INTEGER): REFANY RAISES {FormsVBT.Error} =
    VAR
      z: BOOLEAN;
      n: INTEGER;
    BEGIN
      WHILE args # NIL DO
        n := GetRefInteger (Pop (args))^;
        CASE xc.test OF
        | Test.EQ => z := base = n
        | Test.GE => z := base >= n
        | Test.GT => z := base > n
        | Test.LE => z := base <= n
        | Test.LT => z := base < n
        END;
        IF z THEN base := n ELSE RETURN Sx.False END
      END;
      RETURN Sx.True
    END compareIntegers;
  PROCEDURE compareReals (base: REAL): REFANY RAISES {FormsVBT.Error} =
    VAR
      z: BOOLEAN;
      n: REAL;
    BEGIN
      WHILE args # NIL DO
        n := GetRefReal (Pop (args))^;
        CASE xc.test OF
        | Test.EQ => z := base = n
        | Test.GE => z := base >= n
        | Test.GT => z := base > n
        | Test.LE => z := base <= n
        | Test.LT => z := base < n
        END;
        IF z THEN base := n ELSE RETURN Sx.False END
      END;
      RETURN Sx.True
    END compareReals;
  PROCEDURE compareRefsEQ (base: REFANY): REFANY =
    BEGIN
      WHILE args # NIL DO
        IF base # Pop (args) THEN RETURN Sx.False END
      END;
      RETURN Sx.True
    END compareRefsEQ;
  BEGIN
    a := x.args;
    (* Evaluate all the operands. *)
    WHILE a # NIL DO Push (args, Eval (Pop (a), d)) END;
    args := RefList.ReverseD (args);
    CASE xc.test OF
    | Test.EQ =>
        TYPECASE Pop (args) OF
        | NULL => RETURN compareRefsEQ (NIL)
        | REF INTEGER (ri) => RETURN compareIntegers (ri^)
        | REF REAL (rr) => RETURN compareReals (rr^)
        | REFANY (ref) => RETURN compareRefsEQ (ref)
        END
    ELSE                         (* arithmetic comparison *)
      TYPECASE Pop (args) OF
      | NULL => RAISE FormsVBT.Error ("Invalid comparison")
      | REF INTEGER (ri) => RETURN compareIntegers (ri^)
      | REF REAL (rr) => RETURN compareReals (rr^)
      ELSE RAISE FormsVBT.Error ("Invalid comparison")
      END
    END
  END EvalComparison;


PROCEDURE CompileCat (<* UNUSED *> self   : Compiler;
                                   args   : RefList.T;
                                   formals: RefList.T;
                                   tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  VAR res := NEW (Op, args := args, tc := tc, eval := EvalCat);
  BEGIN
    WHILE args # NIL DO
      args.head := Compile (args.head, formals, tc);
      args := args.tail
    END;
    RETURN res
  END CompileCat;

PROCEDURE EvalCat (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  VAR
    res     := "";
    ops     := x.args;
  BEGIN
    WHILE ops # NIL DO res := res & GetText (Eval (Pop (ops), d)) END;
    RETURN res
  END EvalCat;

PROCEDURE CompileFromName (<* UNUSED *> self   : Compiler;
                                        args   : RefList.T;
                                        formals: RefList.T;
                                        tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  BEGIN
    args.head := Compile (args.head, formals, TextTC);
    RETURN NEW (Op, args := args, tc := tc, eval := EvalFromName)
  END CompileFromName;

PROCEDURE EvalFromName (x: Op; d: Display): REFANY
  RAISES {FormsVBT.Error} =
  BEGIN
    RETURN Atom.FromText (GetText (Eval (x.args.head, d)))
  END EvalFromName;

PROCEDURE CompileSymbolName (<* UNUSED *> self   : Compiler;
                                          args   : RefList.T;
                                          formals: RefList.T;
                                          tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  BEGIN
    args.head := Compile (args.head, formals, SymbolTC);
    RETURN NEW (Op, args := args, tc := tc, eval := EvalSymbolName)
  END CompileSymbolName;

PROCEDURE EvalSymbolName (x: Op; d: Display): REFANY
  RAISES {FormsVBT.Error} =
  BEGIN
    RETURN Atom.ToText (GetSymbol (Eval (x.args.head, d)))
  END EvalSymbolName;

PROCEDURE CompileEmpty (<* UNUSED *> self   : Compiler;
                                     args   : RefList.T;
                                     formals: RefList.T;
                                     tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  BEGIN
    args.head := Compile (args.head, formals,TextTC);
    RETURN NEW (Op, args := args, tc := tc, eval := EvalEmpty)
  END CompileEmpty;

VAR BooleanRefs := ARRAY BOOLEAN OF Atom.T {Sx.False, Sx.True};

PROCEDURE EvalEmpty (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  BEGIN
    RETURN BooleanRefs [Text.Empty (GetText (Eval (x.args.head, d)))]
  END EvalEmpty;

PROCEDURE CompileSub (<* UNUSED *> self   : Compiler;
                                   args   : RefList.T;
                                   formals: RefList.T;
                                   tc     : INTEGER   ): Op
  RAISES {FormsVBT.Error} =
  BEGIN
    SetNth (args, 0, Compile (RefList.Nth (args, 0), formals, tc));
    SetNth (args, 1, Compile (RefList.Nth (args, 1), formals, IntegerTC));
    SetNth (args, 2, Compile (RefList.Nth (args, 2), formals, IntegerTC));
    RETURN NEW (Op, args := args, tc := tc, eval := EvalSub)
  END CompileSub;

PROCEDURE EvalSub (x: Op; d: Display): REFANY RAISES {FormsVBT.Error} =
  BEGIN
    RETURN Text.Sub (GetText (Eval (RefList.Nth (x.args, 0), d)),
                     GetRefCardinal (Eval (RefList.Nth (x.args, 1), d))^,
                     GetRefCardinal (Eval (RefList.Nth (x.args, 2), d))^)
  END EvalSub;

PROCEDURE EvalVar (x: Op; d: Display): REFANY =
  BEGIN
    RETURN d [x.tc]
  END EvalVar;

(* ******** Safe retrieval functions ******* *)

PROCEDURE GetText (ref: REFANY): TEXT RAISES {FormsVBT.Error} =
  BEGIN
    TYPECASE ref OF | NULL => | TEXT (t) => RETURN t ELSE END;
    RETURN Fault ("text", ref)
  END GetText;

PROCEDURE GetBoolean (ref: REFANY): BOOLEAN RAISES {FormsVBT.Error} =
  BEGIN
    IF ref = Sx.True THEN
      RETURN TRUE
    ELSIF ref = Sx.False THEN
      RETURN FALSE
    ELSE
      EVAL Fault ("boolean", ref);
      <* ASSERT FALSE *>
    END
  END GetBoolean;
  
PROCEDURE GetRefInteger (ref: REFANY): REF INTEGER RAISES {FormsVBT.Error} =
  BEGIN
    TYPECASE ref OF | NULL => | REF INTEGER (t) => RETURN t ELSE END;
    RETURN Fault ("integer", ref)
  END GetRefInteger;
  
PROCEDURE GetRefCardinal (ref: REFANY): REF INTEGER RAISES {FormsVBT.Error} =
  BEGIN
    TYPECASE ref OF
    | NULL =>
    | REF INTEGER (t) =>         (* All Sx-integers are REF INTEGER *)
        IF t^ >= 0 THEN RETURN t END
    ELSE
    END;
    RETURN Fault ("integer", ref)
  END GetRefCardinal;
  
PROCEDURE GetRefReal (ref: REFANY): REF REAL RAISES {FormsVBT.Error} =
  BEGIN
    TYPECASE ref OF | NULL => | REF REAL (t) => RETURN t ELSE END;
    RETURN Fault ("real", ref)
  END GetRefReal;
  
PROCEDURE GetList (ref: REFANY): RefList.T RAISES {FormsVBT.Error} =
  BEGIN
    TYPECASE ref OF
    | RefList.T (t) => RETURN t     (* NIL is OK here *)
    ELSE
      RETURN Fault ("list", ref)
    END
  END GetList;

PROCEDURE GetSymbol (ref: REFANY): Atom.T RAISES {FormsVBT.Error} =
  BEGIN
    TYPECASE ref OF | NULL => | Atom.T (t) => RETURN t ELSE END;
    RETURN Fault ("symbol", ref)
  END GetSymbol;
  
PROCEDURE Position (list: RefList.T; item: REFANY): [-1 .. LAST (CARDINAL)] =
  VAR i: CARDINAL := 0;
  BEGIN
    LOOP
      IF list = NIL THEN RETURN -1
      ELSIF RefList.Nth (Pop (list), 0) = item THEN RETURN i
      ELSE INC (i)
      END
    END
  END Position;

(****************** Syntax for reading/writing %foo, =baz ******************)

PROCEDURE ReadEqual (<* UNUSED *> rm    : Sx.ReadMacro;
                                  rd    : Rd.T;
                                  syntax: Sx.Syntax     ): RefList.T
  RAISES {Sx.ReadError, Thread.Alerted} =
  BEGIN
    TRY
      IF Rd.GetChar (rd) IN ASCII.Spaces THEN
        RETURN RefList.List1 (qEQ)
      ELSE
        Rd.UnGetChar (rd);
        RETURN RefList.List1 (
                 RefList.List2 (FVRuntime.qValue, Sx.Read (rd, syntax)))
      END
    EXCEPT
    | Rd.Failure => RAISE Sx.ReadError ("Rd.Failure") (* FIXME *)
    | Rd.EndOfFile => RAISE Sx.ReadError ("Premature EOF")
    END
  END ReadEqual;
          
PROCEDURE ReadPercent (<* UNUSED *> rm    : Sx.ReadMacro;
                                    rd    : Rd.T;
                                    syntax: Sx.Syntax     ): RefList.T
  RAISES {Sx.ReadError, Thread.Alerted} =
  BEGIN
    TRY
      RETURN RefList.List1 (RefList.List2 (FVRuntime.qName, Sx.Read (rd, syntax)))
    EXCEPT
    | Rd.EndOfFile => RAISE Sx.ReadError ("Premature EOF")
    END
  END ReadPercent;
          
PROCEDURE ReadQuote (<* UNUSED *> rm    : Sx.ReadMacro;
                                  rd    : Rd.T;
                                  syntax: Sx.Syntax     ): RefList.T
  RAISES {Sx.ReadError, Thread.Alerted} =
  BEGIN
    TRY
      RETURN RefList.List1 (RefList.List2 (FVRuntime.qQuote, Sx.Read (rd, syntax)))
    EXCEPT
    | Rd.EndOfFile => RAISE Sx.ReadError ("Premature EOF")
    END
  END ReadQuote;
          
PROCEDURE ReadBackquote (rm: ReadMacro; rd: Rd.T; syntax: Sx.Syntax):
  RefList.T RAISES {Sx.ReadError, Thread.Alerted} =
  BEGIN
    TRY
      INC (rm.bqLevel^);
      TRY
        RETURN RefList.List1 (
                 RefList.List2 (FVRuntime.qBackquote, Sx.Read (rd, syntax)))
      FINALLY
        DEC (rm.bqLevel^)
      END
    EXCEPT
    | Rd.EndOfFile => RAISE Sx.ReadError ("Premature EOF")
    END
  END ReadBackquote;
          
PROCEDURE ReadComma (rm: ReadMacro; rd: Rd.T; syntax: Sx.Syntax): RefList.T
  RAISES {Sx.ReadError, Thread.Alerted} =
  BEGIN
    TRY
      IF rm.bqLevel^ = 0 THEN
        RAISE Sx.ReadError ("comma not inside backquote")
      ELSE
        DEC (rm.bqLevel^);
        TRY
          IF Rd.GetChar (rd) = '@' THEN
            RETURN RefList.List1 (RefList.List2 (FVRuntime.qCommaAtsign,
                                                 Sx.Read (rd, syntax)))
          ELSE
            Rd.UnGetChar (rd);
            RETURN RefList.List1 (
                     RefList.List2 (FVRuntime.qComma, Sx.Read (rd, syntax)))
          END
        FINALLY
          INC (rm.bqLevel^)
        END
      END
    EXCEPT
    | Rd.Failure => RAISE Sx.ReadError ("Rd.Failure") (* FIXME *)
    | Rd.EndOfFile => RAISE Sx.ReadError ("Premature EOF")
    END
  END ReadComma;

PROCEDURE ReadSharp (<* UNUSED *> rm    : Sx.ReadMacro;
                                  rd    : Rd.T;
                     <* UNUSED *> syntax: Sx.Syntax     ): RefList.T
  RAISES {Sx.ReadError, Thread.Alerted} =
  VAR
    level         := 0;
    c, prev: CHAR;
  BEGIN
    TRY
      c := Rd.GetChar (rd);
      IF c # '|' THEN
        RAISE Sx.ReadError ("Illegal character after #: " & Fmt.Char (c))
      END;
      LOOP
        prev := c;
        c := Rd.GetChar (rd);
        IF c = '#' AND prev = '|' THEN
          IF level = 0 THEN RETURN NIL ELSE DEC (level) END
        ELSIF c = '|' AND prev = '#' THEN
          INC (level)
        END
      END
    EXCEPT
    | Rd.Failure => RAISE Sx.ReadError ("Rd.Failure") (* FIXME *)
    | Rd.EndOfFile => RAISE Sx.ReadError ("Premature EOF")
    END
  END ReadSharp;


VAR
  NoDefault := NEW (REF CARDINAL); (* Any unique ref will do. *)

PROCEDURE Init () =
  VAR b := NEW (REF CARDINAL);
  BEGIN
    (* Use a special syntax table to handle %name, =value, etc. *)
    FVSyntax := Sx.CopySyntax ();
    b^ := 0;

    Sx.SetReadMacro (
      FVSyntax, '=', NEW (ReadMacro, read := ReadEqual, bqLevel := b));
    Sx.SetReadMacro (
      FVSyntax, '%', NEW (ReadMacro, read := ReadPercent, bqLevel := b));
    Sx.SetReadMacro (
      FVSyntax, '\'', NEW (ReadMacro, read := ReadQuote, bqLevel := b));
    Sx.SetReadMacro (
      FVSyntax, '`', NEW (ReadMacro, read := ReadBackquote, bqLevel := b));
    Sx.SetReadMacro (
      FVSyntax, ',', NEW (ReadMacro, read := ReadComma, bqLevel := b));
    Sx.SetReadMacro (
      FVSyntax, '#', NEW (ReadMacro, read := ReadSharp, bqLevel := b));

    InitCompilers ();
    (* InitTypeCodes (); *)
    FOR i := FIRST (VarOps) TO LAST (VarOps) DO
      VarOps [i] := NEW (Op, tc := i, eval := EvalVar)
    END
  END Init;

BEGIN
END Macro.
