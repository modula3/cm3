(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE M3Const;

IMPORT Fmt, M3ID, M3AST, M3Scope, M3Type, Target, TInt, TFloat, TWord, Text;
IMPORT M3SetVal, M3RecVal, M3ArrVal, M3Builtin;
FROM M3AST IMPORT NodeIndex;
(****
IMPORT Stdio, Wr, Thread;
****)

TYPE
  State = RECORD
    env     : ImportOracle;
    ast     : M3AST.T;
    max_loc : CARDINAL;
    loc     : NodeIndex;
    op      : M3AST.OP;
    info    : INTEGER;
    n_ch    : CARDINAL;
    ch      : ARRAY [0..9] OF NodeIndex;
  END;

TYPE
  EvalProc = PROCEDURE (VAR s: State;  VAR(*OUT*) val: T) RAISES {Error};

VAR
  init_done := FALSE;
  eval_procs: ARRAY M3AST.OP OF EvalProc;

PROCEDURE Eval (ast : M3AST.T;
                loc : NodeIndex;
                env : ImportOracle;
     VAR(*OUT*) val : T)
  RAISES {Error} =
  VAR s: State;
  BEGIN
    IF (NOT init_done) THEN Init(); END;
    IF (ast.nodes = NIL) THEN BadAST(); END;
    s.env     := env;
    s.ast     := ast;
    s.max_loc := NUMBER (ast.nodes^);
    EvalX (s, loc, val);
  END Eval;

PROCEDURE EvalX (VAR s: State;  loc: NodeIndex;  VAR val: T)
  RAISES {Error} =
  BEGIN
    IF (loc > s.max_loc) THEN BadAST (); END;
    WITH z = s.ast.nodes [loc] DO
      WITH desc = M3AST.OpMap [z.op] DO
        s.loc  := loc;
        s.op   := z.op;
        s.info := z.info;
        s.n_ch := M3AST.GetChildren (s.ast, loc, s.ch);
(****
Out ("eval ", Fmt.Int (loc), " => ", Fmt.Int (z.op)
       & "  n_ch = " & Fmt.Int (s.n_ch));
****)
        IF (s.n_ch < desc.min_ch) THEN BadAST (); END;
        IF (s.n_ch > desc.max_ch) AND (desc.max_ch # 255) THEN BadAST (); END;
        eval_procs [s.op] (s, val);
      END;
    END;
  END EvalX;

(*********
PROCEDURE Out (a, b, c, d: TEXT := NIL) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  VAR wr := Stdio.stdout;
  BEGIN
    IF (a # NIL) THEN Wr.PutText (wr, a); END;
    IF (b # NIL) THEN Wr.PutText (wr, b); END;
    IF (c # NIL) THEN Wr.PutText (wr, c); END;
    IF (d # NIL) THEN Wr.PutText (wr, d); END;
    Wr.PutText (wr, Wr.EOL);
    Wr.Flush (wr);
  END Out;
*********)

(*------------------------------------------------------------------ types ---*)

PROCEDURE EvalArray (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR
    index := s.ch[0];
    elt   := s.ch[1];
    arr   := NEW (M3Type.Array);
  BEGIN
    val.class := Class.Type;
    val.type  := arr;
    arr.index := EvalType (s, index);
    arr.element := EvalType (s, elt);
  END EvalArray;

PROCEDURE EvalOpenArray (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR
    elt := s.ch[0];
    arr := NEW (M3Type.OpenArray);
  BEGIN
    val.class := Class.Type;
    val.type  := arr;
    arr.element := EvalType (s, elt);
  END EvalOpenArray;

PROCEDURE EvalEnum (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR
    base := s.loc + 1;
    elts := NEW (REF ARRAY OF M3ID.T, s.n_ch);
    enum := NEW (M3Type.Enum);
  BEGIN
    val.class := Class.Type;
    val.type  := enum;
    enum.elements := elts;
    FOR i := 0 TO s.n_ch - 1 DO
      WITH z = s.ast.nodes [base + i] DO
        IF z.op # M3AST.OP_EnumDefn THEN Err ("bad enumerated type"); END;
        elts[i] := z.info;
      END;
    END;
  END EvalEnum;

PROCEDURE EvalNamedType (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR xx: T;
  BEGIN
    val.class := Class.Type;
    val.type  := M3Type.Integer;  (* to prevent disasterous cycles *)
    EvalX (s, s.ch[0], xx);
    IF (xx.class # Class.Type) THEN
      Err ("bad type (class = " & Fmt.Int (ORD (val.class)) & ")" );
    END;
    val.type := xx.type;
  END EvalNamedType;

PROCEDURE EvalPacked (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR
    bits := s.ch[0];
    tipe := s.ch[1];
    pack := NEW (M3Type.Packed);
    n_bits : T;
  BEGIN
    val.class := Class.Type;
    val.type  := pack;
    EvalX (s, bits, n_bits);
    IF (n_bits.class # Class.Integer)
      OR NOT TInt.ToInt (n_bits.int, pack.bits) THEN
      Err ("bad size specified in BITS FOR");
    END;
    pack.element := EvalType (s, tipe);
  END EvalPacked;

PROCEDURE EvalProcType (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR
    ast       := s.ast;
    self      := s.loc;
    info      := s.info;
    n_ch      := s.n_ch;
    n_formals : CARDINAL := 0;
    n_raises  : CARDINAL := 0;
    loc, ch   : NodeIndex;
    cnt       : CARDINAL;
    proc      := NEW (M3Type.Procedure);
  BEGIN
    val.class := Class.Type;
    val.type  := proc;

    (* get the calling convention *)
    IF (info = M3ID.NoID) THEN
      proc.callingConv := Target.DefaultCall;
    ELSE
      proc.callingConv := Target.FindConvention (M3ID.ToText (info));
      IF (proc.callingConv = NIL) THEN
        Err ("unrecognized calling convention: " & M3ID.ToText (info));
      END;
    END;

    (* count the formals *)
    FOR i := 0 TO n_ch - 3 DO
      loc := M3AST.NthChild (ast, self, i);
      cnt := M3AST.NumChildren (ast, loc);
      IF (cnt <= 2) THEN BadAST (); END;
      INC (n_formals, cnt - 2);
    END;
    proc.formals := NEW (REF ARRAY OF M3Type.FormalDesc, n_formals);

    (* accumulate the formals *)
    n_formals := 0;
    FOR i := 0 TO n_ch - 3 DO
      loc := M3AST.NthChild (ast, self, i);
      AddFormals (s, loc, n_formals, proc.formals);
    END;

    (* grab the return type *)
    proc.return := EvalTypeOrEmpty (s, M3AST.NthChild (ast, self, n_ch - 2));

    (* grab the exceptions *)
    loc := M3AST.NthChild (ast, self, n_ch - 1);
    WITH z = ast.nodes [loc] DO
      IF (z.op = M3AST.OP_RaisesAny) THEN
        n_raises := 1;
        proc.raises := NEW (REF ARRAY OF M3Type.ExceptDesc, 1);
        proc.raises[0].ast  := NIL;
        proc.raises[0].decl := 0;
      ELSIF (z.op = M3AST.OP_Raises) THEN
        n_raises := M3AST.NumChildren (ast, loc);
        proc.raises := NEW (REF ARRAY OF M3Type.ExceptDesc, n_raises);
        n_raises := 0;
        FOR i := 0 TO n_raises - 1 DO
          ch := M3AST.NthChild (ast, loc, i);
          AddException (s, ch, n_raises, proc.raises);
        END;
      ELSE
        Err ("bad procedure type");
      END;
    END;
  END EvalProcType;

PROCEDURE AddFormals (VAR s: State;  loc: NodeIndex;  VAR n_formals: CARDINAL;
                      formals: REF ARRAY OF M3Type.FormalDesc)
  RAISES {Error} =
  VAR
    ast     := s.ast;
    n_ids   := M3AST.NumChildren (ast, loc) - 2;
    ftype   : M3Type.T;
    default : T;
    ch      : NodeIndex;
    mode    : M3Type.Mode;
  BEGIN
    (* get the formal's mode *)
    WITH z = ast.nodes [loc] DO
      CASE z.info OF
      | 0 => mode := M3Type.Mode.Value;
      | 1 => mode := M3Type.Mode.Var;
      | 2 => mode := M3Type.Mode.Readonly;
      ELSE Err ("bad formal parameter mode");
      END;
    END;

    (* get the formal type *)
    ch := M3AST.NthChild (ast, loc, n_ids);
    ftype := EvalTypeOrEmpty (s, ch);

    (* get the default value *)
    ch := M3AST.NthChild (ast, loc, n_ids + 1);
    WITH z = ast.nodes [ch] DO
      IF (z.op = M3AST.OP_Empty)
        THEN default.type := M3Type.Integer;
        ELSE EvalX (s, ch, default);
      END;
    END;

    (* fix the missing type if possible *)
    IF (ftype = NIL) THEN ftype := default.type; END;

    FOR i := 0 TO n_ids - 1 DO
      ch := M3AST.NthChild (ast, loc, i);
      WITH z = ast.nodes [ch] DO
        IF (z.op # M3AST.OP_FormalDefn) THEN Err ("bad formal parameter"); END;
        WITH f = formals [n_formals] DO
          f.name := z.info;
          f.type := ftype;
          f.mode := mode;
          (* f.default := ?? *)
        END;
        INC (n_formals);
      END;
    END;
  END AddFormals;

PROCEDURE AddException (VAR s: State;  loc: NodeIndex;  VAR n_raises: CARDINAL;
                        raises: REF ARRAY OF M3Type.ExceptDesc)
  RAISES {Error} =
  VAR val: T;
  BEGIN
    EvalX (s, loc, val);
    IF (val.class # Class.Exception) THEN
      Err ("bad exception in RAISES clause");
    END;
    WITH z = raises [n_raises] DO
      z.ast  := val.ref;
      z.decl := val.info;
    END;
    INC (n_raises);
  END AddException;

PROCEDURE EvalObject (<*UNUSED*> VAR s: State;  VAR val: T)
  RAISES {Error} =
  BEGIN
    val.class := Class.Type;
    val.type  := NEW (M3Type.Object);
    Err ("object types not implemented yet");
  END EvalObject;

PROCEDURE EvalRecord (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR
    ast       := s.ast;
    self      := s.loc;
    n_ch      := s.n_ch;
    n_fields  : CARDINAL := 0;
    loc       : NodeIndex;
    rec       := NEW (M3Type.Record);
  BEGIN
    val.class := Class.Type;
    val.type  := rec;

    (* count the fields *)
    FOR i := 0 TO n_ch - 1 DO
      loc := M3AST.NthChild (ast, self, i);
      INC (n_fields, M3AST.NumChildren (ast, loc) - 2);
    END;
    rec.fields := NEW (REF ARRAY OF M3Type.FieldDesc, n_fields);

    (* accumulate the fields *)
    n_fields := 0;
    FOR i := 0 TO n_ch - 1 DO
      loc := M3AST.NthChild (ast, self, i);
      AddFields (s, loc, n_fields, rec.fields);
    END;
  END EvalRecord;

PROCEDURE AddFields (VAR s: State;  loc: NodeIndex;  VAR n_fields: CARDINAL;
                      fields: REF ARRAY OF M3Type.FieldDesc)
  RAISES {Error} =
  VAR
    ast     := s.ast;
    n_ch    := M3AST.NumChildren (ast, loc);
    ftype   : M3Type.T;
    default : T;
    ch      : NodeIndex;
  BEGIN
    (* get the field type *)
    ch := M3AST.NthChild (ast, loc, n_ch - 2);
    ftype := EvalTypeOrEmpty (s, ch);

    (* get the default value *)
    ch := M3AST.NthChild (ast, loc, n_ch - 1);
    WITH z = ast.nodes [ch] DO
      IF (z.op = M3AST.OP_Empty)
        THEN default.type := M3Type.Integer;
        ELSE EvalX (s, ch, default);
      END;
    END;

    (* fix the missing type if possible *)
    IF (ftype = NIL) THEN ftype := default.type; END;

    FOR i := 0 TO n_ch - 3 DO
      ch := M3AST.NthChild (ast, loc, i);
      WITH z = ast.nodes [ch] DO
        IF (z.op # M3AST.OP_FieldDefn) THEN Err ("bad field name"); END;
        WITH f = fields [n_fields] DO
          f.name := z.info;
          f.type := ftype;
          (* f.default := ?? *)
        END;
        INC (n_fields);
      END;
    END;
  END AddFields;

PROCEDURE EvalRef (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR
    brand  := s.ch[0];
    target := s.ch[1];
    ref    := NEW (M3Type.Ref, traced := TRUE);
  BEGIN
    val.class := Class.Type;
    val.type  := ref;
    ref.brand  := GetBrand (s, brand);
    ref.target := EvalType (s, target);
  END EvalRef;

PROCEDURE EvalRoot (<*UNUSED*> VAR s: State;  VAR val: T) =
  BEGIN
    val.class := Class.Type;
    val.type  := M3Type.Root;
  END EvalRoot;

PROCEDURE EvalSet (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR
    dom := s.ch[0];
    set := NEW (M3Type.Set);
  BEGIN
    val.class := Class.Type;
    val.type  := set;
    set.domain := EvalType (s, dom);
  END EvalSet;

PROCEDURE EvalSubrange (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR
    min, max : T;
    subrange := NEW (M3Type.Subrange);
  BEGIN
    val.class := Class.Type;
    val.type  := subrange;
    EvalPair (s, min, max);
    subrange.min   := min.int;
    subrange.max   := max.int;
    subrange.super := min.type;
  END EvalSubrange;

PROCEDURE EvalUntracedRef (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR
    brand  := s.ch[0];
    target := s.ch[1];
    ref    := NEW (M3Type.Ref, traced := FALSE);
  BEGIN
    val.class := Class.Type;
    val.type  := ref;
    ref.brand  := GetBrand (s, brand);
    ref.target := EvalType (s, target);
  END EvalUntracedRef;

PROCEDURE EvalUntracedRoot (<*UNUSED*> VAR s: State;  VAR val: T) =
  BEGIN
    val.class := Class.Type;
    val.type  := M3Type.UntracedRoot;
  END EvalUntracedRoot;

PROCEDURE GetBrand (VAR s: State;  loc: NodeIndex): TEXT
  RAISES {Error} =
  VAR op := s.ast.nodes[loc].op;  val: T;
  BEGIN
    IF (op = M3AST.OP_NoBrand) THEN
      RETURN NIL;
    ELSIF (op = M3AST.OP_DefaultBrand) THEN
      RETURN NewBrand ();
    ELSE
      EvalX (s, loc, val);
      IF (val.class # Class.Text) THEN
        Err ("brand is not a TEXT constant");
      END;
      RETURN NARROW (val.ref, TEXT);
    END;
  END GetBrand;

PROCEDURE NewBrand (): TEXT =
  BEGIN
    RETURN "oops";
  END NewBrand;

PROCEDURE EvalTypeOrEmpty (VAR s: State;  loc: NodeIndex): M3Type.T
  RAISES {Error} =
  BEGIN
    WITH z = s.ast.nodes [loc] DO
      IF (z.op = M3AST.OP_Empty)
        THEN RETURN NIL;
        ELSE RETURN EvalType (s, loc);
      END;
    END;
  END EvalTypeOrEmpty;

PROCEDURE EvalType (VAR s: State;  loc: NodeIndex): M3Type.T
  RAISES {Error} =
  VAR val: T;
  BEGIN
    EvalX (s, loc, val);
    IF (val.class # Class.Type) THEN Err ("not a type"); END;
    RETURN val.type;
  END EvalType;

(*---------------------------------------------------- expression operators ---*)

PROCEDURE EvalOr (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR a, b: T;
  BEGIN
    EvalPair (s, a, b);
    IF (a.class # Class.Enum)
      OR (b.class # Class.Enum)
      OR (M3Type.Base (a.type) # M3Type.Boolean)
      OR (M3Type.Base (b.type) # M3Type.Boolean) THEN
      Err ("bad operand for OR");
    END;
    val.class := Class.Enum;
    val.type  := M3Type.Boolean;
    val.info  := MAX (a.info, b.info);
  END EvalOr;

PROCEDURE EvalAnd (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR a, b: T;
  BEGIN
    EvalPair (s, a, b);
    IF (a.class # Class.Enum)
      OR (b.class # Class.Enum)
      OR (M3Type.Base (a.type) # M3Type.Boolean)
      OR (M3Type.Base (b.type) # M3Type.Boolean) THEN
      Err ("bad operand for AND");
    END;
    val.class := Class.Enum;
    val.type  := M3Type.Boolean;
    val.info  := MIN (a.info, b.info);
  END EvalAnd;

PROCEDURE EvalNot (VAR s: State;  VAR val: T)
  RAISES {Error} =
  BEGIN
    EvalX (s, s.ch[0], val);
    IF (val.class # Class.Enum)
      OR (M3Type.Base (val.type) # M3Type.Boolean) THEN
      Err ("bad operand for NOT");
    END;
    val.class := Class.Enum;
    val.type  := M3Type.Boolean;
    val.info  := 1 - val.info;
  END EvalNot;

PROCEDURE EvalEQ (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR a, b: T;
  BEGIN
    EvalPair (s, a, b);
    val.class := Class.Enum;
    val.type  := M3Type.Boolean;
    val.info  := ORD (IsEQ (a, b));
  END EvalEQ;

PROCEDURE EvalNE (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR a, b: T;
  BEGIN
    EvalPair (s, a, b);
    val.class := Class.Enum;
    val.type  := M3Type.Boolean;
    val.info  := ORD (NOT IsEQ (a, b));
  END EvalNE;

PROCEDURE IsEQ (READONLY a, b: T): BOOLEAN =
  VAR eq: BOOLEAN;
  BEGIN
    IF (a.class # b.class) THEN RETURN FALSE; END;
    CASE a.class OF
    | Class.Integer    => eq := TInt.EQ (a.int, b.int);
    | Class.Float      => eq := TFloat.EQ (a.float, b.float);
    | Class.Enum       => eq := (a.info = b.info)
                                AND M3Type.IsEqual (a.type, b.type);
    | Class.Text       => eq := (a.ref # NIL) AND (b.ref # NIL)
                                AND Text.Equal (a.ref, b.ref);
    | Class.Type       => eq := M3Type.IsEqual (a.type, b.type);
    | Class.Addr       => eq := (a.info = b.info);
    | Class.Set        => eq := M3SetVal.Compare (a.ref, b.ref) = 0;
    | Class.Record     => eq := M3RecVal.Compare (a.ref, b.ref) = 0;
    | Class.Array      => eq := M3ArrVal.Compare (a.ref, b.ref) = 0;
    | Class.Exception  => eq := (a.ref = b.ref) AND (a.info = b.info);
    | Class.Proc       => eq := (a.ref = b.ref) AND (a.info = b.info);
    | Class.Var        => eq := (a.ref = b.ref) AND (a.info = b.info);
    | Class.GenericArg => eq := (a.ref = b.ref) AND (a.info = b.info);
    | Class.Formal     => eq := (a.ref = b.ref) AND (a.info = b.info);
    | Class.Module     => eq := (a.ref = b.ref) AND (a.info = b.info);
    | Class.Builtin    => eq := (a.info = b.info);
    END;
    RETURN eq;
  END IsEQ;

PROCEDURE EvalLT (VAR s: State;  VAR val: T)
  RAISES {Error} =
  BEGIN
    EvalCompare (s, val, -1, -1);
  END EvalLT;

PROCEDURE EvalLE (VAR s: State;  VAR val: T)
  RAISES {Error} =
  BEGIN
    EvalCompare (s, val, -1, 0);
  END EvalLE;

PROCEDURE EvalGT (VAR s: State;  VAR val: T)
  RAISES {Error} =
  BEGIN
    EvalCompare (s, val, +1, +1);
  END EvalGT;

PROCEDURE EvalGE (VAR s: State;  VAR val: T)
  RAISES {Error} =
  BEGIN
    EvalCompare (s, val, +1, 0);
  END EvalGE;

PROCEDURE EvalCompare (VAR s: State;  VAR val: T;  s1, s2: INTEGER)
  RAISES {Error} =
  VAR a, b: T;  sign: INTEGER := 0;
  BEGIN
    EvalPair (s, a, b);

    IF (a.class = Class.Integer) AND (b.class = Class.Integer) THEN
      IF    TInt.LT (a.int, b.int) THEN sign := -1;
      ELSIF TInt.LT (b.int, a.int) THEN sign := +1;
      ELSE                              sign :=  0;
      END;

    ELSIF (a.class = Class.Enum) AND (b.class = Class.Enum) THEN
      IF    a.info < b.info THEN sign := -1;
      ELSIF b.info < a.info THEN sign := +1;
      ELSE                       sign :=  0;
      END;

    ELSIF (a.class = Class.Float) AND (b.class = Class.Float)
      AND (a.float.pre = b.float.pre) THEN
      IF    TFloat.LT (a.float, b.float) THEN sign := -1;
      ELSIF TFloat.LT (b.float, a.float) THEN sign := +1;
      ELSE                                    sign :=  0;
      END;

    ELSIF (a.class = Class.Addr) AND (b.class = Class.Addr) THEN
      IF    a.info < b.info THEN sign := -1;
      ELSIF b.info < a.info THEN sign := +1;
      ELSE                       sign :=  0;
      END;

    ELSIF (a.class = Class.Set) AND (b.class = Class.Set) THEN
      sign := M3SetVal.Compare (a.ref, b.ref);

    ELSE
      Err ("bad operand for comparison");
    END;

    val.class := Class.Enum;
    val.type  := M3Type.Boolean;
    val.info  := ORD ((sign = s1) OR (sign = s2));
  END EvalCompare;


PROCEDURE EvalMember (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR a, b: T;
  BEGIN
    EvalPair (s, a, b);
    IF (b.class # Class.Set) THEN
      Err ("bad operand for IN");
    ELSIF (a.class = Class.Integer) AND TInt.ToInt (a.int, a.info) THEN
      (* ok *)
    ELSIF (a.class = Class.Enum) THEN
      (* ok *)
    ELSE
      Err ("bad operand for IN");
    END;
    val.class := Class.Enum;
    val.type  := M3Type.Boolean;
    val.info  := ORD (M3SetVal.IsMember (b.ref, a.info));
  END EvalMember;

PROCEDURE EvalAdd (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR a, b: T;
  BEGIN
    EvalPair (s, a, b);

    IF (a.class = Class.Integer) AND (b.class = Class.Integer)
      AND (a.type = b.type)
      AND TInt.Add (a.int, b.int, val.int) THEN
      val.class := Class.Integer;
      val.type  := a.type;

    ELSIF (a.class = Class.Float) AND (b.class = Class.Float)
      AND (a.float.pre = b.float.pre)
      AND TFloat.Add (a.float, b.float, val.float) THEN
      val.class := Class.Float;
      val.type  := a.type;

    ELSIF (a.class = Class.Addr) AND (b.class = Class.Integer) THEN
      TWord.Add (a.int, b.int, val.int);
      val.class := Class.Addr;
      val.type  := M3Type.Address;

    ELSIF (a.class = Class.Set) AND (b.class = Class.Set) THEN
      val.class := Class.Set;
      val.ref   := M3SetVal.Union (a.ref, b.ref);
      val.type  := a.type;
    ELSE
      Err ("bad operand for '+'");
    END;
  END EvalAdd;

PROCEDURE EvalSubtract (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR a, b: T;
  BEGIN
    EvalPair (s, a, b);

    IF (a.class = Class.Integer) AND (b.class = Class.Integer)
      AND (a.type = b.type)
      AND TInt.Subtract (a.int, b.int, val.int) THEN
      val.class := Class.Integer;
      val.type  := a.type;

    ELSIF (a.class = Class.Float) AND (b.class = Class.Float)
      AND (a.float.pre = b.float.pre)
      AND TFloat.Subtract (a.float, b.float, val.float) THEN
      val.class := Class.Float;
      val.type  := a.type;

    ELSIF (a.class = Class.Addr) AND (b.class = Class.Integer) THEN
      TWord.Subtract (a.int, b.int, val.int);
      val.class := Class.Addr;
      val.type  := M3Type.Address;

    ELSIF (a.class = Class.Set) AND (b.class = Class.Set) THEN
      val.class := Class.Set;
      val.ref   := M3SetVal.Difference (a.ref, b.ref);
      val.type  := a.type;
    ELSE
      Err ("bad operand for '-'");
    END;
  END EvalSubtract;

PROCEDURE EvalConcat (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR a, b: T;
  BEGIN
    EvalPair (s, a, b);
    IF (a.class = Class.Text) AND (b.class = Class.Text) THEN
      val.class := Class.Text;
      val.type  := M3Type.Txt;
      val.ref   := NARROW (a.ref, TEXT) & NARROW (b.ref, TEXT);
    ELSE
      Err ("bad operand for '&'");
    END;
  END EvalConcat;

PROCEDURE EvalMultiply (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR a, b: T;
  BEGIN
    EvalPair (s, a, b);

    IF (a.class = Class.Integer) AND (b.class = Class.Integer)
      AND (a.type = b.type)
      AND TInt.Multiply (a.int, b.int, val.int) THEN
      val.class := Class.Integer;
      val.type  := a.type;

    ELSIF (a.class = Class.Float) AND (b.class = Class.Float)
      AND (a.float.pre = b.float.pre)
      AND TFloat.Multiply (a.float, b.float, val.float) THEN
      val.class := Class.Float;
      val.type  := a.type;

    ELSIF (a.class = Class.Set) AND (b.class = Class.Set) THEN
      val.class := Class.Set;
      val.ref   := M3SetVal.Intersection (a.ref, b.ref);
      val.type  := a.type;
    ELSE
      Err ("bad operand for '*'");
    END;
  END EvalMultiply;

PROCEDURE EvalDivide (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR a, b: T;
  BEGIN
    EvalPair (s, a, b);

    IF (a.class = Class.Float) AND (b.class = Class.Float)
      AND (a.float.pre = b.float.pre)
      AND TFloat.Divide (a.float, b.float, val.float) THEN
      val.class := Class.Float;
      val.type  := a.type;

    ELSIF (a.class = Class.Set) AND (b.class = Class.Set) THEN
      val.class := Class.Set;
      val.ref   := M3SetVal.SymDifference (a.ref, b.ref);
      val.type  := a.type;
    ELSE
      Err ("bad operand for '/'");
    END;
  END EvalDivide;

PROCEDURE EvalDiv (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR a, b: T;
  BEGIN
    EvalPair (s, a, b);

    IF (a.class = Class.Integer) AND (b.class = Class.Integer)
      AND (a.type = b.type)
      AND TInt.Div (a.int, b.int, val.int) THEN
      val.class := Class.Integer;
      val.type  := a.type;
    ELSE
      Err ("bad operand for 'DIV'");
    END;
  END EvalDiv;

PROCEDURE EvalMod (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR a, b: T;
  BEGIN
    EvalPair (s, a, b);

    IF (a.class = Class.Integer) AND (b.class = Class.Integer)
      AND (a.type = b.type)
      AND TInt.Mod (a.int, b.int, val.int) THEN
      val.class := Class.Integer;
      val.type  := a.type;

    ELSIF (a.class = Class.Float) AND (b.class = Class.Float)
      AND (a.float.pre = b.float.pre)
      AND TFloat.Mod (a.float, b.float, val.float) THEN
      val.class := Class.Float;
      val.type  := a.type;

    ELSE
      Err ("bad operand for 'MOD'");
    END;
  END EvalMod;

PROCEDURE EvalUnaryPlus (VAR s: State;  VAR val: T)
  RAISES {Error} =
  BEGIN
    EvalX (s, s.ch[0], val);
  END EvalUnaryPlus;

PROCEDURE EvalUnaryMinus (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR a: T;  zero: Target.Float;
  BEGIN
    EvalX (s, s.ch[0], a);

    IF (a.class = Class.Integer)
      AND TInt.Subtract (TInt.Zero, a.int, val.int) THEN
      val.class := Class.Integer;
      val.type  := a.type;

    ELSIF (a.class = Class.Float) THEN
      IF    (a.float.pre = Target.Precision.Short) THEN zero := TFloat.ZeroR;
      ELSIF (a.float.pre = Target.Precision.Long)  THEN zero := TFloat.ZeroL;
      ELSE                                              zero := TFloat.ZeroX;
      END;
      IF NOT TFloat.Subtract (zero, a.float, val.float) THEN
        Err ("bad operand for unary '-'");
      END;
      val.class := Class.Float;
      val.type  := a.type;

    ELSE
      Err ("bad operand for unary '-'");
    END;
  END EvalUnaryMinus;

PROCEDURE EvalSubscript (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR
    a, b: T;
    min_index, max_index, offs: Target.Int;
    index: INTEGER;
  BEGIN
    EvalPair (s, a, b);
    IF (a.class # Class.Array) THEN
      Err ("bad operand for subscript operation");
    ELSIF (b.class = Class.Integer) THEN
      (* ok *)
    ELSIF (b.class = Class.Enum) AND TInt.FromInt (b.info, b.int) THEN
      (* ok *)
    ELSE
      Err ("bad operand for subscript operation");
    END;

    TYPECASE a.type OF
    | M3Type.Array (x) =>
        IF NOT M3Type.GetBounds (x.index, min_index, max_index) THEN
          Err ("bad operand for subscript operation");
        END;
    | M3Type.OpenArray =>
        min_index := TInt.Zero;
    ELSE
      Err ("bad operand for subscript operation");
    END;

    IF NOT TInt.Subtract (b.int, min_index, offs)
      OR NOT TInt.ToInt (offs, index) OR (index < 0)
      OR NOT M3ArrVal.Index (a.ref, index, val) THEN
      Err ("bad operand for subscript operation");
    END;
  END EvalSubscript;

PROCEDURE EvalCallExpr (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR
    ast  := s.ast;
    loc  := s.loc;
    n_ch := s.n_ch;
    ch0  := s.ch[0];
    ch   : NodeIndex;
    proc : T;
    args : ARRAY [0..4] OF T;
  BEGIN
    EvalX (s, ch0, proc);
    IF (proc.class # Class.Builtin) THEN Err ("not a constant") END;
    FOR i := 1 TO n_ch-1 DO
      ch := M3AST.NthChild (ast, loc, i);
      EvalX (s, ch, args[i-1]);
    END;
    M3Builtin.Eval (VAL (proc.info, M3Builtin.Proc),
                    SUBARRAY (args, 0, n_ch-1), val);
  END EvalCallExpr;

PROCEDURE EvalConsExpr (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR
    self := s.loc;
    tipe := EvalType (s, s.ch[0]);
  BEGIN
    IF (tipe = NIL) THEN
      Err ("bad type on constructor");
    END;
    TYPECASE M3Type.Base (tipe) OF
    | NULL =>
        Err ("bad type on constructor");
    | M3Type.Array(array_type) =>
        EvalArrayCons (s, array_type, self, val);
    | M3Type.OpenArray =>
        EvalOpenArrayCons (s, self, val);
    | M3Type.Set(set_type) =>
        EvalSetCons (s, set_type, self, val);
    | M3Type.Record(record_type) =>
        EvalRecordCons (s, record_type, self, val);
    ELSE
        Err ("bad type on constructor");
    END;
    val.type := tipe;
  END EvalConsExpr;

PROCEDURE EvalArrayCons (VAR s: State;  tipe: M3Type.Array;
                         loc: NodeIndex;  VAR val: T)
  RAISES {Error} =
  VAR
    ast    := s.ast;
    n_ch   := M3AST.NumChildren (ast, loc);
    arr    : M3ArrVal.T;
    ch     : NodeIndex;
    elt    : T;
    n_elts : INTEGER;
    dots   : BOOLEAN;
  BEGIN
    IF NOT TInt.ToInt (M3Type.Number (tipe.index), n_elts) OR (n_elts < 0) THEN
      Err ("bad array constructor");
    END;
    arr := M3ArrVal.NewEmpty (n_elts);

    (* check for a trailing ".." element *)
    ch := M3AST.NthChild (ast, loc, n_ch-1);
    IF (ast.nodes[ch].op = M3AST.OP_Etc) THEN
      dots := TRUE;
      DEC (n_ch);
      IF (n_ch < 2) THEN Err ("bad array constructor"); END;
      IF (n_ch > n_elts+1) THEN Err ("bad array constructor"); END;
    ELSE
      IF (n_ch # n_elts+1) THEN Err ("bad array constructor"); END;
      dots := FALSE;
    END;

    (* get the explicit elements *)
    FOR i := 1 TO n_ch-1 DO
      ch := M3AST.NthChild (ast, loc, i);
      EvalX (s, ch, elt);
      IF NOT M3ArrVal.Set (arr, i-1, elt) THEN
        Err ("illegal array constructor");
      END;
    END;

    (* fill in the ones implied by ".." *)
    FOR i := n_ch TO n_elts-1 DO
      IF NOT M3ArrVal.Set (arr, i-1, elt) THEN
        Err ("illegal array constructor");
      END;
    END;

    val.class := Class.Array;
    val.ref   := arr;
  END EvalArrayCons;

PROCEDURE EvalOpenArrayCons (VAR s: State;  loc: NodeIndex;  VAR val: T)
  RAISES {Error} =
  VAR
    ast    := s.ast;
    n_ch   := M3AST.NumChildren (ast, loc);
    arr    : M3ArrVal.T;
    ch     : NodeIndex;
    elt    : T;
  BEGIN
    (* check for a trailing ".." element *)
    ch := M3AST.NthChild (ast, loc, n_ch-1);
    IF (ast.nodes[ch].op = M3AST.OP_Etc) THEN
      DEC (n_ch);
      IF (n_ch < 2) THEN Err ("bad open array constructor"); END;
    END;

    arr := M3ArrVal.NewEmpty (n_ch-1);

    (* get the explicit elements *)
    FOR i := 1 TO n_ch-1 DO
      ch := M3AST.NthChild (ast, loc, i);
      EvalX (s, ch, elt);
      IF NOT M3ArrVal.Set (arr, i-1, elt) THEN
        Err ("illegal array constructor");
      END;
    END;

    val.class := Class.Array;
    val.ref   := arr;
  END EvalOpenArrayCons;

PROCEDURE EvalSetCons (VAR s: State;  tipe: M3Type.Set;
                       loc: NodeIndex;  VAR val: T)
  RAISES {Error} =
  VAR
    ast    := s.ast;
    n_ch   := M3AST.NumChildren (ast, loc);
    min, max, t0, t1: Target.Int;
    n_elts : INTEGER;
    set    : M3SetVal.T;
    ch     : NodeIndex;
    v1, v2 : T;
    x1, x2 : INTEGER;
  BEGIN
    IF NOT M3Type.GetBounds (tipe.domain, min, max)
      OR NOT TInt.Subtract (max, min, t0)
      OR NOT TInt.Add (t0, TInt.One, t1)
      OR NOT TInt.ToInt (t1, n_elts) THEN
      Err ("illegal set constructor");
    END;
    set := M3SetVal.NewEmpty (n_elts);

    FOR i := 2 TO n_ch - 1 DO
      ch := M3AST.NthChild (ast, loc, i);
      IF (ast.nodes[ch].op = M3AST.OP_RangeExpr) THEN
        s.n_ch := M3AST.GetChildren (s.ast, ch, s.ch);
        EvalPair (s, v1, v2);
      ELSE
        EvalX (s, ch, v1);
        v2 := v1;
      END;

      IF v1.class = Class.Integer AND TInt.ToInt (v1.int, x1) THEN (* ok *)
      ELSIF v1.class = Class.Enum THEN  x1 := v1.info;
      ELSE  Err ("illegal set constructor element");
      END;

      IF v2.class = Class.Integer AND TInt.ToInt (v2.int, x2) THEN (* ok *)
      ELSIF v2.class = Class.Enum THEN  x2 := v2.info;
      ELSE  Err ("illegal set constructor element");
      END;

      FOR z := x1 TO x2 DO
        set := M3SetVal.Include (set, z);
      END;
    END;

    val.class := Class.Set;
    val.ref   := set;
  END EvalSetCons;

PROCEDURE EvalRecordCons (VAR s: State;  tipe: M3Type.Record;
                          loc: NodeIndex;  VAR val: T)
  RAISES {Error} =
  VAR
    ast        := s.ast;
    n_ch       := M3AST.NumChildren (ast, loc);
    rec        := M3RecVal.NewEmpty ();
    next_field := 0;
    by_name    := FALSE;
    field_name := M3ID.NoID;
    v1         : T;
    ch         : NodeIndex;
  BEGIN
    FOR i := 1 TO n_ch-1 DO
      ch := M3AST.NthChild (ast, loc, i);
      IF (ast.nodes[ch].op = M3AST.OP_NameBind) THEN
        s.n_ch := M3AST.GetChildren (ast, ch, s.ch);
        WITH z = ast.nodes[s.ch[0]] DO
          IF z.op # M3AST.OP_Id THEN Err ("illegal record constructor"); END;
          field_name := z.info;
        END;
        EvalX (s, s.ch[1], v1);
        by_name := TRUE;
      ELSIF (by_name) OR (next_field >= NUMBER (tipe.fields^)) THEN
        Err ("illegal record constructor");
      ELSE
        field_name := tipe.fields [next_field].name;
        EvalX (s, ch, v1);
      END;
      rec := M3RecVal.SetField (rec, field_name, v1);
    END;

    val.class := Class.Record;
    val.ref   := rec;
  END EvalRecordCons;

PROCEDURE EvalQualify (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR id := s.info;  ast := s.ast;
  BEGIN
    EvalX (s, s.ch[0], val);
    CASE val.class OF
    | Class.Module =>
        IF ResolveID (val.ref, 0, id, s.env, val) THEN
          s.ast := ast;
          RETURN;
        ELSE
          s.ast := ast;
        END;
    | Class.Record =>
        IF M3RecVal.Qualify (val.ref, id, val) THEN RETURN; END;
    | Class.Type, Class.Var, Class.GenericArg, Class.Formal =>
        (* nope, not handled yet... *)
    | Class.Integer, Class.Float, Class.Enum, Class.Text,
      Class.Addr, Class.Set, Class.Array, Class.Exception,
      Class.Proc, Class.Builtin =>
        (* nope, illegal *)
    END;
    Err ("unknown qualification: " & M3ID.ToText (id));
  END EvalQualify;

PROCEDURE EvalPair (VAR s: State;  VAR v1, v2: T)
  RAISES {Error} =
  VAR ch0 := s.ch[0];  ch1 := s.ch[1];
  BEGIN
    EvalX (s, ch0, v1);
    EvalX (s, ch1, v2);
  END EvalPair;

(*----------------------------------------------------------- literals ---*)

PROCEDURE EvalId (VAR s: State;  VAR val: T)
  RAISES {Error} =
  VAR id := s.info;
  BEGIN
    IF FindBuiltin (id, val) THEN
(****
IF (val.class = Class.Builtin) THEN
Out ("  ", M3ID.ToText (id), " => builtin #", Fmt.Int (val.info));
ELSIF (val.class = Class.Type) THEN
Out ("  ", M3ID.ToText (id), " => builtin type");
ELSE
Out ("  ", M3ID.ToText (id), " => builtin ??? **** ");
END;
****)
      (* ok, we got it... *)
    ELSIF ResolveID (s.ast, s.loc, id, s.env, val) THEN
      (* ok, we got it *)
    ELSE
      Err ("undefined symbol: " & M3ID.ToText (id));
    END;
  END EvalId;

PROCEDURE EvalInt (VAR s: State;  VAR val: T)
  RAISES {Error} =
  BEGIN
    val.class := Class.Integer;
    val.type  := M3Type.Integer;
    IF NOT TInt.FromInt (s.info, val.int) THEN
      Err ("illegal integer value");
    END;
  END EvalInt;

PROCEDURE EvalLInt (VAR s: State;  VAR val: T)
  RAISES {Error} =
  BEGIN
    val.class := Class.Integer;
    val.type  := M3Type.Longint;
    IF NOT TInt.FromInt (s.info, val.int) THEN
      Err ("illegal integer value");
    END;
  END EvalLInt;

PROCEDURE EvalBigInt (VAR s: State;  VAR val: T) =
  BEGIN
    val.class := Class.Integer;
    val.type  := M3Type.Integer;
    val.int   := s.ast.ints [s.info];
  END EvalBigInt;

PROCEDURE EvalBigLInt (VAR s: State;  VAR val: T) =
  BEGIN
    val.class := Class.Integer;
    val.type  := M3Type.Longint;
    val.int   := s.ast.ints [s.info];
  END EvalBigLInt;

PROCEDURE EvalReal (VAR s: State;  VAR val: T) =
  BEGIN
    val.class := Class.Float;
    val.type  := M3Type.Real;
    val.float := s.ast.floats [s.info];
  END EvalReal;

PROCEDURE EvalLReal (VAR s: State;  VAR val: T) =
  BEGIN
    val.class := Class.Float;
    val.type  := M3Type.LongReal;
    val.float := s.ast.floats [s.info];
  END EvalLReal;

PROCEDURE EvalEReal (VAR s: State;  VAR val: T) =
  BEGIN
    val.class := Class.Float;
    val.type  := M3Type.Extended;
    val.float := s.ast.floats [s.info];
  END EvalEReal;

PROCEDURE EvalChar (VAR s: State;  VAR val: T) =
  BEGIN
    val.class := Class.Enum;
    val.type  := M3Type.Char;
    val.info  := s.info;
  END EvalChar;

PROCEDURE EvalText (VAR s: State;  VAR val: T) =
  BEGIN
    val.class := Class.Text;
    val.type  := M3Type.Txt;
    val.ref   := s.ast.texts [s.info];
  END EvalText;

(*------------------------------------------------- user defined identifiers ---*)

TYPE RefConst = REF T;

PROCEDURE ResolveID (ast: M3AST.T;  loc: NodeIndex;
                     id: M3ID.T;  env: ImportOracle;
                     VAR(*OUT*) val: T): BOOLEAN
  RAISES {Error} =
  VAR
    sym  : M3Scope.Defn;
    n_ch : CARDINAL;
    ch   : ARRAY [0..1] OF NodeIndex;
    defn : RefConst;
  BEGIN
    IF FindWordBuiltin (ast, id, val) THEN
(****
Out ("  ", M3ID.ToText (id), " => builtin #", Fmt.Int (val.info));
****)
      RETURN TRUE; END;
    IF NOT M3Scope.LookUp (ast, loc, id, sym) THEN
(****
Out ("*** M3Scope.LookUp failed:  ", M3ID.ToText (id), " @ ", Fmt.Int (loc));
****)
      RETURN FALSE; END;
(****
Out ("  ", M3ID.ToText (id), " => defn @ ", Fmt.Int (sym.loc));
****)

    (* check for a cached evaluation... *)
    TYPECASE sym.info OF
    | NULL =>
        (* nothing defined yet. *)
        defn := NEW (RefConst);
        M3Scope.Define (sym, defn);
    | RefConst (r) =>
        val := r^;
        RETURN TRUE;
    ELSE (* ouch, somebody else is using this slot! *)
        defn := NEW (RefConst);
    END;

    CASE sym.class OF
    | M3Scope.Class.Import =>
        WITH z = sym.ast.nodes [sym.loc] DO
          IF (z.op = M3AST.OP_Import) THEN
            defn.class := Class.Module;
            defn.info  := 0;
            defn.ref   := env.find (z.info);
            IF (defn.ref = NIL) THEN RETURN FALSE; END;
          ELSIF (z.op = M3AST.OP_ImportAs) THEN
            defn.class := Class.Module;
            defn.info  := 0;
            defn.ref   := env.find (sym.ast.nodes[sym.loc+1].info);
            IF (defn.ref = NIL) THEN RETURN FALSE; END;
          ELSIF (z.op = M3AST.OP_FromImport) THEN
            ast := env.find (sym.ast.nodes[sym.loc+1].info);
            IF (ast = NIL) THEN RETURN FALSE; END;
            IF NOT ResolveID (ast, 0, z.info, env, defn^) THEN RETURN FALSE; END;
          ELSE
            RETURN FALSE;
          END;
        END;

    | M3Scope.Class.Const =>
        WITH z = sym.ast.nodes [sym.loc] DO
          IF (z.op # M3AST.OP_ConstDecl) THEN RETURN FALSE; END;
          n_ch := M3AST.GetChildren (sym.ast, sym.loc, ch);
          Eval (sym.ast, ch[1], env, defn^);
        END;

    | M3Scope.Class.Type =>
        n_ch := M3AST.GetChildren (sym.ast, sym.loc, ch);
        WITH z = sym.ast.nodes [sym.loc] DO
          IF (z.op = M3AST.OP_TypeDecl) THEN
            Eval (sym.ast, ch[0], env, defn^);
          ELSIF (z.op = M3AST.OP_OpaqueDecl) THEN
            Eval (sym.ast, ch[0], env, defn^);
            IF (defn.class = Class.Type) THEN
              defn.type := NEW (M3Type.Opaque, super := defn.type);
            END;
          ELSE
            RETURN FALSE;
          END;
        END;
        IF (defn.class # Class.Type) THEN
(****
Out ("***??? Didn't find a type for ", M3ID.ToText (id), " => class ", Fmt.Int(ORD(defn.class)));
****)
          RETURN FALSE; END;

    | M3Scope.Class.Var =>
        defn.class := Class.Var;
        defn.info  := sym.loc;
        defn.ref   := sym.ast;
    | M3Scope.Class.GenericArg =>
        defn.class := Class.GenericArg;
        defn.info  := sym.loc;
        defn.ref   := sym.ast;
    | M3Scope.Class.Formal =>
        defn.class := Class.Var;
        defn.info  := sym.loc;
        defn.ref   := sym.ast;
    | M3Scope.Class.Exception =>
        defn.class := Class.Exception;
        defn.info  := sym.loc;
        defn.ref   := sym.ast;
    | M3Scope.Class.Procedure =>
        defn.class := Class.Proc;
        defn.info  := sym.loc;
        defn.ref   := sym.ast;
    | M3Scope.Class.Module =>
        defn.class := Class.Module;
        defn.info  := sym.loc;
        defn.ref   := sym.ast;
    END;

    val := defn^;
    RETURN TRUE;
  END ResolveID;

(*------------------------------------------- built-in types and procedures ---*)

CONST
  BuiltinNames = ARRAY [0..42] OF TEXT {
    "ABS", "ADDRESS", "ADR", "ADRSIZE", "BITSIZE", "BOOLEAN",
    "BYTESIZE", "CARDINAL", "CEILING", "CHAR", "DEC", "DISPOSE",
    "EXTENDED", "FALSE", "FIRST", "FLOAT", "FLOOR", "INC",
    "INTEGER", "ISTYPE", "LAST", "LONGCARD", "LONGINT", "LONGREAL", "LOOPHOLE",
    "MAX", "MIN", "MUTEX", "NARROW", "NEW", "NIL", "NULL",
    "NUMBER", "ORD", "REAL", "REFANY", "ROUND", "SUBARRAY",
    "TEXT", "TRUE", "TRUNC", "TYPECODE", "VAL"
  };

VAR
  init_builtins := FALSE;
  BuiltinIDs : ARRAY [0..41] OF M3ID.T;

PROCEDURE InitBuiltins () =
  BEGIN
    FOR i := FIRST (BuiltinNames) TO LAST (BuiltinNames) DO
      BuiltinIDs[i] := M3ID.Add (BuiltinNames [i]);
    END;
    init_builtins := TRUE;
  END InitBuiltins;

PROCEDURE FindBuiltin (id: M3ID.T;  VAR(*OUT*) val: T): BOOLEAN =
  BEGIN
    IF (NOT init_builtins) THEN InitBuiltins () END;
    FOR i := FIRST (BuiltinIDs) TO LAST (BuiltinIDs) DO
      IF BuiltinIDs[i] = id THEN
        CASE i OF
        | 00 => (* ABS *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Abs);
                RETURN TRUE;

        | 01 => (* ADDRESS *)
                val.class := Class.Type;
                val.type := M3Type.Address;
                RETURN TRUE;

        | 02 => (* ADR *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Adr);
                RETURN TRUE;

        | 03 => (* ADRSIZE *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.AdrSize);
                RETURN TRUE;

        | 04 => (* BITSIZE *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.BitSize);
                RETURN TRUE;

        | 05 => (* BOOLEAN *)
                val.class := Class.Type;
                val.type := M3Type.Boolean;
                RETURN TRUE;

        | 06 => (* BYTESIZE *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.ByteSize);
                RETURN TRUE;

        | 07 => (* CARDINAL *)
                val.class := Class.Type;
                val.type := M3Type.Cardinal;
                RETURN TRUE;

        | 08 => (* CEILING *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Ceiling);
                RETURN TRUE;

        | 09 => (* CHAR *)
                val.class := Class.Type;
                val.type := M3Type.Char;
                RETURN TRUE;

        | 10 => (* DEC *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Dec);
                RETURN TRUE;

        | 11 => (* DISPOSE *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Dispose);
                RETURN TRUE;

        | 12 => (* EXTENDED *)
                val.class := Class.Type;
                val.type := M3Type.Extended;
                RETURN TRUE;

        | 13 => (* FALSE *)
                val.class := Class.Enum;
                val.info  := ORD (FALSE);
                val.type := M3Type.Boolean;
                RETURN TRUE;

        | 14 => (* FIRST *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.First);
                RETURN TRUE;

        | 15 => (* FLOAT *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Float);
                RETURN TRUE;

        | 16 => (* FLOOR *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Floor);
                RETURN TRUE;

        | 17 => (* INC *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Inc);
                RETURN TRUE;

        | 18 => (* INTEGER *)
                val.class := Class.Type;
                val.type := M3Type.Integer;
                RETURN TRUE;

        | 19 => (* ISTYPE *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.IsType);
                RETURN TRUE;

        | 20 => (* LAST *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Last);
                RETURN TRUE;

        | 21 => (* LONGCARD *)
                val.class := Class.Type;
                val.type := M3Type.Longcard;

        | 22 => (* LONGINT *)
                val.class := Class.Type;
                val.type := M3Type.Longint;

        | 23 => (* LONGREAL *)
                val.class := Class.Type;
                val.type := M3Type.LongReal;
                RETURN TRUE;

        | 24 => (* LOOPHOLE *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Loophole);
                RETURN TRUE;

        | 25 => (* MAX *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Max);
                RETURN TRUE;

        | 26 => (* MIN *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Min);
                RETURN TRUE;

        | 27 => (* MUTEX *)
                val.class := Class.Type;
                val.type := M3Type.Mutex;
                RETURN TRUE;

        | 28 => (* NARROW *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Narrow);
                RETURN TRUE;

        | 29 => (* NEW *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.New);
                RETURN TRUE;

        | 30 => (* NIL *)
                val.class := Class.Addr;
                val.info  := 0;
                val.type  := M3Type.Null;
                RETURN TRUE;

        | 31 => (* NULL *)
                val.class := Class.Type;
                val.type := M3Type.Null;
                RETURN TRUE;

        | 32 => (* NUMBER *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Number);
                RETURN TRUE;

        | 33 => (* ORD *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Ord);
                RETURN TRUE;

        | 34 => (* REAL *)
                val.class := Class.Type;
                val.type := M3Type.Real;
                RETURN TRUE;

        | 35 => (* REFANY *)
                val.class := Class.Type;
                val.type := M3Type.Refany;
                RETURN TRUE;

        | 36 => (* ROUND *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Round);
                RETURN TRUE;

        | 37 => (* SUBARRAY *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Subarray);
                RETURN TRUE;

        | 38 => (* TEXT *)
                val.class := Class.Type;
                val.type := M3Type.Txt;
                RETURN TRUE;

        | 39 => (* TRUE *)
                val.class := Class.Enum;
                val.info  := ORD (TRUE);
                val.type  := M3Type.Boolean;
                RETURN TRUE;

        | 40 => (* TRUNC *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Trunc);
                RETURN TRUE;

        | 41 => (* TYPECODE *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Typecode);
                RETURN TRUE;

        | 42 => (* VAL *)
                val.class := Class.Builtin;
                val.info  := ORD (M3Builtin.Proc.Val);
                RETURN TRUE;

        END; (*CASE*)
      END;
    END;
    RETURN FALSE;
  END FindBuiltin;


CONST
  BuiltinWordNames = ARRAY [0..20] OF TEXT {
    "Plus", "Times", "Minus", "Divide", "Mod", "LT",
    "LE", "GT", "GE", "And", "Or", "Xor", "Not",
    "Shift", "LeftShift", "RightShift", "Rotate",
    "LeftRotate", "RightRotate", "Extract", "Insert"
  };
  BuiltinWordProc = ARRAY [0..20] OF M3Builtin.Proc {
    M3Builtin.Proc.WordPlus, M3Builtin.Proc.WordTimes,
    M3Builtin.Proc.WordMinus, M3Builtin.Proc.WordDivide,
    M3Builtin.Proc.WordMod, M3Builtin.Proc.WordLT,
    M3Builtin.Proc.WordLE, M3Builtin.Proc.WordGT,
    M3Builtin.Proc.WordGE, M3Builtin.Proc.WordAnd,
    M3Builtin.Proc.WordOr, M3Builtin.Proc.WordXor,
    M3Builtin.Proc.WordNot, M3Builtin.Proc.WordShift,
    M3Builtin.Proc.WordLeftShift, M3Builtin.Proc.WordRightShift,
    M3Builtin.Proc.WordRotate, M3Builtin.Proc.WordLeftRotate,
    M3Builtin.Proc.WordRightRotate, M3Builtin.Proc.WordExtract,
    M3Builtin.Proc.WordInsert
  };

VAR
  init_word      := FALSE;
  WordID         : M3ID.T;
  BuiltinWordIDs : ARRAY [0..20] OF M3ID.T;

PROCEDURE InitWordIDs () =
  BEGIN
    WordID := M3ID.Add ("Word");
    FOR i := FIRST (BuiltinWordIDs) TO LAST (BuiltinWordIDs) DO
      BuiltinWordIDs[i] := M3ID.Add (BuiltinWordNames [i]);
    END;
    init_word := TRUE;
  END InitWordIDs;

PROCEDURE FindWordBuiltin (ast: M3AST.T;  id: M3ID.T;
                           VAR(*OUT*) val: T): BOOLEAN =
  BEGIN
    IF (NOT init_word) THEN InitWordIDs (); END;

    IF (ast = NIL) OR (NOT ast.interface) THEN RETURN FALSE; END;
    IF (ast.nodes = NIL) THEN RETURN FALSE; END;
    IF (ast.nodes[0].op # M3AST.OP_Unit) THEN RETURN FALSE END;
    IF (ast.nodes[0].info # WordID) THEN RETURN FALSE; END;

    FOR i := FIRST (BuiltinWordIDs) TO LAST (BuiltinWordIDs) DO
      IF BuiltinWordIDs[i] = id THEN
        val.class := Class.Builtin;
        val.info  := ORD (BuiltinWordProc[i]);
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END FindWordBuiltin;

(*-------------------------------------------------------------- errors ---*)

PROCEDURE NotConst (<*UNUSED*> VAR s: State;  <*UNUSED*> VAR val: T)
  RAISES {Error} =
  BEGIN
    Err ("not a constant");
  END NotConst;

PROCEDURE BadAST () RAISES {Error} =
  BEGIN
    Err ("malformed AST");
  END BadAST;

PROCEDURE Err (msg: TEXT) RAISES {Error} =
  BEGIN
    RAISE Error (msg);
  END Err;

(*------------------------------------------------------- initialization ---*)

PROCEDURE Init () =
  BEGIN
    init_done := TRUE;
    FOR op := FIRST (eval_procs) TO LAST (eval_procs) DO
      eval_procs [op] := NotConst;
    END;

    eval_procs [M3AST.OP_Array]        := EvalArray;
    eval_procs [M3AST.OP_OpenArray]    := EvalOpenArray;
    eval_procs [M3AST.OP_Enum]         := EvalEnum;
    eval_procs [M3AST.OP_NamedType]    := EvalNamedType;
    eval_procs [M3AST.OP_Packed]       := EvalPacked;
    eval_procs [M3AST.OP_ProcType]     := EvalProcType;
    eval_procs [M3AST.OP_Object]       := EvalObject;
    eval_procs [M3AST.OP_Record]       := EvalRecord;
    eval_procs [M3AST.OP_Ref]          := EvalRef;
    eval_procs [M3AST.OP_Root]         := EvalRoot;
    eval_procs [M3AST.OP_Set]          := EvalSet;
    eval_procs [M3AST.OP_Subrange]     := EvalSubrange;
    eval_procs [M3AST.OP_UntracedRef]  := EvalUntracedRef;
    eval_procs [M3AST.OP_UntracedRoot] := EvalUntracedRoot;

    eval_procs [M3AST.OP_Or]           := EvalOr;
    eval_procs [M3AST.OP_And]          := EvalAnd;
    eval_procs [M3AST.OP_Not]          := EvalNot;
    eval_procs [M3AST.OP_EQ]           := EvalEQ;
    eval_procs [M3AST.OP_NE]           := EvalNE;
    eval_procs [M3AST.OP_LT]           := EvalLT;
    eval_procs [M3AST.OP_LE]           := EvalLE;
    eval_procs [M3AST.OP_GT]           := EvalGT;
    eval_procs [M3AST.OP_GE]           := EvalGE;
    eval_procs [M3AST.OP_Member]       := EvalMember;
    eval_procs [M3AST.OP_Add]          := EvalAdd;
    eval_procs [M3AST.OP_Subtract]     := EvalSubtract;
    eval_procs [M3AST.OP_Concat]       := EvalConcat;
    eval_procs [M3AST.OP_Multiply]     := EvalMultiply;
    eval_procs [M3AST.OP_Divide]       := EvalDivide;
    eval_procs [M3AST.OP_Div]          := EvalDiv;
    eval_procs [M3AST.OP_Mod]          := EvalMod;
    eval_procs [M3AST.OP_UnaryPlus]    := EvalUnaryPlus;
    eval_procs [M3AST.OP_UnaryMinus]   := EvalUnaryMinus;
    eval_procs [M3AST.OP_Subscript]    := EvalSubscript;
    eval_procs [M3AST.OP_CallExpr]     := EvalCallExpr;
    eval_procs [M3AST.OP_ConsExpr]     := EvalConsExpr;
    eval_procs [M3AST.OP_Qualify]      := EvalQualify;

    eval_procs [M3AST.OP_Id]           := EvalId;
    eval_procs [M3AST.OP_Int]          := EvalInt;
    eval_procs [M3AST.OP_LInt]         := EvalLInt;
    eval_procs [M3AST.OP_BigInt]       := EvalBigInt;
    eval_procs [M3AST.OP_BigLInt]      := EvalBigLInt;
    eval_procs [M3AST.OP_Real]         := EvalReal;
    eval_procs [M3AST.OP_LReal]        := EvalLReal;
    eval_procs [M3AST.OP_EReal]        := EvalEReal;
    eval_procs [M3AST.OP_Char]         := EvalChar;
    eval_procs [M3AST.OP_Text]         := EvalText;
  END Init;

BEGIN
END M3Const.
