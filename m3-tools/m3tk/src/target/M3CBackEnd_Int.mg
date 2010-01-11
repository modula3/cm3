GENERIC MODULE M3CBackEnd_Int(IntType, WordType);

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT M3AST_AS, M3AST_SM, M3CStdProcs;

IMPORT M3CBackEnd, M3CBackEnd_C, M3CWordProcs;
IMPORT M3CBackEnd_Float_Real, M3CBackEnd_Float_LongReal,
       M3CBackEnd_Float_Extended;
IMPORT M3CBackEnd_Int_Integer AS Integer;

CONST
  False = VAL(ORD(FALSE), IntType.T);
  True = VAL(ORD(TRUE), IntType.T);
  Zero = VAL(0, IntType.T);

VAR
  small := ARRAY [0..255] OF T {NIL, ..};
  first := NEW(T, sm_value := FIRST(IntType.T));
  last := NEW(T, sm_value := LAST(IntType.T));

PROCEDURE New_value(i: IntType.T): T RAISES {} =
  BEGIN
    IF VAL(FIRST(small), IntType.T) <= i
      AND i <= VAL(LAST(small), IntType.T) THEN
      WITH result = small[VAL(i, INTEGER)] DO
        IF result = NIL THEN result := NEW(T, sm_value := i) END;
        RETURN result;
      END;
    ELSIF i = FIRST(IntType.T) THEN RETURN first;
    ELSIF i = LAST(IntType.T)  THEN RETURN last;
    ELSE                            RETURN NEW(T, sm_value := i);
    END;
  END New_value;

PROCEDURE UnaryOp(
    op: M3AST_AS.UNARY;
    e: T;
    VAR (*out*) er: M3AST_SM.Exp_value)
  : M3CBackEnd.NumStatus RAISES {} =
  BEGIN
    TYPECASE op OF
    | M3AST_AS.Unaryplus =>
        er := New_value(e.sm_value);
    | M3AST_AS.Unaryminus =>
        er := New_value(-e.sm_value);
    | M3AST_AS.Not =>
        er := Integer.New_value(ORD(e.sm_value = False));
    ELSE
        RETURN M3CBackEnd.NumStatus.Unknown;
    END; (* case *)
    RETURN M3CBackEnd.NumStatus.Valid;
  END UnaryOp;

PROCEDURE StdUnaryOp(
    f: M3CStdProcs.Func; 
    e: T;
    VAR (*out*) er: M3AST_SM.Exp_value;
    <*UNUSED*> it: M3AST_AS.INT_TYPE := NIL;
    ft: M3AST_AS.FLOAT_TYPE := NIL)
  : M3CBackEnd.NumStatus RAISES {} =
  VAR
    int: IntType.T := e.sm_value;
  BEGIN
    CASE f OF
    | M3CStdProcs.T.Abs =>
        er := New_value(ABS(int));
    | M3CStdProcs.T.Float =>
        TYPECASE ft OF <*NOWARN*>
        | M3AST_AS.Real_type =>
            er := M3CBackEnd_Float_Real.New_value(FLOAT(int, REAL));
        | M3AST_AS.LongReal_type =>
            er := M3CBackEnd_Float_LongReal.New_value(FLOAT(int, LONGREAL));
        | M3AST_AS.Extended_type =>
            er := M3CBackEnd_Float_Extended.New_value(FLOAT(int, EXTENDED));
        END; (* typecase *)
    ELSE
        RETURN M3CBackEnd.NumStatus.Unknown;
    END; (* case *)
    RETURN M3CBackEnd.NumStatus.Valid;
  END StdUnaryOp;

PROCEDURE BinaryOp(
    op: M3AST_AS.BINARY;
    e1, e2: T;
    VAR (*out*) er: M3AST_SM.Exp_value)
  : M3CBackEnd.NumStatus RAISES {} =
  VAR 
    i1 := e1.sm_value;
    i2 := e2.sm_value;
    r: IntType.T;
    bool: BOOLEAN;
  BEGIN
    TYPECASE op OF
    | M3AST_AS.Plus =>  r := i1 + i2;
    | M3AST_AS.Minus => r := i1 - i2;
    | M3AST_AS.Times => r := i1 * i2;
    | M3AST_AS.Div =>
      IF i2 = Zero THEN RETURN M3CBackEnd.NumStatus.Overflow END;
      r := i1 DIV i2;
    | M3AST_AS.Mod =>
      IF i2 = Zero THEN RETURN M3CBackEnd.NumStatus.Overflow END;
      r := i1 MOD i2;
    ELSE
      TYPECASE op OF
      | M3AST_AS.Eq =>  bool := i1 = i2;
      | M3AST_AS.Ne =>  bool := i1 # i2;
      | M3AST_AS.Gt =>  bool := i1 > i2;
      | M3AST_AS.Lt =>  bool := i1 < i2;
      | M3AST_AS.Ge =>  bool := i1 >= i2;
      | M3AST_AS.Le =>  bool := i1 <= i2;
      | M3AST_AS.And => bool := (i1 = True) AND (i2 = True);
      | M3AST_AS.Or =>  bool := (i1 = True) OR (i2 = True);
      ELSE RETURN M3CBackEnd.NumStatus.Unknown;
      END; (* case *)
      er := Integer.New_value(ORD(bool));
      RETURN M3CBackEnd.NumStatus.Valid;
    END;
    er := New_value(r);
    RETURN M3CBackEnd.NumStatus.Valid;
  END BinaryOp;

PROCEDURE StdBinaryOp(f: M3CStdProcs.Func; 
    e1, e2: T;
    VAR (*out*) er: M3AST_SM.Exp_value)
  : M3CBackEnd.NumStatus RAISES {} =
  BEGIN
    CASE f OF <*NOWARN*>
    | M3CStdProcs.T.Min =>
      IF e1.sm_value < e2.sm_value
        THEN er := e1;
        ELSE er := e2;
      END;
    | M3CStdProcs.T.Max =>
      IF e1.sm_value > e2.sm_value
        THEN er := e1;
        ELSE er := e2;
      END;
    END; (* case *)
    RETURN M3CBackEnd.NumStatus.Valid;
  END StdBinaryOp;

PROCEDURE WordOp(
    w: M3CWordProcs.T;
    READONLY args: ARRAY OF M3AST_SM.Exp_value;
    VAR (* out *) er: M3AST_SM.Exp_value)
  : M3CBackEnd.NumStatus RAISES {} =
  VAR
    a0 := NARROW(args[0], T).sm_value;
    r: IntType.T;
    bool: BOOLEAN;
  BEGIN
    CASE w OF
    | M3CWordProcs.T.Not =>  r := WordType.Not(a0);
    | M3CWordProcs.T.Plus =>
      r := WordType.Plus(a0, NARROW(args[1], T).sm_value);
    | M3CWordProcs.T.Times =>
      r := WordType.Times(a0, NARROW(args[1], T).sm_value);
    | M3CWordProcs.T.Minus =>
      r := WordType.Minus(a0, NARROW(args[1], T).sm_value);
    | M3CWordProcs.T.Divide =>
      r := WordType.Divide(a0, NARROW(args[1], T).sm_value);
    | M3CWordProcs.T.Mod =>
      r := WordType.Mod(a0, NARROW(args[1], T).sm_value);
    | M3CWordProcs.T.And =>
      r := WordType.And(a0, NARROW(args[1], T).sm_value);
    | M3CWordProcs.T.Or =>
      r := WordType.Or(a0, NARROW(args[1], T).sm_value);
    | M3CWordProcs.T.Xor =>
      r := WordType.Xor(a0, NARROW(args[1], T).sm_value);
    | M3CWordProcs.T.Shift =>
      r := WordType.Shift(a0, NARROW(args[1], Integer.T).sm_value);
    | M3CWordProcs.T.RightShift =>
      r := WordType.RightShift(a0, NARROW(args[1], Integer.T).sm_value);
    | M3CWordProcs.T.Rotate =>
      r := WordType.Rotate(a0, NARROW(args[1], Integer.T).sm_value);
    | M3CWordProcs.T.RightRotate =>
      r := WordType.RightRotate(a0, NARROW(args[1], Integer.T).sm_value);
    | M3CWordProcs.T.Extract =>
      WITH a1 = NARROW(args[1], Integer.T).sm_value,
           a2 = NARROW(args[2], Integer.T).sm_value DO
        IF a1 < 0 OR a2 < 0 OR a1 + a2 > WordType.Size THEN
          RETURN M3CBackEnd.NumStatus.Unknown;
        END;
        r := WordType.Extract(a0, a1, a2);
      END;
    | M3CWordProcs.T.Insert =>
      WITH a1 = NARROW(args[1], T).sm_value,
           a2 = NARROW(args[2], Integer.T).sm_value,
           a3 = NARROW(args[3], Integer.T).sm_value DO
        IF a2 < 0 OR a3 < 0 OR a2 + a3 > WordType.Size THEN
          RETURN M3CBackEnd.NumStatus.Unknown;
        END;
        r := WordType.Insert(a0, a1, a2, a3);
      END;
    ELSE
      CASE w OF
      | M3CWordProcs.T.LT =>
        bool := WordType.LT(a0, NARROW(args[1], T).sm_value);
      | M3CWordProcs.T.LE =>
        bool := WordType.LE(a0, NARROW(args[1], T).sm_value);
      | M3CWordProcs.T.GT =>
        bool := WordType.GT(a0, NARROW(args[1], T).sm_value);
      | M3CWordProcs.T.GE =>
        bool := WordType.GE(a0, NARROW(args[1], T).sm_value);
      ELSE RETURN M3CBackEnd.NumStatus.Unknown;
      END;
      er := Integer.New_value(ORD(bool));
      RETURN M3CBackEnd.NumStatus.Valid;
    END;
    er := New_value(r);
    RETURN M3CBackEnd.NumStatus.Valid;
  END WordOp;

BEGIN
END M3CBackEnd_Int.
