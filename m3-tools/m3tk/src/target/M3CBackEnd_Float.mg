GENERIC MODULE M3CBackEnd_Float(FloatType);

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT M3AST_AS, M3AST_SM, M3CStdProcs;

IMPORT M3CBackEnd;
IMPORT M3CBackEnd_Int_Integer, M3CBackEnd_Int_Longint;
IMPORT M3CBackEnd_Float_Real, M3CBackEnd_Float_LongReal,
       M3CBackEnd_Float_Extended;

PROCEDURE New_value (r: FloatType.T): T RAISES {}=
  BEGIN
    RETURN NEW(T, sm_value := r);
  END New_value;

PROCEDURE UnaryOp (
    op: M3AST_AS.UNARY;
    e: T;
    VAR (*out*) er: M3AST_SM.Exp_value)
  : M3CBackEnd.NumStatus RAISES {}=
  VAR r: FloatType.T;
  BEGIN
    TYPECASE op OF
    | M3AST_AS.Unaryplus =>
	r := e.sm_value;
    | M3AST_AS.Unaryminus =>
	r := -e.sm_value;
    ELSE
        RETURN M3CBackEnd.NumStatus.Unknown;
    END; (* case *)
    er := New_value(r);
    RETURN M3CBackEnd.NumStatus.Valid;
  END UnaryOp;

PROCEDURE StdUnaryOp (
    f: M3CStdProcs.Func; 
    e: T;
    VAR (*out*) er: M3AST_SM.Exp_value;
    it: M3AST_AS.INT_TYPE := NIL;
    ft: M3AST_AS.FLOAT_TYPE := NIL)
  : M3CBackEnd.NumStatus RAISES {}=
  VAR
    float: FloatType.T := e.sm_value;
  BEGIN
    CASE f OF
    | M3CStdProcs.T.Abs =>
        er := New_value(ABS(float));
    | M3CStdProcs.T.Float =>
        TYPECASE ft OF <*NOWARN*>
        | M3AST_AS.Real_type =>
  	    er := M3CBackEnd_Float_Real.New_value(FLOAT(float, REAL));
        | M3AST_AS.LongReal_type =>
            er := M3CBackEnd_Float_LongReal.New_value(FLOAT(float, LONGREAL));
        | M3AST_AS.Extended_type =>
            er := M3CBackEnd_Float_Extended.New_value(FLOAT(float, EXTENDED));
        END; (* typecase *)

    | M3CStdProcs.T.Floor =>
        TYPECASE it OF <*NOWARN*>
        | M3AST_AS.Integer_type =>
            er := M3CBackEnd_Int_Integer.New_value(FLOOR(float, INTEGER));
        | M3AST_AS.Longint_type =>
            er := M3CBackEnd_Int_Longint.New_value(FLOOR(float, LONGINT));
        END; (* typecase *)
    | M3CStdProcs.T.Ceiling =>
        TYPECASE it OF <*NOWARN*>
        | M3AST_AS.Integer_type =>
            er := M3CBackEnd_Int_Integer.New_value(CEILING(float, INTEGER));
        | M3AST_AS.Longint_type =>
            er := M3CBackEnd_Int_Longint.New_value(CEILING(float, LONGINT));
        END; (* typecase *)
    | M3CStdProcs.T.Round =>
        TYPECASE it OF <*NOWARN*>
        | M3AST_AS.Integer_type =>
            er := M3CBackEnd_Int_Integer.New_value(ROUND(float, INTEGER));
        | M3AST_AS.Longint_type =>
            er := M3CBackEnd_Int_Longint.New_value(ROUND(float, LONGINT));
        END; (* typecase *)
    | M3CStdProcs.T.Trunc =>
        TYPECASE it OF <*NOWARN*>
        | M3AST_AS.Integer_type =>
            er := M3CBackEnd_Int_Integer.New_value(TRUNC(float, INTEGER));
        | M3AST_AS.Longint_type =>
            er := M3CBackEnd_Int_Longint.New_value(TRUNC(float, LONGINT));
        END; (* typecase *)
    ELSE RETURN M3CBackEnd.NumStatus.Unknown;
    END; (* case *)
    RETURN M3CBackEnd.NumStatus.Valid;
  END StdUnaryOp;

PROCEDURE BinaryOp (
    op: M3AST_AS.BINARY;
    e1, e2: T;
    VAR (*out*) er: M3AST_SM.Exp_value)
  : M3CBackEnd.NumStatus RAISES {}=
  VAR 
    float1 := e1.sm_value;
    float2 := e2.sm_value;
    floatr: FloatType.T;
    bool: BOOLEAN;
  BEGIN
    TYPECASE op OF
    | M3AST_AS.Plus =>  floatr := float1 + float2;
    | M3AST_AS.Minus => floatr := float1 - float2;
    | M3AST_AS.Times => floatr := float1 * float2;
    | M3AST_AS.Rdiv =>  floatr := float1 / float2;
    ELSE
      TYPECASE op OF
      | M3AST_AS.Eq => bool := float1 = float2;
      | M3AST_AS.Ne => bool := float1 # float2;
      | M3AST_AS.Gt => bool := float1 > float2;
      | M3AST_AS.Lt => bool := float1 < float2;
      | M3AST_AS.Ge => bool := float1 >= float2;
      | M3AST_AS.Le => bool := float1 <= float2;
      ELSE RETURN M3CBackEnd.NumStatus.Unknown;
      END; (* case *)
      er := M3CBackEnd_Int_Integer.New_value(ORD(bool));
      RETURN M3CBackEnd.NumStatus.Valid;
    END;
    er := New_value(floatr);
    RETURN M3CBackEnd.NumStatus.Valid;
  END BinaryOp;

PROCEDURE StdBinaryOp(
    f: M3CStdProcs.Func; 
    e1, e2: T;
    VAR (*out*) er: M3AST_SM.Exp_value)
  : M3CBackEnd.NumStatus RAISES {}=
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

BEGIN

END M3CBackEnd_Float.
