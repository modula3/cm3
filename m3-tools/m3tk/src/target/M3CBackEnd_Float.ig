GENERIC INTERFACE M3CBackEnd_Float(FloatType);

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT M3AST_AS, M3AST_SM, M3CStdProcs, M3CBackEnd;

REVEAL M3AST_SM.Exp_value <: ROOT;

TYPE T = M3AST_SM.Exp_value BRANDED OBJECT sm_value: FloatType.T END;

PROCEDURE New_value(r: FloatType.T): T RAISES {};

PROCEDURE StdUnaryOp(
    f: M3CStdProcs.Func; 
    e: T;
    VAR (*out*) er: M3AST_SM.Exp_value;
    ft: M3AST_AS.FLOAT_TYPE := NIL
    ): M3CBackEnd.NumStatus RAISES {};

PROCEDURE StdBinaryOp(
    f: M3CStdProcs.Func;
    e1, e2: T;
    VAR (*out*) er: M3AST_SM.Exp_value)
    : M3CBackEnd.NumStatus
    RAISES {};

PROCEDURE UnaryOp(
    op: M3AST_AS.UNARY;
    e: T;
    VAR (*out*) er: M3AST_SM.Exp_value)
    : M3CBackEnd.NumStatus
    RAISES {};

PROCEDURE BinaryOp(
    op: M3AST_AS.BINARY;
    e1, e2: T;
    VAR (*out*) er: M3AST_SM.Exp_value)
    : M3CBackEnd.NumStatus
    RAISES {};

END M3CBackEnd_Float.
