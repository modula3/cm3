(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

INTERFACE M3Builtin;

IMPORT M3Const;

PROCEDURE Eval (p: Proc;  READONLY args: ARRAY OF M3Const.T;
                VAR(*OUT*) val: M3Const.T)  RAISES {M3Const.Error};
(* If possible, evaluate "p (args...)" *)

TYPE
  Proc = {
    Abs, Adr, AdrSize, BitSize, ByteSize, Ceiling, Dec, Dispose,
    First, Float, Floor, Inc, IsType, Last, Loophole, Max, Min,
    Narrow, New, Number, Ord, Round, Subarray, Trunc, Typecode, Val,
    WordPlus, WordTimes, WordMinus, WordDivide, WordMod, WordLT,
    WordLE, WordGT, WordGE, WordAnd, WordOr, WordXor, WordNot,
    WordShift, WordLeftShift, WordRightShift, WordRotate,
    WordLeftRotate, WordRightRotate, WordExtract, WordInsert
  };
   
END M3Builtin.
