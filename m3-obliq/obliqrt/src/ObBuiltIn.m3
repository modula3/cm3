(* Copyright 1991 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

MODULE ObBuiltIn;
IMPORT Text, TextRd, Rd, Lex, Fmt, ObLib, ObValue, SynLocation, TextConv,
       Thread, NetObj, Env, Params, Math, ObEval, ObjectSpace, FloatMode,
       SharedObj, SharedObjRT, ObValueNotify, Obliq, Time, SynWr,
       WorkerPool, Work;

PROCEDURE Setup () =
  BEGIN
    SetupSys();
    SetupBool();
    SetupInt();
    SetupReal();                 (* after Int, so real_+ etc.  have
                                    precedence *)
    SetupMath();
    SetupAscii();
    SetupText();
    SetupArray();
    SetupNet();
    SetupReplica();
    SetupThread();

(*    SetupReflect();*)
  END Setup;

(* ============ "sys" package ============ *)

TYPE

  SysCode = {Address, GetEnvVar, GetParamCount, GetParam, CallFailure,
             Call, Copy, TimeNow, TimeGrain};

  SysOpCode = ObLib.OpCode OBJECT code: SysCode;  END;

  PackageSys = ObLib.T OBJECT OVERRIDES Eval := EvalSys; END;

PROCEDURE NewSysOC (name  : TEXT;
                    arity : INTEGER;
                    code  : SysCode;
                    fixity: ObLib.OpFixity := ObLib.OpFixity.Qualified):
  SysOpCode =
  BEGIN
    RETURN NEW(SysOpCode, name := name, arity := arity, code := code,
               fixity := fixity);
  END NewSysOC;

PROCEDURE SetupSys () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(SysCode));
    opCodes^ :=
      OpCodes{NewSysOC("address", -1, SysCode.Address),
              NewSysOC("getEnvVar", 1, SysCode.GetEnvVar),
              NewSysOC("paramCount", -1, SysCode.GetParamCount),
              NewSysOC("getParam", 1, SysCode.GetParam),
              NewSysOC("callFailure", -1, SysCode.CallFailure),
              NewSysOC("call", 2, SysCode.Call),
              NewSysOC("timeNow", -1, SysCode.TimeNow),
              NewSysOC("timeGrain", -1, SysCode.TimeGrain),
              NewSysOC("copy", 1, SysCode.Copy, ObLib.OpFixity.Prefix)};
    ObLib.Register(NEW(PackageSys, name := "sys", opCodes := opCodes));
  END SetupSys;

PROCEDURE EvalSys (                    self  : PackageSys;
                                       opCode: ObLib.OpCode;
                   <*UNUSED*>          arity : ObLib.OpArity;
                              READONLY args  : ObValue.ArgArray;
                                       temp  : BOOLEAN;
                                       loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    int1        : INTEGER;
    text1, text2: TEXT;
    array1      : REF ObValue.Vals;
    sysProc     : ObValue.SysCallClosure;
  BEGIN
    TRY
      CASE NARROW(opCode, SysOpCode).code OF
      | SysCode.Address => RETURN ObValue.NewText(ObValue.machineAddress);
      | SysCode.GetEnvVar =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
          END;
          text2 := Env.Get(text1);
          RETURN ObValue.NewText(text2);
      | SysCode.GetParamCount =>
          RETURN NEW(ObValue.ValInt, int := Params.Count, temp := temp);
      | SysCode.GetParam =>
          TYPECASE args[1] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(1, "int", self.name, opCode.name, loc);
          END;
          IF (int1 < 0) OR (int1 >= Params.Count) THEN
            ObValue.BadArgVal(1, "in range", self.name, opCode.name, loc);
          END;
          RETURN ObValue.NewText(Params.Get(int1));
      | SysCode.CallFailure => RETURN ObValue.sysCallFailure;
      | SysCode.Call =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
          END;
          TYPECASE args[2] OF
          | ObValue.ValArray (node) => array1 := node.remote.Obtain();
          ELSE
            ObValue.BadArgType(2, "array", self.name, opCode.name, loc)
          END;
          IF NOT ObValue.FetchSysCall(text1, (*out*) sysProc) THEN
            ObValue.RaiseException(ObValue.sysCallFailure,
                                   self.name & "_" & opCode.name & ": \""
                                     & text1 & "\" not found", loc);
          END;
          RETURN sysProc.SysCall(array1^, loc);
      | SysCode.Copy =>
          RETURN ObValue.CopyVal(args[1], ObValue.NewTbl(), loc);
      | SysCode.TimeNow => RETURN Obliq.NewReal(Time.Now());
      | SysCode.TimeGrain =>
          WITH grain = Time.Grain DO RETURN Obliq.NewReal(grain); END;
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc);
        <*ASSERT FALSE*>
      END;
    EXCEPT
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException(
          self.name & "_" & opCode.name, atoms, loc);
      <*ASSERT FALSE*>
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException(
          self.name & "_" & opCode.name, atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    END;
  END EvalSys;

(* ============ "bool" package ============ *)

TYPE

  BoolCode = {Is, IsNot, Not, And, Or};

  BoolOpCode = ObLib.OpCode OBJECT code: BoolCode;  END;

  PackageBool = ObLib.T OBJECT OVERRIDES Eval := EvalBool; END;

PROCEDURE NewBoolOC (name  : TEXT;
                     arity : INTEGER;
                     code  : BoolCode;
                     fixity: ObLib.OpFixity := ObLib.OpFixity.Qualified):
  BoolOpCode =
  BEGIN
    RETURN NEW(BoolOpCode, name := name, arity := arity, code := code,
               fixity := fixity);
  END NewBoolOC;

VAR true, false: ObValue.ValBool;

PROCEDURE SetupBool () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(BoolCode));
    opCodes^ :=
      OpCodes{NewBoolOC("not", 1, BoolCode.Not, ObLib.OpFixity.Prefix),
              NewBoolOC("and", 2, BoolCode.And, ObLib.OpFixity.Infix),
              NewBoolOC("or", 2, BoolCode.Or, ObLib.OpFixity.Infix),
              NewBoolOC("is", 2, BoolCode.Is, ObLib.OpFixity.Infix),
              NewBoolOC("isnot", 2, BoolCode.IsNot, ObLib.OpFixity.Infix)};
    ObLib.Register(NEW(PackageBool, name := "bool", opCodes := opCodes));
    true := NEW(ObValue.ValBool, bool := TRUE);
    false := NEW(ObValue.ValBool, bool := FALSE);
  END SetupBool;

PROCEDURE EvalBool (                    self  : PackageBool;
                                        opCode: ObLib.OpCode;
                    <*UNUSED*>          arity : ObLib.OpArity;
                               READONLY args  : ObValue.ArgArray;
                    <*UNUSED*>          temp  : BOOLEAN;
                                        loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error} =
  VAR bool1, bool2: BOOLEAN;
  BEGIN
    CASE NARROW(opCode, BoolOpCode).code OF
    | BoolCode.Not =>
        TYPECASE args[1] OF
        | ObValue.ValBool (node) => bool1 := node.bool;
        ELSE
          ObValue.BadArgType(1, "bool", self.name, opCode.name, loc);
        END;
        IF NOT bool1 THEN RETURN true ELSE RETURN false END;
    | BoolCode.And =>
        TYPECASE args[1] OF
        | ObValue.ValBool (node) => bool1 := node.bool;
        ELSE
          ObValue.BadArgType(1, "bool", self.name, opCode.name, loc);
        END;
        TYPECASE args[2] OF
        | ObValue.ValBool (node) => bool2 := node.bool;
        ELSE
          ObValue.BadArgType(2, "bool", self.name, opCode.name, loc);
        END;
        IF bool1 AND bool2 THEN RETURN true ELSE RETURN false END;
    | BoolCode.Or =>
        TYPECASE args[1] OF
        | ObValue.ValBool (node) => bool1 := node.bool;
        ELSE
          ObValue.BadArgType(1, "bool", self.name, opCode.name, loc);
        END;
        TYPECASE args[2] OF
        | ObValue.ValBool (node) => bool2 := node.bool;
        ELSE
          ObValue.BadArgType(2, "bool", self.name, opCode.name, loc);
        END;
        IF bool1 OR bool2 THEN RETURN true ELSE RETURN false END;
    | BoolCode.Is =>
        IF ObValue.Is(args[1], args[2], loc) THEN
          RETURN true
        ELSE
          RETURN false
        END;
    | BoolCode.IsNot =>
        IF NOT ObValue.Is(args[1], args[2], loc) THEN
          RETURN true
        ELSE
          RETURN false
        END;
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc);
      <*ASSERT FALSE*>
    END;
  END EvalBool;

(* ============ "int" package ============ *)

TYPE

  IntCode = {Minus, Add, Sub, Mult, Div, Mod, Less, More, LessEq, MoreEq};

  IntOpCode = ObLib.OpCode OBJECT code: IntCode;  END;

  PackageInt = ObLib.T OBJECT OVERRIDES Eval := EvalInt; END;

PROCEDURE NewIntOC (name  : TEXT;
                    arity : INTEGER;
                    code  : IntCode;
                    fixity: ObLib.OpFixity := ObLib.OpFixity.Qualified):
  IntOpCode =
  BEGIN
    RETURN NEW(IntOpCode, name := name, arity := arity, code := code,
               fixity := fixity);
  END NewIntOC;

PROCEDURE SetupInt () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(IntCode));
    opCodes^ :=
      OpCodes{
        NewIntOC("minus", 1, IntCode.Minus), NewIntOC("+", 2, IntCode.Add),
        NewIntOC("-", 2, IntCode.Sub), NewIntOC("*", 2, IntCode.Mult),
        NewIntOC("/", 2, IntCode.Div),
        NewIntOC("%", 2, IntCode.Mod, ObLib.OpFixity.Infix),
        NewIntOC("<", 2, IntCode.Less), NewIntOC(">", 2, IntCode.More),
        NewIntOC("<=", 2, IntCode.LessEq),
        NewIntOC(">=", 2, IntCode.MoreEq)};
    ObLib.Register(NEW(PackageInt, name := "int", opCodes := opCodes));
  END SetupInt;

PROCEDURE EvalInt (                    self  : PackageInt;
                                       opCode: ObLib.OpCode;
                   <*UNUSED*>          arity : ObLib.OpArity;
                              READONLY args  : ObValue.ArgArray;
                                       temp  : BOOLEAN;
                                       loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error} =
  VAR
    int1, int2              : INTEGER;
    intCode                 : IntCode;
    intRes, intVal1, intVal2: ObValue.ValInt;
  BEGIN
    intCode := NARROW(opCode, IntOpCode).code;
    TYPECASE args[1] OF
    | ObValue.ValInt (node) => intVal1 := node; int1 := node.int;
    ELSE
      ObValue.BadArgType(1, "int", self.name, opCode.name, loc);
      <*ASSERT FALSE*>
    END;
    CASE intCode OF
    | IntCode.Minus =>
        RETURN NEW(ObValue.ValInt, int := -int1, temp := temp);
    | IntCode.Add, IntCode.Sub, IntCode.Mult, IntCode.Div, IntCode.Mod,
        IntCode.Less, IntCode.More, IntCode.LessEq, IntCode.MoreEq =>
        TYPECASE args[2] OF
        | ObValue.ValInt (node) => intVal2 := node; int2 := node.int;
        ELSE
          ObValue.BadArgType(2, "int", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        CASE intCode OF
        | IntCode.Minus =>       <*ASSERT FALSE*>(* can't happen *)
        | IntCode.Add =>
            RETURN NEW(ObValue.ValInt, int := int1 + int2, temp := temp);
        | IntCode.Sub =>
            RETURN NEW(ObValue.ValInt, int := int1 - int2, temp := temp);
        | IntCode.Mult =>
            RETURN NEW(ObValue.ValInt, int := int1 * int2, temp := temp);
        | IntCode.Div =>
            IF int2 = 0 THEN
              ObValue.BadArgVal(2, "non-zero", self.name, opCode.name, loc);
              <*ASSERT FALSE*>
            ELSE
              RETURN
                NEW(ObValue.ValInt, int := int1 DIV int2, temp := temp);
            END;
        | IntCode.Mod =>
            IF int2 = 0 THEN
              ObValue.BadArgVal(2, "non-zero", self.name, opCode.name, loc);
              <*ASSERT FALSE*>
            ELSE
              IF intVal1.temp THEN
                intRes := intVal1;
              ELSIF intVal2.temp THEN
                intRes := intVal2;
              ELSE
                intRes := NEW(ObValue.ValInt);
              END;
              intRes.temp := temp;
              intRes.int := int1 MOD int2;
              RETURN intRes;
            END;
        | IntCode.Less => RETURN NEW(ObValue.ValBool, bool := int1 < int2);
        | IntCode.More => RETURN NEW(ObValue.ValBool, bool := int1 > int2);
        | IntCode.LessEq =>
            RETURN NEW(ObValue.ValBool, bool := int1 <= int2);
        | IntCode.MoreEq =>
            RETURN NEW(ObValue.ValBool, bool := int1 >= int2);
        END;
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc);
      <*ASSERT FALSE*>
    END;
  END EvalInt;

(* ============ "real" package ============ *)

TYPE

  RealCode = {Minus, Add, Sub, Mult, Div, Less, More, LessEq, MoreEq,
              Round, Float, Floor, Ceiling};

  RealOpCode = ObLib.OpCode OBJECT code: RealCode;  END;

  PackageReal = ObLib.T OBJECT OVERRIDES Eval := EvalReal; END;

PROCEDURE NewRealOC (name  : TEXT;
                     arity : INTEGER;
                     code  : RealCode;
                     fixity: ObLib.OpFixity := ObLib.OpFixity.Qualified):
  RealOpCode =
  BEGIN
    RETURN NEW(RealOpCode, name := name, arity := arity, code := code,
               fixity := fixity);
  END NewRealOC;

PROCEDURE SetupReal () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(RealCode));
    opCodes^ :=
      OpCodes{NewRealOC("minus", 1, RealCode.Minus),
              NewRealOC("+", 2, RealCode.Add, ObLib.OpFixity.Infix),
              NewRealOC("-", 2, RealCode.Sub, ObLib.OpFixity.Infix),
              NewRealOC("*", 2, RealCode.Mult, ObLib.OpFixity.Infix),
              NewRealOC("/", 2, RealCode.Div, ObLib.OpFixity.Infix),
              NewRealOC("<", 2, RealCode.Less, ObLib.OpFixity.Infix),
              NewRealOC(">", 2, RealCode.More, ObLib.OpFixity.Infix),
              NewRealOC("<=", 2, RealCode.LessEq, ObLib.OpFixity.Infix),
              NewRealOC(">=", 2, RealCode.MoreEq, ObLib.OpFixity.Infix),
              NewRealOC("round", 1, RealCode.Round, ObLib.OpFixity.Prefix),
              NewRealOC("float", 1, RealCode.Float, ObLib.OpFixity.Prefix),
              NewRealOC("floor", 1, RealCode.Floor),
              NewRealOC("ceiling", 1, RealCode.Ceiling)};
    ObLib.Register(NEW(PackageReal, name := "real", opCodes := opCodes));
  END SetupReal;

PROCEDURE EvalReal (                    self  : PackageReal;
                                        opCode: ObLib.OpCode;
                    <*UNUSED*>          arity : ObLib.OpArity;
                               READONLY args  : ObValue.ArgArray;
                                        temp  : BOOLEAN;
                                        loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error} =
  VAR
    realRes, realVal1, realVal2: ObValue.ValReal;
    real1, real2               : LONGREAL;
    intRes, intVal1, intVal2   : ObValue.ValInt;
    int1, int2                 : INTEGER;
    realCode                   : RealCode;
    isReal1, isReal2           : BOOLEAN;
  BEGIN
    realCode := NARROW(opCode, RealOpCode).code;
    TYPECASE args[1] OF
    | ObValue.ValReal (node) =>
        realVal1 := node;
        real1 := node.real;
        isReal1 := TRUE;
    | ObValue.ValInt (node) =>
        intVal1 := node;
        int1 := node.int;
        isReal1 := FALSE;
    ELSE
      ObValue.BadArgType(1, "real or int", self.name, opCode.name, loc);
      <*ASSERT FALSE*>
    END;
    CASE realCode OF
    | RealCode.Minus =>
        IF isReal1 THEN
          IF realVal1.temp THEN
            realRes := realVal1;
          ELSE
            realRes := NEW(ObValue.ValReal);
          END;
          realRes.temp := temp;
          realRes.real := -real1;
          RETURN realRes;
        ELSE
          IF intVal1.temp THEN
            intRes := intVal1;
          ELSE
            intRes := NEW(ObValue.ValInt);
          END;
          intRes.temp := temp;
          intRes.int := -int1;
          RETURN intRes;
        END;
    | RealCode.Float =>
        IF isReal1 THEN
          IF realVal1.temp THEN realVal1.temp := temp; END;
          RETURN realVal1;
        ELSE
          RETURN NEW(ObValue.ValReal, real := FLOAT(int1, LONGREAL),
                     temp := temp);
        END;
    | RealCode.Round =>
        IF isReal1 THEN
          RETURN NEW(ObValue.ValInt, int := ROUND(real1), temp := temp);
        ELSE
          IF intVal1.temp THEN intVal1.temp := temp END;
          RETURN intVal1;
        END;
    | RealCode.Floor =>
        IF isReal1 THEN
          RETURN NEW(ObValue.ValInt, int := FLOOR(real1), temp := temp);
        ELSE
          IF intVal1.temp THEN intVal1.temp := temp END;
          RETURN intVal1;
        END;
    | RealCode.Ceiling =>
        IF isReal1 THEN
          RETURN NEW(ObValue.ValInt, int := CEILING(real1), temp := temp);
        ELSE
          IF intVal1.temp THEN intVal1.temp := temp END;
          RETURN intVal1;
        END;
    | RealCode.Add, RealCode.Sub, RealCode.Mult, RealCode.Div =>
        TYPECASE args[2] OF
        | ObValue.ValReal (node) =>
            realVal2 := node;
            real2 := node.real;
            isReal2 := TRUE;
        | ObValue.ValInt (node) =>
            intVal2 := node;
            int2 := node.int;
            isReal2 := FALSE;
        ELSE
          ObValue.BadArgType(2, "real or int", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        IF isReal1 # isReal2 THEN
          IF isReal1 THEN
            ObValue.BadArgType(
              2, "real (like argument 1)", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          ELSE
            ObValue.BadArgType(
              2, "int (like argument 1)", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
        END;
        IF isReal1 THEN
          IF realVal1.temp THEN
            realRes := realVal1;
          ELSIF realVal2.temp THEN
            realRes := realVal2;
          ELSE
            realRes := NEW(ObValue.ValReal);
          END;
          realRes.temp := temp;
        ELSE
          IF intVal1.temp THEN
            intRes := intVal1;
          ELSIF intVal2.temp THEN
            intRes := intVal2;
          ELSE
            intRes := NEW(ObValue.ValInt);
          END;
          intRes.temp := temp;
        END;
        CASE realCode OF
        | RealCode.Add =>
            IF isReal1 THEN
              realRes.real := real1 + real2;
              RETURN realRes;
            ELSE
              intRes.int := int1 + int2;
              RETURN intRes;
            END;
        | RealCode.Sub =>
            IF isReal1 THEN
              realRes.real := real1 - real2;
              RETURN realRes;
            ELSE
              intRes.int := int1 - int2;
              RETURN intRes;
            END;
        | RealCode.Mult =>
            IF isReal1 THEN
              realRes.real := real1 * real2;
              RETURN realRes;
            ELSE
              intRes.int := int1 * int2;
              RETURN intRes;
            END;
        | RealCode.Div =>
            IF isReal1 THEN
              IF real2 = 0.0d0 THEN
                ObValue.BadArgVal(
                  2, "a non-zero real", self.name, opCode.name, loc);
                <*ASSERT FALSE*>
              ELSE
                realRes.real := real1 / real2;
                RETURN realRes;
              END;
            ELSE
              IF int2 = 0 THEN
                ObValue.BadArgVal(
                  2, "a non-zero int", self.name, opCode.name, loc);
                <*ASSERT FALSE*>
              ELSE
                intRes.int := int1 DIV int2;
                RETURN intRes;
              END;
            END;
        ELSE                     <*ASSERT FALSE*>
        END;
    | RealCode.Less, RealCode.More, RealCode.LessEq, RealCode.MoreEq =>
        TYPECASE args[2] OF
        | ObValue.ValReal (node) => real2 := node.real; isReal2 := TRUE;
        | ObValue.ValInt (node) => int2 := node.int; isReal2 := FALSE;
        ELSE
          ObValue.BadArgType(2, "real or int", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        IF isReal1 # isReal2 THEN
          IF isReal1 THEN
            ObValue.BadArgType(
              2, "real (like argument 1)", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          ELSE
            ObValue.BadArgType(
              2, "int (like argument 1)", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
        END;
        CASE realCode OF
        | RealCode.Less =>
            IF isReal1 THEN
              IF real1 < real2 THEN RETURN true ELSE RETURN false END
            ELSE
              IF int1 < int2 THEN RETURN true ELSE RETURN false END
            END;
        | RealCode.More =>
            IF isReal1 THEN
              IF real1 > real2 THEN RETURN true ELSE RETURN false END
            ELSE
              IF int1 > int2 THEN RETURN true ELSE RETURN false END
            END;
        | RealCode.LessEq =>
            IF isReal1 THEN
              IF real1 <= real2 THEN RETURN true ELSE RETURN false END
            ELSE
              IF int1 <= int2 THEN RETURN true ELSE RETURN false END
            END;
        | RealCode.MoreEq =>
            IF isReal1 THEN
              IF real1 >= real2 THEN RETURN true ELSE RETURN false END
            ELSE
              IF int1 >= int2 THEN RETURN true ELSE RETURN false END
            END;
        ELSE                     <*ASSERT FALSE*>
        END;
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc);
      <*ASSERT FALSE*>
    END;
  END EvalReal;

(* ============ "math" package ============ *)

TYPE

  MathCode = {Pi, E, Degree, Exp, Log, Sqrt, Pow, Cos, Sin, Tan, Acos,
              Asin, Atan, Atan2, Hypot};

  MathOpCode = ObLib.OpCode OBJECT code: MathCode;  END;

  PackageMath = ObLib.T OBJECT OVERRIDES Eval := EvalMath; END;

VAR MathPi, MathE, MathDegree: ObValue.Val;

PROCEDURE NewMathOC (name: TEXT; arity: INTEGER; code: MathCode):
  MathOpCode =
  BEGIN
    RETURN NEW(MathOpCode, name := name, arity := arity, code := code);
  END NewMathOC;

PROCEDURE SetupMath () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(MathCode));
    opCodes^ := OpCodes{NewMathOC("pi", -1, MathCode.Pi),
                        NewMathOC("e", -1, MathCode.E),
                        NewMathOC("degree", -1, MathCode.Degree),
                        NewMathOC("exp", 1, MathCode.Exp),
                        NewMathOC("log", 1, MathCode.Log),
                        NewMathOC("sqrt", 1, MathCode.Sqrt),
                        NewMathOC("pow", 2, MathCode.Pow),
                        NewMathOC("cos", 1, MathCode.Cos),
                        NewMathOC("sin", 1, MathCode.Sin),
                        NewMathOC("tan", 1, MathCode.Tan),
                        NewMathOC("acos", 1, MathCode.Acos),
                        NewMathOC("asin", 1, MathCode.Asin),
                        NewMathOC("atan", 1, MathCode.Atan),
                        NewMathOC("atan2", 2, MathCode.Atan2),
                        NewMathOC("hypot", 2, MathCode.Hypot)};
    ObLib.Register(NEW(PackageMath, name := "math", opCodes := opCodes));
    MathPi := NEW(ObValue.ValReal, real := FLOAT(Math.Pi, LONGREAL),
                  temp := FALSE);
    MathE :=
      NEW(ObValue.ValReal, real := FLOAT(Math.E, LONGREAL), temp := FALSE);
    MathDegree := NEW(ObValue.ValReal,
                      real := FLOAT(Math.Degree, LONGREAL), temp := FALSE);
  END SetupMath;

PROCEDURE EvalMath (                    self  : PackageMath;
                                        opCode: ObLib.OpCode;
                    <*UNUSED*>          arity : ObLib.OpArity;
                               READONLY args  : ObValue.ArgArray;
                                        temp  : BOOLEAN;
                                        loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error} =
  VAR
    real1, real2               : LONGREAL;
    realRes, realVal1, realVal2: ObValue.ValReal;
  BEGIN
    CASE NARROW(opCode, MathOpCode).code OF
    | MathCode.Pi => RETURN MathPi;
    | MathCode.E => RETURN MathE;
    | MathCode.Degree => RETURN MathDegree;
    | MathCode.Exp =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) =>
            IF node.temp THEN
              realRes := node;
            ELSE
              realRes := NEW(ObValue.ValReal);
            END;
            realRes.temp := temp;
            real1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc);
        END;
        realRes.real := Math.exp(real1);
        RETURN realRes;
    | MathCode.Log =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) =>
            IF node.temp THEN
              realRes := node;
            ELSE
              realRes := NEW(ObValue.ValReal);
            END;
            realRes.temp := temp;
            real1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc);
        END;
        realRes.real := Math.log(real1);
        RETURN realRes;
    | MathCode.Sqrt =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) =>
            IF node.temp THEN
              realRes := node;
            ELSE
              realRes := NEW(ObValue.ValReal);
            END;
            realRes.temp := temp;
            real1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc);
        END;
        realRes.real := Math.sqrt(real1);
        RETURN realRes;
    | MathCode.Pow =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) => realVal1 := node; real1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc);
        END;
        TYPECASE args[2] OF
        | ObValue.ValReal (node) => realVal2 := node; real2 := node.real;
        ELSE
          ObValue.BadArgType(2, "real", self.name, opCode.name, loc);
        END;
        IF realVal1.temp THEN
          realRes := realVal1;
        ELSIF realVal2.temp THEN
          realRes := realVal2;
        ELSE
          realRes := NEW(ObValue.ValReal);
        END;
        realRes.temp := temp;
        realRes.real := Math.pow(real1, real2);
        RETURN realRes;
    | MathCode.Cos =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) =>
            IF node.temp THEN
              realRes := node;
            ELSE
              realRes := NEW(ObValue.ValReal);
            END;
            realRes.temp := temp;
            real1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc);
        END;
        realRes.real := Math.cos(real1);
        RETURN realRes;
    | MathCode.Sin =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) =>
            IF node.temp THEN
              realRes := node;
            ELSE
              realRes := NEW(ObValue.ValReal);
            END;
            realRes.temp := temp;
            real1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc);
        END;
        realRes.real := Math.sin(real1);
        RETURN realRes;
    | MathCode.Tan =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) =>
            IF node.temp THEN
              realRes := node;
            ELSE
              realRes := NEW(ObValue.ValReal);
            END;
            realRes.temp := temp;
            real1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc);
        END;
        realRes.real := Math.tan(real1);
        RETURN realRes;
    | MathCode.Acos =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) =>
            IF node.temp THEN
              realRes := node;
            ELSE
              realRes := NEW(ObValue.ValReal);
            END;
            realRes.temp := temp;
            real1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc);
        END;
        realRes.real := Math.acos(real1);
        RETURN realRes;
    | MathCode.Asin =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) =>
            IF node.temp THEN
              realRes := node;
            ELSE
              realRes := NEW(ObValue.ValReal);
            END;
            realRes.temp := temp;
            real1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc);
        END;
        realRes.real := Math.asin(real1);
        RETURN realRes;
    | MathCode.Atan =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) =>
            IF node.temp THEN
              realRes := node;
            ELSE
              realRes := NEW(ObValue.ValReal);
            END;
            realRes.temp := temp;
            real1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc);
        END;
        realRes.real := Math.atan(real1);
        RETURN realRes;
    | MathCode.Atan2 =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) => realVal1 := node; real1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc);
        END;
        TYPECASE args[2] OF
        | ObValue.ValReal (node) => realVal2 := node; real2 := node.real;
        ELSE
          ObValue.BadArgType(2, "real", self.name, opCode.name, loc);
        END;
        IF realVal1.temp THEN
          realRes := realVal1;
        ELSIF realVal2.temp THEN
          realRes := realVal2;
        ELSE
          realRes := NEW(ObValue.ValReal);
        END;
        realRes.temp := temp;
        realRes.real := Math.atan2(real1, real2);
        RETURN realRes;
    | MathCode.Hypot =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) => realVal1 := node; real1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc);
        END;
        TYPECASE args[2] OF
        | ObValue.ValReal (node) => realVal2 := node; real2 := node.real;
        ELSE
          ObValue.BadArgType(2, "real", self.name, opCode.name, loc);
        END;
        IF realVal1.temp THEN
          realRes := realVal1;
        ELSIF realVal2.temp THEN
          realRes := realVal2;
        ELSE
          realRes := NEW(ObValue.ValReal);
        END;
        realRes.temp := temp;
        realRes.real := Math.hypot(real1, real2);
        RETURN realRes;
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc);
      <*ASSERT FALSE*>
    END;
  END EvalMath;


(* ============ "ascii" package ============ *)

TYPE

  AsciiCode = {Char, Val};

  AsciiOpCode = ObLib.OpCode OBJECT code: AsciiCode;  END;

  PackageAscii = ObLib.T OBJECT OVERRIDES Eval := EvalAscii; END;

PROCEDURE NewAsciiOC (name: TEXT; arity: INTEGER; code: AsciiCode):
  AsciiOpCode =
  BEGIN
    RETURN NEW(AsciiOpCode, name := name, arity := arity, code := code);
  END NewAsciiOC;

PROCEDURE SetupAscii () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(AsciiCode));
    opCodes^ := OpCodes{NewAsciiOC("char", 1, AsciiCode.Char),
                        NewAsciiOC("val", 1, AsciiCode.Val)};
    ObLib.Register(NEW(PackageAscii, name := "ascii", opCodes := opCodes));
  END SetupAscii;

PROCEDURE EvalAscii (                    self  : PackageAscii;
                                         opCode: ObLib.OpCode;
                     <*UNUSED*>          arity : ObLib.OpArity;
                                READONLY args  : ObValue.ArgArray;
                                         temp  : BOOLEAN;
                                         loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error} =
  VAR
    int1 : INTEGER;
    char1: CHAR;
  BEGIN
    CASE NARROW(opCode, AsciiOpCode).code OF
    | AsciiCode.Char =>
        TYPECASE args[1] OF
        | ObValue.ValInt (node) => int1 := node.int;
        ELSE
          ObValue.BadArgType(1, "int", self.name, opCode.name, loc);
        END;
        IF (int1 < 0) OR (int1 > 255) THEN
          ObValue.BadArgVal(1, "0..255", self.name, opCode.name, loc);
        END;
        RETURN NEW(ObValue.ValChar, char := VAL(int1, CHAR));
    | AsciiCode.Val =>
        TYPECASE args[1] OF
        | ObValue.ValChar (node) => char1 := node.char;
        ELSE
          ObValue.BadArgType(1, "char", self.name, opCode.name, loc);
        END;
        RETURN NEW(ObValue.ValInt, int := ORD(char1), temp := temp);
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc);
      <*ASSERT FALSE*>
    END;
  END EvalAscii;

(* ============ "text" package ============ *)

TYPE

  TextCode =
    {New, Empty, Length, Equal, Char, Sub, Cat, Precedes, Encode, Decode,
     Implode, Explode, Hash, ToInt, FromInt, FindFirstChar, FindLastChar,
     FindFirst, FindLast, ReplaceAll};

  TextOpCode = ObLib.OpCode OBJECT code: TextCode;  END;

  PackageText = ObLib.T OBJECT OVERRIDES Eval := EvalText; END;

PROCEDURE NewTextOC (name  : TEXT;
                     arity : INTEGER;
                     code  : TextCode;
                     fixity: ObLib.OpFixity := ObLib.OpFixity.Qualified):
  TextOpCode =
  BEGIN
    RETURN NEW(TextOpCode, name := name, arity := arity, code := code,
               fixity := fixity);
  END NewTextOC;

PROCEDURE SetupText () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(TextCode));
    opCodes^ :=
      OpCodes{NewTextOC("new", 2, TextCode.New),
              NewTextOC("empty", 1, TextCode.Empty),
              NewTextOC("length", 1, TextCode.Length),
              NewTextOC("equal", 2, TextCode.Equal),
              NewTextOC("char", 2, TextCode.Char),
              NewTextOC("sub", 3, TextCode.Sub),
              NewTextOC("&", 2, TextCode.Cat, ObLib.OpFixity.Infix),
              NewTextOC("precedes", 2, TextCode.Precedes),
              NewTextOC("encode", 1, TextCode.Encode),
              NewTextOC("decode", 1, TextCode.Decode),
              NewTextOC("implode", 2, TextCode.Implode),
              NewTextOC("explode", 2, TextCode.Explode),
              NewTextOC("hash", 1, TextCode.Hash),
              NewTextOC("toInt", 1, TextCode.ToInt),
              NewTextOC("fromInt", 1, TextCode.FromInt),
              NewTextOC("findFirstChar", 3, TextCode.FindFirstChar),
              NewTextOC("findLastChar", 3, TextCode.FindLastChar),
              NewTextOC("findFirst", 3, TextCode.FindFirst),
              NewTextOC("findLast", 3, TextCode.FindLast),
              NewTextOC("replaceAll", 3, TextCode.ReplaceAll)};
    ObLib.Register(NEW(PackageText, name := "text", opCodes := opCodes));
  END SetupText;

PROCEDURE EvalText (                    self  : PackageText;
                                        opCode: ObLib.OpCode;
                    <*UNUSED*>          arity : ObLib.OpArity;
                               READONLY args  : ObValue.ArgArray;
                                        temp  : BOOLEAN;
                                        loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  TYPE Chars = REF ARRAY OF CHAR;
  TYPE Texts = REF ARRAY OF TEXT;
  TYPE Vals = REF ARRAY OF ObValue.Val;
  VAR
    text1, text2, text3: TEXT;
    int1, int2, len    : INTEGER;
    char1              : CHAR;
    chars              : Chars;
    val                : ObValue.Val;
    texts              : Texts;
    array1             : Vals;
    chSet              : SET OF CHAR;
  BEGIN
    TRY
      CASE NARROW(opCode, TextOpCode).code OF
      | TextCode.New =>
          TYPECASE args[1] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(1, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValChar (node) => char1 := node.char;
          ELSE
            ObValue.BadArgType(2, "char", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF int1 < 0 THEN
            ObValue.BadArgVal(
              1, "non-negative", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          chars := NEW(Chars, int1);
          FOR i := 0 TO int1 - 1 DO chars^[i] := char1; END;
          RETURN ObValue.NewText(Text.FromChars(chars^));
      | TextCode.Empty =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF Text.Empty(text1) THEN RETURN true ELSE RETURN false END;
      | TextCode.Length =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN
            NEW(ObValue.ValInt, int := Text.Length(text1), temp := temp);
      | TextCode.Equal =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF Text.Equal(text1, text2) THEN
            RETURN true
          ELSE
            RETURN false
          END;
      | TextCode.Char =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(2, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF (int1 < 0) OR (int1 >= Text.Length(text1)) THEN
            ObValue.BadArgVal(2, "in range", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN NEW(ObValue.ValChar, char := Text.GetChar(text1, int1));
      | TextCode.Sub =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(2, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValInt (node) => int2 := node.int;
          ELSE
            ObValue.BadArgType(3, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          len := Text.Length(text1);
          IF (int1 < 0) OR (int1 > len) THEN
            ObValue.BadArgVal(2, "in range", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF (int2 < 0) OR (int1 + int2 > len) THEN
            ObValue.BadArgVal(3, "in range", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN ObValue.NewText(Text.Sub(text1, int1, int2));
      | TextCode.Cat =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN ObValue.NewText(Text.Cat(text1, text2));
      | TextCode.Precedes =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF Text.Compare(text1, text2) < 0 THEN
            RETURN true;
          ELSE
            RETURN false;
          END;
      | TextCode.Encode =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN ObValue.NewText(TextConv.Encode(text1, FALSE));
      | TextCode.Decode =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TRY
            val := ObValue.NewText(TextConv.Decode(text1, FALSE));
          EXCEPT
            TextConv.Fail =>
              ObValue.BadArgVal(1, "a well-formed encoded text", self.name,
                                opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN val;
      | TextCode.Implode =>
          TYPECASE args[1] OF
          | ObValue.ValChar (node) => char1 := node.char;
          ELSE
            ObValue.BadArgType(1, "char", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValArray (node) => array1 := node.remote.Obtain();
          ELSE
            ObValue.BadArgType(2, "array", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          texts := NEW(Texts, NUMBER(array1^));
          FOR i := 0 TO NUMBER(texts^) - 1 DO
            TYPECASE array1^[i] OF
            | ObValue.ValText (node) => texts^[i] := node.text;
            ELSE
              ObValue.BadArgType(
                1, "array(text)", self.name, opCode.name, loc);
              <*ASSERT FALSE*>
            END;
          END;
          RETURN ObValue.NewText(TextConv.Implode(texts^, char1));
      | TextCode.Explode =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          chSet := CharSet(text2);
          texts := NEW(Texts, TextConv.ExplodedSize(text1, chSet));
          TextConv.Explode(text1, texts^, chSet);
          array1 := NEW(Vals, NUMBER(texts^));
          FOR i := 0 TO NUMBER(array1^) - 1 DO
            array1[i] := ObValue.NewText(texts[i]);
          END;
          RETURN ObValue.NewArrayFromVals(array1);
      | TextCode.Hash =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN
            NEW(ObValue.ValInt, int := Text.Hash(text1), temp := temp);
      | TextCode.ToInt =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TRY
            RETURN NEW(ObValue.ValInt, int := Lex.Int(TextRd.New(text1)),
                       temp := temp);
          EXCEPT
            Lex.Error, Rd.Failure, FloatMode.Trap =>
              ObValue.BadArgVal(
                1, "a well-formed int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
      | TextCode.FromInt =>
          TYPECASE args[1] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(1, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN ObValue.NewText(Fmt.Int(int1));
      | TextCode.FindFirstChar =>
          TYPECASE args[1] OF
          | ObValue.ValChar (node) => char1 := node.char;
          ELSE
            ObValue.BadArgType(1, "char", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(3, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN
            NEW(ObValue.ValInt, int := Text.FindChar(text1, char1, int1),
                temp := temp);
      | TextCode.FindLastChar =>
          TYPECASE args[1] OF
          | ObValue.ValChar (node) => char1 := node.char;
          ELSE
            ObValue.BadArgType(1, "char", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(3, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN
            NEW(ObValue.ValInt, int := Text.FindCharR(text1, char1, int1),
                temp := temp);
      | TextCode.FindFirst =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(3, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN NEW(ObValue.ValInt, int := FindFirst(text2, int1, text1),
                     temp := temp);
      | TextCode.FindLast =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(3, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN NEW(ObValue.ValInt, int := FindLast(text2, int1, text1),
                     temp := temp);
      | TextCode.ReplaceAll =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (node) => text2 := node.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValText (node) => text3 := node.text;
          ELSE
            ObValue.BadArgType(3, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN ObValue.NewText(ReplaceAll(text3, text1, text2));
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc);
        <*ASSERT FALSE*>
      END;
    EXCEPT
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException(
          self.name & "_" & opCode.name, atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    END;
  END EvalText;

PROCEDURE CharSet (text: TEXT): SET OF CHAR =
  VAR s: SET OF CHAR;
  BEGIN
    s := SET OF CHAR{};
    FOR i := 0 TO Text.Length(text) - 1 DO
      s := s + SET OF CHAR{Text.GetChar(text, i)};
    END;
    RETURN s;
  END CharSet;

PROCEDURE FindFirst (source: TEXT; start: INTEGER; pattern: TEXT):
  INTEGER =
  VAR
    i, ii, j, srcLimit, patLimit: INTEGER;
    patFirst                    : CHAR;
  BEGIN
    srcLimit := Text.Length(source) - start;
    patLimit := Text.Length(pattern);
    IF patLimit = 0 THEN RETURN 0 END;
    patFirst := Text.GetChar(pattern, 0);
    i := start;
    LOOP
      IF i >= srcLimit THEN RETURN -1 END;
      IF Text.GetChar(source, i) = patFirst THEN
        ii := i;
        j := 0;
        LOOP
          INC(j);
          IF j >= patLimit THEN RETURN i END;
          INC(ii);
          IF ii >= srcLimit THEN EXIT END;
          IF Text.GetChar(source, ii) # Text.GetChar(pattern, j) THEN
            EXIT
          END;
        END;
      END;
      INC(i);
    END;
  END FindFirst;

PROCEDURE FindLast (source: TEXT; start: INTEGER; pattern: TEXT): INTEGER =
  VAR
    i, ii, j, patLength: INTEGER;
    patLast            : CHAR;
  BEGIN
    patLength := Text.Length(pattern);
    IF patLength = 0 THEN RETURN i END;
    patLast := Text.GetChar(pattern, patLength - 1);
    i := MIN(Text.Length(source), start);
    LOOP
      DEC(i);
      IF i < 0 THEN RETURN -1 END;
      IF Text.GetChar(source, i) = patLast THEN
        ii := i;
        j := patLength - 1;
        LOOP
          DEC(j);
          IF j < 0 THEN RETURN ii END;
          DEC(ii);
          IF ii < 0 THEN EXIT END;
          IF Text.GetChar(source, ii) # Text.GetChar(pattern, j) THEN
            EXIT
          END;
        END;
      END;
    END;
  END FindLast;

PROCEDURE ReplaceAll (source: TEXT; pattern: TEXT; repl: TEXT): TEXT =
  VAR
    i, ii, j, k, srcLimit, patLimit, replLength, count: INTEGER;
    patFirst, ch                                      : CHAR;
    res                                               : REF ARRAY OF CHAR;
  BEGIN
    srcLimit := Text.Length(source);
    patLimit := Text.Length(pattern);
    IF patLimit = 0 THEN RETURN source END;
    patFirst := Text.GetChar(pattern, 0);
    count := 0;
    i := 0;
    LOOP
      IF i >= srcLimit THEN EXIT END;
      IF Text.GetChar(source, i) = patFirst THEN
        ii := i;
        j := 0;
        LOOP
          INC(j);
          IF j >= patLimit THEN INC(count); INC(i, patLimit); EXIT; END;
          INC(ii);
          IF (ii >= srcLimit)
               OR (Text.GetChar(source, ii) # Text.GetChar(pattern, j)) THEN
            INC(i);
            EXIT;
          END;
        END;
      ELSE
        INC(i);
      END;
    END;
    replLength := Text.Length(repl);
    res := NEW(REF ARRAY OF CHAR,
               (srcLimit - (count * patLimit)) + (count * replLength));
    i := 0;
    k := 0;
    LOOP
      IF i >= srcLimit THEN EXIT END;
      ch := Text.GetChar(source, i);
      IF ch = patFirst THEN
        ii := i;
        j := 0;
        LOOP
          INC(j);
          IF j >= patLimit THEN
            Text.SetChars(SUBARRAY(res^, k, replLength), repl);
            INC(k, replLength);
            INC(i, patLimit);
            EXIT;
          END;
          INC(ii);
          IF (ii >= srcLimit)
               OR (Text.GetChar(source, ii) # Text.GetChar(pattern, j)) THEN
            res^[k] := ch;
            INC(k);
            INC(i);
            EXIT;
          END;
        END;
      ELSE
        res^[k] := ch;
        INC(k);
        INC(i);
      END;
    END;
    RETURN Text.FromChars(res^);
  END ReplaceAll;

(* ============ "array" package ============ *)

TYPE

  ArrayCode = {New, Gen, Size, Get, Set, Sub, Upd, Cat};

  ArrayOpCode = ObLib.OpCode OBJECT code: ArrayCode;  END;

  PackageArray = ObLib.T OBJECT OVERRIDES Eval := EvalArray; END;

PROCEDURE NewArrayOC (name  : TEXT;
                      arity : INTEGER;
                      code  : ArrayCode;
                      fixity: ObLib.OpFixity := ObLib.OpFixity.Qualified):
  ArrayOpCode =
  BEGIN
    RETURN NEW(ArrayOpCode, name := name, arity := arity, code := code,
               fixity := fixity);
  END NewArrayOC;

PROCEDURE SetupArray () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(ArrayCode));
    opCodes^ :=
      OpCodes{NewArrayOC("new", 2, ArrayCode.New),
              NewArrayOC("gen", 2, ArrayCode.Gen),
              NewArrayOC("#", 1, ArrayCode.Size, ObLib.OpFixity.Prefix),
              NewArrayOC("get", 2, ArrayCode.Get),
              NewArrayOC("set", 3, ArrayCode.Set),
              NewArrayOC("sub", 3, ArrayCode.Sub),
              NewArrayOC("upd", 4, ArrayCode.Upd),
              NewArrayOC("@", 2, ArrayCode.Cat, ObLib.OpFixity.Infix)};
    ObLib.Register(NEW(PackageArray, name := "array", opCodes := opCodes));
  END SetupArray;

PROCEDURE EvalArray (                    self  : PackageArray;
                                         opCode: ObLib.OpCode;
                     <*UNUSED*>          arity : ObLib.OpArity;
                                READONLY args  : ObValue.ArgArray;
                                         temp  : BOOLEAN;
                                         loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  TYPE Vals = REF ARRAY OF ObValue.Val;
  VAR
    int1, int2          : INTEGER;
    vals, array1, array2: Vals;
    rem1                : ObValue.RemArray;
    badOp               : INTEGER          := 0;
    clos1               : ObValue.ValFun;
  BEGIN
    TRY
      CASE NARROW(opCode, ArrayOpCode).code OF
      | ArrayCode.New =>
          TYPECASE args[1] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(1, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF int1 < 0 THEN
            ObValue.BadArgVal(
              1, "non-negative", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          vals := NEW(Vals, int1);
          FOR i := 0 TO int1 - 1 DO vals^[i] := args[2]; END;
          RETURN ObValue.NewArrayFromVals(vals);
      | ArrayCode.Gen =>
          TYPECASE args[1] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(1, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValFun (node) => clos1 := node;
          ELSE
            ObValue.BadArgType(1, "procedure", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          IF int1 < 0 THEN
            ObValue.BadArgVal(
              1, "non-negative", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          vals := NEW(Vals, int1);
          FOR i := 0 TO int1 - 1 DO
            vals^[i] :=
              ObEval.Call(clos1, ObValue.Vals{NEW(ObValue.ValInt, int := i,
                                                  temp := FALSE)}, loc);
          END;
          RETURN ObValue.NewArrayFromVals(vals);
      | ArrayCode.Size =>
          TYPECASE args[1] OF
          | ObValue.ValArray (node) =>
              RETURN NEW(ObValue.ValInt, int := node.remote.Size(),
                         temp := temp);
          ELSE
            ObValue.BadArgType(1, "array", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
      | ArrayCode.Get =>
          TYPECASE args[2] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(2, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[1] OF
          | ObValue.ValArray (node) =>
              badOp := 2;
              RETURN node.remote.Get(int1);
          ELSE
            ObValue.BadArgType(1, "array", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
      | ArrayCode.Set =>
          TYPECASE args[1] OF
          | ObValue.ValArray (node) => rem1 := node.remote;
          ELSE
            ObValue.BadArgType(1, "array", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(2, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          rem1.Set(int1, args[3]);
          badOp := 2;
          RETURN ObValue.valOk;
      | ArrayCode.Sub =>
          TYPECASE args[2] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(2, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValInt (node) => int2 := node.int;
          ELSE
            ObValue.BadArgType(3, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[1] OF
          | ObValue.ValArray (node) =>
              badOp := 3;
              RETURN node.remote.Sub(int1, int2);
          ELSE
            ObValue.BadArgType(1, "array", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
      | ArrayCode.Upd =>
          TYPECASE args[1] OF
          | ObValue.ValArray (node) => rem1 := node.remote;
          ELSE
            ObValue.BadArgType(1, "array", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(2, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValInt (node) => int2 := node.int;
          ELSE
            ObValue.BadArgType(3, "int", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValArray (node) => array1 := node.remote.Obtain();
          ELSE
            ObValue.BadArgType(4, "array", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          badOp := 3;
          rem1.Upd(int1, int2, array1);
          RETURN ObValue.valOk;
      | ArrayCode.Cat =>
          TYPECASE args[1] OF
          | ObValue.ValArray (node) => array1 := node.remote.Obtain();
          ELSE
            ObValue.BadArgType(1, "array", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValArray (node) => array2 := node.remote.Obtain();
          ELSE
            ObValue.BadArgType(2, "array", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          badOp := 1;
          RETURN ObValue.ArrayCat(array1, array2);
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc);
        <*ASSERT FALSE*>
      END;
    EXCEPT
    | ObValue.ServerError =>
        ObValue.BadArgVal(badOp, "in range", self.name, opCode.name, loc);
      <*ASSERT FALSE*>
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException(
          self.name & "_" & opCode.name, atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, self.name & "_" & opCode.name, loc);
      <*ASSERT FALSE*>
    END;
  END EvalArray;

(* ============ "net" package ============ *)

TYPE

  NetCode = {Error, Who, Export, Import, ExportEngine, ImportEngine};

  NetOpCode = ObLib.OpCode OBJECT code: NetCode;  END;

  PackageNet = ObLib.T OBJECT OVERRIDES Eval := EvalNet; END;

PROCEDURE NewNetOC (name: TEXT; arity: INTEGER; code: NetCode): NetOpCode =
  BEGIN
    RETURN NEW(NetOpCode, name := name, arity := arity, code := code);
  END NewNetOC;

PROCEDURE SetupNet () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(NetCode));
    opCodes^ := OpCodes{NewNetOC("failure", -1, NetCode.Error),
                        NewNetOC("who", 1, NetCode.Who),
                        NewNetOC("export", 3, NetCode.Export),
                        NewNetOC("import", 2, NetCode.Import),
                        NewNetOC("exportEngine", 3, NetCode.ExportEngine),
                        NewNetOC("importEngine", 2, NetCode.ImportEngine)};
    ObLib.Register(NEW(PackageNet, name := "net", opCodes := opCodes));
  END SetupNet;

PROCEDURE EvalNet (                    self  : PackageNet;
                                       opCode: ObLib.OpCode;
                   <*UNUSED*>          arity : ObLib.OpArity;
                              READONLY args  : ObValue.ArgArray;
                   <*UNUSED*>          temp  : BOOLEAN;
                                       loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR text1, text2: TEXT;
  BEGIN
    CASE NARROW(opCode, NetOpCode).code OF
    | NetCode.Error => RETURN ObValue.netException;
    | NetCode.Who =>
        TYPECASE args[1] OF
        | ObValue.ValObj (node) => RETURN NetObjectWho(node, loc);
        | ObValue.ValEngine (node) =>
            RETURN NetEngineWho(node.remote, loc);
        ELSE
          ObValue.BadArgType(
            1, "object or engine", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
    | NetCode.Export =>
        TYPECASE args[1] OF
        | ObValue.ValText (node) => text1 := node.text;
        ELSE
          ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValText (node) => text2 := node.text;
        ELSE
          ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TYPECASE args[3] OF
        | ObValue.ValRemObj, ObValue.ValReplObj, ObValue.ValSimpleObj =>
        ELSE
          ObValue.BadArgType(3, "object", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        NetExport(text1, text2, args[3], loc);
        RETURN args[3];
    | NetCode.Import =>
        TYPECASE args[1] OF
        | ObValue.ValText (node) => text1 := node.text;
        ELSE
          ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValText (node) => text2 := node.text;
        ELSE
          ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN NetImport(text1, text2, loc);
    | NetCode.ExportEngine =>
        TYPECASE args[1] OF
        | ObValue.ValText (node) => text1 := node.text;
        ELSE
          ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValText (node) => text2 := node.text;
        ELSE
          ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        NetExportEngine(text1, text2, args[3], loc);
        RETURN ObValue.valOk;
    | NetCode.ImportEngine =>
        TYPECASE args[1] OF
        | ObValue.ValText (node) => text1 := node.text;
        ELSE
          ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValText (node) => text2 := node.text;
        ELSE
          ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN NetImportEngine(text1, text2, loc);
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc);
      <*ASSERT FALSE*>
    END;
  END EvalNet;

PROCEDURE NetLocate (            server    : TEXT;
                     VAR (*out*) address   : TEXT;
                     VAR (*out*) netAddress: NetObj.Address;
                                 location  : SynLocation.T   )
  RAISES {ObValue.Exception} =
  BEGIN
    IF Text.Empty(server) THEN
      address := ObValue.machineAddress;
      netAddress := NIL;
    ELSE
      address := server;
      TRY
        netAddress := NetObj.Locate(address);
      EXCEPT
      | NetObj.Invalid, NetObj.Error =>
          ObValue.RaiseNetException("Could not locate name server for '"
                                      & address & "'", NIL, location);
      | Thread.Alerted =>
          ObValue.RaiseException(
            ObValue.threadAlerted, "net_locate", location);
      END;
    END;
  END NetLocate;

PROCEDURE NetObjectWho (valObj: ObValue.ValObj; loc: SynLocation.T):
  ObValue.Val RAISES {ObValue.Exception} =
  VAR protected, serialized: BOOLEAN;
  BEGIN
    TRY
      RETURN
        ObValue.NewText(valObj.Who( (*out*)protected, (*out*) serialized));
    EXCEPT
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException("net_who", atoms, loc);
      <*ASSERT FALSE*>
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException("net_who", atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(ObValue.threadAlerted, "net_who", loc);
      <*ASSERT FALSE*>
    END;
  END NetObjectWho;

PROCEDURE NetEngineWho (remObj: ObValue.RemEngine; loc: SynLocation.T):
  ObValue.Val RAISES {ObValue.Exception} =
  BEGIN
    TRY
      RETURN ObValue.NewText(remObj.Who());
    EXCEPT
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException("net_who", atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(ObValue.threadAlerted, "net_who", loc);
      <*ASSERT FALSE*>
    END;
  END NetEngineWho;

PROCEDURE NetExport (name, server: TEXT;
                     valObj      : ObValue.ValObj;
                     loc         : SynLocation.T   )
  RAISES {ObValue.Exception} =
  VAR
    address   : TEXT;
    netAddress: NetObj.Address;
  BEGIN
    NetLocate(server, (*out*) address, (*out*) netAddress, loc);
    TRY
      TYPECASE valObj OF
      | ObValue.ValRemObj (o) => NetObj.Export(name, o.remote, netAddress);
      | ObValue.ValReplObj (o) =>
          NetObj.Export(
            name, NEW(ObValue.NonRemObjHookServer).init(o), netAddress);
      | ObValue.ValSimpleObj (o) =>
          NetObj.Export(
            name, NEW(ObValue.NonRemObjHookServer).init(o), netAddress);
      ELSE                       <*ASSERT FALSE*>
      END;
    EXCEPT
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException(
          "net_export: '" & name & "' at '" & address & "'", atoms, loc);
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted,
          "net_export: '" & name & "' at '" & address & "'", loc);
    END;
    TYPECASE valObj OF
    | ObValue.ValRemObj (remObj) =>
        TYPECASE remObj.remote OF
        | ObValue.RemObjServer (serv) =>
            IF Text.Empty(serv.who) THEN
              serv.who := name & "@" & address;
            END;
        ELSE
        END;
    ELSE
    END;
  END NetExport;

PROCEDURE NetImport (name, server: TEXT; loc: SynLocation.T): ObValue.Val
  RAISES {ObValue.Exception} =
  VAR
    address   : TEXT;
    netAddress: NetObj.Address;
    netObj    : NetObj.T;
  BEGIN
    NetLocate(server, (*out*) address, (*out*) netAddress, loc);
    TRY
      netObj := NetObj.Import(name, netAddress);
    EXCEPT
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException(
          "net_import: '" & name & "' at '" & address & "'", atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted,
          "net_import: '" & name & "' at '" & address & "'", loc);
      <*ASSERT FALSE*>
    END;
    IF netObj = NIL THEN
      ObValue.RaiseException(
        ObValue.netException,
        "net_import: '" & name & "' was not found at '" & address & "'",
        loc);
      <*ASSERT FALSE*>
    END;
    TYPECASE netObj OF
    | ObValue.RemObj (remObj) =>
        RETURN NEW(ObValue.ValRemObj, remote := remObj);
    | ObValue.NonRemObjHook (hook) =>
        TRY
          RETURN hook.Get();
        EXCEPT
        | NetObj.Error (atoms) =>
            ObValue.RaiseNetException("net_import: '" & name & "' at '"
                                        & address & "'", atoms, loc);
          <*ASSERT FALSE*>
        | Thread.Alerted =>
            ObValue.RaiseException(
              ObValue.threadAlerted,
              "net_import: '" & name & "' at '" & address & "'", loc);
          <*ASSERT FALSE*>
        END;
    ELSE
      ObValue.RaiseException(
        ObValue.netException, "net_import failed: '" & name & "' at '"
                                & address & "' is not a network object",
        loc);
      <*ASSERT FALSE*>
    END;
  END NetImport;

PROCEDURE NetExportEngine (name, server: TEXT;
                           arg         : ObValue.Val;
                           loc         : SynLocation.T)
  RAISES {ObValue.Exception} =
  VAR
    address   : TEXT;
    netAddress: NetObj.Address;
    remEngine : ObValue.RemEngine;
  BEGIN
    NetLocate(server, (*out*) address, (*out*) netAddress, loc);
    remEngine := NEW(ObValue.RemEngineServer, who := name & "@" & address,
                     arg := arg);
    TRY
      NetObj.Export(name, remEngine, netAddress);
    EXCEPT
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException("net_exportEngine: '" & name & "' at '"
                                    & address & "'", atoms, loc);
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted,
          "net_exportEngine: '" & name & "' at '" & address & "'", loc);
    END;
  END NetExportEngine;

PROCEDURE NetImportEngine (name, server: TEXT; loc: SynLocation.T):
  ObValue.Val RAISES {ObValue.Exception} =
  VAR
    address   : TEXT;
    netAddress: NetObj.Address;
    netObj    : NetObj.T;
  BEGIN
    NetLocate(server, (*out*) address, (*out*) netAddress, loc);
    TRY
      netObj := NetObj.Import(name, netAddress);
    EXCEPT
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException("net_importEngine: '" & name & "' at '"
                                    & address & "'", atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted,
          "net_importEngine: '" & name & "' at '" & address & "'", loc);
      <*ASSERT FALSE*>
    END;
    IF netObj = NIL THEN
      ObValue.RaiseException(
        ObValue.netException, "net_importEngine: '" & name
                                & "' was not found at '" & address & "'",
        loc);
      <*ASSERT FALSE*>
    END;
    TYPECASE netObj OF
    | ObValue.RemEngine (remEngine) =>
        RETURN NEW(ObValue.ValEngine, remote := remEngine);
    ELSE
      ObValue.RaiseException(
        ObValue.netException,
        "net_importEngine failed: '" & name & "' at '" & address
          & "' is not a network engine", loc);
      <*ASSERT FALSE*>
    END;
  END NetImportEngine;

(* ============ "replica" package ============ *)

TYPE
  ReplicaCode =
    {Error, Fatal, AcquireGlobalLock, ReleaseGlobalLock, SetNodeName,
     SetDefaultSequencer, Notify, CancelNotifier, DumpState};

  ReplicaOpCode = ObLib.OpCode OBJECT code: ReplicaCode;  END;

  PackageReplica = ObLib.T OBJECT OVERRIDES Eval := EvalReplica; END;

PROCEDURE NewReplicaOC (name  : TEXT;
                        arity : INTEGER;
                        code  : ReplicaCode;
                        fixity: ObLib.OpFixity := ObLib.OpFixity.Qualified):
  ReplicaOpCode =
  BEGIN
    RETURN NEW(ReplicaOpCode, name := name, arity := arity, code := code,
               fixity := fixity);
  END NewReplicaOC;

PROCEDURE SetupReplica () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(ReplicaCode));
    opCodes^ :=
      OpCodes{
        NewReplicaOC("failure", -1, ReplicaCode.Error),
        NewReplicaOC("fatal", -1, ReplicaCode.Fatal),
        NewReplicaOC("acquire", 1, ReplicaCode.AcquireGlobalLock),
        NewReplicaOC("release", 1, ReplicaCode.ReleaseGlobalLock),
        NewReplicaOC("setNodeName", 1, ReplicaCode.SetNodeName),
        NewReplicaOC("notify", 2, ReplicaCode.Notify),
        NewReplicaOC("cancelNotifier", 1, ReplicaCode.CancelNotifier),
        NewReplicaOC(
          "setDefaultSequencer", 2, ReplicaCode.SetDefaultSequencer),
        NewReplicaOC("dumpState", 0, ReplicaCode.DumpState)};
    ObLib.Register(
      NEW(PackageReplica, name := "replica", opCodes := opCodes));
  END SetupReplica;

PROCEDURE EvalReplica (                    self  : PackageReplica;
                                           opCode: ObLib.OpCode;
                       <*UNUSED*>          arity : ObLib.OpArity;
                                  READONLY args  : ObValue.ArgArray;
                       <*UNUSED*>          temp  : BOOLEAN;
                                           loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR text1, text2: TEXT;
  BEGIN
    CASE NARROW(opCode, ReplicaOpCode).code OF
    | ReplicaCode.Error => RETURN ObValue.sharedException;
    | ReplicaCode.Fatal => RETURN ObValue.sharedFatal;
    | ReplicaCode.Notify => RETURN ReplicaNotify(args[1], args[2], loc);
    | ReplicaCode.CancelNotifier =>
        ReplicaCancelNotifier(args[1], loc);
        RETURN ObValue.valOk;
    | ReplicaCode.DumpState => ReplicaDumpState(loc); RETURN ObValue.valOk;
    | ReplicaCode.AcquireGlobalLock =>
        TYPECASE args[1] OF
        | ObValue.ValReplObj =>
        ELSE
          ObValue.BadArgType(
            1, "replicated object", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN ReplicaAcquireLock(args[1], loc);
    | ReplicaCode.ReleaseGlobalLock =>
        TYPECASE args[1] OF
        | ObValue.ValReplObj =>
        ELSE
          ObValue.BadArgType(
            1, "replicated object", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN ReplicaReleaseLock(args[1], loc);
    | ReplicaCode.SetNodeName =>
        TYPECASE args[1] OF
        | ObValue.ValText (node) => text1 := node.text;
        ELSE
          ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN ReplicaSetNodeName(text1, loc);
    | ReplicaCode.SetDefaultSequencer =>
        TYPECASE args[1] OF
        | ObValue.ValText (node) => text1 := node.text;
        ELSE
          ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValText (node) => text2 := node.text;
        ELSE
          ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN ReplicaSetDefaultSequencer(text1, text2, loc);
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc);
      <*ASSERT FALSE*>
    END;
  END EvalReplica;

PROCEDURE ReplicaNotify (valObj   : ObValue.Val;
                         notifyObj: ObValue.ValObj;
                         loc      : SynLocation.T   ): ObValue.Val
  RAISES {ObValue.Exception} =
  BEGIN
    TYPECASE valObj OF
    | ObValue.ValReplObj (obj) =>
        TYPECASE notifyObj OF
        | ObValue.ValSimpleObj (notifier) =>
            RETURN ObValueNotify.New(obj, notifier, loc);
        ELSE
          ObValue.RaiseException(
            ObValue.sharedException,
            "replica_notify failed: '"
              & "second argument must be a simple object", loc);
          <*ASSERT FALSE*>
        END;
    ELSE
      ObValue.RaiseException(
        ObValue.sharedException,
        "replica_notify failed: '"
          & "first argument must be a replicated object", loc);
      <*ASSERT FALSE*>
    END;
  END ReplicaNotify;

PROCEDURE ReplicaCancelNotifier (notifier: ObValue.Val; loc: SynLocation.T)
  RAISES {ObValue.Exception} =
  BEGIN
    TYPECASE notifier OF
    | ObValueNotify.ValObjCB (obj) => obj.cancel();
    ELSE
      ObValue.RaiseException(
        ObValue.sharedException,
        "replica_cancelNotifier failed: '"
          & "first argument must be a notifier callback", loc);
      <*ASSERT FALSE*>
    END;
  END ReplicaCancelNotifier;

PROCEDURE ReplicaAcquireLock (valObj: ObValue.ValObj; loc: SynLocation.T):
  ObValue.Val RAISES {ObValue.Exception} =
  BEGIN
    TRY
      TYPECASE valObj OF
      | ObValue.ValReplObj (obj) =>
          SharedObj.AcquireGlobalLock(obj.replica);
      ELSE
        ObValue.RaiseException(
          ObValue.sharedException,
          "replica_acquire failed: '"
            & "argument must be a replicated object", loc);
        <*ASSERT FALSE*>
      END;
    EXCEPT
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException("replica_acquire: ", atoms, loc);
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, "replica_acquire: ", loc);
    END;
    RETURN ObValue.valOk;
  END ReplicaAcquireLock;

PROCEDURE ReplicaReleaseLock (valObj: ObValue.ValObj; loc: SynLocation.T):
  ObValue.Val RAISES {ObValue.Exception} =
  BEGIN
    TRY
      TYPECASE valObj OF
      | ObValue.ValReplObj (obj) =>
          SharedObj.ReleaseGlobalLock(obj.replica);
      ELSE
        ObValue.RaiseException(
          ObValue.sharedException,
          "replica_release failed: '"
            & "argument must be a replicated object", loc);
        <*ASSERT FALSE*>
      END;
    EXCEPT
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException("replica_release: ", atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, "replica_release: ", loc);
      <*ASSERT FALSE*>
    END;
    RETURN ObValue.valOk;
  END ReplicaReleaseLock;

PROCEDURE ReplicaSetNodeName (name: TEXT; loc: SynLocation.T): ObValue.Val
  RAISES {ObValue.Exception} =
  BEGIN
    TRY
      IF Text.Equal(name, "") THEN name := ObValue.machineAddress; END;
      SharedObjRT.ExportSpace(name);
      RETURN ObValue.NewText(name);
    EXCEPT
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException("replica_setNodeName: ", atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, "replica_setNodeName: ", loc);
      <*ASSERT FALSE*>
    END;
  END ReplicaSetNodeName;

PROCEDURE ReplicaDumpState (loc: SynLocation.T)
  RAISES {ObValue.Exception} =
  BEGIN
    TRY
      SharedObjRT.LocalSpace().printState();
    EXCEPT
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException("replica_dumpState: ", atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, "replica_setNodeName: ", loc);
      <*ASSERT FALSE*>
    END;
  END ReplicaDumpState;

PROCEDURE ReplicaSetDefaultSequencer (host, name: TEXT; loc: SynLocation.T):
  ObValue.Val RAISES {ObValue.Exception} =
  VAR space: ObjectSpace.T;
  BEGIN
    TRY
      IF Text.Equal("", host) THEN
        WITH defhost = Env.Get("SEQUENCERHOST") DO
          IF defhost # NIL THEN host := defhost; END;
        END;
      END;
      IF Text.Equal("", name) THEN
        WITH defname = Env.Get("SEQUENCERNAME") DO
          IF defname # NIL THEN name := defname; END;
        END;
      END;

      IF NOT Text.Equal("", host) OR NOT Text.Equal("", name) THEN
        space := SharedObjRT.ImportSpace(host, name);
        IF space = NIL THEN
          ObValue.RaiseException(
            ObValue.sharedException,
            "replica_setDefaultSequencer: node " & name & "@" & host
              & " is unavailable", loc);
          <*ASSERT FALSE*>
        END;
      ELSE
        space := SharedObjRT.LocalSpace();
      END;
      SharedObjRT.SetDfltSequencer(space);
      RETURN ObValue.valOk;
    EXCEPT
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException(
          "replica_setDefaultSequencer: ", atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, "replica_setDefaultSequencer: ", loc);
      <*ASSERT FALSE*>
    END;
  END ReplicaSetDefaultSequencer;

(* ============ "thread" package ============ *)

TYPE

  ThreadCode = {Alerted, NewMutex, NewCondition, Self, Fork, Join, Wait,
                Acquire, Release, Broadcast, Signal, Pause, Alert,
                TestAlert, AlertWait, AlertJoin, AlertPause, Lock, 
                NewPool, AddWork, StealWorker, FinishWork};

  ThreadOpCode = ObLib.OpCode OBJECT code: ThreadCode;  END;

  PackageThread = ObLib.T OBJECT OVERRIDES Eval := EvalThread; END;

PROCEDURE IsMutex (self: ValMutex; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValMutex (oth) => RETURN self.mutex = oth.mutex;
    ELSE
      RETURN FALSE
    END;
  END IsMutex;

PROCEDURE IsCondition (self: ValCondition; other: ObValue.ValAnything):
  BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValCondition (oth) => RETURN self.condition = oth.condition;
    ELSE
      RETURN FALSE
    END;
  END IsCondition;

PROCEDURE IsThread (self: ValThread; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValThread (oth) => RETURN self.thread = oth.thread;
    ELSE
      RETURN FALSE
    END;
  END IsThread;

PROCEDURE IsPool (self: ValPool; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValPool (oth) => RETURN self.pool = oth.pool;
    ELSE
      RETURN FALSE
    END;
  END IsPool;

PROCEDURE CopyMutex (<*UNUSED*> self: ObValue.ValAnything;
                     <*UNUSED*> tbl : ObValue.Tbl;
                     <*UNUSED*> loc : SynLocation.T        ):
  ObValue.ValAnything =
  BEGIN
    RETURN NEW(ValMutex, what := "<a Thread.Mutex>", picklable := FALSE,
               tag:= "Thread`Mutex",
               mutex := NEW(Thread.Mutex));
  END CopyMutex;

PROCEDURE CopyCondition (<*UNUSED*> self: ObValue.ValAnything;
                         <*UNUSED*> tbl : ObValue.Tbl;
                         <*UNUSED*> loc : SynLocation.T        ):
  ObValue.ValAnything =
  BEGIN
    RETURN NEW(ValCondition, what := "<a Thread.Condition>",
               tag:="Thread`Condition",
               picklable := FALSE, condition := NEW(Thread.Condition));
  END CopyCondition;

TYPE
  ObliqThreadClosure = Thread.SizedClosure OBJECT
                         fun      : ObValue.ValFun;
                         location : SynLocation.T;
                         result   : ObValue.Val;
                         error    : ObValue.ErrorPacket;
                         exception: ObValue.ExceptionPacket;
                       OVERRIDES
                         apply := ApplyThreadClosure;
                       END;

TYPE
  ObliqWork = Work.T OBJECT
                         fun      : ObValue.ValFun;
                         location : SynLocation.T;
                       OVERRIDES
                         handle := HandleWork;
                       END;

PROCEDURE Msg (swr           : SynWr.T;
               msg           : TEXT;
               sourceLocation: SynLocation.T) =
  BEGIN
    SynWr.Beg(swr, 2, loud := TRUE);
    SynWr.Text(swr, msg, loud := TRUE);
    SynLocation.PrintLocation(swr, sourceLocation);
    SynWr.End(swr, loud := TRUE);
    SynWr.NewLine(swr, loud := TRUE);
    SynWr.Flush(swr, loud := TRUE);
  END Msg;

PROCEDURE ApplyThreadClosure (self: ObliqThreadClosure): REFANY =
  VAR noArgs: ARRAY [0 .. -1] OF ObValue.Val;
  BEGIN
    TRY
      self.result := ObEval.Call(self.fun, noArgs, self.location);
    EXCEPT
    | ObValue.Error (packet) =>
        self.error := packet;
        ObValue.ErrorMsg(SynWr.err, packet);
        Msg(SynWr.err, "<Thread.T> terminated by execution error",
            self.location); 
    | ObValue.Exception (packet) =>
        self.exception := packet;
        ObValue.ExceptionMsg(SynWr.err, packet);
        Msg(SynWr.err, "<Thread.T> terminated by unhandled exception",
            self.location); 
    END;
    RETURN self;
  END ApplyThreadClosure;

PROCEDURE HandleWork (self: ObliqWork) =
  VAR noArgs: ARRAY [0 .. -1] OF ObValue.Val;
  BEGIN
    TRY
      EVAL ObEval.Call(self.fun, noArgs, self.location);
    EXCEPT
    | ObValue.Error (packet) =>
        ObValue.ErrorMsg(SynWr.err, packet);
        Msg(SynWr.err, "<work> terminated by execution error",
            self.location); 
    | ObValue.Exception (packet) =>
        ObValue.ExceptionMsg(SynWr.err, packet);
        Msg(SynWr.err, "<work> terminated by unhandled exception",
            self.location); 
    END;
  END HandleWork;

PROCEDURE NewPool (maxThreads, idleThreads, stackSize: INTEGER): ValPool =
  VAR
    pool       : WorkerPool.T;
  BEGIN
    stackSize := MIN(MAX(stackSize, 4096), LAST(CARDINAL));
    pool := NEW(WorkerPool.T).init(maxThreads, idleThreads, stackSize);
    RETURN NEW(ValPool, what := "<a WorkerPool.T>", picklable := FALSE,
               tag:="Workerpool`T",
               pool := pool);
  END NewPool; 

PROCEDURE ForkThread (fun      : ObValue.ValFun;
                      stackSize: INTEGER;
                      loc      : SynLocation.T   ): ValThread =
  VAR
    thread       : Thread.T;
    threadClosure: ObliqThreadClosure;
  BEGIN
    stackSize := MIN(MAX(stackSize, 4096), LAST(CARDINAL));
    threadClosure :=
      NEW(ObliqThreadClosure, stackSize := stackSize, fun := fun,
          location := loc, result := NIL, error := NIL, exception := NIL);
    thread := Thread.Fork(threadClosure);
    RETURN NEW(ValThread, what := "<a Thread.T>", picklable := FALSE,
               tag:="Thread`T",
               thread := thread, joinedMu := NEW(Thread.Mutex),
               joined := FALSE);
  END ForkThread;

PROCEDURE JoinThread (threadVal: ValThread; loc: SynLocation.T):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR threadClosure: ObliqThreadClosure;
  BEGIN
    LOCK threadVal.joinedMu DO
      IF threadVal.joined THEN
        ObValue.RaiseError("Thread already joined", loc);
      ELSE
        threadVal.joined := TRUE;
      END;
    END;
    threadClosure := Thread.Join(threadVal.thread);
    IF threadClosure.error # NIL THEN
      RAISE ObValue.Error(threadClosure.error);
    ELSIF threadClosure.exception # NIL THEN
      RAISE ObValue.Exception(threadClosure.exception);
    ELSE
      RETURN threadClosure.result;
    END;
  END JoinThread;

PROCEDURE NewThreadOC (name  : TEXT;
                       arity : INTEGER;
                       code  : ThreadCode;
                       fixity: ObLib.OpFixity := ObLib.OpFixity.Qualified):
  ThreadOpCode =
  BEGIN
    RETURN NEW(ThreadOpCode, name := name, arity := arity, code := code,
               fixity := fixity);
  END NewThreadOC;

PROCEDURE SetupThread () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(ThreadCode));
    opCodes^ :=
      OpCodes{NewThreadOC("alerted", -1, ThreadCode.Alerted),
              NewThreadOC(
                "mutex", 0, ThreadCode.NewMutex, ObLib.OpFixity.Prefix),
              NewThreadOC("condition", 0, ThreadCode.NewCondition,
                          ObLib.OpFixity.Prefix),
              NewThreadOC("self", 0, ThreadCode.Self),
              NewThreadOC(
                "fork", 2, ThreadCode.Fork, ObLib.OpFixity.Prefix),
              NewThreadOC(
                "join", 1, ThreadCode.Join, ObLib.OpFixity.Prefix),
              NewThreadOC(
                "wait", 2, ThreadCode.Wait, ObLib.OpFixity.Prefix),
              NewThreadOC("acquire", 1, ThreadCode.Acquire),
              NewThreadOC("release", 1, ThreadCode.Release),
              NewThreadOC("broadcast", 1, ThreadCode.Broadcast,
                          ObLib.OpFixity.Prefix),
              NewThreadOC(
                "signal", 1, ThreadCode.Signal, ObLib.OpFixity.Prefix),
              NewThreadOC(
                "pause", 1, ThreadCode.Pause, ObLib.OpFixity.Prefix),
              NewThreadOC("alert", 1, ThreadCode.Alert),
              NewThreadOC("testAlert", 0, ThreadCode.TestAlert),
              NewThreadOC("alertWait", 2, ThreadCode.AlertWait),
              NewThreadOC("alertJoin", 1, ThreadCode.AlertJoin),
              NewThreadOC("alertPause", 1, ThreadCode.AlertPause),
              NewThreadOC("pool", 3, ThreadCode.NewPool),
              NewThreadOC("addWork", 2, ThreadCode.AddWork),
              NewThreadOC("stealWorker", 1, ThreadCode.StealWorker),
              NewThreadOC("finish", 1, ThreadCode.FinishWork),
              NewThreadOC("lock", 2, ThreadCode.Lock)};
    ObLib.Register(
      NEW(PackageThread, name := "thread", opCodes := opCodes));
    ObValue.InhibitTransmission(
      TYPECODE(ValMutex), "mutexes cannot be transmitted/duplicated");
    ObValue.InhibitTransmission(
      TYPECODE(ValCondition), "conditions cannot be transmitted/duplicated");
    ObValue.InhibitTransmission(
      TYPECODE(ValThread), "threads cannot be transmitted/duplicated");
  END SetupThread;

PROCEDURE EvalThread (                    self  : PackageThread;
                                          opCode: ObLib.OpCode;
                      <*UNUSED*>          arity : ObLib.OpArity;
                                 READONLY args  : ObValue.ArgArray;
                      <*UNUSED*>          temp  : BOOLEAN;
                                          loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    thread1   : Thread.T;
    threadVal1: ValThread;
    fun1      : ObValue.ValFun;
    mutex1    : Thread.Mutex;
    condition1: Thread.Condition;
    longReal1 : LONGREAL;
    int1,int2,int3: INTEGER;
    noArgs    : ARRAY [0 .. -1] OF ObValue.Val;
    pool1     : WorkerPool.T;
  BEGIN
    CASE NARROW(opCode, ThreadOpCode).code OF
    | ThreadCode.Alerted => RETURN ObValue.threadAlerted;
    | ThreadCode.NewMutex =>
        mutex1 := NEW(Thread.Mutex);
        RETURN NEW(ValMutex, what := "<a Thread.Mutex>",
                   tag:="Thread`Mutex",
                   picklable := FALSE, mutex := mutex1);
    | ThreadCode.NewCondition =>
        condition1 := NEW(Thread.Condition);
        RETURN NEW(ValCondition, what := "<a Thread.Condition>",
                   tag:="Thread`Condition",
                   picklable := FALSE, condition := condition1);
    | ThreadCode.Self =>
        thread1 := Thread.Self();
        RETURN NEW(ValThread, what := "<a Thread.T>", picklable := FALSE,
                   tag:="Thread`T",
                   thread := thread1, joinedMu := NEW(Thread.Mutex),
                   joined := FALSE);
    | ThreadCode.Fork =>
        TYPECASE args[1] OF
        | ObValue.ValFun (node) => fun1 := node;
        ELSE
          ObValue.BadArgType(1, "procedure", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValInt (node) => int1 := node.int;
        ELSE
          ObValue.BadArgType(2, "int", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN ForkThread(fun1, int1, loc);
    | ThreadCode.Join =>
        TYPECASE args[1] OF
        | ValThread (node) => threadVal1 := node;
        ELSE
          ObValue.BadArgType(1, "thread", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN JoinThread(threadVal1, loc);
    | ThreadCode.Wait =>
        TYPECASE args[1] OF
        | ValMutex (node) => mutex1 := node.mutex;
        ELSE
          ObValue.BadArgType(1, "mutex", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ValCondition (node) => condition1 := node.condition;
        ELSE
          ObValue.BadArgType(2, "condition", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        Thread.Wait(mutex1, condition1);
        RETURN ObValue.valOk;
    | ThreadCode.Acquire =>
        TYPECASE args[1] OF
        | ValMutex (node) => mutex1 := node.mutex;
        ELSE
          ObValue.BadArgType(1, "mutex", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        Thread.Acquire(mutex1);
        RETURN ObValue.valOk;
    | ThreadCode.Release =>
        TYPECASE args[1] OF
        | ValMutex (node) => mutex1 := node.mutex;
        ELSE
          ObValue.BadArgType(1, "mutex", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        Thread.Release(mutex1);
        RETURN ObValue.valOk;
    | ThreadCode.Broadcast =>
        TYPECASE args[1] OF
        | ValCondition (node) => condition1 := node.condition;
        ELSE
          ObValue.BadArgType(1, "condition", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        Thread.Broadcast(condition1);
        RETURN ObValue.valOk;
    | ThreadCode.Signal =>
        TYPECASE args[1] OF
        | ValCondition (node) => condition1 := node.condition;
        ELSE
          ObValue.BadArgType(1, "condition", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        Thread.Signal(condition1);
        RETURN ObValue.valOk;
    | ThreadCode.Pause =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) => longReal1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        IF longReal1 < 0.0d0 THEN
          ObValue.BadArgVal(1, "non-negative", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        Thread.Pause(longReal1);
        RETURN ObValue.valOk;
    | ThreadCode.Alert =>
        TYPECASE args[1] OF
        | ValThread (node) => thread1 := node.thread;
        ELSE
          ObValue.BadArgType(1, "thread", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        Thread.Alert(thread1);
        RETURN ObValue.valOk;
    | ThreadCode.TestAlert =>
        IF Thread.TestAlert() THEN RETURN true ELSE RETURN false END;
    | ThreadCode.AlertJoin =>
        TYPECASE args[1] OF
        | ValThread (node) => thread1 := node.thread;
        ELSE
          ObValue.BadArgType(1, "thread", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TRY
          RETURN Thread.AlertJoin(thread1);
        EXCEPT
          Thread.Alerted =>
            ObValue.RaiseException(ObValue.threadAlerted, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
    | ThreadCode.AlertWait =>
        TYPECASE args[1] OF
        | ValMutex (node) => mutex1 := node.mutex;
        ELSE
          ObValue.BadArgType(1, "mutex", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ValCondition (node) => condition1 := node.condition;
        ELSE
          ObValue.BadArgType(2, "condition", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TRY
          Thread.AlertWait(mutex1, condition1);
          RETURN ObValue.valOk;
        EXCEPT
          Thread.Alerted =>
            ObValue.RaiseException(ObValue.threadAlerted, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
    | ThreadCode.AlertPause =>
        TYPECASE args[1] OF
        | ObValue.ValReal (node) => longReal1 := node.real;
        ELSE
          ObValue.BadArgType(1, "real", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        IF longReal1 < 0.0d0 THEN
          ObValue.BadArgVal(1, "non-negative", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TRY
          Thread.AlertPause(longReal1);
          RETURN ObValue.valOk;
        EXCEPT
          Thread.Alerted =>
            ObValue.RaiseException(ObValue.threadAlerted, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
    | ThreadCode.Lock =>
        TYPECASE args[1] OF
        | ValMutex (node) => mutex1 := node.mutex;
        ELSE
          ObValue.BadArgType(1, "mutex", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValFun (node) => fun1 := node;
        ELSE
          ObValue.BadArgType(2, "procedure", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        LOCK mutex1 DO RETURN ObEval.Call(fun1, noArgs, loc) END;
    | ThreadCode.NewPool =>
        TYPECASE args[1] OF
        | ObValue.ValInt (node) => int1 := node.int;
        ELSE
          ObValue.BadArgType(1, "int", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValInt (node) => int2 := node.int;
        ELSE
          ObValue.BadArgType(2, "int", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TYPECASE args[3] OF
        | ObValue.ValInt (node) => int3 := node.int;
        ELSE
          ObValue.BadArgType(3, "int", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN NewPool(int1, int2, int3);
    | ThreadCode.AddWork =>
        TYPECASE args[1] OF
        | ValPool (node) => pool1 := node.pool;
        ELSE
          ObValue.BadArgType(1, "pool", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValFun (node) => fun1 := node;
        ELSE
          ObValue.BadArgType(2, "procedure", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        pool1.add(NEW(ObliqWork, fun := fun1, location := loc));
        RETURN ObValue.valOk;
    | ThreadCode.StealWorker =>
        TYPECASE args[1] OF
        | ValPool (node) => pool1 := node.pool;
        ELSE
          ObValue.BadArgType(1, "pool", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN NEW(ObValue.ValBool, bool := pool1.stealWorker());
    | ThreadCode.FinishWork =>
        TYPECASE args[1] OF
        | ValPool (node) => pool1 := node.pool;
        ELSE
          ObValue.BadArgType(1, "pool", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        pool1.finish();
        RETURN ObValue.valOk;
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc);
      <*ASSERT FALSE*>
    END;
  END EvalThread;

(*


(* ============ "reflect" package ============ *)

(* the possible option tags are:
   valOk, valBool, valChar, valText, valInt, valReal, valException,
   valOption, valVar, valArray, valMethod, valClosure, valAlias,
   valObject, 
   valAnything objects will have the ta
*)

TYPE
  ReflectCode =
    {Error, 
     isVar,             (* does the ide refer to an updatable value? *)

     isArray,           (* is the Val an Array? *)
     isObject,          (*            an Object? *)
     isPrimitive,       (*            a Primitive? *)
     isOpaque,          (* is this a opaque wrapper of an M3 value? *)

     isSimple,
     isReplicated,
     isRemote,
     objectType,        (* get an option describing which of the 3 it is *)

     isProtected,
     isSynchronized,
     isClosure,         (* is this value a closure? *)

     isCompatible,      (* does o1 have AT LEAST the same fields as o2? *)
                        (* the fields must be compatible (meth=meth,
                           non-meth=non-meth) and have the same number
                           of params, etc *)
     
     tagCompatible,     (* pass an object and an array of option
                           tagged objects, return the object option
                           tagged with the tag of the first array
                           element it is compatible with *)

     (* we do not allow method fields to be retrieved *)
     getType,           (* return an option describing the type *)
     getFields,         (* return an array of non-method fields of o *)
     getField,          (* return the named non-method field of o *)
     isAlias,           (* is the named slot an alias? *)
     
     getName,
     getValue,
     setValue,
     invoke,
     equals,
     toString};

  ReflectOpCode = ObLib.OpCode OBJECT code: ReflectCode;  END;

  PackageReflect = ObLib.T OBJECT OVERRIDES Eval := EvalReflect; END;

PROCEDURE NewReflectOC (name  : TEXT;
                        arity : INTEGER;
                        code  : ReflectCode;
                        fixity: ObLib.OpFixity := ObLib.OpFixity.Qualified):
  ReflectOpCode =
  BEGIN
    RETURN NEW(ReflectOpCode, name := name, arity := arity, code := code,
               fixity := fixity);
  END NewReflectOC;

PROCEDURE SetupReflect () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(ReflectCode));
    opCodes^ :=
      OpCodes{
        NewReflectOC("failure", -1, ReflectCode.Error),
        NewReflectOC("fatal", -1, ReflectCode.Fatal),
        NewReflectOC("acquire", 1, ReflectCode.AcquireGlobalLock),
        NewReflectOC("release", 1, ReflectCode.ReleaseGlobalLock),
        NewReflectOC("setNodeName", 1, ReflectCode.SetNodeName),
        NewReflectOC("notify", 2, ReflectCode.Notify),
        NewReflectOC("cancelNotifier", 1, ReflectCode.CancelNotifier),
        NewReflectOC(
          "setDefaultSequencer", 2, ReflectCode.SetDefaultSequencer),
        NewReflectOC("dumpState", 0, ReflectCode.DumpState)};
    ObLib.Register(
      NEW(PackageReflect, name := "reflect", opCodes := opCodes));
  END SetupReflect;

PROCEDURE EvalReflect (                    self  : PackageReflect;
                                           opCode: ObLib.OpCode;
                       <*UNUSED*>          arity : ObLib.OpArity;
                                  READONLY args  : ObValue.ArgArray;
                       <*UNUSED*>          temp  : BOOLEAN;
                                           loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR text1, text2: TEXT;
  BEGIN
    CASE NARROW(opCode, ReflectOpCode).code OF
    | ReflectCode.Error => RETURN ObValue.sharedException;
    | ReflectCode.Fatal => RETURN ObValue.sharedFatal;
    | ReflectCode.Notify => RETURN ReflectNotify(args[1], args[2], loc);
    | ReflectCode.CancelNotifier =>
        ReflectCancelNotifier(args[1], loc);
        RETURN ObValue.valOk;
    | ReflectCode.DumpState => ReflectDumpState(loc); RETURN ObValue.valOk;
    | ReflectCode.AcquireGlobalLock =>
        TYPECASE args[1] OF
        | ObValue.ValReplObj =>
        ELSE
          ObValue.BadArgType(
            1, "reflectted object", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN ReflectAcquireLock(args[1], loc);
    | ReflectCode.ReleaseGlobalLock =>
        TYPECASE args[1] OF
        | ObValue.ValReplObj =>
        ELSE
          ObValue.BadArgType(
            1, "reflectted object", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN ReflectReleaseLock(args[1], loc);
    | ReflectCode.SetNodeName =>
        TYPECASE args[1] OF
        | ObValue.ValText (node) => text1 := node.text;
        ELSE
          ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN ReflectSetNodeName(text1, loc);
    | ReflectCode.SetDefaultSequencer =>
        TYPECASE args[1] OF
        | ObValue.ValText (node) => text1 := node.text;
        ELSE
          ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValText (node) => text2 := node.text;
        ELSE
          ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN ReflectSetDefaultSequencer(text1, text2, loc);
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc);
      <*ASSERT FALSE*>
    END;
  END EvalReflect;

PROCEDURE ReflectNotify (valObj   : ObValue.Val;
                         notifyObj: ObValue.ValObj;
                         loc      : SynLocation.T   ): ObValue.Val
  RAISES {ObValue.Exception} =
  BEGIN
    TYPECASE valObj OF
    | ObValue.ValReplObj (obj) =>
        TYPECASE notifyObj OF
        | ObValue.ValSimpleObj (notifier) =>
            RETURN ObValueNotify.New(obj, notifier, loc);
        ELSE
          ObValue.RaiseException(
            ObValue.sharedException,
            "reflect_notify failed: '"
              & "second argument must be a simple object", loc);
          <*ASSERT FALSE*>
        END;
    ELSE
      ObValue.RaiseException(
        ObValue.sharedException,
        "reflect_notify failed: '"
          & "first argument must be a reflectted object", loc);
      <*ASSERT FALSE*>
    END;
  END ReflectNotify;

PROCEDURE ReflectCancelNotifier (notifier: ObValue.Val; loc: SynLocation.T)
  RAISES {ObValue.Exception} =
  BEGIN
    TYPECASE notifier OF
    | ObValueNotify.ValObjCB (obj) => obj.cancel();
    ELSE
      ObValue.RaiseException(
        ObValue.sharedException,
        "reflect_cancelNotifier failed: '"
          & "first argument must be a notifier callback", loc);
      <*ASSERT FALSE*>
    END;
  END ReflectCancelNotifier;

PROCEDURE ReflectAcquireLock (valObj: ObValue.ValObj; loc: SynLocation.T):
  ObValue.Val RAISES {ObValue.Exception} =
  BEGIN
    TRY
      TYPECASE valObj OF
      | ObValue.ValReplObj (obj) =>
          SharedObj.AcquireGlobalLock(obj.reflect);
      ELSE
        ObValue.RaiseException(
          ObValue.sharedException,
          "reflect_acquire failed: '"
            & "argument must be a reflectted object", loc);
        <*ASSERT FALSE*>
      END;
    EXCEPT
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException("reflect_acquire: ", atoms, loc);
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, "reflect_acquire: ", loc);
    END;
    RETURN ObValue.valOk;
  END ReflectAcquireLock;

PROCEDURE ReflectReleaseLock (valObj: ObValue.ValObj; loc: SynLocation.T):
  ObValue.Val RAISES {ObValue.Exception} =
  BEGIN
    TRY
      TYPECASE valObj OF
      | ObValue.ValReplObj (obj) =>
          SharedObj.ReleaseGlobalLock(obj.reflect);
      ELSE
        ObValue.RaiseException(
          ObValue.sharedException,
          "reflect_release failed: '"
            & "argument must be a reflectted object", loc);
        <*ASSERT FALSE*>
      END;
    EXCEPT
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException("reflect_release: ", atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, "reflect_release: ", loc);
      <*ASSERT FALSE*>
    END;
    RETURN ObValue.valOk;
  END ReflectReleaseLock;

PROCEDURE ReflectSetNodeName (name: TEXT; loc: SynLocation.T): ObValue.Val
  RAISES {ObValue.Exception} =
  BEGIN
    TRY
      IF Text.Equal(name, "") THEN name := ObValue.machineAddress; END;
      SharedObjRT.ExportSpace(name);
      RETURN ObValue.NewText(name);
    EXCEPT
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException("reflect_setNodeName: ", atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, "reflect_setNodeName: ", loc);
      <*ASSERT FALSE*>
    END;
  END ReflectSetNodeName;

PROCEDURE ReflectDumpState (loc: SynLocation.T)
  RAISES {ObValue.Exception} =
  BEGIN
    TRY
      SharedObjRT.LocalSpace().printState();
    EXCEPT
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException("reflect_dumpState: ", atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, "reflect_setNodeName: ", loc);
      <*ASSERT FALSE*>
    END;
  END ReflectDumpState;

PROCEDURE ReflectSetDefaultSequencer (host, name: TEXT; loc: SynLocation.T):
  ObValue.Val RAISES {ObValue.Exception} =
  VAR space: ObjectSpace.T;
  BEGIN
    TRY
      IF Text.Equal("", host) THEN
        WITH defhost = Env.Get("SEQUENCERHOST") DO
          IF defhost # NIL THEN host := defhost; END;
        END;
      END;
      IF Text.Equal("", name) THEN
        WITH defname = Env.Get("SEQUENCERNAME") DO
          IF defname # NIL THEN name := defname; END;
        END;
      END;

      IF NOT Text.Equal("", host) OR NOT Text.Equal("", name) THEN
        space := SharedObjRT.ImportSpace(host, name);
        IF space = NIL THEN
          ObValue.RaiseException(
            ObValue.sharedException,
            "reflect_setDefaultSequencer: node " & name & "@" & host
              & " is unavailable", loc);
          <*ASSERT FALSE*>
        END;
      ELSE
        space := SharedObjRT.LocalSpace();
      END;
      SharedObjRT.SetDfltSequencer(space);
      RETURN ObValue.valOk;
    EXCEPT
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException(
          "reflect_setDefaultSequencer: ", atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, "reflect_setDefaultSequencer: ", loc);
      <*ASSERT FALSE*>
    END;
  END ReflectSetDefaultSequencer;
*)
BEGIN
END ObBuiltIn.
