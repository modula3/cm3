(* Copyright 1991 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

MODULE ObBuiltIn;
IMPORT Text, TextRd, Rd, Lex, Fmt, ObLib, ObValue, SynLocation, TextConv,
       Thread, NetObj, Env, Params, Math, ObEval, ObjectSpace, FloatMode,
       SharedObj, SharedObjRT, ObValueNotify, Obliq, Time, SynWr, FmtTime,
       WorkerPool, Work, RegEx, LongFloat, Scheduler, RTHeapDebug,
       RTCollector, RTHeapStats, RTProcess, ASCII, ThreadF, Text8;

PROCEDURE Setup () =
  BEGIN
    SetupSys();
    SetupDebug();

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

    SetupRegEx();
    SetupReflect();
  END Setup;

(* ============ "sys" package ============ *)

TYPE

  SysCode = {Address, GetEnvVar, GetParamCount, GetParam, CallFailure,
             Call, Copy, TimeNow, TimeGrain, TimeLong, TimeShort,
             RegisterExitor};

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
              NewSysOC("timeLong", 1, SysCode.TimeLong),
              NewSysOC("timeShort", 1, SysCode.TimeShort),
              NewSysOC("copy", 1, SysCode.Copy, ObLib.OpFixity.Prefix),
              NewSysOC("registerExitor", 1, SysCode.RegisterExitor)};
    ObLib.Register(NEW(PackageSys, name := "sys", opCodes := opCodes));
  END SetupSys;

PROCEDURE EvalSys (                    self  : PackageSys;
                                       opCode: ObLib.OpCode;
                   <*UNUSED*>          arity : ObLib.OpArity;
                              READONLY args  : ObValue.ArgArray;
                                       temp  : BOOLEAN;
                                       swr   : SynWr.T;
                                       loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    int1        : INTEGER;
    longReal1   : LONGREAL;
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
          | ObValue.ValArray (node) => array1 := node.Obtain();
          ELSE
            ObValue.BadArgType(2, "array", self.name, opCode.name, loc)
          END;
          IF NOT ObValue.FetchSysCall(text1, (*out*) sysProc) THEN
            ObValue.RaiseException(ObValue.sysCallFailure,
                                   self.name & "_" & opCode.name & ": \""
                                     & text1 & "\" not found", loc);
          END;
          RETURN sysProc.SysCall(array1^, swr, loc);
      | SysCode.Copy =>
          RETURN ObValue.CopyVal(args[1], ObValue.NewTbl(), loc);
      | SysCode.TimeNow => RETURN Obliq.NewReal(Time.Now());
      | SysCode.TimeGrain =>
          WITH grain = Time.Grain DO RETURN Obliq.NewReal(grain); END;
      | SysCode.TimeShort => 
        TYPECASE args[1] OF
        | ObValue.ValReal (node) => longReal1 := node.real;
        ELSE
          ObValue.BadArgType(1, "time", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN Obliq.NewText(FmtTime.Short(longReal1));
      | SysCode.TimeLong => 
        TYPECASE args[1] OF
        | ObValue.ValReal (node) => longReal1 := node.real;
        ELSE
          ObValue.BadArgType(1, "time", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN Obliq.NewText(FmtTime.Long(longReal1));
      | SysCode.RegisterExitor =>
        TYPECASE args[1] OF
        | ObValue.ValFun =>
        ELSE
          ObValue.BadArgType(1, "procedure", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        LOCK exMu DO
          IF exitors = NIL THEN exitors := NEW (REF ObValue.Vals, 1) END;
          WITH newEx = NEW(REF ObValue.Vals, NUMBER(exitors^)+1) DO
            SUBARRAY(newEx^,0,NUMBER(exitors^)) := exitors^;
            newEx[NUMBER(exitors^)] := args[1];
            exitors := newEx;
          END;
        END;
        RETURN ObValue.valOk;
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

VAR exitors: REF ObValue.Vals := NIL;
    exMu: MUTEX := NEW(MUTEX);

PROCEDURE ObliqExitor () =
  BEGIN
    IF exitors = NIL THEN RETURN END;
    FOR i := 0 TO NUMBER(exitors^)-1 DO
      TRY
        EVAL ObEval.Call(exitors[i], ObValue.Vals{}, Obliq.Console());
      EXCEPT 
      | ObValue.Error (er) => ObValue.ErrorMsg(Obliq.Console(), er);
      | ObValue.Exception (ex) => ObValue.ExceptionMsg(Obliq.Console(), ex);
      END;
    END;
  END ObliqExitor;

(* ============ "debug" package ============ *)

TYPE

  DebugCode = {AssertFree, CheckHeap, CollectNow, ReportReachable, 
               EnableCollector, DisableCollector, DumpReplicaState,
               ReplicaDebugLevel}; 

  DebugOpCode = ObLib.OpCode OBJECT code: DebugCode;  END;

  PackageDebug = ObLib.T OBJECT OVERRIDES Eval := EvalDebug; END;

PROCEDURE NewDebugOC (name  : TEXT;
                    arity : INTEGER;
                    code  : DebugCode;
                    fixity: ObLib.OpFixity := ObLib.OpFixity.Qualified):
  DebugOpCode =
  BEGIN
    RETURN NEW(DebugOpCode, name := name, arity := arity, code := code,
               fixity := fixity);
  END NewDebugOC;

PROCEDURE SetupDebug () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(DebugCode));
    opCodes^ :=
      OpCodes{NewDebugOC("assertFree", 1, DebugCode.AssertFree),
              NewDebugOC("checkHeap", 0, DebugCode.CheckHeap),
              NewDebugOC("collectNow", 0, DebugCode.CollectNow),
              NewDebugOC("reportReachable", 0, DebugCode.ReportReachable),
              NewDebugOC("enableCollector", 0, DebugCode.EnableCollector),
              NewDebugOC("disableCollector", 0, DebugCode.DisableCollector),
              NewDebugOC("dumpReplicaState", 0, DebugCode.DumpReplicaState),
              NewDebugOC("replicaDebugLevel", 1, DebugCode.ReplicaDebugLevel)
              };
    ObLib.Register(NEW(PackageDebug, name := "debug", opCodes := opCodes));
  END SetupDebug;

PROCEDURE EvalDebug (                    self  : PackageDebug;
                                       opCode: ObLib.OpCode;
                   <*UNUSED*>          arity : ObLib.OpArity;
                              READONLY args  : ObValue.ArgArray;
                              <*UNUSED*>temp  : BOOLEAN;
                              <*UNUSED*>swr   : SynWr.T;
                                       loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR int1: INTEGER;
  BEGIN
      CASE NARROW(opCode, DebugOpCode).code OF
      | DebugCode.AssertFree => 
        RTHeapDebug.Free(args[1]); 
        RETURN ObValue.valOk;
      | DebugCode.CheckHeap => 
        RTHeapDebug.CheckHeap(); 
        RETURN ObValue.valOk;
      | DebugCode.CollectNow => 
        RTCollector.Collect(); 
        RETURN ObValue.valOk;
      | DebugCode.DisableCollector => 
        RTCollector.Disable(); 
        RETURN ObValue.valOk;
      | DebugCode.EnableCollector => 
        RTCollector.Enable(); 
        RETURN ObValue.valOk;
      | DebugCode.ReportReachable => 
        RTHeapStats.ReportReachable(); 
        RETURN ObValue.valOk;
      | DebugCode.DumpReplicaState => 
        ReplicaDumpState(loc); 
        RETURN ObValue.valOk;
      | DebugCode.ReplicaDebugLevel =>
          TYPECASE args[1] OF
          | ObValue.ValInt (node) => int1 := node.int;
          ELSE
            ObValue.BadArgType(1, "int", self.name, opCode.name, loc);
          END;
          SharedObjRT.DebugLevel(int1);
          RETURN ObValue.valOk;
      END;
  END EvalDebug;

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
          ObValue.threadAlerted, "replica_dumpState: ", loc);
      <*ASSERT FALSE*>
    END;
  END ReplicaDumpState;

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
                    <*UNUSED*>          swr   : SynWr.T;
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
                   <*UNUSED*>          swr   : SynWr.T;
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
              Round, Float, Floor, Ceiling, IsNaN};

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
              NewRealOC("isNaN", 1, RealCode.IsNaN),
              NewRealOC("floor", 1, RealCode.Floor),
              NewRealOC("ceiling", 1, RealCode.Ceiling)};
    ObLib.Register(NEW(PackageReal, name := "real", opCodes := opCodes));
  END SetupReal;

PROCEDURE EvalReal (                    self  : PackageReal;
                                        opCode: ObLib.OpCode;
                    <*UNUSED*>          arity : ObLib.OpArity;
                               READONLY args  : ObValue.ArgArray;
                                        temp  : BOOLEAN;
                    <*UNUSED*>          swr   : SynWr.T;
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
    | RealCode.IsNaN =>
        IF isReal1 THEN
          RETURN NEW(ObValue.ValBool, bool := LongFloat.IsNaN(real1));
        ELSE
          RETURN NEW(ObValue.ValBool, bool := FALSE);
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
                    <*UNUSED*>          swr   : SynWr.T;
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
                     <*UNUSED*>          swr   : SynWr.T;
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
     FindFirst, FindLast, ReplaceAll, ToUpper, ToLower};

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
              NewTextOC("toUpper", 1, TextCode.ToUpper),
              NewTextOC("toLower", 1, TextCode.ToLower),
              NewTextOC("replaceAll", 3, TextCode.ReplaceAll)};
    ObLib.Register(NEW(PackageText, name := "text", opCodes := opCodes));
  END SetupText;

PROCEDURE EvalText (                    self  : PackageText;
                                        opCode: ObLib.OpCode;
                    <*UNUSED*>          arity : ObLib.OpArity;
                               READONLY args  : ObValue.ArgArray;
                                        temp  : BOOLEAN;
                    <*UNUSED*>          swr   : SynWr.T;
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
          | ObValue.ValArray (node) => array1 := node.Obtain();
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
      | TextCode.ToUpper =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          WITH l = Text.Length(text1) DO
            text2 := Text8.Create(l);
            FOR i:=0 TO l-1 DO
              NARROW(text2, Text8.Public).contents[i] := ASCII.Upper[Text.GetChar(text1,i)];
            END;
          END;
          RETURN ObValue.NewText(text2);
      | TextCode.ToLower =>
          TYPECASE args[1] OF
          | ObValue.ValText (node) => text1 := node.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          WITH l = Text.Length(text1) DO
            text2 := Text8.Create(l);
            FOR i:=0 TO l-1 DO
              NARROW(text2, Text8.Public).contents[i] := ASCII.Lower[Text.GetChar(text1,i)];
            END;
          END;
          RETURN ObValue.NewText(text2);
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
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException(
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

  ArrayCode = {New, NewRepl, NewSimple, Gen, GenRepl, GenSimple, 
               Size, Get, Set, Sub, Upd, Cat};

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
    opCodes := NEW(REF OpCodes, NUMBER(ArrayCode)+2);
    opCodes^ :=
      OpCodes{NewArrayOC("new", 2, ArrayCode.New),
              NewArrayOC("newRemote", 2, ArrayCode.New),
              NewArrayOC("newReplicated", 2, ArrayCode.NewRepl),
              NewArrayOC("newSimple", 2, ArrayCode.NewSimple),
              NewArrayOC("gen", 2, ArrayCode.Gen),
              NewArrayOC("genRemote", 2, ArrayCode.Gen),
              NewArrayOC("genReplicated", 2, ArrayCode.GenRepl),
              NewArrayOC("genSimple", 2, ArrayCode.GenSimple),
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
                                         swr   : SynWr.T;
                                         loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  TYPE Vals = REF ARRAY OF ObValue.Val;
  VAR
    int1, int2          : INTEGER;
    vals, array1, array2: Vals;
    arr1                : ObValue.ValArray;
    badOp               : INTEGER          := 0;
    clos1               : ObValue.ValFun;
  BEGIN
    TRY
      CASE NARROW(opCode, ArrayOpCode).code OF
      | ArrayCode.New, ArrayCode.NewRepl, ArrayCode.NewSimple =>
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
          CASE NARROW(opCode, ArrayOpCode).code OF
          | ArrayCode.New => RETURN ObValue.NewArrayFromVals(vals);
          | ArrayCode.NewRepl => RETURN ObValue.NewReplArrayFromVals(vals);
          | ArrayCode.NewSimple => RETURN ObValue.NewSimpleArrayFromVals(vals);
          ELSE <*ASSERT FALSE*>
          END;
      | ArrayCode.Gen, ArrayCode.GenRepl, ArrayCode.GenSimple =>
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
                                                  temp := FALSE)}, swr, loc);
          END;
          CASE NARROW(opCode, ArrayOpCode).code OF
          | ArrayCode.Gen => RETURN ObValue.NewArrayFromVals(vals);
          | ArrayCode.GenRepl => RETURN ObValue.NewReplArrayFromVals(vals);
          | ArrayCode.GenSimple => RETURN ObValue.NewSimpleArrayFromVals(vals);
          ELSE <*ASSERT FALSE*>
          END;
      | ArrayCode.Size =>
          TYPECASE args[1] OF
          | ObValue.ValArray (node) =>
              RETURN NEW(ObValue.ValInt, int := node.Size(),
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
              RETURN node.Get(int1);
          ELSE
            ObValue.BadArgType(1, "array", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
      | ArrayCode.Set =>
          TYPECASE args[1] OF
          | ObValue.ValArray (node) => arr1 := node;
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
          arr1.Set(int1, args[3]);
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
              RETURN node.Sub(int1, int2);
          ELSE
            ObValue.BadArgType(1, "array", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
      | ArrayCode.Upd =>
          TYPECASE args[1] OF
          | ObValue.ValArray (node) => arr1 := node;
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
          | ObValue.ValArray (node) => array1 := node.Obtain();
          ELSE
            ObValue.BadArgType(4, "array", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          badOp := 3;
          arr1.Upd(int1, int2, array1);
          RETURN ObValue.valOk;
      | ArrayCode.Cat =>
          TYPECASE args[1] OF
          | ObValue.ValArray (node) => array1 := node.Obtain();
          ELSE
            ObValue.BadArgType(1, "array", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValArray (node) => array2 := node.Obtain();
          ELSE
            ObValue.BadArgType(2, "array", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          badOp := 1;
          TYPECASE args[1] OF
          | ObValue.ValRemArray => 
            RETURN ObValue.ArrayCat(array1, array2);
          | ObValue.ValReplArray => 
            RETURN ObValue.ReplArrayCat(array1, array2);
          | ObValue.ValSimpleArray => 
            RETURN ObValue.SimpleArrayCat(array1,array2);
          ELSE <*ASSERT FALSE*>
          END;
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
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException(
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

  NetCode = {Error, Who, Export, Import, ExportEngine, ImportEngine,
             SetSiteName, SetDefaultSequencer};

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
                        NewNetOC("importEngine", 2, NetCode.ImportEngine),
                        NewNetOC("setSiteName", 1, NetCode.SetSiteName),
                        NewNetOC("setDefaultSequencer", 2, 
                                 NetCode.SetDefaultSequencer)};
    ObLib.Register(NEW(PackageNet, name := "net", opCodes := opCodes));
  END SetupNet;

PROCEDURE EvalNet (                    self  : PackageNet;
                                       opCode: ObLib.OpCode;
                   <*UNUSED*>          arity : ObLib.OpArity;
                              READONLY args  : ObValue.ArgArray;
                   <*UNUSED*>          temp  : BOOLEAN;
                   <*UNUSED*>          swr   : SynWr.T;
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
    | NetCode.SetSiteName =>
        TYPECASE args[1] OF
        | ObValue.ValText (node) => text1 := node.text;
        ELSE
          ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN ReplicaSetSiteName(text1, loc);
    | NetCode.SetDefaultSequencer =>
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
            name, NEW(ObValue.NonRemValHookServer).init(o), netAddress);
      | ObValue.ValSimpleObj (o) =>
          NetObj.Export(
            name, NEW(ObValue.NonRemValHookServer).init(o), netAddress);
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
    | ObValue.NonRemValHook (hook) =>
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

PROCEDURE ReplicaSetSiteName (name: TEXT; loc: SynLocation.T): ObValue.Val
  RAISES {ObValue.Exception} =
  BEGIN
    TRY
      IF Text.Equal(name, "") THEN name := ObValue.machineAddress; END;
      SharedObjRT.ExportSpace(name);
      RETURN ObValue.NewText(name);
    EXCEPT
    | SharedObj.Error (atoms) =>
        ObValue.RaiseNetException("net_setSiteName: ", atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, "net_setSiteName: ", loc);
      <*ASSERT FALSE*>
    END;
  END ReplicaSetSiteName;

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
            ObValue.netException,
            "net_setDefaultSequencer: node " & name & "@" & host
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
        ObValue.RaiseNetException(
          "net_setDefaultSequencer: ", atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, "net_setDefaultSequencer: ", loc);
      <*ASSERT FALSE*>
    END;
  END ReplicaSetDefaultSequencer;

(* ============ "replica" package ============ *)

TYPE
  ReplicaCode =
    {Error, Fatal, AcquireGlobalLock, ReleaseGlobalLock, 
     Notify, CancelNotifier, FlushIncomingUpdates,
     FlushQueuedUpdates};

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
        NewReplicaOC("notify", 2, ReplicaCode.Notify),
        NewReplicaOC("cancelNotifier", 1, ReplicaCode.CancelNotifier),
        NewReplicaOC("flushIncomingUpdates", 0, 
                     ReplicaCode.FlushIncomingUpdates),
        NewReplicaOC("flushQueuedUpdates", 0, 
                     ReplicaCode.FlushQueuedUpdates)};
    ObLib.Register(
      NEW(PackageReplica, name := "replica", opCodes := opCodes));
  END SetupReplica;

PROCEDURE EvalReplica (                    self  : PackageReplica;
                                           opCode: ObLib.OpCode;
                       <*UNUSED*>          arity : ObLib.OpArity;
                                  READONLY args  : ObValue.ArgArray;
                       <*UNUSED*>          temp  : BOOLEAN;
                       <*UNUSED*>          swr   : SynWr.T;
                                           loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  BEGIN
    CASE NARROW(opCode, ReplicaOpCode).code OF
    | ReplicaCode.Error => RETURN ObValue.sharedException;
    | ReplicaCode.Fatal => RETURN ObValue.sharedFatal;
    | ReplicaCode.Notify => RETURN ReplicaNotify(args[1], args[2], loc);
    | ReplicaCode.FlushIncomingUpdates => 
      TRY
        SharedObjRT.FlushIncomingUpdates();
        RETURN ObValue.valOk;
      EXCEPT Thread.Alerted =>
        ObValue.RaiseException(
            ObValue.threadAlerted, "replica_flushIncomingUpdates: ", loc);
        <*ASSERT FALSE*>
      END;
    | ReplicaCode.FlushQueuedUpdates => 
      TRY
        SharedObjRT.FlushQueuedUpdates();
        RETURN ObValue.valOk;
      EXCEPT Thread.Alerted =>
        ObValue.RaiseException(
            ObValue.threadAlerted, "replica_flushQueuedUpdates: ", loc);
        <*ASSERT FALSE*>
      END;
    | ReplicaCode.CancelNotifier =>
        ReplicaCancelNotifier(args[1], loc);
        RETURN ObValue.valOk;
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

(* ============ "thread" package ============ *)

TYPE

  ThreadCode = {Alerted, NewMutex, NewCondition, Self, Fork, Join, Wait,
                Acquire, Release, Broadcast, Signal, Pause, Alert,
                TestAlert, AlertWait, AlertJoin, AlertPause, Lock, Id, 
                NewPool, AddWork, StealWorker, FinishWork, Yield};

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
                         swr      : SynWr.T;
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
                         swr      : SynWr.T;
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
      self.result := ObEval.Call(self.fun, noArgs,
                                 self.swr, self.location); 
    EXCEPT
    | ObValue.Error (packet) =>
        self.error := packet;
        ObValue.ErrorMsg(self.swr, packet);
        Msg(self.swr, "<Thread.T> terminated by execution error",
            self.location); 
    | ObValue.Exception (packet) =>
        self.exception := packet;
        ObValue.ExceptionMsg(self.swr, packet);
        Msg(self.swr, "<Thread.T> terminated by unhandled exception",
            self.location); 
    END;
    RETURN self;
  END ApplyThreadClosure;

PROCEDURE HandleWork (self: ObliqWork) =
  VAR noArgs: ARRAY [0 .. -1] OF ObValue.Val;
  BEGIN
    TRY
      EVAL ObEval.Call(self.fun, noArgs, self.swr, self.location);
    EXCEPT
    | ObValue.Error (packet) =>
        ObValue.ErrorMsg(self.swr, packet);
        Msg(self.swr, "<work> terminated by execution error",
            self.location); 
    | ObValue.Exception (packet) =>
        ObValue.ExceptionMsg(self.swr, packet);
        Msg(self.swr, "<work> terminated by unhandled exception",
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
                      swr      : SynWr.T;
                      loc      : SynLocation.T   ): ValThread =
  VAR
    thread       : Thread.T;
    threadClosure: ObliqThreadClosure;
  BEGIN
    stackSize := MIN(MAX(stackSize, 4096), LAST(CARDINAL));
    threadClosure :=
      NEW(ObliqThreadClosure, stackSize := stackSize, fun := fun,
          swr:=swr, location := loc, result := NIL, error := NIL, 
          exception := NIL);
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
              NewThreadOC("id", 1, ThreadCode.Id),
              NewThreadOC("pool", 3, ThreadCode.NewPool),
              NewThreadOC("addWork", 2, ThreadCode.AddWork),
              NewThreadOC("stealWorker", 1, ThreadCode.StealWorker),
              NewThreadOC("finish", 1, ThreadCode.FinishWork),
              NewThreadOC("yield", 0, ThreadCode.Yield),
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
                                          swr   : SynWr.T;
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
    | ThreadCode.Yield =>
        Scheduler.Yield();
        RETURN ObValue.valOk;
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
        RETURN ForkThread(fun1, int1, swr, loc);
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
    | ThreadCode.Id =>
        TYPECASE args[1] OF
        | ValThread (node) => thread1 := node.thread;
        ELSE
          ObValue.BadArgType(1, "thread", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN Obliq.NewInt(ThreadF.MyId());
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
        LOCK mutex1 DO RETURN ObEval.Call(fun1, noArgs, swr, loc) END;
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
        pool1.add(NEW(ObliqWork, swr := swr, fun := fun1, location := loc));
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

(* ============ "regex" package ============ *)

TYPE

  RegExCode = {Error, Compile, Decompile, Dump, Execute, ExecuteRes,
               ExecuteSub, ExecuteSubRes};

  RegExOpCode = ObLib.OpCode OBJECT code: RegExCode;  END;

  PackageRegEx = ObLib.T OBJECT OVERRIDES Eval := EvalRegEx; END;

PROCEDURE NewRegExOC (name  : TEXT;
                    arity : INTEGER;
                    code  : RegExCode;
                    fixity: ObLib.OpFixity := ObLib.OpFixity.Qualified):
  RegExOpCode =
  BEGIN
    RETURN NEW(RegExOpCode, name := name, arity := arity, code := code,
               fixity := fixity);
  END NewRegExOC;

PROCEDURE SetupRegEx () =
  TYPE OpCodes = ARRAY OF ObLib.OpCode;
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(RegExCode));
    opCodes^ :=
      OpCodes{NewRegExOC("error", -1, RegExCode.Error),
              NewRegExOC("compile", 1, RegExCode.Compile),
              NewRegExOC("decompile", 1, RegExCode.Decompile),
              NewRegExOC("dump", 1, RegExCode.Dump),
              NewRegExOC("execute", 2, RegExCode.Execute),
              NewRegExOC("executeRes", 2, RegExCode.ExecuteRes),
              NewRegExOC("executeSub", 4, RegExCode.ExecuteSub),
              NewRegExOC("executeSubRes", 4, RegExCode.ExecuteSubRes)};
    ObLib.Register(NEW(PackageRegEx, name := "regex", opCodes := opCodes));
    regexError := NEW(ObValue.ValException, name := "regex_error");
  END SetupRegEx;

PROCEDURE EvalRegEx (                    self  : PackageRegEx;
                                         opCode: ObLib.OpCode;
                     <*UNUSED*>          arity : ObLib.OpArity;
                                READONLY args  : ObValue.ArgArray;
                                         temp  : BOOLEAN;
                     <*UNUSED*>          swr   : SynWr.T;
                                         loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    text1     : TEXT;
    pattern1  : RegEx.Pattern;
    mem       : REF RegEx.Memory := NIL;
    start, len: CARDINAL;
    matchNo   : INTEGER;
    vals: ARRAY [0 .. NUMBER(RegEx.Memory) - 1] OF ObValue.Val;
    arr : ARRAY [0 .. 1] OF ObValue.Val;
  BEGIN
    CASE NARROW(opCode, RegExOpCode).code OF
    | RegExCode.Error => RETURN regexError;
    | RegExCode.Compile =>
        TYPECASE args[1] OF
        | ObValue.ValText (node) => text1 := node.text;
        ELSE
          ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
        END;
        TRY
          RETURN NEW(ValPattern, what := "<a RegEx.Pattern>",
                     tag := "RegEx`Pattern", picklable := TRUE,
                     pattern := RegEx.Compile(text1));
        EXCEPT
          RegEx.Error (txt) =>
            ObValue.RaiseException(
              regexError, "regex_compile failed: '" & txt & "'", loc);
          <*ASSERT FALSE*>
        END;
    | RegExCode.Decompile =>
        TYPECASE args[1] OF
        | ValPattern (node) => pattern1 := node.pattern;
        ELSE
          ObValue.BadArgType(1, "pattern", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN NEW(ObValue.ValText, text := RegEx.Decompile(pattern1));
    | RegExCode.Dump =>
        TYPECASE args[1] OF
        | ValPattern (node) => pattern1 := node.pattern;
        ELSE
          ObValue.BadArgType(1, "pattern", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        RETURN NEW(ObValue.ValText, text := RegEx.Dump(pattern1));
    | RegExCode.Execute, RegExCode.ExecuteRes, RegExCode.ExecuteSub,
        RegExCode.ExecuteSubRes =>
        TYPECASE args[1] OF
        | ValPattern (node) => pattern1 := node.pattern;
        ELSE
          ObValue.BadArgType(1, "pattern", self.name, opCode.name, loc);
          <*ASSERT FALSE*>
        END;
        TYPECASE args[2] OF
        | ObValue.ValText (node) => text1 := node.text;
        ELSE
          ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
        END;
        CASE NARROW(opCode, RegExOpCode).code OF
        | RegExCode.ExecuteRes, RegExCode.ExecuteSubRes =>
            mem := NEW(REF RegEx.Memory);
        ELSE
        END;
        CASE NARROW(opCode, RegExOpCode).code OF
        | RegExCode.ExecuteSub, RegExCode.ExecuteSubRes =>
            TYPECASE args[3] OF
            | ObValue.ValInt (node) =>
                IF node.int < 0 THEN
                  ObValue.BadArgVal(
                    3, "cardinal", self.name, opCode.name, loc);
                END;
                start := node.int;
            ELSE
              ObValue.BadArgType(3, "int", self.name, opCode.name, loc);
            END;
            TYPECASE args[4] OF
            | ObValue.ValInt (node) =>
                IF node.int < 0 THEN
                  ObValue.BadArgVal(
                    4, "cardinal", self.name, opCode.name, loc);
                END;
                len := node.int;
            ELSE
              ObValue.BadArgType(4, "int", self.name, opCode.name, loc);
            END;
        ELSE
          start := 0;
          len := LAST(CARDINAL);
        END;
        WITH res = RegEx.Execute(pattern1, text1, start, len, mem) DO
          IF mem = NIL THEN
            RETURN NEW(ObValue.ValInt, int := res, temp := temp);
          ELSE
            IF res = -1 THEN RETURN ObValue.valOk END;
            matchNo := 0;
            FOR i := FIRST(mem^) TO LAST(mem^) DO
              IF mem[i].start > -1 THEN
                arr[0] := Obliq.NewInt(mem[i].start);
                arr[1] := Obliq.NewInt(mem[i].stop);
                vals[matchNo] := ObValue.NewSimpleArray(arr);
                INC(matchNo);
              END;
            END;
            RETURN ObValue.NewSimpleArray(SUBARRAY(vals, 0, matchNo));
          END;
        END;
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc);
      <*ASSERT FALSE*>
    END;
  END EvalRegEx;

PROCEDURE IsPattern (self: ValPattern; other: ObValue.ValAnything): BOOLEAN =
  BEGIN
    TYPECASE other OF
      ValPattern (oth) => RETURN self.pattern = oth.pattern;
    ELSE
      RETURN FALSE
    END;
  END IsPattern;

(* ============ "reflect" package ============ *)

(* the possible option tags are:
   valOk, valBool, valChar, valText, valInt, valReal, valException,
   valOption, valVar, valArray, valMethod, valClosure, valAlias,
   valObject, 
   valAnything objects will have the tags defined by the programmer.
*)

(* Alias handling:
   we want to be secure, so we do not provide a way to retrieve the
   object and field of the alias -> you get an alias back from
   "getFields" and can use it in a new object, but you can't extract
   info from it or change it.  Similarly, getType returns the
   value of the desination of the alias, not "alias" *)

TYPE
  ReflectCode =
    {Error, 

     IsArray,           (* is the Val an Array? *)
     IsObject,          (*            an Object? *)
     IsClosure,         (* is this value a closure? *)
     IsException,
     IsMethod,
     IsUpdateMethod,
     IsOption,         
     IsBasic,       (* any of the data types not covered above *)
     IsNative,          (* is this a opaque wrapper of an M3 value? *)
     IsAlias,

     IsLocal,           (* is the object in question local. Simple and
                           replicated always return True.*)
     IsProtected,       (* is the object protected? *)
     IsSerialized,      (* is the object serialized? *)
     IsReplicated,      (* is the object replicated? *)
     IsSimple,          (* is the object simple? *)
     IsRemote,          (* is the object remote? *)

     GetOptionTag,      (* return the tag of the option *)
     GetOptionVal,      (* return the value of the option *)
     GetType,           (* return an option describing the type *)
                        (* the value is ok *)
                        (* includes some interesting details:
                           - methods/procs include num parameters
                        *)
     GetTypedVal,       (* return an option describing the type *)
                        (* the value is the original object *)
     GetFieldTypes,     (* return an option describing an obj fields types *)
     GetObjectType,     (* return a single string describing the obj fields *)
     GetObjectInterface,(* return a single string describing the obj methods *)
     ObjectWho,         (* return the "who" string used for printing *)
     GetField,          (* return a single field of o *)
     GetFields,         (* return an array of fields of o *)
     NewObject,         (* create an object from the needed parameters:
                           - type (repl, remote, simple)
                           - protected?
                           - serialized?
                           - who
                           - fields
                        *)
     NewAlias,          (* delegate a named field to another object field *)
     (* equivalent to the normal obliq ops, including security concerns *)
     Select,            (* return the named field of o *)
     Update,            (* set the value of the named field *)
     Invoke,            (* invoke the named field *)
     Match              (* match the two objects *)
     };

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
      OpCodes{NewReflectOC("error", -1, ReflectCode.Error),
              NewReflectOC("isArray", 1, ReflectCode.IsArray),
              NewReflectOC("isObject", 1, ReflectCode.IsObject),
              NewReflectOC("isClosure", 1, ReflectCode.IsClosure),
              NewReflectOC("isException", 1, ReflectCode.IsException),
              NewReflectOC("isMethod", 1, ReflectCode.IsMethod),
              NewReflectOC("isUpdateMethod", 1, ReflectCode.IsUpdateMethod),
              NewReflectOC("isOption", 1, ReflectCode.IsOption),
              NewReflectOC("isBasic", 1, ReflectCode.IsBasic),
              NewReflectOC("isNative", 1, ReflectCode.IsNative),
              NewReflectOC("isAlias", 1, ReflectCode.IsAlias),
              NewReflectOC("isLocal", 1, ReflectCode.IsLocal),
              NewReflectOC("isProtected", 1, ReflectCode.IsProtected),
              NewReflectOC("isSerialized", 1, ReflectCode.IsSerialized),
              NewReflectOC("isSimple", 1, ReflectCode.IsSimple),
              NewReflectOC("isRemote", 1, ReflectCode.IsRemote),
              NewReflectOC("isReplicated", 1, ReflectCode.IsReplicated),
              NewReflectOC("getOptionTag", 1, ReflectCode.GetOptionTag),
              NewReflectOC("getOptionVal", 1, ReflectCode.GetOptionVal),
              NewReflectOC("getType", 1, ReflectCode.GetType),
              NewReflectOC("getTypedVal", 1, ReflectCode.GetTypedVal),
              NewReflectOC("getFieldTypes", 1, ReflectCode.GetFieldTypes),
              NewReflectOC("getObjectType", 1, ReflectCode.GetObjectType),
              NewReflectOC("getObjectInterface", 1,
                           ReflectCode.GetObjectInterface),
              NewReflectOC("objectWho", 1, ReflectCode.ObjectWho),
              NewReflectOC("getField", 2, ReflectCode.GetField),
              NewReflectOC("getFields", 1, ReflectCode.GetFields),
              NewReflectOC("select", 2, ReflectCode.Select),
              NewReflectOC("update", 3, ReflectCode.Update),
              NewReflectOC("invoke", 3, ReflectCode.Invoke),
              NewReflectOC("match", 2, ReflectCode.Match),
              NewReflectOC("newAlias", 2, ReflectCode.NewAlias),
              NewReflectOC("newObject", 5, ReflectCode.NewObject)};
    ObLib.Register(
      NEW(PackageReflect, name := "reflect", opCodes := opCodes));
    reflectError := NEW(ObValue.ValException, name := "reflect_error");
  END SetupReflect;

PROCEDURE NewOption(tag: TEXT; val: ObValue.Val): ObValue.Val =
  BEGIN
    RETURN NEW(ObValue.ValOption, tag := tag, val := val);
  END NewOption;

PROCEDURE EvalReflect (                    self  : PackageReflect;
                                           opCode: ObLib.OpCode;
                       <*UNUSED*>          arity : ObLib.OpArity;
                                  READONLY args  : ObValue.ArgArray;
                       <*UNUSED*>          temp  : BOOLEAN;
                                           swr   : SynWr.T;
                                           loc   : SynLocation.T     ):
  ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  TYPE Vals = REF ARRAY OF ObValue.Val;
  VAR text1,text2: TEXT;
      protected, serialized: BOOLEAN;
      obj1 : ObValue.ValObj;
      hint : INTEGER;
      array1             : Vals;
  BEGIN
    TRY
      CASE NARROW(opCode, ReflectOpCode).code OF
      | ReflectCode.Error => RETURN reflectError;
      | ReflectCode.IsArray =>
          TYPECASE args[1] OF
          | ObValue.ValArray => RETURN Obliq.true
          ELSE
            RETURN Obliq.false
          END;
      | ReflectCode.IsObject =>
          TYPECASE args[1] OF
          | ObValue.ValObj => RETURN Obliq.true
          ELSE
            RETURN Obliq.false
          END;
      | ReflectCode.IsClosure =>
          TYPECASE args[1] OF
          | ObValue.ValFun => RETURN Obliq.true
          ELSE
            RETURN Obliq.false
          END;
      | ReflectCode.IsException =>
          TYPECASE args[1] OF
          | ObValue.ValException => RETURN Obliq.true
          ELSE
            RETURN Obliq.false
          END;
      | ReflectCode.IsMethod =>
          TYPECASE args[1] OF
          | ObValue.ValMeth => RETURN Obliq.true
          ELSE
            RETURN Obliq.false
          END;
      | ReflectCode.IsUpdateMethod =>
          TYPECASE args[1] OF
          | ObValue.ValMeth(meth) => RETURN Obliq.NewBool(meth.meth.update);
          ELSE
            RETURN Obliq.false
          END;
      | ReflectCode.IsOption =>
          TYPECASE args[1] OF
          | ObValue.ValOption => RETURN Obliq.true
          ELSE
            RETURN Obliq.false
          END;
      | ReflectCode.IsBasic =>
          TYPECASE args[1] OF
          | ObValue.ValOk, ObValue.ValBool, ObValue.ValChar, 
            ObValue.ValText, ObValue.ValInt, ObValue.ValReal =>
              RETURN Obliq.true
          ELSE
            RETURN Obliq.false
          END;
      | ReflectCode.IsNative =>
          TYPECASE args[1] OF
          | ObValue.ValAnything => RETURN Obliq.true
          ELSE
            RETURN Obliq.false
          END;
      | ReflectCode.IsAlias =>
          TYPECASE args[1] OF
          | ObValue.ValAlias => RETURN Obliq.true
          ELSE
            RETURN Obliq.false
          END;
      | ReflectCode.IsProtected =>
          TYPECASE args[1] OF
          | ObValue.ValObj (node) => EVAL node.Who(protected, serialized);
          ELSE
            ObValue.BadArgType(1, "object", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN NEW(ObValue.ValBool, bool := protected);
      | ReflectCode.IsSerialized =>
          TYPECASE args[1] OF
          | ObValue.ValObj (node) => EVAL node.Who(protected, serialized);
          ELSE
            ObValue.BadArgType(1, "object", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN NEW(ObValue.ValBool, bool := serialized);
      | ReflectCode.IsRemote =>
          TYPECASE args[1] OF
          | ObValue.ValRemObj, ObValue.ValRemArray, ObValue.ValRemVar =>
            RETURN Obliq.true;
          ELSE
            RETURN Obliq.false;
          END;
      | ReflectCode.IsReplicated =>
          TYPECASE args[1] OF
          | ObValue.ValReplObj, ObValue.ValReplArray, ObValue.ValReplVar =>
            RETURN Obliq.true;
          ELSE
            RETURN Obliq.false;
          END;
      | ReflectCode.IsSimple =>
          TYPECASE args[1] OF
          | ObValue.ValSimpleObj, ObValue.ValSimpleArray, 
            ObValue.ValSimpleVar =>
            RETURN Obliq.true;
          ELSE
            RETURN Obliq.false;
          END;
      | ReflectCode.IsLocal =>
          TYPECASE args[1] OF
          | ObValue.ValRemObj (node) =>
              RETURN NEW(ObValue.ValBool, bool := 
                TYPECODE(node.remote) = TYPECODE(ObValue.RemObjServer));
          | ObValue.ValRemArray (node) =>
              RETURN NEW(ObValue.ValBool, bool := 
                TYPECODE(node.remote) = TYPECODE(ObValue.RemVarServer));
          | ObValue.ValRemVar (node) =>
              RETURN NEW(ObValue.ValBool, bool := 
                TYPECODE(node.remote) = TYPECODE(ObValue.RemArrayServer));
          ELSE
            RETURN Obliq.true
          END;
      | ReflectCode.GetOptionTag =>
          TYPECASE args[1] OF
          | ObValue.ValOption (o) => RETURN Obliq.NewText(o.tag);
          ELSE
            ObValue.BadArgType(1, "option", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
      | ReflectCode.GetOptionVal =>
          TYPECASE args[1] OF
          | ObValue.ValOption (o) => RETURN o.val;
          ELSE
            ObValue.BadArgType(1, "option", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
      | ReflectCode.GetType =>
          RETURN NewOption(ObValue.GetTypeString(args[1]), ObValue.valOk);
      | ReflectCode.GetTypedVal =>
          RETURN NewOption(ObValue.GetTypeString(args[1]), args[1]);
      | ReflectCode.ObjectWho =>
          TYPECASE args[1] OF
          | ObValue.ValObj (node) =>
              text1 := node.Who(protected, serialized);
          ELSE
            ObValue.BadArgType(1, "object", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          RETURN NEW(ObValue.ValText, text := text1);
      | ReflectCode.Select =>
          TYPECASE args[1] OF
          | ObValue.ValObj (obj) => obj1 := obj;
          ELSE
            ObValue.BadArgType(1, "object", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (text) => text1 := text.text;
          ELSE
            ObValue.BadArgType(1, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          hint := -1;
          TRY
            RETURN obj1.Select(swr, text1, FALSE, hint);
          EXCEPT
          | ObValue.ServerError(msg) => 
            ObValue.RaiseException(
                reflectError, self.name & "_" & opCode.name & ": " & msg, loc);
            <*ASSERT FALSE*>
          END;
      | ReflectCode.Update =>
          TYPECASE args[1] OF
          | ObValue.ValObj (obj) => obj1 := obj;
          ELSE
            ObValue.BadArgType(1, "object", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (text) => text1 := text.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          hint := -1;
          TRY
            obj1.Update(text1, args[3], FALSE, hint);
            RETURN ObValue.valOk;
          EXCEPT
          | ObValue.ServerError(msg) => 
            ObValue.RaiseException(
                reflectError, self.name & "_" & opCode.name & ": " & msg, loc);
              <*ASSERT FALSE*>
          END;
      | ReflectCode.Invoke =>
          TYPECASE args[1] OF
          | ObValue.ValObj (obj) => obj1 := obj;
          ELSE
            ObValue.BadArgType(1, "object", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (text) => text1 := text.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValArray (arr) => array1 := arr.Obtain();
          ELSE
            ObValue.BadArgType(3, "array", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          hint := -1;
          TRY
            RETURN obj1.Invoke(swr, text1, NUMBER(array1^), array1^, 
                               FALSE, hint);
          EXCEPT
          | ObValue.ServerError(msg) => 
            ObValue.RaiseException(
                reflectError, self.name & "_" & opCode.name & ": " & msg, loc);
              <*ASSERT FALSE*>
          END;
      | ReflectCode.NewAlias =>
          TYPECASE args[1] OF
          | ObValue.ValObj (obj) => obj1 := obj;
          ELSE
            ObValue.BadArgType(1, "object", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (text) => text1 := text.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          hint := -1;
          RETURN ObValue.NewAlias(obj1, text1, loc);
      | ReflectCode.GetFieldTypes =>
          TYPECASE args[1] OF
          | ObValue.ValObj (obj) => obj1 := obj;
          ELSE
            ObValue.BadArgType(1, "object", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TRY
            VAR desc := obj1.ObtainDescriptions();
                vals := NEW(REF ARRAY OF ObValue.Val, NUMBER(desc^));
                arr : ARRAY [0 .. 1] OF ObValue.Val;
            BEGIN
              FOR i := FIRST(desc^) TO LAST(desc^) DO
                arr[0] := Obliq.NewText(desc[i].label);
                arr[1] := NewOption(desc[i].type, ObValue.valOk);
                vals[i] := ObValue.NewSimpleArray(arr);
              END;
              RETURN ObValue.NewSimpleArrayFromVals(vals);
            END;
          EXCEPT
          | ObValue.ServerError(msg) => 
            ObValue.RaiseException(
                reflectError, self.name & "_" & opCode.name & ": " & msg, loc);
              <*ASSERT FALSE*>
          END;
      | ReflectCode.GetObjectType =>
          TYPECASE args[1] OF
          | ObValue.ValObj (obj) => obj1 := obj;
          ELSE
            ObValue.BadArgType(1, "object", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TRY
            VAR desc := obj1.ObtainDescriptions();
                ret  := ".*";
            BEGIN
              FOR i := 0 TO LAST(desc^) DO
                ret := ret & "|" & desc[i].label & "=>" & desc[i].type & "|.*";
              END;
              RETURN NewOption(ret, ObValue.valOk);
            END;
          EXCEPT
          | ObValue.ServerError(msg) => 
            ObValue.RaiseException(
                reflectError, self.name & "_" & opCode.name & ": " & msg, loc);
              <*ASSERT FALSE*>
          END;
      | ReflectCode.GetObjectInterface =>
          TYPECASE args[1] OF
          | ObValue.ValObj (obj) => obj1 := obj;
          ELSE
            ObValue.BadArgType(1, "object", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TRY
            VAR desc := obj1.ObtainDescriptions();
                ret  := ".*";
            BEGIN
              FOR i := 0 TO LAST(desc^) DO
                IF Text.Equal("Method", Text.Sub(desc[i].type, 0, 6)) THEN
                  ret := ret & "|" & desc[i].label & "=>" & 
                             desc[i].type & "|.*";
                END;
              END;
              RETURN NewOption(ret, ObValue.valOk);
            END;
          EXCEPT
          | ObValue.ServerError(msg) => 
            ObValue.RaiseException(
                reflectError, self.name & "_" & opCode.name & ": " & msg, loc);
              <*ASSERT FALSE*>
          END;
      | ReflectCode.GetField =>
          TYPECASE args[1] OF
          | ObValue.ValObj (obj) => obj1 := obj;
          ELSE
            ObValue.BadArgType(1, "object", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValText (text) => text1 := text.text;
          ELSE
            ObValue.BadArgType(2, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TRY
            RETURN obj1.ObtainField(text1,FALSE);
          EXCEPT
          | ObValue.ServerError(msg) => 
            ObValue.RaiseException(
                reflectError, self.name & "_" & opCode.name & ": " & msg, loc);
              <*ASSERT FALSE*>
          END;
      | ReflectCode.GetFields =>
          TYPECASE args[1] OF
          | ObValue.ValObj (obj) => obj1 := obj;
          ELSE
            ObValue.BadArgType(1, "object", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TRY
            VAR fields := obj1.Obtain(FALSE);
                vals := NEW(REF ARRAY OF ObValue.Val, NUMBER(fields^));
                arr : ARRAY [0 .. 1] OF ObValue.Val;
            BEGIN
              FOR i := FIRST(fields^) TO LAST(fields^) DO
                arr[0] := ObValue.NewText(fields[i].label);
                arr[1] := fields[i].field;
                vals[i] := ObValue.NewSimpleArray(arr);
              END;
              RETURN ObValue.NewSimpleArrayFromVals(vals);
            END;
          EXCEPT
          | ObValue.ServerError(msg) => 
            ObValue.RaiseException(
                reflectError, self.name & "_" & opCode.name & ": " & msg, loc);
              <*ASSERT FALSE*>
          END;
      | ReflectCode.Match =>
          RETURN Obliq.NewBool(Match(args[1],args[2],loc));
      | ReflectCode.NewObject =>
          TYPECASE args[1] OF
          | ObValue.ValText (text) => text1 := text.text;
          | ObValue.ValOption (o) => text1 := o.tag;
          ELSE
            ObValue.BadArgType(1, "text or option", self.name,
                               opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[2] OF
          | ObValue.ValBool (bool) => protected := bool.bool;
          ELSE
            ObValue.BadArgType(2, "bool", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[3] OF
          | ObValue.ValBool (bool) => serialized := bool.bool;
          ELSE
            ObValue.BadArgType(3, "bool", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[4] OF
          | ObValue.ValText (text) => text2 := text.text;
          ELSE
            ObValue.BadArgType(4, "text", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          TYPECASE args[5] OF
          | ObValue.ValArray (arr) => array1 := arr.Obtain();
          ELSE
            ObValue.BadArgType(5, "array", self.name, opCode.name, loc);
            <*ASSERT FALSE*>
          END;
          VAR fields := NEW(REF ObValue.ObjFields, NUMBER(array1^));
              sync   : ObValue.Sync := NIL;
          BEGIN
            FOR i := FIRST(fields^) TO LAST(fields^) DO
              TYPECASE array1[i] OF
              | ObValue.ValArray(arr) =>
                WITH vals = arr.Obtain() DO
                  IF NUMBER(vals^) # 2 THEN
                    ObValue.BadArgVal(5, "a [text,val] pair in element " &
                                         Fmt.Int(i),
                                      self.name, opCode.name, loc);
                    <*ASSERT FALSE*>
                  END;
                  TYPECASE vals[0] OF
                  | ObValue.ValText(text) => fields[i].label := text.text;
                  ELSE
                    ObValue.BadArgVal(5, "a text in element [" &
                                         Fmt.Int(i) & ", 0]",
                                      self.name, opCode.name, loc);
                    <*ASSERT FALSE*>
                  END;
                  fields[i].field := vals[1];
                END;
              ELSE
                ObValue.BadArgVal(5, "an array of [text,val] pairs",
                                  self.name, opCode.name, loc);
                <*ASSERT FALSE*>
              END;
            END;
            IF Text.Equal(text1, "Simple") OR 
               Text.Equal(text1, "Object`Simple") THEN
              IF serialized THEN
                sync := NEW(ObValue.Sync, mutex := NEW(Thread.Mutex))
              END;
              RETURN ObValue.NewSimpleObjectFromFields(fields, text2,
                                                       protected, sync);
            ELSIF Text.Equal(text1, "Remote") OR
                  Text.Equal(text1, "Object`Remote") THEN
              IF serialized THEN
                sync := NEW(ObValue.Sync, mutex := NEW(Thread.Mutex))
              END;
              RETURN ObValue.NewObjectFromFields(fields, text2,
                                                 protected, sync);
            ELSIF Text.Equal(text1, "Replicated") OR
                  Text.Equal(text1, "Object`Replicated") THEN
              TRY
                RETURN ObValue.NewReplObjectFromFields(fields, text2,
                                                       protected);
              EXCEPT
              | ObValue.ServerError(msg) => 
                ObValue.RaiseException(
                    reflectError, self.name & "_" & opCode.name & ": " & msg, 
                    loc);
                <*ASSERT FALSE*>
              END;
            ELSE
              ObValue.BadArgVal(1, "one of {Object`Simple, Simple, Object`Remote, Remote, Object`Replicated, Replicated}",
                                self.name, opCode.name, loc);
              <*ASSERT FALSE*>
            END;
          END;
      ELSE
        ObValue.BadOp(self.name, opCode.name, loc);
        <*ASSERT FALSE*>
      END;
    EXCEPT
    | NetObj.Error (atoms) =>
        ObValue.RaiseNetException(
          self.name & "_" & opCode.name & ": ", atoms, loc);
      <*ASSERT FALSE*>
    | Thread.Alerted =>
        ObValue.RaiseException(
          ObValue.threadAlerted, self.name & "_" & opCode.name & ": ", loc);
      <*ASSERT FALSE*>
    | SharedObj.Error (atoms) =>
        ObValue.RaiseSharedException(
          self.name & "_" & opCode.name & ": ", atoms, loc);
      <*ASSERT FALSE*>
    END;
  END EvalReflect;

  (* Match a value "val" against a match object.  The "match" object
     is a positive match if:
       - "match" is ok
       - "match" and "val" are equal, or
       - "val" is a text string and "match" is a regular expression
         that matches all of val (ie. "val" matches "^" & match & "$")
       - "match" is a regular expression that matches all of the TypeString
         of "val"
       - "match" is an option whose regular expression matches all of the
         TypeString of "val", and the value of the option is either
         "ok" or also matches "val".
       - "val" and "match" are objects, and for each field of "match",
         "val" has a corresponding field whose contents are matched by the
         contents of the field of "match".
       - "val" and "match" are arrays of the same size, and each
         element of the array matches
  *)
PROCEDURE Match (match, val: Obliq.Val; loc: SynLocation.T): BOOLEAN  
  RAISES {ObValue.Error, ObValue.Exception, NetObj.Error,
          SharedObj.Error, Thread.Alerted} =
  BEGIN
    IF ObValue.Is(match, val, loc) THEN RETURN TRUE END;

    TYPECASE match OF
    | ObValue.ValOk => RETURN TRUE;
    | ObValue.ValOption (m1) =>
      TRY
        WITH typeVal = ObValue.GetTypeString (val),
             compiled = RegEx.Compile("^" & m1.tag & "$") DO
          IF RegEx.Execute(compiled, typeVal) = -1 THEN
            RETURN FALSE;
          END;
        END;
      EXCEPT RegEx.Error (txt2) =>
        ObValue.RaiseError("Option regular expression '"&
          m1.tag & "' error:" & txt2, loc)
      END;
      TYPECASE m1.val OF 
        ObValue.ValOk => RETURN TRUE;
      ELSE
        RETURN Match(m1.val, val, loc);
      END;
    | ObValue.ValText (t1) =>
      TRY
        WITH typeVal = ObValue.GetTypeString (val),
             compiled = RegEx.Compile("^" & t1.text & "$") DO
          IF RegEx.Execute(compiled, typeVal) = -1 THEN
            RETURN FALSE;
          END;
        END;
      EXCEPT RegEx.Error (txt2) =>
        ObValue.RaiseError("Option regular expression '"&
          t1.text & "' error:" & txt2, loc)
      END;
      RETURN TRUE;

    | ObValue.ValArray (ma) =>
      TRY
        TYPECASE val OF
        | ObValue.ValArray (va) =>
          WITH ms = ma.Size() DO
            IF ms # va.Size() THEN RETURN FALSE END;
            FOR i := 0 TO ms-1 DO
              IF NOT Match(ma.Get(i), va.Get(i), loc) THEN
                RETURN FALSE;
              END;
            END;
            RETURN TRUE;
          END;
        ELSE
          RETURN FALSE;
        END;
      EXCEPT
      | ObValue.ServerError(msg) => 
        ObValue.RaiseException(reflectError, "reflect_match: " & msg, 
                               loc);
        <*ASSERT FALSE*>
      END;

    | ObValue.ValObj (mo) =>
      TRY
        TYPECASE val OF
        | ObValue.ValObj (vo) =>
          VAR mfields := mo.Obtain(FALSE);
              vfields := vo.Obtain(FALSE);
              vi: INTEGER := 0;
          BEGIN
            FOR i := FIRST(mfields^) TO LAST(mfields^) DO
              IF vi > LAST(vfields^) THEN RETURN FALSE END;
              WHILE vi <= LAST(vfields^) DO
                WITH cmp = Text.Compare(vfields[vi].label,
                                        mfields[i].label) DO
                  IF cmp = 1 THEN RETURN FALSE END;
                  IF cmp = 0 THEN 
                    IF NOT Match(mfields[i].field, vfields[vi].field,
                                 loc) THEN
                      RETURN FALSE;
                    END;
                    INC(vi);
                    EXIT;
                  END;
                  INC(vi);
                END;
              END;
            END;
            RETURN TRUE;
          END;
        ELSE
          RETURN FALSE;
        END;
      EXCEPT
      | ObValue.ServerError(msg) => 
        ObValue.RaiseException(reflectError, "reflect_match: " & msg, 
                               loc);
        <*ASSERT FALSE*>
      END;
    ELSE
      RETURN FALSE;
    END;
  END Match;
       
BEGIN
  RTProcess.RegisterExitor(ObliqExitor);
END ObBuiltIn.
