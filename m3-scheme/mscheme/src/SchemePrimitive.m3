(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemePrimitive;
IMPORT Debug;
IMPORT SchemeEnvironment, SchemeProcedureClass;
IMPORT Scheme, SchemeClass;

FROM Scheme IMPORT Object, Symbol, Vector, E;
FROM SchemeClass IMPORT GetCons, ReturnCons;

FROM SchemeUtils IMPORT Length, First, Second, Third,
                        Stringify, StringifyQ, StringifyB,
                        Error, Warn, Equal, Eqv,
                        Rest, PedanticFirst, PedanticRest, Cons, 
                        SetFirst, SetRest, Reverse, Str, List2, ListToString,
                        Vec, InPort, OutPort, List1, ListToVector, ListStar,
                        VectorToList, Write, SetWarningsAreErrors;

IMPORT SchemeInputPort, SchemeContinuation, SchemeMacro, SchemeString;
FROM SchemeBoolean IMPORT Truth, False, True, TruthO;
FROM SchemeProcedure IMPORT Proc; IMPORT SchemeProcedure;
FROM SchemeLongReal IMPORT FromLR, FromO, Zero, One;
FROM SchemeChar IMPORT Character, Char, IChr, LowerCase, UpperCase, Digits,
                       White, Upcase, Downcase;
IMPORT SchemeChar;
IMPORT SchemeSymbol;
IMPORT SchemeLongReal;
IMPORT SchemeBoolean, CitTextUtils AS TextUtils;

IMPORT Fmt, Text, Wx, Wr;
IMPORT Math, Scan, Lex, FloatMode;
IMPORT Process;
IMPORT OSError, FileWr, FileRd, AL, File;
IMPORT Thread;
IMPORT SchemePair;
IMPORT CardRefTbl, Random;
IMPORT RefSeq;
IMPORT XTime AS Time;
IMPORT RefRecord;
IMPORT SchemeClosure;
IMPORT SchemeInt, Mpz;
IMPORT Pickle, Rd;

<* FATAL Thread.Alerted *>

TYPE Pair      =  SchemePair.T;
     String    =  SchemeString.T;
     Procedure =  SchemeProcedure.T;

REVEAL
  T = Public BRANDED Brand OBJECT
    minArgs, maxArgs : CARDINAL;
    definer : Definer;
    id : CARDINAL;
  OVERRIDES
    init  := Init;
    apply := Apply;
    apply1 := Apply1;
    apply2 := Apply2;
    getMinArgs := GetMinArgs;
    getMaxArgs := GetMaxArgs;
    getId := GetId;
  END;

PROCEDURE GetMinArgs(t : T) : CARDINAL = BEGIN RETURN t.minArgs END GetMinArgs;

PROCEDURE GetMaxArgs(t : T) : CARDINAL = BEGIN RETURN t.maxArgs END GetMaxArgs;

PROCEDURE GetId(t : T) : CARDINAL = BEGIN RETURN t.id END GetId;

PROCEDURE Init(t : T; 
               id : CARDINAL;
               definer : Definer; 
               minArgs, maxArgs : CARDINAL) : T =
  BEGIN 
    t.minArgs := minArgs; 
    t.maxArgs := maxArgs; 
    t.definer := definer;
    t.id := id; 
    RETURN t
  END Init;

TYPE
  P = { Eq, Lt, Gt, Ge, Le,
        Abs, EofObject, EqQ, EqualQ, Force,
        Car, Floor,  Ceiling, Cons, 
        Divide, Length, List, ListQ, Apply,
        Max, Min, Minus, Newline, 
        Not, NullQ, NumberQ, PairQ, Plus, 
        ProcedureQ, Read, ReadBigInt, Cdr, Round, Second, 
        SymbolQ, Times, Truncate, Write, Append,
        BooleanQ, Sqrt, Expt, Reverse, Assoc, 
        AssQ, AssV, Member, MemQ, MemV, EqvQ,
        ListRef, ListTail, StringQ, MakeString, String,
        StringLength, StringRef, StringSet, Substring, 
        StringAppend, StringToList, ListToString, 
        SymbolToString, StringToSymbol, Exp, Log, Sin,
        Cos, Tan, Acos, Asin, Atan, 
        NumberToString, StringToNumber, CharQ,
        CharAlphabeticQ, CharNumericQ, CharWhitespaceQ,
        CharUppercaseQ, CharLowercaseQ, CharToInteger,
        IntegerToChar, CharUpcase, CharDowncase,
        VectorQ, MakeVector, Vector, VectorLength,
        VectorRef, VectorSet, ListToVector, Map, 
        Foreach, CallCC, VectorToList, Load, Display,
        InputPortQ, CurrentInputPort, OpenInputFile, 
        CloseInputPort, OutputportQ, CurrentOutputPort,
        OpenOutputFile, CloseOutputPort, ReadChar,
        PeekChar, Eval, Quotient, Remainder,
        IntDiv, Modulo, Third, EofObjectQ, Gcd, Lcm, 
        Cxr, OddQ, EvenQ, ZeroQ, PositiveQ,
        NegativeQ, 

        CharCmpEq, CharCmpLt, CharCmpGt, CharCmpGe, CharCmpLe, 

        CharCiCmpEq, CharCiCmpLt, CharCiCmpGt, CharCiCmpGe, CharCiCmpLe,

        StringCmpEq, StringCmpLt, StringCmpGt, StringCmpGe, StringCmpLe, 

        StringCiCmpEq, StringCiCmpLt, StringCiCmpGt, StringCiCmpGe, StringCiCmpLe,

        ExactQ, InexactQ, IntegerQ,
        CallWithInputFile, CallWithOutputFile,

        (* extensions follow *)

        New, Class, Method, Exit,
        SetCar, SetCdr, TimeCall, MacroExpand,
        Error, ListStar,
        
        Random, Normal, SetWarningsAreErrors, NumberToLONGREAL, NumberToEXTENDED, NumberToREAL, StringHaveSub,
        EnableTracebacks, DisableTracebacks, RefRecordFormat, SetRTErrorMapping,

        DisplayNoFlush, WriteNoFlush,

        EqMemo, EqualMemo,

        Cosh, Sinh, Tanh, Acosh, Asinh, Atanh,

        ChangeGlobalEnv, GetParentEnv,

        DumpEnvironment, LoadEnvironment,

        System,

        WithInputFromFile, WithOutputToFile,

        CharReadyQ,

        ExactToInexact, InexactToExact,

        BitwiseAnd, BitwiseIor, BitwiseXor, BitwiseNot,
        ArithmeticShift, IntegerLength
  };

REVEAL 
  Definer = PubDefiner BRANDED Brand & " Definer" OBJECT
  END;

  DefaultDefiner = Definer BRANDED Brand & " Default Definer" OBJECT 
  OVERRIDES
    installPrimitives := InstallDefaultPrimitives;
  END;

  DefaultExtendedDefiner = Definer BRANDED Brand & " DefaultExtended Definer" OBJECT 
  OVERRIDES
    installPrimitives := InstallDefaultExtendedPrimitives;
  END;

  SandboxDefiner = Definer BRANDED Brand & " Sandbox Definer" OBJECT 
  OVERRIDES
    installPrimitives := InstallSandboxPrimitives;
  END;

  ExtDefiner = PubExtensibleDefiner BRANDED Brand & " Extensible Definer" OBJECT
    tbl : CardRefTbl.T := NIL;
    random : Random.T;
  METHODS
    apply(t : T; interp : Scheme.T; args : Object) : Object 
        RAISES { E } := EDApply;
  OVERRIDES
    init := InitED;
    addPrim := AddPrim;
    installPrimitives := EDInstallPrimitives;
  END;

PROCEDURE InitED(ed : ExtDefiner) : ExtDefiner =
  BEGIN
    ed.random := NEW(Random.Default).init();
    ed.tbl := NEW(CardRefTbl.Default).init();
    RETURN ed
  END InitED;

TYPE
  PrimRec = OBJECT
    name : TEXT;
    proc : SchemeProcedure.T;
    minArgs, maxArgs : CARDINAL;
  END;

PROCEDURE AddPrim(ed : ExtDefiner; 
                  name : TEXT; 
                  proc : SchemeProcedure.T;
                  minArgs, maxArgs : CARDINAL) =
  VAR 
    id : CARDINAL;
    dummy : REFANY;
  BEGIN
    REPEAT
      id := ed.random.integer(ORD(LAST(P))+1,LAST(CARDINAL))
    UNTIL NOT ed.tbl.get(id,dummy);
    
    EVAL ed.tbl.put(id, NEW(PrimRec, name := name, proc := proc,
                            minArgs := minArgs, maxArgs := maxArgs))
  END AddPrim;

PROCEDURE EDApply(ed : ExtDefiner; 
                  t : T; interp : Scheme.T; args : Object) : Object
  RAISES { E } = 
  VAR ref : REFANY;
  BEGIN
    IF ed.tbl.get(t.id, ref) THEN
      RETURN NARROW(ref, PrimRec).proc.apply(interp,args)
    ELSE
      RAISE E("INTERNAL ERROR: NO PRIMITIVE DEFINED FOR id = " & Fmt.Int(t.id)&
            ", PLEASE CALL A WIZARD IMMEDIATELY.")
    END
  END EDApply;

PROCEDURE EDInstallPrimitives(ed : ExtDefiner; 
                              env : SchemeEnvironment.T) : SchemeEnvironment.T=
  BEGIN
    EVAL InstallDefaultPrimitives(ed,env);
    VAR
      iter := ed.tbl.iterate();
      id : CARDINAL;
      ref : REFANY;
    BEGIN
      WHILE iter.next(id,ref) DO
        WITH rec = NARROW(ref,PrimRec) DO
          EVAL env.defPrim(rec.name, id, ed, rec.minArgs, rec.maxArgs)
        END
      END
    END;
    RETURN env
  END EDInstallPrimitives;

PROCEDURE InstallFileIOPrimitives(dd : Definer;
                            env : SchemeEnvironment.T) : SchemeEnvironment.T =
  BEGIN
    EVAL env
    .defPrim("call-with-input-file", ORD(P.CallWithInputFile), dd,2)
    .defPrim("call-with-output-file", ORD(P.CallWithOutputFile), dd,2)
    .defPrim("close-input-port", ORD(P.CloseInputPort), dd,1)
    .defPrim("close-output-port", ORD(P.CloseOutputPort), dd,1)
    .defPrim("current-input-port", ORD(P.CurrentInputPort), dd,0)
    .defPrim("current-output-port", ORD(P.CurrentOutputPort), dd,0)
    .defPrim("eof-object?",    ORD(P.EofObjectQ), dd,1)
    .defPrim("input-port?",    ORD(P.InputPortQ), dd,1)
    .defPrim("open-input-file",ORD(P.OpenInputFile), dd,1)
    .defPrim("open-output-file", ORD(P.OpenOutputFile), dd,1)
    .defPrim("output-port?",   ORD(P.OutputportQ), dd,1);

    RETURN env

  END InstallFileIOPrimitives;

PROCEDURE InstallSandboxPrimitives(dd : Definer;
                            env : SchemeEnvironment.T) : SchemeEnvironment.T =
  CONST n = LAST(CARDINAL);
  BEGIN
    EVAL env
    .defPrim("*",              ORD(P.Times), dd,    0, n)
    .defPrim("+",              ORD(P.Plus), dd,     0, n)
    .defPrim("-",              ORD(P.Minus), dd,    1, n)
    .defPrim("/",              ORD(P.Divide), dd,   1, n)
    .defPrim("<",              ORD(P.Lt), dd,       2, n)
    .defPrim("<=",             ORD(P.Le), dd,       2, n)
    .defPrim("=",              ORD(P.Eq), dd,       2, n)
    .defPrim(">",              ORD(P.Gt), dd,       2, n)
    .defPrim(">=",             ORD(P.Ge), dd,       2, n)
    .defPrim("abs",            ORD(P.Abs), dd,      1)
    .defPrim("acos",           ORD(P.Acos), dd,     1)
    .defPrim("append",         ORD(P.Append), dd,   0, n)
    .defPrim("apply",          ORD(P.Apply), dd,    2, n)
    .defPrim("asin",           ORD(P.Asin), dd,     1)
    .defPrim("assoc",          ORD(P.Assoc), dd,    2)
    .defPrim("assq",           ORD(P.AssQ), dd,     2)
    .defPrim("assv",           ORD(P.AssV), dd,     2)
    .defPrim("atan",           ORD(P.Atan), dd,     1, 2)
    .defPrim("boolean?",       ORD(P.BooleanQ), dd, 1)
    .defPrim("caaaar",         ORD(P.Cxr), dd,      1)
    .defPrim("caaadr",         ORD(P.Cxr), dd,      1)
    .defPrim("caaar",          ORD(P.Cxr), dd,      1)
    .defPrim("caadar",         ORD(P.Cxr), dd,      1)
    .defPrim("caaddr",         ORD(P.Cxr), dd,      1)
    .defPrim("caadr",          ORD(P.Cxr), dd,      1)
    .defPrim("caar",           ORD(P.Cxr), dd,      1)
    .defPrim("cadaar",         ORD(P.Cxr), dd,      1)
    .defPrim("cadadr",         ORD(P.Cxr), dd,      1)
    .defPrim("cadar",          ORD(P.Cxr), dd,      1)
    .defPrim("caddar",         ORD(P.Cxr), dd,      1)
    .defPrim("cadddr",         ORD(P.Cxr), dd,      1)
    .defPrim("caddr",          ORD(P.Third), dd,    1)
    .defPrim("cadr",           ORD(P.Second), dd,   1)
    .defPrim("call-with-current-continuation",        ORD(P.CallCC), dd,   1)
    .defPrim("car",            ORD(P.Car), dd,      1)
    .defPrim("cdaaar",         ORD(P.Cxr), dd,      1)
    .defPrim("cdaadr",         ORD(P.Cxr), dd,      1)
    .defPrim("cdaar",          ORD(P.Cxr), dd,      1)
    .defPrim("cdadar",         ORD(P.Cxr), dd,      1)
    .defPrim("cdaddr",         ORD(P.Cxr), dd,      1)
    .defPrim("cdadr",          ORD(P.Cxr), dd,      1)
    .defPrim("cdar",           ORD(P.Cxr), dd,      1)
    .defPrim("cddaar",         ORD(P.Cxr), dd,      1)
    .defPrim("cddadr",         ORD(P.Cxr), dd,      1)
    .defPrim("cddar",          ORD(P.Cxr), dd,      1)
    .defPrim("cdddar",         ORD(P.Cxr), dd,      1)
    .defPrim("cddddr",         ORD(P.Cxr), dd,      1)
    .defPrim("cdddr",          ORD(P.Cxr), dd,      1)
    .defPrim("cddr",           ORD(P.Cxr), dd,      1)
    .defPrim("cdr",            ORD(P.Cdr), dd,      1)
    .defPrim("char->integer",  ORD(P.CharToInteger), dd,     1)
    .defPrim("char-alphabetic?",ORD(P.CharAlphabeticQ), dd,     1)
    .defPrim("char-ci<=?",     ORD(P.CharCiCmpLe), dd,2)
    .defPrim("char-ci<?" ,     ORD(P.CharCiCmpLt), dd,2)
    .defPrim("char-ci=?" ,     ORD(P.CharCiCmpEq), dd,2)
    .defPrim("char-ci>=?",     ORD(P.CharCiCmpGe), dd,2)
    .defPrim("char-ci>?" ,     ORD(P.CharCiCmpGt), dd,2)
    .defPrim("char-downcase",  ORD(P.CharDowncase), dd,     1)
    .defPrim("char-lower-case?",ORD(P.CharLowercaseQ), dd,     1)
    .defPrim("char-numeric?",  ORD(P.CharNumericQ), dd,     1)
    .defPrim("char-upcase",    ORD(P.CharUpcase), dd,     1)
    .defPrim("char-upper-case?",ORD(P.CharUppercaseQ), dd,     1)
    .defPrim("char-whitespace?",ORD(P.CharWhitespaceQ), dd,     1)
    .defPrim("char<=?",        ORD(P.CharCmpLe), dd,2)
    .defPrim("char<?",         ORD(P.CharCmpLt), dd,2)
    .defPrim("char=?",         ORD(P.CharCmpEq), dd,2)
    .defPrim("char>=?",        ORD(P.CharCmpGe), dd,2)
    .defPrim("char>?",         ORD(P.CharCmpGt), dd,2)
    .defPrim("char?",          ORD(P.CharQ), dd,    1)
    .defPrim("complex?",       ORD(P.NumberQ), dd,  1)
    .defPrim("cons",           ORD(P.Cons), dd,     2)
    .defPrim("cos",            ORD(P.Cos), dd,      1)
    .defPrim("display",        ORD(P.Display), dd,  1, 2)
    .defPrim("display-noflush",        ORD(P.DisplayNoFlush), dd,  1, 2)
    .defPrim("eq?",            ORD(P.EqQ), dd,      2)
    .defPrim("equal?",         ORD(P.EqualQ), dd,   2)
    .defPrim("eqv?",           ORD(P.EqvQ), dd,     2)
    .defPrim("eval",           ORD(P.Eval), dd,     1, 2)
    .defPrim("even?",          ORD(P.EvenQ), dd,    1)
    .defPrim("exact?",         ORD(P.ExactQ), dd, 1)
    .defPrim("exp",            ORD(P.Exp), dd,      1)
    .defPrim("expt",           ORD(P.Expt), dd,     2)
    .defPrim("force",          ORD(P.Force), dd,    1)
    .defPrim("for-each",       ORD(P.Foreach), dd,  1, n)
    .defPrim("gcd",            ORD(P.Gcd), dd,      0, n)
    .defPrim("inexact?",       ORD(P.InexactQ), dd, 1)
    .defPrim("integer->char",  ORD(P.IntegerToChar), dd,     1)
    .defPrim("integer?",       ORD(P.IntegerQ), dd, 1)
    .defPrim("lcm",            ORD(P.Lcm), dd,      0, n)
    .defPrim("length",         ORD(P.Length), dd,   1)
    .defPrim("list",           ORD(P.List), dd,     0, n)
    .defPrim("list->string",   ORD(P.ListToString), dd,1)
    .defPrim("list->vector",   ORD(P.ListToVector), dd,     1)
    .defPrim("list-ref",       ORD(P.ListRef), dd,  2)
    .defPrim("list-tail",      ORD(P.ListTail), dd, 2)
    .defPrim("list?",          ORD(P.ListQ), dd,    1)
    .defPrim("load",           ORD(P.Load), dd,     1)
    .defPrim("log",            ORD(P.Log), dd,      1)
    .defPrim("macro-expand",   ORD(P.MacroExpand),dd, 1)
    .defPrim("make-string",    ORD(P.MakeString),dd,1, 2)
    .defPrim("make-vector",    ORD(P.MakeVector),dd ,1, 2)
    .defPrim("map",            ORD(P.Map), dd,      1, n)
    .defPrim("max",            ORD(P.Max), dd,      1, n)
    .defPrim("member",         ORD(P.Member), dd,   2)
    .defPrim("memq",           ORD(P.MemQ), dd,     2)
    .defPrim("memv",           ORD(P.MemV), dd,     2)
    .defPrim("min",            ORD(P.Min), dd,      1, n)
    .defPrim("div",            ORD(P.IntDiv), dd,   2)
    .defPrim("modulo",         ORD(P.Modulo), dd,   2)
    .defPrim("negative?",      ORD(P.NegativeQ), dd,1)
    .defPrim("newline",        ORD(P.Newline), dd,  0, 1)
    .defPrim("not",            ORD(P.Not), dd,      1)
    .defPrim("null?",          ORD(P.NullQ), dd,    1)
    .defPrim("number->string", ORD(P.NumberToString), dd,  1, 2)
    .defPrim("number?",        ORD(P.NumberQ), dd,  1)
    .defPrim("odd?",           ORD(P.OddQ), dd,     1)
    .defPrim("pair?",          ORD(P.PairQ), dd,    1)
    .defPrim("peek-char",      ORD(P.PeekChar), dd, 0, 1)
    .defPrim("positive?",      ORD(P.PositiveQ), dd,1)
    .defPrim("procedure?",     ORD(P.ProcedureQ),dd,1)
    .defPrim("quotient",       ORD(P.Quotient), dd, 2)
    .defPrim("rational?",      ORD(P.NumberQ), dd,1)
    .defPrim("read",           ORD(P.Read), dd,     0, 1)
    .defPrim("read-big-int",   ORD(P.ReadBigInt), dd,     0, 1)
    .defPrim("read-char",      ORD(P.ReadChar), dd, 0, 1)
    .defPrim("real?",          ORD(P.NumberQ), dd,  1)
    .defPrim("remainder",      ORD(P.Remainder), dd,2)
    .defPrim("reverse",        ORD(P.Reverse), dd,  1)
    .defPrim("round",          ORD(P.Round), dd,    1)
    .defPrim("ceiling",        ORD(P.Ceiling), dd,    1)
    .defPrim("floor",          ORD(P.Floor), dd,    1)
    .defPrim("truncate",       ORD(P.Truncate), dd,    1)
    .defPrim("set-car!",       ORD(P.SetCar), dd,   2)
    .defPrim("set-cdr!",       ORD(P.SetCdr), dd,   2)
    .defPrim("sin",            ORD(P.Sin), dd,      1)
    .defPrim("sqrt",           ORD(P.Sqrt), dd,     1)
    .defPrim("string",         ORD(P.String), dd,   0, n)
    .defPrim("string->list",   ORD(P.StringToList), dd,1)
    .defPrim("string->number", ORD(P.StringToNumber), dd,  1, 2)
    .defPrim("string->symbol", ORD(P.StringToSymbol), dd,  1)
    .defPrim("string-append",  ORD(P.StringAppend), dd,0, n)
    .defPrim("string-ci<=?",   ORD(P.StringCiCmpLe), dd,2)
    .defPrim("string-ci<?" ,   ORD(P.StringCiCmpLt), dd,2)
    .defPrim("string-ci=?" ,   ORD(P.StringCiCmpEq), dd,2)
    .defPrim("string-ci>=?",   ORD(P.StringCiCmpGe), dd,2)
    .defPrim("string-ci>?" ,   ORD(P.StringCiCmpGt), dd,2)
    .defPrim("string-length",  ORD(P.StringLength), dd,  1)
    .defPrim("string-ref",     ORD(P.StringRef), dd,2)
    .defPrim("string-set!",    ORD(P.StringSet), dd,3)
    .defPrim("string<=?",      ORD(P.StringCmpLe), dd,2)
    .defPrim("string<?",       ORD(P.StringCmpLt), dd,2)
    .defPrim("string=?",       ORD(P.StringCmpEq), dd,2)
    .defPrim("string>=?",      ORD(P.StringCmpGe), dd,2)
    .defPrim("string>?",       ORD(P.StringCmpGt), dd,2)
    .defPrim("string?",        ORD(P.StringQ), dd,  1)
    .defPrim("substring",      ORD(P.Substring), dd,3)
    .defPrim("symbol->string", ORD(P.SymbolToString), dd,  1)
    .defPrim("symbol?",        ORD(P.SymbolQ), dd,  1)
    .defPrim("tan",            ORD(P.Tan), dd,      1)

    .defPrim("tanh",           ORD(P.Tanh), dd,     1)
    .defPrim("cosh",           ORD(P.Cosh), dd,     1)
    .defPrim("sinh",           ORD(P.Sinh), dd,     1)

    .defPrim("atanh",           ORD(P.Atanh), dd,     1)
    .defPrim("acosh",           ORD(P.Acosh), dd,     1)
    .defPrim("asinh",           ORD(P.Asinh), dd,     1)

    .defPrim("vector",         ORD(P.Vector), dd,   0, n)
    .defPrim("vector->list",   ORD(P.VectorToList), dd,1)
    .defPrim("vector-length",  ORD(P.VectorLength), dd,1)
    .defPrim("vector-ref",     ORD(P.VectorRef), dd,2)
    .defPrim("vector-set!",    ORD(P.VectorSet), dd,3)
    .defPrim("vector?",        ORD(P.VectorQ), dd,  1)
    .defPrim("write",          ORD(P.Write), dd,    1, 2)
    .defPrim("write-noflush",          ORD(P.WriteNoFlush), dd,    1, 2)
    .defPrim("write-char",     ORD(P.Display), dd,  1, 2)
    .defPrim("with-input-from-file", ORD(P.WithInputFromFile), dd, 2)
    .defPrim("with-output-to-file", ORD(P.WithOutputToFile), dd, 2)
    .defPrim("write-char-noflush",     ORD(P.DisplayNoFlush), dd,  1, 2)
    .defPrim("char-ready?",    ORD(P.CharReadyQ), dd, 0, 1)
    .defPrim("zero?",          ORD(P.ZeroQ), dd,    1)
    .defPrim("exact->inexact", ORD(P.ExactToInexact), dd, 1)
    .defPrim("inexact->exact", ORD(P.InexactToExact), dd, 1)
    .defPrim("eq?-memo",       ORD(P.EqMemo), dd, 1, 1)
    .defPrim("equal?-memo",       ORD(P.EqualMemo), dd, 1, 1)
    .defPrim("bitwise-and",    ORD(P.BitwiseAnd), dd, 0, n)
    .defPrim("bitwise-ior",    ORD(P.BitwiseIor), dd, 0, n)
    .defPrim("bitwise-xor",    ORD(P.BitwiseXor), dd, 0, n)
    .defPrim("bitwise-not",    ORD(P.BitwiseNot), dd, 1)
    .defPrim("arithmetic-shift", ORD(P.ArithmeticShift), dd, 2)
    .defPrim("integer-length", ORD(P.IntegerLength), dd, 1)
    ;
    
    RETURN env

  END InstallSandboxPrimitives;

PROCEDURE InstallNorvigPrimitives(dd : Definer;
                            env : SchemeEnvironment.T) : SchemeEnvironment.T =
  CONST n = LAST(CARDINAL);
  BEGIN
    EVAL env
    (*///////////// Extensions ////////////////*)

    .defPrim("new",                ORD(P.New), dd,      1)
    .defPrim("class",              ORD(P.Class), dd,    1)
    .defPrim("method",             ORD(P.Method), dd,   2, n)
    .defPrim("exit",               ORD(P.Exit), dd,     0, 1)
    .defPrim("error",              ORD(P.Error), dd,    0, n)
    .defPrim("time-call",          ORD(P.TimeCall), dd, 1, 2)
    .defPrim("_list*",             ORD(P.ListStar), dd, 0, n);

    RETURN env;

  END InstallNorvigPrimitives;

PROCEDURE InstallDefaultExtendedPrimitives(dd : Definer;
                            env : SchemeEnvironment.T) : SchemeEnvironment.T =
  BEGIN
    EVAL env
    (*///////////// Extensions ////////////////*)

    .defPrim("set-warnings-are-errors!",
             ORD(P.SetWarningsAreErrors), dd,      1, 1)
    .defPrim("random",
             ORD(P.Random), dd,      0, 0)
    .defPrim("enable-tracebacks!",
             ORD(P.EnableTracebacks), dd,      0, 0)
    .defPrim("disable-tracebacks!",
             ORD(P.DisableTracebacks), dd,      0, 0)
    .defPrim("number->LONGREAL", ORD(P.NumberToLONGREAL), dd, 1, 1)
    .defPrim("number->EXTENDED", ORD(P.NumberToEXTENDED), dd, 1, 1)
    .defPrim("number->REAL", ORD(P.NumberToREAL), dd, 1, 1)
    .defPrim("string-havesub?", ORD(P.StringHaveSub), dd, 2, 2)
    .defPrim("normal",                ORD(P.Normal), dd,      0, 2)
    .defPrim("refrecord-format", ORD(P.RefRecordFormat), dd, 1, 1)
    .defPrim("set-rt-error-mapping!", ORD(P.SetRTErrorMapping), dd, 1, 1)
    .defPrim("change-global-environment!", ORD(P.ChangeGlobalEnv), dd, 1, 1)
    .defPrim("get-parent-environment", ORD(P.GetParentEnv), dd, 1, 1)
    .defPrim("dump-environment", ORD(P.DumpEnvironment), dd, 1, 1)
    .defPrim("load-environment!", ORD(P.LoadEnvironment), dd, 1, 1)
    .defPrim("system",           ORD(P.System), dd, 1, 1)
    ;
    RETURN env;

  END InstallDefaultExtendedPrimitives;

PROCEDURE InstallDefaultPrimitives(dd : Definer;
                            env : SchemeEnvironment.T) : SchemeEnvironment.T =
  BEGIN
    env := InstallSandboxPrimitives(dd, env);
    env := InstallFileIOPrimitives(dd, env);
    env := InstallNorvigPrimitives(dd, env);
    env := InstallDefaultExtendedPrimitives(dd,env);
    RETURN env
  END InstallDefaultPrimitives;

PROCEDURE Apply(t : T; interp : Scheme.T; args : Object) : Object 
  RAISES { E } =
  VAR
    dummy : BOOLEAN;
  BEGIN
    WITH nArgs = Length(args) DO
      IF    nArgs < t.minArgs THEN
        RETURN Error("too few args, " & Fmt.Int(nArgs) & 
               ", for " & t.name & ": " & Stringify(args))
      ELSIF nArgs > t.maxArgs THEN
        RETURN Error("too many args, " & Fmt.Int(nArgs) & 
               ", for " & t.name & ": " & Stringify(args))
      END
    END;

    WITH x = First(args),
         y = Second(args) DO
      RETURN Prims(t, interp, args, x, y, dummy)
    END
  END Apply;

PROCEDURE Apply1(t : T; interp : Scheme.T; a1 : Object) : Object 
  RAISES { E } =
  BEGIN
    WITH nArgs = 1 DO
      IF    nArgs < t.minArgs THEN
        RETURN Error("too few args, " & Fmt.Int(nArgs) & 
               ", for " & t.name & ": " & Stringify(List1(a1)))
      ELSIF nArgs > t.maxArgs THEN
        RETURN Error("too many args, " & Fmt.Int(nArgs) & 
               ", for " & t.name & ": " & Stringify(List1(a1)))
      END
    END;

    VAR 
      d1 := GetCons(interp);
      free := DefFree;
    BEGIN
      d1.first := a1;
      d1.rest := NIL;
      
      WITH res = Prims(t, interp, d1, a1, NIL, free) DO
        IF free THEN ReturnCons(interp,d1) END;
        RETURN res
      END
    END
  END Apply1;
    
PROCEDURE Apply2(t : T; interp : Scheme.T; a1, a2 : Object) : Object 
  RAISES { E } =
  BEGIN
    WITH nArgs = 2 DO
      IF    nArgs < t.minArgs THEN
        RETURN Error("too few args, " & Fmt.Int(nArgs) & 
               ", for " & t.name & ": " & Stringify(List2(a1,a2)))
      ELSIF nArgs > t.maxArgs THEN
        RETURN Error("too many args, " & Fmt.Int(nArgs) & 
               ", for " & t.name & ": " & Stringify(List2(a1,a2)))
      END
    END;
    
    VAR
      d1, d2 := GetCons(interp);
      free := DefFree;
    BEGIN
      d1.first := a1;
      d1.rest := d2;
      d2.first := a2;
      d2.rest := NIL;
      
      WITH res = Prims(t, interp, d1, a1, a2, free) DO
        IF free THEN
          ReturnCons(interp,d1)
        END;
        RETURN res
      END
    END
  END Apply2;

CONST DefFree = TRUE;

PROCEDURE CheckVectorIdx(vec : Vector; idx : INTEGER) RAISES { E } =
  BEGIN
    IF idx < FIRST(vec^) OR idx > LAST(vec^) THEN
      RAISE E("Vector subscript " & Fmt.Int(idx) & 
            " out of range " & Fmt.Int(FIRST(vec^)) & ".." &
            Fmt.Int(LAST(vec^)))
    END
  END CheckVectorIdx;

PROCEDURE BitwiseNary(args : Object; op : CHAR) : Object RAISES { E } =
  (* N-ary bitwise: 'A'=and, 'O'=ior, 'X'=xor *)
  VAR
    result : Mpz.T;
    first := TRUE;
  BEGIN
    IF args = NIL THEN
      (* identity elements *)
      CASE op OF
        'A' => RETURN SchemeInt.FromI(-1)
      | 'O' => RETURN SchemeInt.FromI(0)
      | 'X' => RETURN SchemeInt.FromI(0)
      ELSE <* ASSERT FALSE *>
      END
    END;

    WHILE args # NIL AND ISTYPE(args, SchemePair.T) DO
      WITH a = First(args) DO
        IF NOT SchemeInt.IsExactInt(a) THEN
          RETURN Error("bitwise op: not an exact integer: " & Stringify(List1(a)))
        END;
        IF first THEN
          result := SchemeInt.ToMpz(a);
          first := FALSE
        ELSE
          VAR mt := SchemeInt.ToMpz(a); mr := Mpz.New(); BEGIN
            CASE op OF
              'A' => Mpz.and(mr, result, mt)
            | 'O' => Mpz.ior(mr, result, mt)
            | 'X' => Mpz.xor(mr, result, mt)
            ELSE <* ASSERT FALSE *>
            END;
            result := mr
          END
        END
      END;
      args := Rest(args)
    END;
    RETURN SchemeInt.MpzToScheme(result)
  END BitwiseNary;

PROCEDURE Prims(t          : T;
                interp     : Scheme.T;
                args, x, y : Object;
                VAR free   : BOOLEAN) : Object
  RAISES { E } =
  VAR z : Object;
  BEGIN
    IF t.id > ORD(LAST(P)) THEN
      (* call definer method *)
      <* ASSERT ISTYPE(t.definer, ExtDefiner) *>
      free := FALSE;
      RETURN NARROW(t.definer,ExtDefiner).apply(t,interp,args)
    ELSE
      (* t.id <= ORD(LAST(P)) *)

      CASE VAL(t.id,P) OF
        P.Eq => RETURN NumCompare(args, '=')
      |
        P.Lt => RETURN NumCompare(args, '<')
      |
        P.Gt => RETURN NumCompare(args, '>')
      |
        P.Ge => RETURN NumCompare(args, 'G')
      | 
        P.Le => RETURN NumCompare(args, 'L')
      |
        P.Abs =>  RETURN DoAbs(x)
      |
        P.EofObject =>  RETURN Truth(SchemeInputPort.IsEOF(x))
      |
        P.EqQ => RETURN Truth(x = y)
      |
        P.EqualQ => RETURN Truth(Equal(x,y))
      |
        P.Force => 
        IF x = NIL OR NOT ISTYPE(x,Procedure) THEN RETURN x
        ELSE RETURN Proc(x).apply(interp,NIL)
        END
      |
        
        P.Car => RETURN PedanticFirst(x)
      |
        P.Floor =>
        IF SchemeInt.IsExactInt(x) THEN RETURN x
        ELSE RETURN FromLR(FLOAT(FLOOR(FromO(x)),LONGREAL))
        END
      |
        P.Ceiling =>
        IF SchemeInt.IsExactInt(x) THEN RETURN x
        ELSE RETURN FromLR(FLOAT(CEILING(FromO(x)),LONGREAL))
        END
      |
        P.Cons => RETURN Cons(x,y,interp)
      |
        
        P.Divide => RETURN NumComputeStart(Rest(args), '/', x)
      |
        P.Length => RETURN SchemeInt.FromI(Length(x))
      |
        P.List => free := FALSE; RETURN args
      |
        P.ListQ => RETURN Truth(IsList(x))
      |
        P.Apply => free := FALSE; 
        RETURN Proc(x).apply(interp,ListStar(Rest(args)))
      |
        
        P.Max => RETURN NumComputeStart(args, 'X', x)
      |
        P.Min => RETURN NumComputeStart(args, 'N', x)
      |
        P.Minus => RETURN NumComputeStart(Rest(args), '-', x)
      |
        P.Newline => 
        TRY
          Wr.PutChar(OutPort(x,interp), '\n');
          Wr.Flush(OutPort(x,interp));
          RETURN True()
        EXCEPT
          Wr.Failure(err) => RAISE E("newline: Wr.Failure: " & AL.Format(err))
        END
      |
        
        P.Not => RETURN Truth(x = False())
      |
        P.NullQ => RETURN Truth(x = NIL)
      |
        P.NumberQ => RETURN Truth(SchemeInt.IsNumber(x))
      |
        P.PairQ => RETURN Truth(x # NIL AND ISTYPE(x,Pair))
      |
        P.Plus => RETURN NumCompute(args, '+')
      |
        
        P.ProcedureQ => RETURN Truth(x # NIL AND ISTYPE(x,Procedure))
      |
        P.Read => RETURN InPort(x, interp).read()
      |
        P.ReadBigInt => RETURN InPort(x, interp).readBigInt()
      |
        P.Cdr => RETURN PedanticRest(x)
      |
        P.Round =>
        IF SchemeInt.IsExactInt(x) THEN RETURN x
        ELSE
          WITH lr = FromO(x),
               fl = Math.floor(lr),
               frac = lr - fl DO
            IF frac # 0.5d0 AND frac # -0.5d0 THEN
              RETURN FromLR(FLOAT(ROUND(lr), LONGREAL))
            ELSE
              (* half-to-even: round to nearest even *)
              WITH hi = fl + 1.0d0 DO
                IF TRUNC(fl) MOD 2 = 0 THEN
                  RETURN FromLR(fl)
                ELSE
                  RETURN FromLR(hi)
                END
              END
            END
          END
        END
      |
        P.Second => RETURN Second(x)
      |
        
        P.SymbolQ => RETURN Truth(x # NIL AND ISTYPE(x,Symbol))
      |
        P.Times => RETURN NumCompute(args, '*')
      |
        P.Truncate =>
        IF SchemeInt.IsExactInt(x) THEN RETURN x
        ELSE RETURN FromLR(FLOAT(TRUNC(FromO(x)),LONGREAL))
        END
      |
        P.Write => RETURN Write(x, OutPort(y, interp), TRUE,
                                interp := interp)
      |
        P.WriteNoFlush => RETURN Write(x, OutPort(y, interp), TRUE,
                                interp := interp, flush := FALSE)
      |
        P.Append => free := FALSE; 
        IF args = NIL THEN RETURN NIL
        ELSE RETURN Append(args)
        END
      |
        
        P.BooleanQ => RETURN Truth(x = True() OR x = False())
      |
        P.Sqrt =>
        IF SchemeInt.IsExactInt(x) AND NOT SchemeInt.IsNegative(x) THEN
          VAR
            mx := SchemeInt.ToMpz(x);
            mr := Mpz.New();
          BEGIN
            Mpz.sqrt(mr, mx);
            (* check if perfect square *)
            VAR mr2 := Mpz.New(); BEGIN
              Mpz.mul(mr2, mr, mr);
              IF Mpz.cmp(mr2, mx) = 0 THEN
                RETURN SchemeInt.MpzToScheme(mr)
              ELSE
                RETURN FromLR(Math.sqrt(FromO(x)))
              END
            END
          END
        ELSE
          RETURN FromLR(Math.sqrt(FromO(x)))
        END
      |
        P.Expt =>
        IF SchemeInt.IsExactInt(x) AND SchemeInt.IsExactInt(y)
           AND NOT SchemeInt.IsNegative(y) THEN
          (* both exact, non-negative exponent: exact result *)
          VAR
            mx := SchemeInt.ToMpz(x);
            mr := Mpz.New();
            exp : INTEGER;
          BEGIN
            TYPECASE y OF
              SchemeInt.T(ry) => exp := ry^
            | Mpz.T(my) =>
              IF Mpz.fits_slong_p(my) # 0 THEN
                exp := Mpz.get_si(my)
              ELSE
                RETURN FromLR(Math.pow(FromO(x),FromO(y)))
              END
            ELSE
              exp := 0
            END;
            IF exp < 0 THEN
              RETURN FromLR(Math.pow(FromO(x),FromO(y)))
            ELSE
              Mpz.pow_ui(mr, mx, exp);
              RETURN SchemeInt.MpzToScheme(mr)
            END
          END
        ELSE
          RETURN FromLR(Math.pow(FromO(x),FromO(y)))
        END
      |
        P.Reverse => RETURN Reverse(x)
      |
        P.Assoc => RETURN MemberAssoc(x, y, 'a', ' ')
      |
        P.AssQ => RETURN MemberAssoc(x, y, 'a', 'q')
      |
        P.AssV => RETURN MemberAssoc(x, y, 'a', 'v')
      |
        P.Member =>RETURN MemberAssoc(x, y, 'm', ' ')
      |
        P.MemQ => RETURN MemberAssoc(x, y, 'm', 'q')
      |
        P.MemV => RETURN MemberAssoc(x, y, 'm', 'v')
      |
        P.EqvQ => RETURN Truth(Eqv(x,y))
      |
        
        P.ListRef =>
        VAR p := x; BEGIN
          FOR k := SchemeLongReal.Int(y) TO 1 BY -1 DO p:= Rest(p) END;
          RETURN First(p)
        END
      |
        P.ListTail =>
        VAR p := x; BEGIN
          FOR k := SchemeLongReal.Int(y) TO 1 BY -1 DO p:= Rest(p) END;
          RETURN p
        END
      |
        P.StringQ => RETURN Truth(x # NIL AND ISTYPE(x,String))
      |
        P.MakeString =>
        VAR
          str := NEW(String, SchemeLongReal.Int(x));
        BEGIN
          IF y # NIL THEN
            WITH c = Char(y) DO
              FOR i := FIRST(str^) TO LAST(str^) DO
                str[i] := c
              END
            END
          END;
          RETURN str
        END
      |
        P.String => RETURN ListToString(args)
      |
        P.StringLength => RETURN SchemeInt.FromI(NUMBER(Str(x)^))
      |
        P.StringRef =>
        WITH str = Str(x)^, yi = SchemeLongReal.Int(y) DO
          IF yi < FIRST(str) OR yi > LAST(str) THEN
            RETURN Error("string index out of bounds")
          ELSE
            RETURN Character(str[yi])
          END
        END
      |
        P.StringSet =>
        WITH z = Third(args), str = Str(x)^, yi = SchemeLongReal.Int(y) DO
          IF yi < FIRST(str) OR yi > LAST(str) THEN
            RETURN Error("string index out of bounds")
          ELSE
            str[yi] := Char(z);
            RETURN z
          END
        END
      |
        P.Substring =>
        VAR
          str := Str(x);
          start := SchemeLongReal.Int(y);  (* inclusive *)
          end   := SchemeLongReal.Int(Third(args));
        BEGIN
          (* crimp pointers *)
          start := MAX(start, 0);  (* at least 0 *)
          start := MIN(start, LAST(str^)+1); (* no more than last *)

          end := MIN(end, LAST(str^)+1); (* no more than last+1 *)
          end := MAX(end, start);        (* no less than start *)

          WITH res = NEW(String, end-start) DO
            res^ := SUBARRAY(str^, start, end-start);
            RETURN res
          END
        END
      |
        
        P.StringAppend => RETURN StringAppend(interp, args)
      |
        P.StringToList =>
        VAR
          result : Pair := NIL;
          str := Str(x);
        BEGIN
          FOR i := LAST(str^) TO FIRST(str^) BY -1 DO
            result := Cons(Character(str[i]),result,interp)
          END;
          RETURN result
        END
      |
        P.ListToString => RETURN ListToString(x)
      |
        P.SymbolToString => 
        IF x = NIL OR NOT ISTYPE(x, Symbol) THEN
          RETURN Error("Not a symbol") 
        END;
        RETURN SchemeString.FromText(SchemeSymbol.ToText(x))
      |
        P.StringToSymbol => 
        RETURN SchemeSymbol.Symbol(Text.FromChars(Str(x)^))
      |
        P.Exp => RETURN FromLR(Math.exp(FromO(x)))
      |
        P.Log => RETURN FromLR(Math.log(FromO(x)))
      |
        P.Sin => RETURN FromLR(Math.sin(FromO(x)))
      |
        P.Cos => RETURN FromLR(Math.cos(FromO(x)))
      |
        P.Tan => RETURN FromLR(Math.tan(FromO(x)))
      |
        P.Acos => RETURN FromLR(Math.acos(FromO(x)))
      |
        P.Asin => RETURN FromLR(Math.asin(FromO(x)))
      |
        P.Atan =>
        IF y = NIL THEN RETURN FromLR(Math.atan(FromO(x)))
        ELSE RETURN FromLR(Math.atan2(FromO(x), FromO(y)))
        END
      |
        
        P.NumberToString => RETURN NumberToString(x,y)
      |
        P.StringToNumber => RETURN StringToNumber(x,y)
      |
        P.CharQ => RETURN Truth(x # NIL AND ISTYPE(x, SchemeChar.T))
      |
        
        P.CharAlphabeticQ => RETURN Truth(Char(x) IN LowerCase + UpperCase)
      |
        P.CharNumericQ => RETURN Truth(Char(x) IN Digits)
      |
        P.CharWhitespaceQ => RETURN Truth(Char(x) IN White)
      |
        
        P.CharUppercaseQ => RETURN Truth(Char(x) IN UpperCase)
      |
        P.CharLowercaseQ => RETURN Truth(Char(x) IN LowerCase)
      |
        P.CharToInteger => RETURN SchemeInt.FromI(ORD(Char(x)))
      |
        
        P.IntegerToChar => RETURN IChr(SchemeLongReal.Int(x, roundOK := FALSE))
      |
        P.CharUpcase => RETURN Character(Upcase(Char(x)))
      |
        P.CharDowncase => RETURN Character(Downcase(Char(x)))
      |
        
        P.VectorQ => RETURN Truth(x # NIL AND ISTYPE(x, Vector))
      |
        P.MakeVector =>
        WITH num = SchemeLongReal.Int(x) DO
          IF num < 0 THEN
            RETURN Error("Can't make vector of size " & Fmt.Int(num))
          END;
          WITH vec = NEW(Vector, num) DO
            IF y # NIL THEN
              FOR i := 0 TO num-1 DO
                vec[i] := y
              END
            END;
            RETURN vec
          END
        END
      |
        P.Vector => RETURN ListToVector(args)
      |
        P.VectorLength => RETURN SchemeInt.FromI(NUMBER(Vec(x)^))
      |
        
        P.VectorRef =>
        WITH vec = Vec(x),
             idx = SchemeLongReal.Int(y) DO
          CheckVectorIdx(vec,idx);
          RETURN vec[idx]
        END
      |
        P.VectorSet =>
        WITH v = Third(args),
             vec = Vec(x),
             idx = SchemeLongReal.Int(y) DO

          CheckVectorIdx(vec,idx);
          vec[idx] := v;
          RETURN v 
        END
      |
        P.ListToVector => RETURN ListToVector(x)
      |
        P.Map => free := FALSE; RETURN Map(Proc(x), Rest(args), interp, List1(NIL))
      |
        
        P.Foreach =>free := FALSE; RETURN Map(Proc(x), Rest(args), interp, NIL)
      |
        P.CallCC =>
        (* make a new arbitrary text --- N.B. changing this string
           must be synchronized with a modification to Scheme.Eval! *)
        WITH txt = "CallCC" & Fmt.Int(123),
             proc =  NEW(SchemeContinuation.T).init(txt) DO
          TRY 
            RETURN Proc(x).apply(interp, List1(proc))
          EXCEPT 
            E(e) => 
            IF e = txt THEN 
              RETURN proc.value 
            ELSE 
              RAISE E(e) 
            END
          END
        END
        
      |
        P.VectorToList => RETURN VectorToList(x)
      |
        P.Load => RETURN interp.loadFile(x)
      |
        P.Display => RETURN Write(x, OutPort(y, interp), FALSE,
                                  interp := interp)
      |
        P.DisplayNoFlush => RETURN Write(x, OutPort(y, interp), FALSE,
                                  interp := interp,
                                  flush := FALSE)
      |
        
        P.InputPortQ => RETURN Truth(x # NIL AND ISTYPE(x,SchemeInputPort.T))
      |
        P.CurrentInputPort => RETURN interp.input
      |
        P.OpenInputFile => RETURN OpenInputFile(x)
      |
        
        P.CloseInputPort => RETURN InPort(x, interp).close()
      |
        P.OutputportQ => RETURN Truth(x # NIL AND ISTYPE(x,Wr.T))
      |
        P.CurrentOutputPort => RETURN interp.output
      |
        
        P.OpenOutputFile => RETURN OpenOutputFile(x)
      |
        P.CloseOutputPort => 
        TRY
          Wr.Close(OutPort(x, interp)); RETURN True()
        EXCEPT
          Wr.Failure(err) => RAISE E("close-output-port: Wr.Failure: " & AL.Format(err))
        END

      |
        P.ReadChar => RETURN InPort(x, interp).readChar()
      |
        P.PeekChar => RETURN InPort(x, interp).peekChar()
      |
        P.Eval => RETURN interp.evalInGlobalEnv(x)
      |
        P.Quotient =>
        IF SchemeInt.IsExactInt(x) AND SchemeInt.IsExactInt(y) THEN
          VAR
            mx := SchemeInt.ToMpz(x);
            my := SchemeInt.ToMpz(y);
            mq := Mpz.New();
          BEGIN
            IF Mpz.cmp(my, MpzZero) = 0 THEN
              RETURN Error("quotient: division by zero")
            END;
            Mpz.tdiv_q(mq, mx, my);
            RETURN SchemeInt.MpzToScheme(mq)
          END
        ELSE
          VAR d := FromO(x) / FromO(y); BEGIN
            IF d > 0.0d0 THEN
              RETURN FromLR(FLOAT(FLOOR(d),LONGREAL))
            ELSE
              RETURN FromLR(FLOAT(CEILING(d),LONGREAL))
            END
          END
        END
      |
        P.Remainder =>
        IF SchemeInt.IsExactInt(x) AND SchemeInt.IsExactInt(y) THEN
          VAR
            mx := SchemeInt.ToMpz(x);
            my := SchemeInt.ToMpz(y);
            mr := Mpz.New();
          BEGIN
            IF Mpz.cmp(my, MpzZero) = 0 THEN
              RETURN Error("remainder: division by zero")
            END;
            Mpz.tdiv_r(mr, mx, my);
            RETURN SchemeInt.MpzToScheme(mr)
          END
        ELSE
          WITH a = TRUNC(FromO(x)), b = TRUNC(FromO(y)),
               r = a MOD b DO
            IF r # 0 AND (r > 0) # (a > 0) THEN
              RETURN FromLR(FLOAT(r - b, LONGREAL))
            ELSE
              RETURN FromLR(FLOAT(r, LONGREAL))
            END
          END
        END
      |
        P.IntDiv =>
        IF SchemeInt.IsExactInt(x) AND SchemeInt.IsExactInt(y) THEN
          VAR
            mx := SchemeInt.ToMpz(x);
            my := SchemeInt.ToMpz(y);
            mq := Mpz.New();
          BEGIN
            IF Mpz.cmp(my, MpzZero) = 0 THEN
              RETURN Error("div: division by zero")
            END;
            Mpz.fdiv_q(mq, mx, my);
            RETURN SchemeInt.MpzToScheme(mq)
          END
        ELSE
          RETURN FromLR(FLOAT(TRUNC(FromO(x)) DIV TRUNC(FromO(y)), LONGREAL))
        END
      |
        P.Modulo =>
        IF SchemeInt.IsExactInt(x) AND SchemeInt.IsExactInt(y) THEN
          VAR
            mx := SchemeInt.ToMpz(x);
            my := SchemeInt.ToMpz(y);
            mr := Mpz.New();
          BEGIN
            IF Mpz.cmp(my, MpzZero) = 0 THEN
              RETURN Error("modulo: division by zero")
            END;
            Mpz.fdiv_r(mr, mx, my);
            RETURN SchemeInt.MpzToScheme(mr)
          END
        ELSE
          RETURN FromLR(FLOAT(TRUNC(FromO(x)) MOD TRUNC(FromO(y)), LONGREAL))
        END
      |
        P.Third => RETURN Third(x)
      |
        P.EofObjectQ => RETURN Truth(x = SchemeInputPort.EOF)
      |
        P.Gcd =>
        IF args = NIL THEN RETURN Zero ELSE RETURN Gcd(args) END
      |
        P.Lcm =>
        IF args = NIL THEN RETURN One ELSE RETURN Lcm(args) END
      |
        P.Cxr =>
        VAR p := x; BEGIN
          FOR i := Text.Length(t.name)-2 TO 1 BY -1 DO
            IF Text.GetChar(t.name,i) = 'a' THEN
              p := PedanticFirst(p)
            ELSE
              p := PedanticRest(p)
            END
          END;
          RETURN p
        END
      |
        P.OddQ =>
        TYPECASE x OF
          SchemeInt.T(ri) => RETURN Truth(ABS(ri^) MOD 2 # 0)
        | Mpz.T(m) => RETURN Truth(Mpz.tstbit(m, 0) # 0)
        ELSE
          RETURN Truth(ABS(TRUNC(FromO(x))) MOD 2 # 0)
        END
      |
        P.EvenQ =>
        TYPECASE x OF
          SchemeInt.T(ri) => RETURN Truth(ABS(ri^) MOD 2 = 0)
        | Mpz.T(m) => RETURN Truth(Mpz.tstbit(m, 0) = 0)
        ELSE
          RETURN Truth(ABS(TRUNC(FromO(x))) MOD 2 = 0)
        END
      |
        P.ZeroQ =>
        TYPECASE x OF
          SchemeInt.T(ri) => RETURN Truth(ri^ = 0)
        | Mpz.T(m) => RETURN Truth(Mpz.cmp(m, MpzZero) = 0)
        ELSE
          RETURN Truth(FromO(x) = 0.0d0)
        END
      |
        P.PositiveQ =>
        TYPECASE x OF
          SchemeInt.T(ri) => RETURN Truth(ri^ > 0)
        | Mpz.T(m) => RETURN Truth(Mpz.cmp(m, MpzZero) > 0)
        ELSE
          RETURN Truth(FromO(x) > 0.0d0)
        END
      |
        P.NegativeQ =>
        TYPECASE x OF
          SchemeInt.T(ri) => RETURN Truth(ri^ < 0)
        | Mpz.T(m) => RETURN Truth(Mpz.cmp(m, MpzZero) < 0)
        ELSE
          RETURN Truth(FromO(x) < 0.0d0)
        END
      |
        P.CharCmpEq => RETURN Truth(CharCompare(x, y, FALSE) =  0)
      |
        P.CharCmpLt => RETURN Truth(CharCompare(x, y, FALSE) <  0)
      |
        P.CharCmpGt =>RETURN Truth(CharCompare(x, y, FALSE) >  0)
      |
        P.CharCmpGe => RETURN Truth(CharCompare(x, y, FALSE) >=  0)
      |
        P.CharCmpLe =>RETURN Truth(CharCompare(x, y, FALSE) <=  0)
      |
        P.CharCiCmpEq =>RETURN Truth(CharCompare(x, y, TRUE) =  0)
      |
        P.CharCiCmpLt =>RETURN Truth(CharCompare(x, y, TRUE) <  0)
      |
        P.CharCiCmpGt =>RETURN Truth(CharCompare(x, y, TRUE) >  0)
      |
        P.CharCiCmpGe =>RETURN Truth(CharCompare(x, y, TRUE) >=  0)
      |
        P.CharCiCmpLe =>RETURN Truth(CharCompare(x, y, TRUE) <=  0)
      |
        P.StringCmpEq => RETURN Truth(StringCompare(x, y, FALSE) =  0)
      |
        P.StringCmpLt => RETURN Truth(StringCompare(x, y, FALSE) <  0)
      |
        P.StringCmpGt => RETURN Truth(StringCompare(x, y, FALSE) >  0)
      |
        P.StringCmpGe => RETURN Truth(StringCompare(x, y, FALSE) >= 0)
      |
        P.StringCmpLe => RETURN Truth(StringCompare(x, y, FALSE) <= 0)
      |
        P.StringCiCmpEq => RETURN Truth(StringCompare(x, y, TRUE) =  0)
      |
        P.StringCiCmpLt => RETURN Truth(StringCompare(x, y, TRUE) <  0)
      |
        P.StringCiCmpGt => RETURN Truth(StringCompare(x, y, TRUE) >  0)
      |
        P.StringCiCmpGe => RETURN Truth(StringCompare(x, y, TRUE) >= 0)
      |
        P.StringCiCmpLe => RETURN Truth(StringCompare(x, y, TRUE) <= 0)
      |
        P.InexactQ => RETURN Truth(x # NIL AND ISTYPE(x, SchemeLongReal.T))
      |
        P.ExactQ => RETURN Truth(SchemeInt.IsExactInt(x))
      |
        P.IntegerQ =>
        IF SchemeInt.IsExactInt(x) THEN RETURN Truth(TRUE)
        ELSIF x # NIL AND ISTYPE(x, SchemeLongReal.T) THEN
          WITH lr = NARROW(x, SchemeLongReal.T)^ DO
            RETURN Truth(lr = FLOAT(ROUND(lr), LONGREAL))
          END
        ELSE
          RETURN Truth(FALSE)
        END
      |
        P.CallWithInputFile =>
        VAR p : SchemeInputPort.T := NIL;
        BEGIN
          TRY p := OpenInputFile(x);
            z := Proc(y).apply(interp, List1(p)) 
          FINALLY
            IF p # NIL THEN EVAL p.close() END
          END;
          RETURN z
        END
      |
        P.CallWithOutputFile => 
        VAR p : Wr.T := NIL;
        BEGIN
          TRY p := OpenOutputFile(x);
            z := Proc(y).apply(interp, List1(p))
          FINALLY
            IF p # NIL THEN 
              TRY
                Wr.Close(p) 
              EXCEPT
                Wr.Failure(err) => RAISE E("call-with-output-file: on close, Wr.Failure: " & AL.Format(err))

              END
            END
          END;
          RETURN z
        END

      |
        P.WithInputFromFile =>
        VAR p : SchemeInputPort.T := NIL;
            saved := interp.input;
        BEGIN
          TRY p := OpenInputFile(x);
            interp.input := p;
            z := Proc(y).apply(interp, NIL)
          FINALLY
            interp.input := saved;
            IF p # NIL THEN EVAL p.close() END
          END;
          RETURN z
        END
      |
        P.WithOutputToFile =>
        VAR p : Wr.T := NIL;
            saved := interp.output;
        BEGIN
          TRY p := OpenOutputFile(x);
            interp.output := p;
            z := Proc(y).apply(interp, NIL)
          FINALLY
            interp.output := saved;
            IF p # NIL THEN
              TRY
                Wr.Close(p)
              EXCEPT
                Wr.Failure(err) => RAISE E("with-output-to-file: on close, Wr.Failure: " & AL.Format(err))
              END
            END
          END;
          RETURN z
        END
      |
        P.CharReadyQ =>
        RETURN Truth(InPort(x, interp).charReady())
      |
        P.Tanh => RETURN FromLR(Math.tanh(FromO(x)))
      |
        P.Cosh => RETURN FromLR(Math.cosh(FromO(x)))
      |
        P.Sinh => RETURN FromLR(Math.sinh(FromO(x)))
      |
        P.Atanh => RETURN FromLR(Math.atanh(FromO(x)))
      |
        P.Acosh => RETURN FromLR(Math.acosh(FromO(x)))
      |
        P.Asinh => RETURN FromLR(Math.asinh(FromO(x)))

      |
        P.Random => RETURN FromLR(NEW(Random.Default).init().longreal(0.0d0,1.0d0))
      |
        P.NumberToLONGREAL => RETURN NumberToLONGREAL(x)
      |
        P.NumberToEXTENDED => RETURN NumberToEXTENDED(x)
      |
        P.NumberToREAL => RETURN NumberToREAL(x)
      |
        P.RefRecordFormat => RETURN SchemeString.FromText(
                                        RefRecord.Format(x))
      |
        P.SetRTErrorMapping => 
        interp.setRTErrorMapping(SchemeBoolean.TruthO(x)); 
        RETURN SchemeBoolean.Truth(interp.attemptToMapRuntimeErrors())
      |
        P.StringHaveSub =>
        RETURN SchemeBoolean.Truth(TextUtils.HaveSub(SchemeString.ToText(x),
                                                     SchemeString.ToText(y)))
      | 
        P.EnableTracebacks =>
        Scheme.DoTracebacks := TRUE;
        RETURN SchemeBoolean.Truth(Scheme.DoTracebacks)
      | 
        P.DisableTracebacks =>
        Scheme.DoTracebacks := FALSE;
        RETURN SchemeBoolean.Truth(Scheme.DoTracebacks)
      |
        P.Normal =>
        VAR mean := 0.0d0;
            sdev := 1.0d0;
            rand := NEW(Random.Default).init(); 
        BEGIN
          IF x # NIL THEN mean := SchemeLongReal.FromO(x) END;
          IF y # NIL THEN sdev := SchemeLongReal.FromO(y) END;
          RETURN FromLR(NormalDeviate(rand, mean, sdev))
        END
      |
        P.SetWarningsAreErrors =>
        SetWarningsAreErrors(TruthO(x));
        RETURN x
      |
        P.New => RETURN False() (* not impl *)
      |
        P.Class => RETURN False() (* not impl *)
      |
        P.Method => RETURN False() (* not impl *)
      |
        P.Exit => 
        IF x = NIL THEN Process.Exit(0) 
        ELSE Process.Exit(TRUNC(FromO(x)))
        END;
        <* ASSERT FALSE *>
      |
        P.SetCar => RETURN SetFirst(x,y)
      |
        P.SetCdr => RETURN SetRest(x,y)
      |
        P.TimeCall => 
        WITH start = Time.Now(),
             p = SchemeProcedure.Proc(x) DO
          EVAL p.apply(interp,Rest(args));
          RETURN SchemeLongReal.FromLR(Time.Now()-start)
        END
      |
        P.MacroExpand => RETURN SchemeMacro.MacroExpand(interp,x)
      |
        P.Error => RETURN Error(Stringify(args))
      |
        P.ListStar => free := FALSE; RETURN ListStar(args)
      |
        P.EqMemo => RETURN DoEqMemo(x)
      |
        P.EqualMemo => RETURN DoEqualMemo(x)
      |
        P.ChangeGlobalEnv => RETURN ChangeGlobalEnv(interp, x)
      |
        P.GetParentEnv => RETURN GetParentEnv(x)
      |
        P.DumpEnvironment => RETURN DumpEnvironment(interp, x)
      |
        P.LoadEnvironment => RETURN LoadEnvironment(interp, x)
      |
        P.ExactToInexact =>
        RETURN SchemeLongReal.FromLR(FromO(x))
      |
        P.InexactToExact =>
        IF SchemeInt.IsExactInt(x) THEN
          RETURN x
        ELSE
          WITH lr = FromO(x) DO
            IF Math.floor(lr) # lr THEN
              RETURN Error("inexact->exact: not an integer: " & Stringify(List1(x)))
            ELSIF lr >= FLOAT(FIRST(INTEGER), LONGREAL) AND
                  lr <= FLOAT(LAST(INTEGER), LONGREAL) THEN
              RETURN SchemeInt.FromI(ROUND(lr))
            ELSE
              (* large integer-valued float: convert via Mpz *)
              WITH m = Mpz.New() DO
                Mpz.set_d(m, lr);
                RETURN SchemeInt.MpzToScheme(m)
              END
            END
          END
        END
      |
        P.System =>
        TRY
          VAR
            cmd := SchemeString.ToText(First(args));
            si, so, se : File.T;
            child : Process.T;
            ret : Process.ExitCode;
          BEGIN
            Process.GetStandardFileHandles(si, so, se);
            child := Process.Create(
                       "/bin/sh", ARRAY OF TEXT{"-c", cmd},
                       stdin := si, stdout := so, stderr := se);
            ret := Process.Wait(child);
            RETURN SchemeInt.FromI(ret)
          END
        EXCEPT
          OSError.E(e) => RAISE E("system: " & AL.Format(e))
        END
      |
        P.BitwiseAnd =>
        free := FALSE;
        RETURN BitwiseNary(args, 'A')
      |
        P.BitwiseIor =>
        free := FALSE;
        RETURN BitwiseNary(args, 'O')
      |
        P.BitwiseXor =>
        free := FALSE;
        RETURN BitwiseNary(args, 'X')
      |
        P.BitwiseNot =>
        IF NOT SchemeInt.IsExactInt(x) THEN
          RETURN Error("bitwise-not: not an exact integer: " & Stringify(List1(x)))
        END;
        VAR mx := SchemeInt.ToMpz(x); mr := Mpz.New(); BEGIN
          Mpz.com(mr, mx);
          RETURN SchemeInt.MpzToScheme(mr)
        END
      |
        P.ArithmeticShift =>
        IF NOT SchemeInt.IsExactInt(x) OR NOT SchemeInt.IsExactInt(y) THEN
          RETURN Error("arithmetic-shift: not exact integers")
        END;
        VAR
          mx := SchemeInt.ToMpz(x);
          mr := Mpz.New();
          my := SchemeInt.ToMpz(y);
        BEGIN
          Mpz.ShiftMpz(mr, mx, my);
          RETURN SchemeInt.MpzToScheme(mr)
        END
      |
        P.IntegerLength =>
        IF NOT SchemeInt.IsExactInt(x) THEN
          RETURN Error("integer-length: not an exact integer: " & Stringify(List1(x)))
        END;
        VAR mx := SchemeInt.ToMpz(x); BEGIN
          IF Mpz.cmp(mx, MpzZero) < 0 THEN
            Mpz.com(mx, mx)  (* ~x for negative numbers *)
          END;
          IF Mpz.cmp(mx, MpzZero) = 0 THEN
            RETURN SchemeInt.FromI(0)
          ELSE
            RETURN SchemeInt.FromI(Mpz.sizeinbase(mx, 2))
          END
        END
      END
    END
  END Prims;

PROCEDURE DumpEnvironment( interp : Scheme.T; x : Object ) : Object 
  RAISES { E } =
  BEGIN
    IF x = NIL OR NOT ISTYPE(x, Wr.T) OR Wr.Closed(x) THEN
      RAISE E ("not an open Wr.T : " & Stringify(x))
    END;

    TRY
      interp.pickleGlobalEnv(x);
      RETURN x
    EXCEPT
      Wr.Failure(e) =>
      RAISE E ("Wr.Failure : " & AL.Format(e))
    |
      Pickle.Error =>
      RAISE E ("Pickle.Error")
    END
  END DumpEnvironment;

PROCEDURE LoadEnvironment( interp : Scheme.T; x : Object ) : Object 
  RAISES { E } =
  BEGIN
    IF x = NIL OR NOT ISTYPE(x, Rd.T) OR Rd.Closed(x) THEN
      RAISE E ("not an open Rd.T : " & Stringify(x))
    END;

    TRY
      interp.unpickleGlobalEnv(x);
      RETURN x
    EXCEPT
      Rd.Failure(e) =>
      RAISE E ("Rd.Failure : " & AL.Format(e))
    |
      Rd.EndOfFile =>
      RAISE E ("Rd.EndOfFile")
    |
      Pickle.Error =>
      RAISE E ("Pickle.Error")
    END
  END LoadEnvironment;

PROCEDURE GetParentEnv(   x      : Object) : Object
  RAISES { E } =
  BEGIN
    TYPECASE x OF
      SchemeEnvironment.T(env) => RETURN env.getParent()
    ELSE
      RAISE E ("expected an environment, got " & Stringify(x))
    END;
  END GetParentEnv;

PROCEDURE ChangeGlobalEnv(interp : Scheme.T;
                          x      : Object) : Object
  RAISES { E } =
  BEGIN
    IF NOT ISTYPE(x, SchemeEnvironment.T) THEN
      RAISE E ("expected an environment, got " & Stringify(x))
    END;

    WITH res = interp.getGlobalEnvironment() DO
      interp.changeGlobalEnvironment(x);
      RETURN res
    END
    
  END ChangeGlobalEnv;

PROCEDURE DoEqMemo(x : Object) : Object RAISES { E } =
  BEGIN
    IF x = NIL OR NOT ISTYPE(x, SchemeClosure.T) THEN
      RAISE E ("expected closure, got " & Stringify(x))
    END;

    RETURN NEW(MemObj, c := x, mem := NIL, apply := MOApply)
  END DoEqMemo;

PROCEDURE DoEqualMemo(x : Object) : Object RAISES { E } =
  BEGIN
    IF x = NIL OR NOT ISTYPE(x, SchemeClosure.T) THEN
      RAISE E ("expected closure, got " & Stringify(x))
    END;

    RETURN NEW(MemObj, c := x, mem := NIL, apply := MOEApply)
  END DoEqualMemo;

TYPE 
  Mem = OBJECT tag, val : Object; nxt : Mem END;

  MemObj = SchemeProcedure.T OBJECT
    c   : SchemeClosure.T;
    mem : Mem; 
  END;

PROCEDURE MOApply(mo     : MemObj; 
                  interp : Scheme.T; 
                  args   : Object) : Object RAISES { E } =
  VAR 
    p := mo.mem;
  BEGIN
    WITH a1 = First(args) DO
      WHILE p # NIL DO
        IF p.tag = a1 THEN RETURN p.val END;
        p := p.nxt
      END;

      WITH new = mo.c.apply(interp,args) DO
        mo.mem := NEW(Mem, tag := a1, val := new, nxt := mo.mem);
        RETURN new
      END
    END
  END MOApply;

PROCEDURE MOEApply(mo     : MemObj; 
                  interp : Scheme.T; 
                  args   : Object) : Object RAISES { E } =
  VAR 
    p := mo.mem;
  BEGIN
    WITH a1 = First(args) DO
      WHILE p # NIL DO
        IF Equal(p.tag,a1) THEN RETURN p.val END;
        p := p.nxt
      END;

      WITH new = mo.c.apply(interp,args) DO
        mo.mem := NEW(Mem, tag := a1, val := new, nxt := mo.mem);
        RETURN new
      END
    END
  END MOEApply;

(**********************************************************************)

PROCEDURE IsList(x : Object) : BOOLEAN =
  VAR
    slow, fast := x;
  BEGIN
    LOOP
      IF fast = NIL THEN RETURN TRUE END;
      IF slow = Rest(fast) OR NOT ISTYPE(fast, Pair) OR 
         slow = NIL OR NOT ISTYPE(slow, Pair) THEN
        RETURN FALSE
      END;
      slow := Rest(slow);
      fast := Rest(fast);
      IF fast = NIL THEN RETURN TRUE END;
      IF NOT ISTYPE(fast, Pair) THEN RETURN FALSE END;
      fast := Rest(fast)
    END
  END IsList;

PROCEDURE Append(args : Object; interp : Scheme.T := NIL) : Object =
  BEGIN
    IF Rest(args) = NIL THEN RETURN First(args) 
    ELSE RETURN Append2(First(args), Append(Rest(args)),interp)
    END
  END Append;

PROCEDURE Append2(x, y : Object; interp : Scheme.T := NIL) : Object =
  BEGIN
    IF x # NIL AND ISTYPE(x,Pair) THEN RETURN Cons(First(x),
                                                   Append2(Rest(x),y,interp),
                                                   interp)
    ELSE RETURN y
    END
  END Append2;

PROCEDURE MemberAssoc(obj, list : Object; m, eq : CHAR) : Object RAISES { E } =
  BEGIN
    WHILE list # NIL AND ISTYPE(list, Pair) DO
      VAR target : Object; 
          found : BOOLEAN;
      BEGIN
        IF m = 'm' THEN target := First(list) ELSE 
          target := First(First(list)) 
        END;

        CASE eq OF
          'q' => found := target = obj
        |
          'v' => found := Eqv(target,obj)
        |
          ' ' => found := Equal(target,obj)
        ELSE
          EVAL Warn("Bad option to memberAssoc:" & Text.FromChar(eq)); 
          RETURN False()
        END;
        
        IF found THEN
          IF m = 'm' THEN RETURN list ELSE RETURN First(list) END
        END;

        list := Rest(list)
      END
    END;
    RETURN False()
  END MemberAssoc;

PROCEDURE NumCompare(args : Object; op : CHAR) : Object RAISES { E } =

  PROCEDURE CmpTwo(a, b : Object) : INTEGER RAISES { E } =
    VAR aExact := SchemeInt.IsExactInt(a);
        bExact := SchemeInt.IsExactInt(b);
    BEGIN
      IF aExact AND bExact THEN
        RETURN SchemeInt.Compare(a, b)
      ELSE
        WITH ax = FromO(a), bx = FromO(b) DO
          IF ax # ax OR bx # bx THEN
            (* NaN: unordered *)
            RETURN LAST(INTEGER)
          ELSIF ax < bx THEN RETURN -1
          ELSIF ax > bx THEN RETURN 1
          ELSE RETURN 0
          END
        END
      END
    END CmpTwo;

  BEGIN
    WHILE Rest(args) # NIL AND ISTYPE(Rest(args), Pair) DO
      VAR
        a := First(args);
        b : Object;
        c : INTEGER;
      BEGIN
        args := Rest(args);
        b := First(args);
        c := CmpTwo(a, b);

        IF c = LAST(INTEGER) THEN
          (* NaN: all comparisons are false, except maybe = for NaN *)
          IF op = '=' THEN RETURN False() END;
          RETURN False()
        END;
        CASE op OF
          '>' => IF NOT c >  0 THEN RETURN False() END
        |
          '<' => IF NOT c <  0 THEN RETURN False() END
        |
          '=' => IF NOT c =  0 THEN RETURN False() END
        |
          'L' => IF NOT c <= 0 THEN RETURN False() END
        |
          'G' => IF NOT c >= 0 THEN RETURN False() END
        ELSE
          <* ASSERT FALSE *>
        END
      END
    END;
    RETURN True()
  END NumCompare;
      
PROCEDURE HasInexact(args : Object) : BOOLEAN =
  (* Scan arg list for any inexact (SchemeLongReal.T) *)
  VAR p := args;
  BEGIN
    WHILE p # NIL AND ISTYPE(p, SchemePair.T) DO
      IF ISTYPE(NARROW(p, SchemePair.T).first, SchemeLongReal.T) THEN
        RETURN TRUE
      END;
      p := Rest(p)
    END;
    RETURN FALSE
  END HasInexact;

PROCEDURE ExactAdd(a, b : Object) : Object =
  BEGIN
    TYPECASE a OF
      SchemeInt.T(ra) =>
      TYPECASE b OF
        SchemeInt.T(rb) => RETURN SchemeInt.Add(ra^, rb^)
      | Mpz.T(mb) =>
        VAR ma := Mpz.NewInt(ra^); mr := Mpz.New(); BEGIN
          Mpz.add(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      ELSE <* ASSERT FALSE *>
      END
    | Mpz.T(ma) =>
      TYPECASE b OF
        SchemeInt.T(rb) =>
        VAR mb := Mpz.NewInt(rb^); mr := Mpz.New(); BEGIN
          Mpz.add(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      | Mpz.T(mb) =>
        VAR mr := Mpz.New(); BEGIN
          Mpz.add(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      ELSE <* ASSERT FALSE *>
      END
    ELSE <* ASSERT FALSE *>
    END
  END ExactAdd;

PROCEDURE ExactSub(a, b : Object) : Object =
  BEGIN
    TYPECASE a OF
      SchemeInt.T(ra) =>
      TYPECASE b OF
        SchemeInt.T(rb) => RETURN SchemeInt.Sub(ra^, rb^)
      | Mpz.T(mb) =>
        VAR ma := Mpz.NewInt(ra^); mr := Mpz.New(); BEGIN
          Mpz.sub(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      ELSE <* ASSERT FALSE *>
      END
    | Mpz.T(ma) =>
      TYPECASE b OF
        SchemeInt.T(rb) =>
        VAR mb := Mpz.NewInt(rb^); mr := Mpz.New(); BEGIN
          Mpz.sub(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      | Mpz.T(mb) =>
        VAR mr := Mpz.New(); BEGIN
          Mpz.sub(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      ELSE <* ASSERT FALSE *>
      END
    ELSE <* ASSERT FALSE *>
    END
  END ExactSub;

PROCEDURE ExactMul(a, b : Object) : Object =
  BEGIN
    TYPECASE a OF
      SchemeInt.T(ra) =>
      TYPECASE b OF
        SchemeInt.T(rb) => RETURN SchemeInt.Mul(ra^, rb^)
      | Mpz.T(mb) =>
        VAR ma := Mpz.NewInt(ra^); mr := Mpz.New(); BEGIN
          Mpz.mul(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      ELSE <* ASSERT FALSE *>
      END
    | Mpz.T(ma) =>
      TYPECASE b OF
        SchemeInt.T(rb) =>
        VAR mb := Mpz.NewInt(rb^); mr := Mpz.New(); BEGIN
          Mpz.mul(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      | Mpz.T(mb) =>
        VAR mr := Mpz.New(); BEGIN
          Mpz.mul(mr, ma, mb); RETURN SchemeInt.MpzToScheme(mr)
        END
      ELSE <* ASSERT FALSE *>
      END
    ELSE <* ASSERT FALSE *>
    END
  END ExactMul;

(* --- Generic two-argument arithmetic for compiled code --- *)

PROCEDURE NumericAdd(a, b : Object) : Object RAISES { E } =
  BEGIN
    IF SchemeInt.IsExactInt(a) AND SchemeInt.IsExactInt(b) THEN
      RETURN ExactAdd(a, b)
    ELSE
      RETURN FromLR(FromO(a) + FromO(b))
    END
  END NumericAdd;

PROCEDURE NumericSub(a, b : Object) : Object RAISES { E } =
  BEGIN
    IF SchemeInt.IsExactInt(a) AND SchemeInt.IsExactInt(b) THEN
      RETURN ExactSub(a, b)
    ELSE
      RETURN FromLR(FromO(a) - FromO(b))
    END
  END NumericSub;

PROCEDURE NumericMul(a, b : Object) : Object RAISES { E } =
  BEGIN
    IF SchemeInt.IsExactInt(a) AND SchemeInt.IsExactInt(b) THEN
      RETURN ExactMul(a, b)
    ELSE
      RETURN FromLR(FromO(a) * FromO(b))
    END
  END NumericMul;

PROCEDURE NumericEQ(a, b : Object) : BOOLEAN RAISES { E } =
  BEGIN
    IF SchemeInt.IsExactInt(a) AND SchemeInt.IsExactInt(b) THEN
      RETURN SchemeInt.Compare(a, b) = 0
    ELSE
      RETURN FromO(a) = FromO(b)
    END
  END NumericEQ;

PROCEDURE NumericLT(a, b : Object) : BOOLEAN RAISES { E } =
  BEGIN
    IF SchemeInt.IsExactInt(a) AND SchemeInt.IsExactInt(b) THEN
      RETURN SchemeInt.Compare(a, b) < 0
    ELSE
      RETURN FromO(a) < FromO(b)
    END
  END NumericLT;

PROCEDURE NumericGT(a, b : Object) : BOOLEAN RAISES { E } =
  BEGIN
    IF SchemeInt.IsExactInt(a) AND SchemeInt.IsExactInt(b) THEN
      RETURN SchemeInt.Compare(a, b) > 0
    ELSE
      RETURN FromO(a) > FromO(b)
    END
  END NumericGT;

PROCEDURE NumericLE(a, b : Object) : BOOLEAN RAISES { E } =
  BEGIN
    IF SchemeInt.IsExactInt(a) AND SchemeInt.IsExactInt(b) THEN
      RETURN SchemeInt.Compare(a, b) <= 0
    ELSE
      RETURN FromO(a) <= FromO(b)
    END
  END NumericLE;

PROCEDURE NumericGE(a, b : Object) : BOOLEAN RAISES { E } =
  BEGIN
    IF SchemeInt.IsExactInt(a) AND SchemeInt.IsExactInt(b) THEN
      RETURN SchemeInt.Compare(a, b) >= 0
    ELSE
      RETURN FromO(a) >= FromO(b)
    END
  END NumericGE;

PROCEDURE DoAbs(x : Object) : Object RAISES { E } =
  BEGIN
    TYPECASE x OF
      SchemeInt.T(ri) =>
      IF ri^ >= 0 THEN RETURN x
      ELSIF ri^ = FIRST(INTEGER) THEN
        (* -FIRST(INTEGER) overflows *)
        RETURN Mpz.NewInt(LAST(INTEGER))  (* close enough for ABS(FIRST(INTEGER)) *)
      ELSE
        RETURN SchemeInt.FromI(-ri^)
      END
    | Mpz.T(m) =>
      VAR mr := Mpz.New(); BEGIN
        Mpz.abs(mr, m); RETURN SchemeInt.MpzToScheme(mr)
      END
    ELSE
      RETURN FromLR(ABS(FromO(x)))
    END
  END DoAbs;

PROCEDURE NumComputeStart(args : Object; op : CHAR; start : Object) : Object
  RAISES { E } =
  BEGIN
    IF NOT HasInexact(args) AND SchemeInt.IsExactInt(start) THEN
      (* exact path *)
      RETURN NumComputeExact(args, op, start)
    ELSE
      (* inexact path *)
      RETURN NumComputeInexact(args, op, FromO(start))
    END
  END NumComputeStart;

PROCEDURE NumCompute(args : Object; op : CHAR) : Object
  RAISES { E } =
  BEGIN
    IF args = NIL THEN
      CASE op OF
        '+' => RETURN SchemeInt.Zero
      | '*' => RETURN SchemeInt.One
      ELSE <* ASSERT FALSE *>
      END
    ELSIF NOT HasInexact(args) AND SchemeInt.IsExactInt(First(args)) THEN
      RETURN NumComputeExact(Rest(args), op, First(args))
    ELSE
      RETURN NumComputeInexact(Rest(args), op, FromO(First(args)))
    END
  END NumCompute;

PROCEDURE NumComputeExact(args : Object; op : CHAR; start : Object) : Object
  RAISES { E } =
  VAR
    result := start;
  BEGIN
    IF args = NIL THEN
      (* unary - or / *)
      CASE op OF
        '-' => RETURN ExactSub(SchemeInt.Zero, result)
      | '/' =>
        (* 1/x -- if x divides 1 exactly, return exact, else inexact *)
        IF SchemeInt.IsExactInt(result) THEN
          TYPECASE result OF
            SchemeInt.T(ri) =>
            IF ri^ = 1 THEN RETURN SchemeInt.One
            ELSIF ri^ = -1 THEN RETURN SchemeInt.NegOne
            ELSIF ri^ = 0 THEN RETURN Error("/: division by zero")
            ELSE RETURN FromLR(1.0d0 / FromO(result))
            END
          ELSE
            RETURN FromLR(1.0d0 / FromO(result))
          END
        ELSE
          RETURN FromLR(1.0d0 / FromO(result))
        END
      | 'X', 'N' => RETURN result
      ELSE
        RETURN result
      END
    ELSE
      WHILE args # NIL AND ISTYPE(args, SchemePair.T) DO
        WITH arg = NARROW(args, SchemePair.T).first DO
          IF NOT SchemeInt.IsExactInt(arg) THEN
            (* switch to inexact path *)
            RETURN NumComputeInexact(Rest(args), op,
                     InexactOp(op, FromO(result), FromO(arg)))
          END;
          CASE op OF
            '+' => result := ExactAdd(result, arg)
          | '-' => result := ExactSub(result, arg)
          | '*' => result := ExactMul(result, arg)
          | '/' =>
            IF SchemeInt.IsZero(arg) THEN
              RETURN Error("/: division by zero")
            END;
            (* check if division is exact *)
            VAR
              mr := SchemeInt.ToMpz(result);
              md := SchemeInt.ToMpz(arg);
              mq := Mpz.New();
              mrem := Mpz.New();
            BEGIN
              Mpz.tdiv_qr(mq, mrem, mr, md);
              IF Mpz.cmp(mrem, MpzZero) = 0 THEN
                result := SchemeInt.MpzToScheme(mq)
              ELSE
                (* not exact: switch to inexact *)
                RETURN NumComputeInexact(Rest(args), op,
                         FromO(result) / FromO(arg))
              END
            END
          | 'X' => (* max *)
            IF SchemeInt.Compare(arg, result) > 0 THEN result := arg END
          | 'N' => (* min *)
            IF SchemeInt.Compare(arg, result) < 0 THEN result := arg END
          ELSE
            <* ASSERT FALSE *>
          END
        END;
        args := Rest(args)
      END;
      RETURN result
    END
  END NumComputeExact;

PROCEDURE InexactOp(op : CHAR; a, b : LONGREAL) : LONGREAL =
  BEGIN
    CASE op OF
      '+' => RETURN a + b
    | '-' => RETURN a - b
    | '*' => RETURN a * b
    | '/' => RETURN a / b
    | 'X' => IF b > a THEN RETURN b ELSE RETURN a END
    | 'N' => IF b < a THEN RETURN b ELSE RETURN a END
    ELSE
      <* ASSERT FALSE *>
    END
  END InexactOp;

PROCEDURE NumComputeInexact(args : Object; op : CHAR; start : LONGREAL) : Object
  RAISES { E } =
  VAR
    result := start;
  BEGIN
    IF args = NIL THEN
      CASE op OF
        '-' => RETURN FromLR(0.0d0 - result)
      | '/' =>
        IF result = 0.0d0 THEN
          RETURN Error("/: division by zero")
        END;
        RETURN FromLR(1.0d0 / result)
      ELSE
        RETURN FromLR(result)
      END
    ELSE
      WHILE args # NIL AND ISTYPE(args, SchemePair.T) DO
        WITH x = FromO(NARROW(args, SchemePair.T).first) DO
          result := InexactOp(op, result, x)
        END;
        args := Rest(args)
      END;
      RETURN FromLR(result)
    END
  END NumComputeInexact;

PROCEDURE NumberToLONGREAL(x : Object) : Object RAISES { E } =
  BEGIN
    RETURN SchemeString.FromText(Fmt_LongReal(FromO(x), literal := TRUE))
  END NumberToLONGREAL;

PROCEDURE NumberToEXTENDED(x : Object) : Object RAISES { E } =
  BEGIN
    RETURN SchemeString.FromText(Fmt_Extended(FromO(x), literal := TRUE))
  END NumberToEXTENDED;

PROCEDURE NumberToREAL(x : Object) : Object RAISES { E } =
  BEGIN
    RETURN SchemeString.FromText(Fmt_Real(FromO(x), literal := TRUE))
  END NumberToREAL;

PROCEDURE NumberToString(x, y : Object) : Object RAISES { E } =
  VAR
    base : INTEGER;
  BEGIN
    IF y # NIL AND SchemeInt.IsNumber(y) THEN
      base := SchemeLongReal.Int(y, roundOK := TRUE)
    ELSE
      base := 10
    END;

    IF base < 2 THEN base := 2 ELSIF base > 16 THEN base := 16 END;

    (* exact integers: use Mpz formatting or Fmt.Int *)
    TYPECASE x OF
      SchemeInt.T(ri) =>
      IF base = 10 THEN
        RETURN SchemeString.FromText(Fmt.Int(ri^))
      ELSE
        RETURN SchemeString.FromText(Fmt.Int(ri^, base := base))
      END
    | Mpz.T(m) =>
      IF base = 10 THEN
        RETURN SchemeString.FromText(Mpz.FormatDecimal(m))
      ELSIF base = 16 THEN
        RETURN SchemeString.FromText(Mpz.FormatHexadecimal(m))
      ELSIF base = 8 THEN
        RETURN SchemeString.FromText(Mpz.FormatOctal(m))
      ELSE
        RETURN SchemeString.FromText(Mpz.FormatBased(m, base))
      END
    ELSE
      (* inexact *)
      IF base # 10 THEN
        RETURN SchemeString.FromText(Fmt.Int(ROUND(FromO(x)), base := base))
      ELSE
        RETURN SchemeString.FromText(Fmt_LongReal(FromO(x)))
      END
    END
  END NumberToString;

PROCEDURE Fmt_LongReal(lr : LONGREAL; literal := FALSE) : TEXT RAISES { E } =
  VAR txt : TEXT;
  BEGIN
    TRY
      txt := Fmt.LongReal(lr, literal := literal);
    EXCEPT
      FloatMode.Trap => RAISE E("Cannot format that as LONGREAL")
    END;
    (* Ensure a decimal point so inexact numbers are distinguishable
       from exact integers.  Skip for NaN/Infinity. *)
    IF Text.FindChar(txt, '.') < 0 AND
       Text.FindChar(txt, 'e') < 0 AND
       Text.FindChar(txt, 'E') < 0 AND
       Text.FindChar(txt, 'N') < 0 AND
       Text.FindChar(txt, 'I') < 0 THEN
      txt := txt & ".0"
    END;
    RETURN txt
  END Fmt_LongReal;

PROCEDURE Fmt_Extended(lr : LONGREAL; literal := FALSE) : TEXT RAISES { E } =
  (* this is a hack ? *)
  BEGIN
    TRY
      IF FLOAT(ROUND(lr),LONGREAL) = lr THEN
        RETURN Fmt.Int(ROUND(lr))
      ELSIF FLOAT(LAST(CARDINAL),LONGREAL) = lr THEN
        (* tricky special case for 64-bit machines.  Possible loss
           of precision! *)
        RETURN Fmt.Int(LAST(CARDINAL))
      ELSIF ABS(lr) > 1.0d10 AND FLOAT(FIRST(INTEGER),LONGREAL) = lr THEN
        (* this is actually wrong... but compiler problems *)
        RETURN "-" & Fmt.Int(LAST(INTEGER))
      ELSIF ABS(lr) > 1.0d10 AND 
        lr >= FLOAT(FIRST(INTEGER), LONGREAL) AND  
        lr <= FLOAT(LAST(INTEGER), LONGREAL) THEN
        WITH o  = Fmt.LongReal(ABS(lr)),
             s  = Scan.LongReal(o),
             o1 = Fmt.LongReal(ABS(lr)-1.0d0),
             s1 = Scan.LongReal(o1) DO
          IF s = s1 THEN
            RETURN Fmt.Int(ROUND(lr))
          ELSE
            RETURN Fmt.Extended(FLOAT(lr,EXTENDED),literal := literal)
          END
        END
      ELSE
        RETURN Fmt.Extended(FLOAT(lr,EXTENDED),literal := literal)
      END
    EXCEPT
      Lex.Error, FloatMode.Trap => RAISE E("Cannot format that as LONGREAL")
    END 
  END Fmt_Extended;

PROCEDURE Fmt_Real(lr : LONGREAL; literal := FALSE) : TEXT RAISES { E } =
  BEGIN
    TRY
      IF    lr < FLOAT(FIRST(REAL), LONGREAL) THEN
        Debug.Warning("Loss of accuracy, number too negative for REAL type.");
      RETURN Fmt.Real(FIRST(REAL), literal := literal)
      ELSIF lr > FLOAT(LAST(REAL), LONGREAL) THEN
        Debug.Warning("Loss of accuracy, number too positive for REAL type.");
        RETURN Fmt.Real(LAST(REAL), literal := literal)
      END;
      
      (* number is in range *)

      WITH r = FLOAT(lr, REAL) DO
        IF FLOAT(ROUND(lr),LONGREAL) = lr THEN
          RETURN Fmt.Int(ROUND(lr))
        ELSIF FLOAT(LAST(CARDINAL),LONGREAL) = lr THEN
          (* tricky special case for 64-bit machines.  Possible loss
             of precision! *)
          RETURN Fmt.Int(LAST(CARDINAL))
        ELSIF ABS(lr) > 1.0d10 AND FLOAT(FIRST(INTEGER),LONGREAL) = lr THEN
          (* this is actually wrong... but compiler problems *)
          RETURN "-" & Fmt.Int(LAST(INTEGER))
        ELSIF ABS(lr) > 1.0d10 AND 
          lr >= FLOAT(FIRST(INTEGER), LONGREAL) AND  
          lr <= FLOAT(LAST(INTEGER), LONGREAL) THEN
          WITH o  = Fmt.Real(ABS(r)),
               s  = Scan.Real(o),
               o1 = Fmt.Real(ABS(r)-1.0e0),
               s1 = Scan.Real(o1) DO
            IF s = s1 THEN
              RETURN Fmt.Int(ROUND(lr))
            ELSE
              RETURN Fmt.Real(r, literal := literal)
            END
          END
        ELSE
          RETURN Fmt.Real(r, literal := literal)
        END
      END
    EXCEPT
      Lex.Error, FloatMode.Trap => RAISE E("Cannot format that as REAL")
    END
  END Fmt_Real;

PROCEDURE StringToNumber(x, y : Object) : Object RAISES { E } =
  VAR base : INTEGER;
  BEGIN
    IF y # NIL AND SchemeInt.IsNumber(y) THEN
      base := SchemeLongReal.Int(y, roundOK := TRUE)
    ELSE
      base := 10
    END;

    IF base < 2 THEN base := 2 ELSIF base > 16 THEN base := 16 END;

    VAR
      str : TEXT;
    BEGIN
      IF ISTYPE(x, SchemeString.T) THEN
        str := SchemeString.ToText(x)
      ELSE
        str := StringifyQ(x,FALSE)
      END;

      IF base = 10 THEN
        (* try integer first, then float *)
        IF IsPureIntegerStr(str) THEN
          TRY
            RETURN SchemeInt.FromI(Scan.Int(str))
          EXCEPT Lex.Error, FloatMode.Trap =>
            VAR m: Object; BEGIN
              TRY m := Mpz.InitScan(str, 10)
              EXCEPT ELSE m := NIL END;
              IF m # NIL THEN RETURN m END
            END
          END
        END;
        TRY
          RETURN FromLR(Scan.LongReal(str))
        EXCEPT
          FloatMode.Trap, Lex.Error => RETURN False()
        END
      ELSE
        TRY
          RETURN SchemeInt.FromI(Scan.Int(str, defaultBase := base))
        EXCEPT
          Lex.Error, FloatMode.Trap =>
          VAR m: Object; BEGIN
            TRY m := Mpz.InitScan(str, base)
            EXCEPT ELSE m := NIL END;
            IF m # NIL THEN RETURN m END;
            RETURN False()
          END
        END
      END
    END
  END StringToNumber;

PROCEDURE IsPureIntegerStr(txt : TEXT) : BOOLEAN =
  VAR
    start := 0;
    len := Text.Length(txt);
  BEGIN
    IF len = 0 THEN RETURN FALSE END;
    IF Text.GetChar(txt, 0) = '+' OR Text.GetChar(txt, 0) = '-' THEN
      start := 1
    END;
    IF start >= len THEN RETURN FALSE END;
    FOR i := start TO len-1 DO
      WITH c = Text.GetChar(txt, i) DO
        IF c < '0' OR c > '9' THEN RETURN FALSE END
      END
    END;
    RETURN TRUE
  END IsPureIntegerStr;

PROCEDURE Gcd(args : Object) : Object RAISES { E } =
  VAR
    allExact := TRUE;
    mg := Mpz.NewInt(0);
    mt := Mpz.New();
  BEGIN
    VAR p := args; BEGIN
      WHILE p # NIL AND ISTYPE(p, Pair) DO
        IF NOT SchemeInt.IsExactInt(First(p)) THEN allExact := FALSE END;
        p := Rest(p)
      END
    END;
    IF allExact THEN
      WHILE args # NIL AND ISTYPE(args, Pair) DO
        mt := SchemeInt.ToMpz(First(args));
        Mpz.abs(mt, mt);
        Mpz.gcd(mg, mg, mt);
        args := Rest(args)
      END;
      RETURN SchemeInt.MpzToScheme(mg)
    ELSE
      VAR gcd := 0; BEGIN
        WHILE args # NIL AND ISTYPE(args, Pair) DO
          gcd := Gcd2(ROUND(ABS(FromO(First(args)))), gcd);
          args := Rest(args)
        END;
        RETURN FromLR(FLOAT(gcd,LONGREAL))
      END
    END
  END Gcd;

PROCEDURE Gcd2(a, b : INTEGER) : INTEGER =
  BEGIN
    IF b = 0 THEN RETURN a
    ELSE RETURN Gcd2(b, a MOD b)
    END
  END Gcd2;

PROCEDURE Lcm(args : Object) : Object RAISES { E } =
  VAR
    allExact := TRUE;
    ml := Mpz.NewInt(1);
    mt := Mpz.New();
  BEGIN
    VAR p := args; BEGIN
      WHILE p # NIL AND ISTYPE(p, Pair) DO
        IF NOT SchemeInt.IsExactInt(First(p)) THEN allExact := FALSE END;
        p := Rest(p)
      END
    END;
    IF allExact THEN
      WHILE args # NIL AND ISTYPE(args, Pair) DO
        mt := SchemeInt.ToMpz(First(args));
        Mpz.abs(mt, mt);
        Mpz.lcm(ml, ml, mt);
        args := Rest(args)
      END;
      RETURN SchemeInt.MpzToScheme(ml)
    ELSE
      VAR L, g := 1; BEGIN
        WHILE args # NIL AND ISTYPE(args, Pair) DO
          WITH n = ABS(ROUND(FromO(First(args)))) DO
            g := Gcd2(n, L);
            IF g = 0 THEN
              L := g
            ELSE
              L := (n DIV g) * L
            END;
            args := Rest(args)
          END
        END;
        RETURN FromLR(FLOAT(L,LONGREAL))
      END
    END
  END Lcm;

PROCEDURE CharCompare(x, y : Object; ci : BOOLEAN) : INTEGER RAISES { E } =
  BEGIN
    IF ci THEN RETURN ORD(Downcase(Char(x))) - ORD(Downcase(Char(y)))
    ELSE       RETURN ORD(Char(x)) - ORD(Char(y))
    END         
  END CharCompare;

PROCEDURE StringCompare(x, y : Object; ci : BOOLEAN) : INTEGER RAISES { E } =
  BEGIN
    IF x # NIL AND y # NIL AND ISTYPE(x, String) AND ISTYPE(y, String) THEN
      WITH xc = NARROW(x,String), yc = NARROW(y, String) DO
        FOR i := 0 TO MIN(LAST(xc^),LAST(yc^)) DO
          VAR diff : INTEGER; BEGIN
            IF ci THEN
              diff := ORD(Upcase(xc[i])) - ORD(Upcase(yc[i]))
            ELSE
              diff := ORD(xc[i]) - ORD(yc[i])
            END;
            IF diff # 0 THEN RETURN diff END
          END
        END;
        
        RETURN NUMBER(xc^) - NUMBER(yc^)
      END
    ELSE
      EVAL Error("expected two strings, got: " & Stringify(List2(x, y)));
      RETURN 0
    END
  END StringCompare;

PROCEDURE StringAppend(interp : Scheme.T;
                       args : Object) : String  RAISES { E } =
  BEGIN
    IF interp.wx = NIL THEN interp.wx := Wx.New() END;
    IF interp.refseq = NIL THEN interp.refseq := NEW(RefSeq.T) END;

    EVAL interp.refseq.init();

    WHILE args # NIL AND ISTYPE(args,Pair) DO
      StringifyB(First(args),FALSE,interp.wx,interp.refseq);
      args := Rest(args)
    END;
    RETURN SchemeString.FromText(Wx.ToText(interp.wx))
  END StringAppend;

PROCEDURE OpenOutputFile(filename : Object) : Wr.T RAISES { E } =
  BEGIN
    TRY
      RETURN FileWr.Open(StringifyQ(filename, FALSE)) 
    EXCEPT
      OSError.E(err) => RETURN Error("Error opening " & Stringify(filename) & " : "&
                                     AL.Format(err))
    END
  END OpenOutputFile;

PROCEDURE OpenInputFile(filename : Object) : SchemeInputPort.T RAISES { E } =
  BEGIN
    TRY
      WITH rd = FileRd.Open(StringifyQ(filename, FALSE)) DO
        RETURN NEW(SchemeInputPort.T).init(rd)
      END
    EXCEPT
      OSError.E(err) => RETURN Error("Error opening " & Stringify(filename) & " : "&
                                     AL.Format(err))
    END 
  END OpenInputFile;

PROCEDURE Map(proc : SchemeProcedure.T;
              args : Object;
              interp : Scheme.T;
              result : Pair) : Pair RAISES { E } =
  VAR
    accum := result;
  BEGIN
    IF Rest(args) = NIL THEN
      args := First(args);
      WHILE args # NIL AND ISTYPE(args, Pair) DO
        WITH x = proc.apply(interp, List1(First(args),interp)) DO
          IF accum # NIL THEN 
            accum.rest := List1(x,interp);
            accum := accum.rest;
          END;
          args := Rest(args)
        END
      END
    ELSE
      WITH car = Proc(interp.evalInGlobalEnv(SchemeSymbol.Symbol("car"))),
           cdr = Proc(interp.evalInGlobalEnv(SchemeSymbol.Symbol("cdr"))) DO
        WHILE First(args) # NIL AND ISTYPE(First(args),Pair) DO
          WITH x = proc.apply(interp, Map(car, List1(args,interp), interp, List1(NIL,interp))) DO
            IF accum # NIL THEN 
              accum.rest := List1(x,interp);
              accum := accum.rest;
            END
          END;
          args := Map(cdr, List1(args,interp), interp, List1(NIL,interp))
        END
      END
    END;
    RETURN Rest(result)
  END Map;

PROCEDURE NormalDeviate(rand : Random.T; mean, sdev : LONGREAL) : LONGREAL =
  (* this is called a Box-Muller transformation.
     Num. Recip. in Fortran 77, sec. 7-2 *)
  VAR
    v1, v2, rsq : LONGREAL;
  BEGIN
    REPEAT
      v1 := 2.0d0 * rand.longreal(0.0d0,1.0d0)-1.0d0;
      v2 := 2.0d0 * rand.longreal(0.0d0,1.0d0)-1.0d0;
      rsq := v1*v1 + v2*v2
    UNTIL rsq > 0.0d0 AND rsq < 1.0d0;
    
    WITH fac = Math.sqrt(-2.0d0*Math.log(rsq)/rsq) DO
      RETURN mean + sdev*v2*fac
    END
  END NormalDeviate;
       
VAR MpzZero := Mpz.NewInt(0);

BEGIN END SchemePrimitive.
