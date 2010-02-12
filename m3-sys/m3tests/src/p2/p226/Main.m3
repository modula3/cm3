UNSAFE MODULE Main;
FROM Cstdint IMPORT uint8_t, uint16_t, uint32_t, uint64_t;
IMPORT RTIO;
IMPORT AtomicAddress;
IMPORT AtomicBoolean;
IMPORT AtomicChar;
IMPORT AtomicInteger;
IMPORT AtomicLongint;
IMPORT AtomicRefany;
IMPORT AtomicWideChar;
IMPORT Address;
IMPORT Boolean;
IMPORT Char;
IMPORT Integer;
IMPORT Longint;
IMPORT Refany;
IMPORT WideChar;

VAR
    atomicBooleanA: AtomicBoolean.T;
    atomicCharA: AtomicChar.T;
    atomicIntegerA: AtomicInteger.T;
    atomicLongintA: AtomicLongint.T;
    atomicRefanyA: AtomicRefany.T;
    atomicWidecharA: AtomicWideChar.T;

    integerA: Integer.T;
    booleanA: Boolean.T;
    charA: Char.T;
    longintA: Longint.T;
    refanyA: Refany.T;
    widecharA: WideChar.T;

    integerB: Integer.T;
    booleanB: Boolean.T;
    charB: Char.T;
    longintB: Longint.T;
    refanyB: Refany.T;
    widecharB: WideChar.T;

    integerC: Integer.T;
    booleanC: Boolean.T;
    charC: Char.T;
    longintC: Longint.T;
    refanyC: Refany.T;
    widecharC: WideChar.T;


    b := 2;
    c := 3;
    a8: uint8_t := 1;
    b8: uint8_t := 2;
    c8: uint8_t := 3;
    d8: uint8_t := 4;
    a16: uint16_t := 10;
    b16: uint16_t := 20;
    c16: uint16_t := 30;
    d16: uint16_t := 40;
    a32: uint32_t := 100;
    b32: uint32_t := 200;
    c32: uint32_t := 300;
    d32: uint32_t := 400;
    a64: uint64_t := 1000L;
    b64: uint64_t := 2000L;
    c64: uint64_t := 3000L;
    d64: uint64_t := 4000L;
    ap := LOOPHOLE(16_1000, ADDRESS);
    bp := LOOPHOLE(16_2000, ADDRESS);
    cp := LOOPHOLE(16_3000, ADDRESS);
    dp := LOOPHOLE(16_4000, ADDRESS);
    bool:BOOLEAN;

PROCEDURE PrintA(integerA:TEXT; b:ADDRESS)=
BEGIN
  RTIO.PutText(integerA);
  RTIO.PutText(":");
  RTIO.PutAddr(b);
  RTIO.PutText("\n");
  RTIO.Flush();
END PrintA;

PROCEDURE PrintI(integerA:TEXT; b:INTEGER)=
BEGIN
  RTIO.PutText(integerA);
  RTIO.PutText(":");
  RTIO.PutInt(b);
  RTIO.PutText("\n");
  RTIO.Flush();
END PrintI;

PROCEDURE PrintB(integerA:TEXT; b:BOOLEAN)=
BEGIN
  RTIO.PutText(integerA);
  RTIO.PutText(":");
  RTIO.PutText(ARRAY [FALSE..TRUE] OF TEXT{"FALSE", "TRUE"}[b]);
  RTIO.PutText("\n");
  RTIO.Flush();
END PrintB;

PROCEDURE PrintS(integerA:TEXT)=
BEGIN
  RTIO.PutText(integerA);
END PrintS;


PROCEDURE Test_AtomicInteger_Fence() =
BEGIN
    AtomicInteger.Fence();
    AtomicInteger.Fence();
    AtomicInteger.Fence();
END Test_AtomicInteger_Fence;

PROCEDURE Test_AtomicInteger_CompareSwap() =
BEGIN
    bool := AtomicInteger.CompareSwap(atomicIntegerA, b, c);
END Test_AtomicInteger_CompareSwap;

PROCEDURE Test_AtomicInteger_FetchAnd() =
BEGIN
    c := AtomicInteger.FetchAnd(atomicIntegerA, b);
END Test_AtomicInteger_FetchAnd;

PROCEDURE Test_AtomicInteger_FetchDec() =
BEGIN
    c := AtomicInteger.FetchDec(atomicIntegerA);
END Test_AtomicInteger_FetchDec;

PROCEDURE Test_AtomicInteger_FetchInc() =
BEGIN
    c := AtomicInteger.FetchInc(atomicIntegerA);
END Test_AtomicInteger_FetchInc;

PROCEDURE Test_AtomicInteger_FetchOr() =
BEGIN
    c := AtomicInteger.FetchOr(atomicIntegerA, b);
END Test_AtomicInteger_FetchOr;

PROCEDURE Test_AtomicInteger_FetchXor() =
BEGIN
    c := AtomicInteger.FetchXor(atomicIntegerA, b);
END Test_AtomicInteger_FetchXor;

PROCEDURE Test_AtomicInteger_IsLockFree() =
BEGIN
    EVAL AtomicInteger.IsLockFree();
END Test_AtomicInteger_IsLockFree;

PROCEDURE Test_AtomicInteger_LoadStore() =
VAR integerC: Integer.T;
BEGIN
    b := AtomicInteger.Load(atomicIntegerA);
    AtomicInteger.Store(atomicIntegerA, 1 + 2 + 3);
    b := AtomicInteger.Load(atomicIntegerA);
    AtomicInteger.Store(atomicIntegerA, 1 + 2 + 3 + 4);

    integerC := AtomicInteger.Load(atomicIntegerA);
    integerC := AtomicInteger.Load(atomicIntegerA);
END Test_AtomicInteger_LoadStore;

PROCEDURE Test_AtomicInteger_Swap() =
BEGIN
    c := AtomicInteger.Swap(atomicIntegerA, b);
END Test_AtomicInteger_Swap;

PROCEDURE Test_AtomicInteger() =
BEGIN
  Test_AtomicInteger_Fence();
  Test_AtomicInteger_CompareSwap();
  Test_AtomicInteger_FetchAnd();
  Test_AtomicInteger_FetchDec();
  Test_AtomicInteger_FetchInc();
  Test_AtomicInteger_FetchOr();
  Test_AtomicInteger_FetchXor();
  Test_AtomicInteger_IsLockFree();
  Test_AtomicInteger_LoadStore();
  Test_AtomicInteger_Swap();
END Test_AtomicInteger;




PROCEDURE Test_AtomicLongint_Fence() =
BEGIN
    AtomicLongint.Fence();
END Test_AtomicLongint_Fence;

PROCEDURE Test_AtomicLongint_CompareSwap() =
BEGIN
    (* bool := AtomicLongint.CompareSwap(atomicLongintA, longintB, longintC); *)
END Test_AtomicLongint_CompareSwap;

PROCEDURE FakeFetchAndL(VAR var: AtomicLongint.T; mask: Longint.T): Longint.T =
BEGIN
  RETURN var.rep;
END FakeFetchAndL;

PROCEDURE Test_AtomicLongint_FetchAnd() =
BEGIN
    longintC := FakeFetchAndL(atomicLongintA, longintB);
    (*longintC := AtomicLongint.FetchAnd(atomicLongintA, longintB);*)
END Test_AtomicLongint_FetchAnd;

PROCEDURE Test_AtomicLongint_FetchDec() =
BEGIN
    (*longintB := AtomicLongint.FetchDec(atomicLongintA);*)
END Test_AtomicLongint_FetchDec;

PROCEDURE Test_AtomicLongint_FetchInc() =
BEGIN
    (*longintB := AtomicLongint.FetchInc(atomicLongintA);*)
END Test_AtomicLongint_FetchInc;

PROCEDURE Test_AtomicLongint_FetchOr() =
BEGIN
    (*longintC := AtomicLongint.FetchOr(atomicLongintA, longintB);*)
END Test_AtomicLongint_FetchOr;

PROCEDURE Test_AtomicLongint_FetchXor() =
BEGIN
    (*longintC := AtomicLongint.FetchXor(atomicLongintA, longintB);*)
END Test_AtomicLongint_FetchXor;

PROCEDURE Test_AtomicLongint_IsLockFree() =
BEGIN
    EVAL AtomicLongint.IsLockFree();
END Test_AtomicLongint_IsLockFree;

PROCEDURE Test_AtomicLongint_Load() =
BEGIN
    (*longintB := AtomicLongint.Load(atomicLongintA);*)
END Test_AtomicLongint_Load;

PROCEDURE Test_AtomicLongint_Store() =
BEGIN
    (*AtomicLongint.Store(atomicLongintA, longintB);*)
END Test_AtomicLongint_Store;

PROCEDURE Test_AtomicLongint_Swap() =
BEGIN
    (*longintC := AtomicLongint.Swap(atomicLongintA, longintB);*)
END Test_AtomicLongint_Swap;


PROCEDURE Test_AtomicLongint() =
BEGIN
  Test_AtomicLongint_Fence();
  Test_AtomicLongint_CompareSwap();
  Test_AtomicLongint_FetchAnd();
  Test_AtomicLongint_FetchDec();
  Test_AtomicLongint_FetchInc();
  Test_AtomicLongint_FetchOr();
  Test_AtomicLongint_FetchXor();
  Test_AtomicLongint_IsLockFree();
  Test_AtomicLongint_Load();
  Test_AtomicLongint_Store();
  Test_AtomicLongint_Swap();
END Test_AtomicLongint;





BEGIN

(* not portable, just for debugging
  PrintS("addresses for debugging\n\n");
  PrintA("b", ADR(b));
  PrintA("a8", ADR(a8));
  PrintA("b8", ADR(b8));
  PrintA("c8", ADR(c8));
  PrintA("d8", ADR(d8));
  PrintA("a16", ADR(a16));
  PrintA("b16", ADR(b16));
  PrintA("c16", ADR(c16));
  PrintA("d16", ADR(d16));
  PrintA("a32", ADR(a32));
  PrintA("b32", ADR(b32));
  PrintA("c32", ADR(c32));
  PrintA("d32", ADR(d32));
  PrintA("a64", ADR(a64));
  PrintA("b64", ADR(b64));
  PrintA("c64", ADR(c64));
  PrintA("d64", ADR(d64));
  PrintA("ap", ADR(ap));
  PrintA("bp", ADR(bp));
  PrintA("cp", ADR(cp));
  PrintA("dp", ADR(dp));
  PrintS("\n");
  PrintS("\n");
*)
  PrintS("initial values\n\n");

  PrintB("b", bool);
  PrintI("a8", a8);
  PrintI("b8", b8);
  PrintI("c8", c8);
  PrintI("d8", d8);
  PrintI("a16", a16);
  PrintI("b16", b16);
  PrintI("c16", c16);
  PrintI("d16", d16);
  PrintI("a32", a32);
  PrintI("b32", b32);
  PrintI("c32", c32);
  PrintI("d32", d32);
(*PrintI("a64", a64);
  PrintI("b64", b64);
  PrintI("c64", c64);
  PrintI("d64", d64); *)
  PrintA("ap", ap);
  PrintA("bp", bp);
  PrintA("cp", cp);
  PrintA("dp", dp);
  PrintS("\n");
  PrintS("\n");

  PrintS("\nshould print false\n\n");

(* 8 bit parameters not yet supported by integrated backend *)

(* d8 := CAS(a8, b8, c8);*)
  (*b := CASP(a8, b8, c8); *)
  PrintB("b", bool);
  PrintI("d8", d8);

  PrintS("\n");
(*d16 := CAS(a16, b16, c16);
    b := CASP(a16, b16, c16);*)
  PrintB("b", bool);
  PrintI("d16", d16);

  PrintS("\n");
(* d32 := CAS(a32, b32, b32);
    b := CASP(a32, b32, c32); *)
  PrintB("b", bool);
  PrintI("d32", d32);


  Test_AtomicInteger();
  Test_AtomicLongint();


(*PrintS("\n");
(* d64 := CAS(a64, b64, b64);
    b := CASP(a64, b64, c64); *)
  PrintB("b", bool);
  PrintI("d64", d64);*)

  PrintS("\n");
 (* dp := CAS(ap, bp, cp);
    b := CASP(ap, bp, cp); *)
  PrintB("b", bool);
  PrintA("dp", dp);


  PrintS("\n");
  PrintS("\n");

  (* generate new values *)

  INC(a8, 100);
  INC(b8, 100);
  INC(c8, 100);
  INC(d8, 100);
  INC(a16);
  INC(b16);
  INC(c16);
  INC(d16);
  INC(a32);
  INC(b32);
  INC(c32);
  INC(d32);
  INC(a64);
  INC(b64);
  INC(c64);
  INC(d64);
  INC(ap);
  INC(bp);
  INC(cp);
  INC(dp);

  PrintS("\nshould print true\n\n");

(*d8 := CAS(a8, a8, c8);
    b := CASP(a8, a8, c8);*)
  PrintB("b", bool);
  PrintI("d8", d8);

(*  d16 := CAS(a16, a16, c16);
    b := CASP(a16, a16, c16);*)
  PrintB("b", bool);
  PrintI("d16", d16);

  PrintS("\n");
(* d32 := CAS(a32, a32, b32);
    b := CASP(a32, a32, c32); *)
  PrintB("b", bool);
  PrintI("d32", d32);

(*PrintS("\n");
  d64 := CAS(a64, a64, b64);
    b := CASP(a64, a64, c64);
  PrintB("b", bool);
  PrintI("d64", d64);*)

  PrintS("\n");
(*   dp := CAS(ap, ap, cp);
    b := CASP(ap, ap, cp);*)
  PrintB("b", bool);
  PrintA("dp", dp);


END Main.
