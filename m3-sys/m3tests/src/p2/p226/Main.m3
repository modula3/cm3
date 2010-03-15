UNSAFE MODULE Main;
IMPORT AtomicAddress, AtomicBoolean, AtomicChar, AtomicInteger, AtomicLongint, AtomicRefany;
IMPORT AtomicWideChar, Address, Boolean, Char, Integer, Longint, Refany, WideChar;

VAR
  atomicBooleanA: AtomicBoolean.T;
  atomicCharA: AtomicChar.T;
  atomicIntegerA: AtomicInteger.T;
  atomicLongintA: AtomicLongint.T;
  atomicRefanyA: AtomicRefany.T;
  atomicWidecharA: AtomicWideChar.T;
  atomicAddressA: AtomicAddress.T;
  integerB, integerC: Integer.T;
  booleanB, booleanC: Boolean.T;
  charB, charC: Char.T;
  longintB, longintC: Longint.T;
  refanyB, refanyC: Refany.T;
  widecharB, widecharC: WideChar.T;
  addressB, addressC: Address.T;
  bool:BOOLEAN;


PROCEDURE Test_AtomicBoolean_Fence() =
BEGIN
  AtomicBoolean.Fence();
END Test_AtomicBoolean_Fence;

PROCEDURE Test_AtomicBoolean_CompareSwap() =
BEGIN
  bool := AtomicBoolean.CompareSwap(atomicBooleanA, booleanB, booleanC);
END Test_AtomicBoolean_CompareSwap;

PROCEDURE Test_AtomicBoolean_FetchAnd() =
BEGIN
  booleanC := AtomicBoolean.FetchAnd(atomicBooleanA, booleanB);
END Test_AtomicBoolean_FetchAnd;

PROCEDURE Test_AtomicBoolean_FetchDec() =
BEGIN
  booleanC := AtomicBoolean.FetchDec(atomicBooleanA);
END Test_AtomicBoolean_FetchDec;

PROCEDURE Test_AtomicBoolean_FetchInc() =
BEGIN
  booleanC := AtomicBoolean.FetchInc(atomicBooleanA);
END Test_AtomicBoolean_FetchInc;

PROCEDURE Test_AtomicBoolean_FetchOr() =
BEGIN
  booleanC := AtomicBoolean.FetchOr(atomicBooleanA, booleanB);
END Test_AtomicBoolean_FetchOr;

PROCEDURE Test_AtomicBoolean_FetchXor() =
BEGIN
  booleanC := AtomicBoolean.FetchXor(atomicBooleanA, booleanB);
END Test_AtomicBoolean_FetchXor;

PROCEDURE Test_AtomicBoolean_IsLockFree():BOOLEAN =
BEGIN
  RETURN AtomicBoolean.IsLockFree();
END Test_AtomicBoolean_IsLockFree;

PROCEDURE Test_AtomicBoolean_LoadStore() =
BEGIN
  booleanB := AtomicBoolean.Load(atomicBooleanA);
  AtomicBoolean.Store(atomicBooleanA, FALSE);
  booleanB := AtomicBoolean.Load(atomicBooleanA);
  AtomicBoolean.Store(atomicBooleanA, TRUE);

  booleanC := AtomicBoolean.Load(atomicBooleanA);
  booleanC := AtomicBoolean.Load(atomicBooleanA);
END Test_AtomicBoolean_LoadStore;

PROCEDURE Test_AtomicBoolean_Swap() =
BEGIN
  booleanC := AtomicBoolean.Swap(atomicBooleanA, booleanB);
END Test_AtomicBoolean_Swap;

PROCEDURE Test_AtomicBoolean() =
BEGIN
  Test_AtomicBoolean_Fence();
  Test_AtomicBoolean_CompareSwap();
  Test_AtomicBoolean_FetchAnd();
  Test_AtomicBoolean_FetchDec();
  Test_AtomicBoolean_FetchInc();
  Test_AtomicBoolean_FetchOr();
  Test_AtomicBoolean_FetchXor();
  EVAL Test_AtomicBoolean_IsLockFree();
  Test_AtomicBoolean_LoadStore();
  Test_AtomicBoolean_Swap();
END Test_AtomicBoolean;



PROCEDURE Test_AtomicChar_Fence() =
BEGIN
  AtomicChar.Fence();
END Test_AtomicChar_Fence;

PROCEDURE Test_AtomicChar_CompareSwap() =
BEGIN
  bool := AtomicChar.CompareSwap(atomicCharA, charB, charC);
END Test_AtomicChar_CompareSwap;

PROCEDURE Test_AtomicChar_FetchAnd() =
BEGIN
  charC := AtomicChar.FetchAnd(atomicCharA, charB);
END Test_AtomicChar_FetchAnd;

PROCEDURE Test_AtomicChar_FetchDec() =
BEGIN
  charC := AtomicChar.FetchDec(atomicCharA);
END Test_AtomicChar_FetchDec;

PROCEDURE Test_AtomicChar_FetchInc() =
BEGIN
  charC := AtomicChar.FetchInc(atomicCharA);
END Test_AtomicChar_FetchInc;

PROCEDURE Test_AtomicChar_FetchOr() =
BEGIN
  charC := AtomicChar.FetchOr(atomicCharA, charB);
END Test_AtomicChar_FetchOr;

PROCEDURE Test_AtomicChar_FetchXor() =
BEGIN
  charC := AtomicChar.FetchXor(atomicCharA, charB);
END Test_AtomicChar_FetchXor;

PROCEDURE Test_AtomicChar_IsLockFree():BOOLEAN =
BEGIN
  RETURN AtomicChar.IsLockFree();
END Test_AtomicChar_IsLockFree;

PROCEDURE Test_AtomicChar_LoadStore() =
BEGIN
  charB := AtomicChar.Load(atomicCharA);
  AtomicChar.Store(atomicCharA, VAL(1 + 2 + 3, CHAR));
  charB := AtomicChar.Load(atomicCharA);
  AtomicChar.Store(atomicCharA, VAL(1 + 2 + 3, CHAR));

  charC := AtomicChar.Load(atomicCharA);
  charC := AtomicChar.Load(atomicCharA);
END Test_AtomicChar_LoadStore;

PROCEDURE Test_AtomicChar_Swap() =
BEGIN
  charC := AtomicChar.Swap(atomicCharA, charB);
END Test_AtomicChar_Swap;

PROCEDURE Test_AtomicChar() =
BEGIN
  Test_AtomicChar_Fence();
  Test_AtomicChar_CompareSwap();
  Test_AtomicChar_FetchAnd();
  Test_AtomicChar_FetchDec();
  Test_AtomicChar_FetchInc();
  Test_AtomicChar_FetchOr();
  Test_AtomicChar_FetchXor();
  EVAL Test_AtomicChar_IsLockFree();
  Test_AtomicChar_LoadStore();
  Test_AtomicChar_Swap();
END Test_AtomicChar;







PROCEDURE Test_AtomicWidechar_Fence() =
BEGIN
  AtomicWideChar.Fence();
END Test_AtomicWidechar_Fence;

PROCEDURE Test_AtomicWidechar_CompareSwap() =
BEGIN
  bool := AtomicWideChar.CompareSwap(atomicWidecharA, widecharB, widecharC);
END Test_AtomicWidechar_CompareSwap;

PROCEDURE Test_AtomicWidechar_FetchAnd() =
BEGIN
  widecharC := AtomicWideChar.FetchAnd(atomicWidecharA, widecharB);
END Test_AtomicWidechar_FetchAnd;

PROCEDURE Test_AtomicWidechar_FetchDec() =
BEGIN
  widecharC := AtomicWideChar.FetchDec(atomicWidecharA);
END Test_AtomicWidechar_FetchDec;

PROCEDURE Test_AtomicWidechar_FetchInc() =
BEGIN
  widecharC := AtomicWideChar.FetchInc(atomicWidecharA);
END Test_AtomicWidechar_FetchInc;

PROCEDURE Test_AtomicWidechar_FetchOr() =
BEGIN
  widecharC := AtomicWideChar.FetchOr(atomicWidecharA, widecharB);
END Test_AtomicWidechar_FetchOr;

PROCEDURE Test_AtomicWidechar_FetchXor() =
BEGIN
  widecharC := AtomicWideChar.FetchXor(atomicWidecharA, widecharB);
END Test_AtomicWidechar_FetchXor;

PROCEDURE Test_AtomicWidechar_IsLockFree():BOOLEAN =
BEGIN
  RETURN AtomicWideChar.IsLockFree();
END Test_AtomicWidechar_IsLockFree;

PROCEDURE Test_AtomicWidechar_LoadStore() =
VAR integerC: WideChar.T;
BEGIN
  widecharB := AtomicWideChar.Load(atomicWidecharA);
  AtomicWideChar.Store(atomicWidecharA, VAL(1 + 2 + 3, WIDECHAR));
  widecharB := AtomicWideChar.Load(atomicWidecharA);
  AtomicWideChar.Store(atomicWidecharA, VAL(1 + 2 + 3, WIDECHAR));

  integerC := AtomicWideChar.Load(atomicWidecharA);
  integerC := AtomicWideChar.Load(atomicWidecharA);
END Test_AtomicWidechar_LoadStore;

PROCEDURE Test_AtomicWidechar_Swap() =
BEGIN
  widecharC := AtomicWideChar.Swap(atomicWidecharA, widecharB);
END Test_AtomicWidechar_Swap;

PROCEDURE Test_AtomicWidechar() =
BEGIN
  Test_AtomicWidechar_Fence();
  Test_AtomicWidechar_CompareSwap();
  Test_AtomicWidechar_FetchAnd();
  Test_AtomicWidechar_FetchDec();
  Test_AtomicWidechar_FetchInc();
  Test_AtomicWidechar_FetchOr();
  Test_AtomicWidechar_FetchXor();
  EVAL Test_AtomicWidechar_IsLockFree();
  Test_AtomicWidechar_LoadStore();
  Test_AtomicWidechar_Swap();
END Test_AtomicWidechar;





PROCEDURE Test_AtomicRefany_Fence() =
BEGIN
  AtomicRefany.Fence();
  AtomicRefany.Fence();
  AtomicRefany.Fence();
END Test_AtomicRefany_Fence;

PROCEDURE Test_AtomicRefany_CompareSwap() =
BEGIN
  bool := AtomicRefany.CompareSwap(atomicRefanyA, refanyB, refanyC);
END Test_AtomicRefany_CompareSwap;

PROCEDURE Test_AtomicRefany_FetchAnd() =
BEGIN
  (*refanyC := AtomicRefany.FetchAnd(atomicRefanyA, refanyB);*)
END Test_AtomicRefany_FetchAnd;

PROCEDURE Test_AtomicRefany_FetchDec() =
BEGIN
  (*refanyC := AtomicRefany.FetchDec(atomicRefanyA);*)
END Test_AtomicRefany_FetchDec;

PROCEDURE Test_AtomicRefany_FetchInc() =
BEGIN
  (*refanyC := AtomicRefany.FetchInc(atomicRefanyA);*)
END Test_AtomicRefany_FetchInc;

PROCEDURE Test_AtomicRefany_FetchOr() =
BEGIN
  (*refanyC := AtomicRefany.FetchOr(atomicRefanyA, refanyB);*)
END Test_AtomicRefany_FetchOr;

PROCEDURE Test_AtomicRefany_FetchXor() =
BEGIN
  (*refanyC := AtomicRefany.FetchXor(atomicRefanyA, refanyB);*)
END Test_AtomicRefany_FetchXor;

PROCEDURE Test_AtomicRefany_IsLockFree():BOOLEAN =
BEGIN
  RETURN AtomicRefany.IsLockFree();
END Test_AtomicRefany_IsLockFree;

PROCEDURE Test_AtomicRefany_LoadStore() =
VAR refanyC: Refany.T;
BEGIN
  refanyB := AtomicRefany.Load(atomicRefanyA);
  AtomicRefany.Store(atomicRefanyA, LOOPHOLE(1 + 2 + 3, REFANY));
  refanyB := AtomicRefany.Load(atomicRefanyA);
  AtomicRefany.Store(atomicRefanyA, LOOPHOLE(1 + 2 + 3, REFANY));

  refanyC := AtomicRefany.Load(atomicRefanyA);
  refanyC := AtomicRefany.Load(atomicRefanyA);
END Test_AtomicRefany_LoadStore;

PROCEDURE Test_AtomicRefany_Swap() =
BEGIN
  refanyC := AtomicRefany.Swap(atomicRefanyA, refanyB);
END Test_AtomicRefany_Swap;

PROCEDURE Test_AtomicRefany() =
BEGIN
  Test_AtomicRefany_Fence();
  Test_AtomicRefany_CompareSwap();
  Test_AtomicRefany_FetchAnd();
  Test_AtomicRefany_FetchDec();
  Test_AtomicRefany_FetchInc();
  Test_AtomicRefany_FetchOr();
  Test_AtomicRefany_FetchXor();
  EVAL Test_AtomicRefany_IsLockFree();
  Test_AtomicRefany_LoadStore();
  Test_AtomicRefany_Swap();
END Test_AtomicRefany;





PROCEDURE Test_AtomicAddress_Fence() =
BEGIN
  AtomicAddress.Fence();
  AtomicAddress.Fence();
  AtomicAddress.Fence();
END Test_AtomicAddress_Fence;

PROCEDURE Test_AtomicAddress_CompareSwap() =
BEGIN
  bool := AtomicAddress.CompareSwap(atomicAddressA, addressB, addressC);
END Test_AtomicAddress_CompareSwap;

PROCEDURE Test_AtomicAddress_FetchAnd() =
BEGIN
  (*addressC := AtomicAddress.FetchAnd(atomicAddressA, addressB);*)
END Test_AtomicAddress_FetchAnd;

PROCEDURE Test_AtomicAddress_FetchDec() =
BEGIN
  (*addressC := AtomicAddress.FetchDec(atomicAddressA);*)
END Test_AtomicAddress_FetchDec;

PROCEDURE Test_AtomicAddress_FetchInc() =
BEGIN
  (*addressC := AtomicAddress.FetchInc(atomicAddressA);*)
END Test_AtomicAddress_FetchInc;

PROCEDURE Test_AtomicAddress_FetchOr() =
BEGIN
  (*addressC := AtomicAddress.FetchOr(atomicAddressA, addressB);*)
END Test_AtomicAddress_FetchOr;

PROCEDURE Test_AtomicAddress_FetchXor() =
BEGIN
  (*addressC := AtomicAddress.FetchXor(atomicAddressA, addressB);*)
END Test_AtomicAddress_FetchXor;

PROCEDURE Test_AtomicAddress_IsLockFree():BOOLEAN =
BEGIN
  RETURN AtomicAddress.IsLockFree();
END Test_AtomicAddress_IsLockFree;

PROCEDURE Test_AtomicAddress_LoadStore() =
VAR addressC: Address.T;
BEGIN
  addressB := AtomicAddress.Load(atomicAddressA);
  AtomicAddress.Store(atomicAddressA, LOOPHOLE(1 + 2 + 3, ADDRESS));
  addressB := AtomicAddress.Load(atomicAddressA);
  AtomicAddress.Store(atomicAddressA, LOOPHOLE(1 + 2 + 3, ADDRESS));

  addressC := AtomicAddress.Load(atomicAddressA);
  addressC := AtomicAddress.Load(atomicAddressA);
END Test_AtomicAddress_LoadStore;

PROCEDURE Test_AtomicAddress_Swap() =
BEGIN
  addressC := AtomicAddress.Swap(atomicAddressA, addressB);
END Test_AtomicAddress_Swap;

PROCEDURE Test_AtomicAddress() =
BEGIN
  Test_AtomicAddress_Fence();
  Test_AtomicAddress_CompareSwap();
  Test_AtomicAddress_FetchAnd();
  Test_AtomicAddress_FetchDec();
  Test_AtomicAddress_FetchInc();
  Test_AtomicAddress_FetchOr();
  Test_AtomicAddress_FetchXor();
  EVAL Test_AtomicAddress_IsLockFree();
  Test_AtomicAddress_LoadStore();
  Test_AtomicAddress_Swap();
END Test_AtomicAddress;






PROCEDURE Test_AtomicInteger_Fence() =
BEGIN
  AtomicInteger.Fence();
  AtomicInteger.Fence();
  AtomicInteger.Fence();
END Test_AtomicInteger_Fence;

PROCEDURE Test_AtomicInteger_CompareSwap() =
BEGIN
  bool := AtomicInteger.CompareSwap(atomicIntegerA, integerB, integerC);
END Test_AtomicInteger_CompareSwap;

PROCEDURE Test_AtomicInteger_FetchAnd() =
BEGIN
  integerC := AtomicInteger.FetchAnd(atomicIntegerA, integerB);
END Test_AtomicInteger_FetchAnd;

PROCEDURE Test_AtomicInteger_FetchDec() =
BEGIN
  integerC := AtomicInteger.FetchDec(atomicIntegerA);
END Test_AtomicInteger_FetchDec;

PROCEDURE Test_AtomicInteger_FetchInc() =
BEGIN
  integerC := AtomicInteger.FetchInc(atomicIntegerA);
END Test_AtomicInteger_FetchInc;

PROCEDURE Test_AtomicInteger_FetchOr() =
BEGIN
  integerC := AtomicInteger.FetchOr(atomicIntegerA, integerB);
END Test_AtomicInteger_FetchOr;

PROCEDURE Test_AtomicInteger_FetchXor() =
BEGIN
  integerC := AtomicInteger.FetchXor(atomicIntegerA, integerB);
END Test_AtomicInteger_FetchXor;

PROCEDURE Test_AtomicInteger_IsLockFree():BOOLEAN =
BEGIN
  RETURN AtomicInteger.IsLockFree();
END Test_AtomicInteger_IsLockFree;

PROCEDURE Test_AtomicInteger_LoadStore() =
VAR integerC: Integer.T;
BEGIN
  integerB := AtomicInteger.Load(atomicIntegerA);
  AtomicInteger.Store(atomicIntegerA, 1 + 2 + 3);
  integerB := AtomicInteger.Load(atomicIntegerA);
  AtomicInteger.Store(atomicIntegerA, 1 + 2 + 3 + 4);

  integerC := AtomicInteger.Load(atomicIntegerA);
  integerC := AtomicInteger.Load(atomicIntegerA);
END Test_AtomicInteger_LoadStore;

PROCEDURE Test_AtomicInteger_Swap() =
BEGIN
  integerC := AtomicInteger.Swap(atomicIntegerA, integerB);
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
  EVAL Test_AtomicInteger_IsLockFree();
  Test_AtomicInteger_LoadStore();
  Test_AtomicInteger_Swap();
END Test_AtomicInteger;




PROCEDURE Test_AtomicLongint_Fence() =
BEGIN
  AtomicLongint.Fence();
END Test_AtomicLongint_Fence;

PROCEDURE Test_AtomicLongint_CompareSwap() =
BEGIN
  bool := AtomicLongint.CompareSwap(atomicLongintA, longintB, longintC); 
END Test_AtomicLongint_CompareSwap;

PROCEDURE Test_AtomicLongint_FetchAnd() =
BEGIN
  longintC := AtomicLongint.FetchAnd(atomicLongintA, longintB);
END Test_AtomicLongint_FetchAnd;

PROCEDURE Test_AtomicLongint_FetchDec() =
BEGIN
  longintB := AtomicLongint.FetchDec(atomicLongintA);
END Test_AtomicLongint_FetchDec;

PROCEDURE Test_AtomicLongint_FetchInc() =
BEGIN
  longintB := AtomicLongint.FetchInc(atomicLongintA);
END Test_AtomicLongint_FetchInc;

PROCEDURE Test_AtomicLongint_FetchOr() =
BEGIN
  longintC := AtomicLongint.FetchOr(atomicLongintA, longintB);
END Test_AtomicLongint_FetchOr;

PROCEDURE Test_AtomicLongint_FetchXor() =
BEGIN
  longintC := AtomicLongint.FetchXor(atomicLongintA, longintB);
END Test_AtomicLongint_FetchXor;

PROCEDURE Test_AtomicLongint_IsLockFree():BOOLEAN =
BEGIN
  RETURN AtomicLongint.IsLockFree();
END Test_AtomicLongint_IsLockFree;

PROCEDURE Test_AtomicLongint_Load() =
BEGIN
  longintB := AtomicLongint.Load(atomicLongintA);
END Test_AtomicLongint_Load;

PROCEDURE Test_AtomicLongint_Store() =
BEGIN
  AtomicLongint.Store(atomicLongintA, longintB);
END Test_AtomicLongint_Store;

PROCEDURE Test_AtomicLongint_Swap() =
BEGIN
  longintC := AtomicLongint.Swap(atomicLongintA, longintB);
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
  EVAL Test_AtomicLongint_IsLockFree();
  Test_AtomicLongint_Load();
  Test_AtomicLongint_Store();
  Test_AtomicLongint_Swap();
END Test_AtomicLongint;





BEGIN

  Test_AtomicBoolean();
  Test_AtomicChar();
  Test_AtomicWidechar();
  Test_AtomicLongint();
  Test_AtomicInteger();
  Test_AtomicAddress();
  Test_AtomicRefany();

END Main.
