/* End-to-end link test for MSIR-generated LLVM IR.
   Calls M3 procs compiled from Main.ll via LLVM and checks results. */
#include <stdio.h>
#include <string.h>

typedef long          M3Int;
typedef unsigned char M3Byte;   /* i8  — enum / CHAR */
typedef _Bool         M3Bool;   /* i1  — BOOLEAN */

typedef struct { M3Int x; M3Int y; } Point;
typedef struct { M3Int *data; M3Int  size; } OpenArray;

/* Fmt__Int, IO__Put, Fmt__Bool, RTHooks__Check* are provided by libm3/libm3core.
   Harness test procedures (Add, Factorial, etc.) never call any of them, so
   no initialised runtime is needed.  RTHooks__Raise is in raise_stub.cpp. */

/* _ZTI6_M3Exc is provided by raise_stub.cpp (the C++ compiler generates a
   proper typeinfo for struct _M3Exc when raise_stub.cpp is compiled). */

/* ---- M3 procedure declarations ---- */
extern M3Int  Main__Add(M3Int a, M3Int b);
extern M3Int  Main__Factorial(M3Int n);
extern M3Int  Main__Abs(M3Int n);
extern M3Int  Main__Sign(M3Int n);
extern M3Int  Main__SumTo(M3Int n);
extern M3Int  Main__AbsSum(M3Int a, M3Int b);
extern M3Int  Main__FactSum(M3Int n);

extern Point  Main__MakePoint(M3Int x, M3Int y);
extern M3Int  Main__PointSum(Point p);
extern M3Int  Main__ColorToInt(M3Byte c);

extern void   Main__Swap(M3Int *a, M3Int *b);
extern void   Main__IncBy(M3Int *a, M3Int b);
extern M3Int  Main__ReadOnlySum(Point *p);

extern M3Int  Main__SumOpenArr(OpenArray *a);
extern void   Main__SetFirst(OpenArray *a, M3Int v);
extern M3Int  Main__RelaySum(OpenArray *a);

extern void   Main__FillVec(M3Int *v);
extern M3Int  Main__SumVec(M3Int *v);
extern M3Int  Main__GetSecond(M3Int *v);

extern M3Int  Main__SumForUp(M3Int n);
extern M3Int  Main__SumForDown(M3Int n);
extern M3Int  Main__SumForBy2(M3Int n);

extern M3Int  Main__WeekdayNum(M3Int n);
extern M3Int  Main__ColorCode(M3Byte c);
extern M3Int  Main__RangeCase(M3Int n);

extern M3Int  Main__DivTest(M3Int a, M3Int b);
extern M3Int  Main__ModTest(M3Int a, M3Int b);

extern M3Int  Main__CountWithINC(M3Int n);
extern M3Int  Main__CountDownWithDEC(M3Int n);
extern M3Int  Main__IncBy3(M3Int n);
extern M3Int  Main__DecBy5(M3Int n);
extern M3Int  Main__RepeatSum(M3Int n);
extern M3Int  Main__RepeatCountdown(M3Int n);

extern M3Int  Main__WithField(void);
extern M3Int  Main__WithScalar(M3Int n);
extern M3Int  Main__WithDesignator(OpenArray *a);

extern M3Bool Main__BothPos(M3Int a, M3Int b);
extern M3Bool Main__EitherPos(M3Int a, M3Int b);
extern M3Bool Main__Neither(M3Int a, M3Int b);

extern void   Main__IncrCounter(void);
extern void   Main__AddToCounter(M3Int n);
extern M3Int  Main__GetCounter(void);

extern M3Int  Main__TryRaise(void);
extern M3Int  Main__AllocInt(M3Int n);    /* NEW(REF INTEGER): stores n, returns n */
extern M3Int  Main__AllocPair(M3Int a, M3Int b); /* NEW(REF Point): sets x/y, returns x+y */
extern M3Int  Main__AllocIntArr(M3Int n); /* NEW(REF ARRAY OF INTEGER, n): elem0=n*2, returns n*2 */
extern M3Int  Main__AllocSquare(M3Int side);   /* NEW(Square): sets side, returns side*side */
extern M3Int  Main__NestedSum(M3Int n);        /* nested proc: sum 1..n */
extern M3Int  Main__NestedScale(M3Int base, M3Int n); /* read-only capture: base*n */
extern M3Int  Main__WideLen(void);    /* WIDECHAR literal: Text.Length(W"Hi")=2 */
extern M3Int  Main__DispatchSquare(M3Int side); /* NEW(Square) + vtable dispatch */
extern M3Int  Main__TryRaiseArg(void);  /* raises TestExceptArg(42), catches, returns 42 */
extern M3Int  Main__TryFinNormal(void);
extern M3Int  Main__TryExceptNormal(void);

/* Method dispatch + heap object field access */
extern M3Int  Main__SquareArea(void *self);  /* reads self->side via field GEP */

/* TYPECASE test */
extern M3Int  Main__TypecaseKind(void *r);

/* IN operator — element IN constant SET */
extern M3Bool Main__IsWeekend(M3Byte d);   /* SET OF Weekday{Sat,Sun} */
extern M3Bool Main__IsWorkday(M3Byte d);   /* SET OF Weekday{Mon..Fri} */

/* TRUNC / FLOOR / CEILING / ROUND rounding builtins */
extern M3Int  Main__TruncTest(float x);
extern M3Int  Main__FloorTest(float x);
extern M3Int  Main__CeilingTest(float x);
extern M3Int  Main__RoundTest(double y);

/* CONST array subscript — runtime index into compile-time constant array */
extern M3Int  Main__GetPrime(M3Int i);       /* SmallPrimes[i] */
extern void  *Main__GetBoolName(M3Bool b);   /* BoolName[b] — returns TEXT ptr */

/* VALUE open-array formal — caller provides dope vector, callee sums elements */
extern M3Int  Main__SumOA(OpenArray *a);

/* VALUE open-array formal with open actual: SumViaOpenActual(VAR src) calls SumOA(src) */
extern M3Int  Main__SumViaOpenActual(OpenArray *src);

/* Array-copy: open→fixed.  Scalar wrappers index into the result. */
extern M3Int  Main__FirstFourElem(OpenArray *src, M3Int i);
extern M3Int  Main__CopyFirst4Elem(OpenArray *src, M3Int i);

/* Indirect (proc-variable) calls */
typedef M3Int (*BinaryIntOp)(M3Int, M3Int);
typedef M3Int (*UnaryIntOp)(M3Int);
extern M3Int  Main__ApplyBinOp(BinaryIntOp f, M3Int a, M3Int b);
extern M3Int  Main__ApplyUnary(UnaryIntOp f, M3Int n);

/* RTHooks__ScanTypecase is provided by libm3core.  For ref=NIL (our only
   harness test) it returns 0 immediately without touching runtime state. */

/* SUBARRAY — slicing fixed and open arrays */
extern M3Int  Main__SubarrayFixedElem(M3Int start, M3Int len, M3Int idx);
extern M3Int  Main__SubarrayOpenElem(OpenArray *a, M3Int start, M3Int len, M3Int idx);
extern M3Int  Main__SumSubarray(OpenArray *a, M3Int start, M3Int len);

/* ISTYPE / NARROW / TYPECASE-with-var */
extern void  *Main__MakeIntRef(M3Int n);
extern M3Int  Main__TestIsType(void *r);
extern M3Int  Main__TestNarrow(void *r);
extern M3Int  Main__TestTypecaseVar(void *r);

/* Direct access to module globals (zeroinitialised — no M3 module init runs) */
extern M3Int  Main__gCounter;
extern M3Int  Main__gBase;

/* Initialise TypeLink defn pointers so allocator hooks get real TypeCells.
   This is a harness-only helper emitted by MSIRToLLVM — in production,
   RTLinker.ResolveTypeLinks walks MI_type_cell_ptrs instead. */
extern void MSIR_InitTypeLinks(void);

/* ---- test harness ---- */

static int failures = 0;


static void check_int(const char *name, M3Int got, M3Int expected) {
    if (got == expected)
        printf("ok   %-30s = %ld\n", name, (long)got);
    else {
        printf("FAIL %-30s : got %ld, expected %ld\n",
               name, (long)got, (long)expected);
        failures++;
    }
}

static void check_bool(const char *name, M3Bool got, M3Bool expected) {
    if (got == expected)
        printf("ok   %-30s = %s\n", name, got ? "TRUE" : "FALSE");
    else {
        printf("FAIL %-30s : got %s, expected %s\n",
               name, got?"TRUE":"FALSE", expected?"TRUE":"FALSE");
        failures++;
    }
}

int main(void) {
    /* Resolve TypeLink defn pointers before any allocator calls. */
    MSIR_InitTypeLinks();

    /* arithmetic */
    check_int("Add(2,3)",         Main__Add(2, 3),         5);
    check_int("Factorial(5)",     Main__Factorial(5),       120);
    check_int("Abs(-7)",          Main__Abs(-7),            7);
    check_int("Abs(4)",           Main__Abs(4),             4);
    check_int("Sign(-3)",         Main__Sign(-3),           -1);
    check_int("Sign(0)",          Main__Sign(0),            0);
    check_int("Sign(5)",          Main__Sign(5),            1);
    check_int("SumTo(10)",        Main__SumTo(10),          55);
    check_int("AbsSum(-3,4)",     Main__AbsSum(-3, 4),      7);
    check_int("FactSum(4)",       Main__FactSum(4),         34);

    /* records */
    Point p = Main__MakePoint(3, 4);
    check_int("MakePoint(3,4).x", p.x,                     3);
    check_int("MakePoint(3,4).y", p.y,                     4);
    check_int("PointSum({3,4})",  Main__PointSum(p),        7);

    /* enum */
    check_int("ColorToInt(2)",    Main__ColorToInt(2),      2);

    /* VAR / READONLY params */
    M3Int x = 11, y = 22;
    Main__Swap(&x, &y);
    check_int("Swap: x",          x,                        22);
    check_int("Swap: y",          y,                        11);
    Main__IncBy(&x, 100);
    check_int("IncBy(x,100)",     x,                        122);
    Point q = {5, 7};
    check_int("ReadOnlySum({5,7})",Main__ReadOnlySum(&q),   12);

    /* fixed arrays (Vec5 = ARRAY [0..4] OF INTEGER) */
    M3Int v[5];
    Main__FillVec(v);
    check_int("FillVec v[0]",     v[0],   0);
    check_int("FillVec v[1]",     v[1],   1);
    check_int("FillVec v[2]",     v[2],   4);
    check_int("FillVec v[3]",     v[3],   9);
    check_int("FillVec v[4]",     v[4],   16);
    check_int("SumVec",           Main__SumVec(v),          30);
    check_int("GetSecond",        Main__GetSecond(v),       1);

    /* open arrays */
    OpenArray oa = { v, 5 };
    check_int("SumOpenArr[0+1]",  Main__SumOpenArr(&oa),    1);   /* 0+1 */
    Main__SetFirst(&oa, 99);
    check_int("SetFirst(99)",     v[0],                     99);
    check_int("RelaySum",         Main__RelaySum(&oa),      104); /* 99+1+4 */

    /* FOR loops */
    check_int("SumForUp(10)",     Main__SumForUp(10),       55);
    check_int("SumForDown(10)",   Main__SumForDown(10),     55);
    check_int("SumForBy2(10)",    Main__SumForBy2(10),      30);

    /* CASE */
    check_int("WeekdayNum(3)",    Main__WeekdayNum(3),      30);
    check_int("WeekdayNum(9)",    Main__WeekdayNum(9),      -1);
    check_int("ColorCode(Red=0)", Main__ColorCode(0),       0xFF0000);
    check_int("ColorCode(Blue=2)",Main__ColorCode(2),       0x0000FF);
    check_int("RangeCase(2)",     Main__RangeCase(2),       1);
    check_int("RangeCase(5)",     Main__RangeCase(5),       2);
    check_int("RangeCase(8)",     Main__RangeCase(8),       3);
    check_int("RangeCase(0)",     Main__RangeCase(0),       0);

    /* DIV / MOD — floor semantics (critical) */
    check_int("DivTest(17,5)",    Main__DivTest(17, 5),     3);
    check_int("ModTest(17,5)",    Main__ModTest(17, 5),     2);
    check_int("DivTest(-7,2)",    Main__DivTest(-7, 2),     -4);
    check_int("ModTest(-7,2)",    Main__ModTest(-7, 2),     1);

    /* INC / DEC / REPEAT */
    check_int("CountWithINC(7)",  Main__CountWithINC(7),    7);
    check_int("CountDownDEC(5)",  Main__CountDownWithDEC(5),0);
    check_int("IncBy3(10)",       Main__IncBy3(10),         13);
    check_int("DecBy5(10)",       Main__DecBy5(10),         5);
    check_int("RepeatSum(5)",     Main__RepeatSum(5),       15);
    check_int("RepeatCountdown(4)",Main__RepeatCountdown(4),0);

    /* WITH */
    check_int("WithField()",      Main__WithField(),         35);
    check_int("WithScalar(7)",    Main__WithScalar(7),       15);
    M3Int arr[3] = {10, 20, 30};
    OpenArray arr_oa = { arr, 3 };
    check_int("WithDesignator",   Main__WithDesignator(&arr_oa), 40);

    /* AND / OR / NOT */
    check_bool("BothPos(3,4)",    Main__BothPos(3, 4),      1);
    check_bool("BothPos(-1,4)",   Main__BothPos(-1, 4),     0);
    check_bool("EitherPos(-1,4)", Main__EitherPos(-1, 4),   1);
    check_bool("EitherPos(-1,-2)",Main__EitherPos(-1,-2),   0);
    check_bool("Neither(-1,-2)",  Main__Neither(-1, -2),    1);

    /* globals (gBase = 0 at link time; M3 module init not called) */
    Main__IncrCounter();
    Main__IncrCounter();
    Main__AddToCounter(8);
    check_int("gCounter (direct)", Main__gCounter,          10);
    check_int("GetCounter()",     Main__GetCounter(),       10); /* gBase=0 */

    /* NEW(REF INTEGER) — allocate, store 99, read back */
    check_int("AllocInt(99)",       Main__AllocInt(99),         99);

    /* NEW(REF Point) — allocate, set x=3/y=4, return x+y */
    check_int("AllocPair(3,4)",     Main__AllocPair(3, 4),      7);

    /* NEW(REF ARRAY OF INTEGER, 5) — alloc, set r^[0]=10, return 10 */
    check_int("AllocIntArr(5)",     Main__AllocIntArr(5),       10);

    /* NEW(Square) — allocate OBJECT, set side, return side*side */
    check_int("AllocSquare(6)",     Main__AllocSquare(6),       36);

    /* Nested procedure: sum 1..10 = 55 */
    check_int("NestedSum(10)",      Main__NestedSum(10),        55);
    /* Read-only capture: base passed by value, not pointer */
    check_int("NestedScale(7,6)",   Main__NestedScale(7, 6),    42);
    /* WIDECHAR literal: Text.Length(W"Hi") = 2 */
    check_int("WideLen()",          Main__WideLen(),             2);
    /* NEW(Square) + vtable dispatch through ShapeDispatch */
    check_int("DispatchSquare(5)",  Main__DispatchSquare(5),    25);

    /* EH — normal-path tests and RAISE round-trip */
    check_int("TryRaise()",         Main__TryRaise(),           1); /* raises and catches TestExcept */
    check_int("TryRaiseArg()",      Main__TryRaiseArg(),        42); /* raises TestExceptArg(42), binds v */
    check_int("TryFinNormal()",     Main__TryFinNormal(),      11);
    check_int("TryExceptNormal()",  Main__TryExceptNormal(),    8);

    /* Object field access: construct a fake Square on the stack.
       CM3 object layout: [vtable_ptr(8), side(8)].
       SquareArea reads self.side (at byte offset 8) and returns side*side. */
    struct { void *vtable; M3Int side; } fake_square = { NULL, 7 };
    check_int("SquareArea(side=7)",   Main__SquareArea(&fake_square), 49);

    /* TYPECASE dispatch — NIL path: ScanTypecase(NIL)=-1 → ELSE clause → 0 */
    check_int("TypecaseKind(NULL)",   Main__TypecaseKind(NULL), 0);

    /* IN operator — element IN constant SET (Weekday enum, ordinals 0..6) */
    check_bool("IsWeekend(Sat=5)",   Main__IsWeekend(5),  1); /* Sat is ordinal 5 */
    check_bool("IsWeekend(Mon=0)",   Main__IsWeekend(0),  0);
    check_bool("IsWorkday(Wed=2)",   Main__IsWorkday(2),  1);
    check_bool("IsWorkday(Sun=6)",   Main__IsWorkday(6),  0);

    /* TRUNC / FLOOR / CEILING / ROUND */
    check_int("TruncTest(2.7f)",   Main__TruncTest(2.7f),    2);
    check_int("TruncTest(-1.3f)",  Main__TruncTest(-1.3f),  -1);
    check_int("FloorTest(2.7f)",   Main__FloorTest(2.7f),    2);
    check_int("FloorTest(-1.3f)",  Main__FloorTest(-1.3f),  -2);
    check_int("CeilingTest(2.7f)", Main__CeilingTest(2.7f),  3);
    check_int("CeilingTest(-1.3f)",Main__CeilingTest(-1.3f),-1);
    check_int("RoundTest(3.5)",    Main__RoundTest(3.5),     4); /* nearest even: 3.5 -> 4 */
    check_int("RoundTest(2.5)",    Main__RoundTest(2.5),     2); /* nearest even: 2.5 -> 2, not 3 */
    check_int("RoundTest(-0.5)",   Main__RoundTest(-0.5),    0); /* nearest even: -0.5 -> 0, not -1 */

    /* CONST array subscript — integer array */
    check_int("GetPrime(0)",  Main__GetPrime(0),  2);
    check_int("GetPrime(2)",  Main__GetPrime(2),  5);
    check_int("GetPrime(4)",  Main__GetPrime(4),  11);
    /* CONST array subscript — TEXT array (check non-NULL) */
    check_bool("GetBoolName(0)!=NIL", Main__GetBoolName(0) != NULL, 1);
    check_bool("GetBoolName(1)!=NIL", Main__GetBoolName(1) != NULL, 1);

    /* VALUE open-array formal — caller builds dope vector, MSIR callee sums */
    { M3Int d3[3] = {10, 20, 30};
      OpenArray oa3 = {d3, 3};
      check_int("SumOA({10,20,30})", Main__SumOA(&oa3), 60); }
    { M3Int d5[5] = {1, 2, 3, 4, 5};
      OpenArray oa5 = {d5, 5};
      check_int("SumOA({1,2,3,4,5})", Main__SumOA(&oa5), 15); }

    /* VALUE open-array formal with open actual (dynamic alloca + memcpy) */
    { M3Int d3[3] = {10, 20, 30};  OpenArray oa3 = {d3, 3};
      check_int("SumViaOpenActual({10,20,30})", Main__SumViaOpenActual(&oa3), 60); }
    { M3Int d5[5] = {1, 2, 3, 4, 5};  OpenArray oa5 = {d5, 5};
      check_int("SumViaOpenActual({1,2,3,4,5})", Main__SumViaOpenActual(&oa5), 15); }

    /* Indirect (proc-variable) calls */
    check_int("ApplyBinOp(Add,7,8)",   Main__ApplyBinOp(Main__Add, 7, 8),    15);
    check_int("ApplyUnary(Abs,-5)",    Main__ApplyUnary(Main__Abs, -5),        5);
    check_int("ApplyUnary(Fact,5)",    Main__ApplyUnary(Main__Factorial, 5),  120);

    /* Array-copy: open→fixed (FirstFour, CopyFirst4) via scalar wrappers */
    { M3Int src4[] = {7,8,9,10};  OpenArray oa4 = {src4, 4};
      check_int("FirstFour({7,8,9,10})[0]", Main__FirstFourElem(&oa4, 0), 7);
      check_int("FirstFour({7,8,9,10})[3]", Main__FirstFourElem(&oa4, 3), 10); }
    { M3Int src4[] = {1,2,3,4};  OpenArray oa4 = {src4, 4};
      check_int("CopyFirst4({1,2,3,4})[0]", Main__CopyFirst4Elem(&oa4, 0), 1);
      check_int("CopyFirst4({1,2,3,4})[3]", Main__CopyFirst4Elem(&oa4, 3), 4); }

    /* REF FixedArray deref-copy: r^ := src; copy := r^; return copy[idx]
       src is READONLY FixedIntArr — passed as ptr in C (decayed array). */
    extern M3Int Main__RefFixedArrCopy(M3Int *arr, M3Int idx);
    MSIR_InitTypeLinks();
    { M3Int arr[4] = {10, 20, 30, 40};
      check_int("RefFixedArrCopy({10,20,30,40},0)", Main__RefFixedArrCopy(arr, 0), 10);
      check_int("RefFixedArrCopy({10,20,30,40},3)", Main__RefFixedArrCopy(arr, 3), 40); }

    /* SUBARRAY — fixed array slice */
    check_int("SubarrayFixed(2,4,0)", Main__SubarrayFixedElem(2, 4, 0), 30); /* a[2]=30 */
    check_int("SubarrayFixed(2,4,3)", Main__SubarrayFixedElem(2, 4, 3), 60); /* a[5]=60 */

    /* SUBARRAY — open array slice */
    { M3Int data8[8] = {10,20,30,40,50,60,70,80};
      OpenArray oa8 = {data8, 8};
      check_int("SubarrayOpen(3,3,0)", Main__SubarrayOpenElem(&oa8, 3, 3, 0), 40); /* a[3]=40 */
      check_int("SubarrayOpen(3,3,2)", Main__SubarrayOpenElem(&oa8, 3, 3, 2), 60); /* a[5]=60 */
      check_int("SumSubarray(2,4)",    Main__SumSubarray(&oa8, 2, 4),        180); /* 30+40+50+60 */
    }

    /* ISTYPE / NARROW / TYPECASE-with-var */
    { void *ri = Main__MakeIntRef(42);
      check_int("IsType(ri, REF INTEGER)", Main__TestIsType(ri),       1);
      check_int("Narrow(ri)^",            Main__TestNarrow(ri),       42);
      check_int("TypecaseVar(ri)",        Main__TestTypecaseVar(ri),  42);
    }

    printf("\n%s\n", failures == 0 ? "All tests passed." : "*** FAILURES ABOVE ***");
    return failures;
}
