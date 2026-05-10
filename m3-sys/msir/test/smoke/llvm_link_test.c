/* End-to-end link test for MSIR-generated LLVM IR.
   Calls M3 procs compiled from Main.ll via LLVM and checks results. */
#include <stdio.h>
#include <string.h>

typedef long          M3Int;
typedef unsigned char M3Byte;   /* i8  — enum / CHAR */
typedef _Bool         M3Bool;   /* i1  — BOOLEAN */

typedef struct { M3Int x; M3Int y; } Point;
typedef struct { M3Int *data; M3Int  size; } OpenArray;

/* Stubs for M3 runtime externs referenced in Main__Main_M3.
   We never call that proc from the harness, but the linker needs them. */
void *Fmt__Int(M3Int n, M3Int base) { return NULL; }
void  IO__Put(void *t, void *wr)    { (void)t; (void)wr; }
void *Fmt__Bool(M3Bool b)           { return NULL; }

/* Stubs for GC write/read barrier slow paths.  These are only called when
   a heap object is gray (read barrier) or not dirty (write barrier) during
   an incremental collection.  In the link test no GC runs, so these are
   never actually invoked. */
void RTHooks__CheckLoadTracedRef(void *ref) { (void)ref; }
void RTHooks__CheckStoreTraced(void *dst)   { (void)dst; }

/* Stub for the C++ typeinfo of _M3Exc, required by the EH exception tables
   emitted by LLVM for TRY/EXCEPT procs.  Content is irrelevant for these
   normal-path tests since no exception is actually thrown. */
void *_ZTI6_M3Exc[2] = { 0, 0 };

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

extern M3Int  Main__TryFinNormal(void);
extern M3Int  Main__TryExceptNormal(void);

/* Method dispatch + heap object field access */
extern M3Int  Main__SquareArea(void *self);  /* reads self->side via field GEP */

/* TYPECASE test */
extern M3Int  Main__TypecaseKind(void *r);

/* Stub for RTHooks__ScanTypecase used by TYPECASE lowering.
   ScanTypecase(NIL, table) returns 0 per M3 spec (first clause) without
   accessing any runtime state — safe in the uninitialised harness.
   For non-NIL refs: walk the cell array to find the ELSE index (uid=0). */
typedef struct { void *defn; long uid; } M3_TCCell;
long RTHooks__ScanTypecase(void *ref, M3_TCCell *table) {
    long i = 0;
    if (ref == NULL) return 0;
    while (table[i].uid != 0) ++i;
    return i; /* ELSE index */
}

/* Direct access to module globals (zeroinitialised — no M3 module init runs) */
extern M3Int  Main__gCounter;
extern M3Int  Main__gBase;

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

    /* EH — normal-path tests (no exception raised) */
    check_int("TryFinNormal()",     Main__TryFinNormal(),      11);
    check_int("TryExceptNormal()",  Main__TryExceptNormal(),    8);

    /* Object field access: construct a fake Square on the stack.
       CM3 object layout: [vtable_ptr(8), side(8)].
       SquareArea reads self.side (at byte offset 8) and returns side*side. */
    struct { void *vtable; M3Int side; } fake_square = { NULL, 7 };
    check_int("SquareArea(side=7)",   Main__SquareArea(&fake_square), 49);

    /* TYPECASE dispatch — NIL path: ScanTypecase(NIL)=0 → first clause → 1 */
    check_int("TypecaseKind(NULL)",   Main__TypecaseKind(NULL), 1);

    printf("\n%s\n", failures == 0 ? "All tests passed." : "*** FAILURES ABOVE ***");
    return failures;
}
