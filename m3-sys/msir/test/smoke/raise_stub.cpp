/* C++ stub for RTHooks__Raise used in the MSIR link-test harness.
   The real RTHooks__Raise allocates a heap RaiseActivation and calls
   RTStack::ThrowM3Exc.  Here we construct the activation on the stack,
   populate exception (= ex, the ExceptionDesc*) and arg, then throw
   _M3Exc{&act} so the MSIR landing-pad machinery can catch it.

   The catch handler in TryRaise does:
     act        = *(ptr*)exc_obj_ptr   (_M3Exc.act)
     exc_desc   = *(ptr*)act           (act->exception = ExceptionDesc*)
     uid        = *(i64*)exc_desc      (ExceptionDesc.uid)
   and compares uid against the compile-time fingerprint.  Storing
   ex (= the descriptor pointer) in act.exception makes this work. */

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
struct _M3Exc { void *act; };

struct RaiseActivation {
    void *exception;  /* ExceptionDesc* — uid is the first field */
    void *arg;
    void *module;
    long  line;
    void *pc;
    void *info0;
    void *info1;
    void *un_except;
    void *un_arg;
};

/* Stubs for RTAllocator_i.o → RTAllocator_m.o chain.
   RTAllocator_i.o needs RTAllocator_M3 which is in RTAllocator_m.o; that
   archive member defines RTHooks__AllocateTracedRef which conflicts with
   our stub below.  Providing empty stubs prevents the pull-in. */
extern "C" void *RTAllocator_I3(long)  { return nullptr; }
extern "C" void *RTAllocator_M3(long)  { return nullptr; }

/* Stub for RTHooks__AllocateTracedRef: allocates dataSize bytes via malloc
   and returns a pointer to the data area (skipping a fake 8-byte header).
   Not GC-managed; safe for short-lived harness tests.
   typeDescr may be null for foreign types whose TypeLink was not resolved
   by MSIR_InitTypeLinks; fall back to allocating 64 bytes in that case. */
extern "C" void *RTHooks__AllocateTracedRef(void *typeDescr) {
    long dataSize = 64;  /* safe fallback for unresolved TypeLinks */
    long typecode = 0;
    if (typeDescr) {
        /* TC_typecode is at byte offset 0 (long index 0).
           TC_dataSize is at byte offset 32 (long index 4). */
        long *tc = (long *)typeDescr;
        typecode = tc[0];
        dataSize = tc[4];
        if (dataSize <= 0) dataSize = 8;
    }
    char *mem = (char *)calloc(1, (size_t)(dataSize + 8));
    /* Store typecode in GC header: RH_typecode_offset=1, so header = typecode<<1 */
    *(long *)mem = typecode << 1;
    return mem + 8;  /* skip fake header */
}

/* Stub for RTHooks__AllocateTracedObj: allocates vtable-ptr + dataSize bytes,
   then mirrors RTAllocator.InitObj by storing OTC_defaultMethods as the vtable.
   TC_dataSize is at byte 32 (long index 4).
   OTC_defaultMethods is at byte 136 (long index 17).
   typeDescr may be null for foreign types (unresolved TypeLink); fall back to
   allocating 64 bytes with a null vtable. */
extern "C" void *RTHooks__AllocateTracedObj(void *typeDescr) {
    long dataSize = 64;  /* safe fallback */
    long typecode = 0;
    void *defaultMethods = nullptr;
    if (typeDescr) {
        long *tc = (long *)typeDescr;
        typecode = tc[0];          /* TC_typecode */
        dataSize = tc[4];          /* TC_dataSize */
        if (dataSize < 0) dataSize = 0;
        defaultMethods = (void *)tc[17];  /* OTC_defaultMethods */
    }
    size_t total = (size_t)(8 + 8 + dataSize);  /* fake-header + vtable + fields */
    char *mem = (char *)calloc(1, total);
    /* Store typecode in GC header: RH_typecode_offset=1, so header = typecode<<1 */
    *(long *)mem = typecode << 1;
    char *obj = mem + 8;            /* skip fake GC header */
    *(void **)obj = defaultMethods; /* InitObj: obj[0] = vtable ptr */
    return obj;
}

/* Stub for RTHooks__AllocateOpenArray (= RTHooks.NewTracedArray).
   Signature: (void *typeDescr, void *sizesStruct) -> void*
   sizesStruct layout (matches M3 OA sizes convention):
     byte  0: ptr  -> first dimension value (= &sizesStruct + 16)
     byte  8: i64  -> number of dimensions
     byte 16: i64  -> dimension 0 size
   ATC layout: nDimensions at byte 96 (long[12]), elementSize at byte 104 (long[13]).
   typeDescr may be null for foreign types (unresolved TypeLink); fall back to
   elemSize=8 in that case. */
extern "C" void *RTHooks__AllocateOpenArray(void *typeDescr, void *sizesStruct) {
    long *sizes = (long *)sizesStruct;
    long elemSize = 8;  /* safe fallback for unresolved TypeLinks */
    if (typeDescr) {
        long *tc = (long *)typeDescr;
        elemSize = tc[13];          /* ATC_elementSize at byte 104 */
        if (elemSize <= 0) elemSize = 8;
    }
    long ndims = sizes[1];           /* OA_size_0 at byte 8 */
    long dim0  = (ndims >= 1) ? sizes[2] : 0;  /* OA_size_1 at byte 16 */
    if (dim0 < 0) dim0 = 0;
    /* Dope vector: { ptr data, i64 dim0 [, i64 dim1 ...] } */
    long dopeSize = 8 + 8 * ndims;   /* ptr (8) + ndims * i64 (8 each) */
    long elemTotal = dim0 * elemSize;
    /* Allocate: fake GC header (8) + dope vector + element storage */
    char *mem  = (char *)calloc(1, 8 + (size_t)(dopeSize + elemTotal));
    char *dope = mem + 8;            /* skip fake GC header */
    char *elems = dope + dopeSize;   /* element storage follows dope */
    *(void **)dope = elems;          /* data_ptr field of dope vector */
    *((long *)(dope + 8)) = dim0;    /* dimension 0 count */
    return dope;                     /* GcRef value = ptr to dope vector */
}

/* Stub out ALL of RTHooks_m.o's symbols to prevent the archive member from
   being pulled in.  IO_m.o references RTHooks__ResumeRaise etc., which would
   drag in RTHooks_m.o and conflict with our RTHooks__Raise below.
   These are never called from harness tests; the harness never invokes
   IO.Put, Assert, or runtime error reporting. */
extern "C" void *RTHooks_I3(long)        { return nullptr; }
extern "C" void *RTHooks_M3(long)        { return nullptr; }
extern "C" void  RTHooks__AssertFailed(void *, long) { }
extern "C" void  RTHooks__NoOp()         { }
extern "C" void  RTHooks__ReportFault(void *, long)  { }
extern "C" void  RTHooks__ResumeRaise(void *)        { }

extern "C" void RTHooks__Raise(void *ex, void *arg,
                                void *module, long line) {
    static RaiseActivation act;
    act.exception = ex;   /* ex IS the ExceptionDesc* with uid at offset 0 */
    act.arg       = arg;
    act.module    = module;
    act.line      = line;
    act.pc = act.info0 = act.info1 = act.un_except = act.un_arg = nullptr;
    throw _M3Exc{&act};
}

/* Import-chain binder stubs: stored as function pointers in ImportInfo globals
   but never invoked in the harness (RTLinker is never run). */
extern "C" void *Thread_I3(long) { return nullptr; }
extern "C" void *Fmt_I3(long)    { return nullptr; }
extern "C" void *IO_I3(long)     { return nullptr; }
extern "C" void *Text_I3(long)   { return nullptr; }

/* Fmt / IO stubs: referenced from Main__Main_M3 (module body). */
extern "C" void *Fmt__Int(long, long)  { return nullptr; }
extern "C" void *Fmt__Bool(int)        { return nullptr; }
extern "C" void *Fmt__Real(float, int, int, int) { return nullptr; }
extern "C" void  IO__Put(void *, void *) { }

/* Text.Length: read cnt field (i64 at offset 8 from TEXT ptr).
   For normal strings cnt > 0; for wide-char strings cnt < 0 (negated length). */
extern "C" long  Text__Length(void *t) {
    if (!t) return 0;
    int64_t cnt; memcpy(&cnt, (char*)t + 8, 8);
    return (long)(cnt < 0 ? -cnt : cnt);
}

/* GC read barrier: a no-op in the harness — no real GC runs. */
extern "C" void  RTHooks__CheckLoadTracedRef(void *) { }

/* GC write barrier: a no-op in the harness — dirty-bit fast-path skips it
   for newly allocated objects (dirty bit = 0), so this is only called when
   a real GC is running and the object is not already dirty.  No GC here. */
extern "C" void  RTHooks__CheckStoreTraced(void *) { }

/* TYPECASE dispatch: NIL → no match (returns -1 → switch default = ELSE);
   non-NIL → first clause (returns 0). */
extern "C" long  RTHooks__ScanTypecase(void *ref, void *) {
    return (ref == nullptr) ? -1L : 0L;
}

/* ISTYPE / NARROW runtime check: stub always returns 1 (TRUE / non-zero INTEGER).
   Correct for tests that pass a valid ref; NIL→1 matches M3 semantics
   (NIL is accepted by ISTYPE for any traced ref type). */
extern "C" long  RTHooks__CheckIsType(void *, void *) { return 1L; }

/* RTHooks__ReportFault is declared above; Hook.Abort maps to it. */

/* TEXT concatenation — only called from module body, not harness tests. */
extern "C" void *RTHooks__Concat(void *, void *) { return nullptr; }

/* RTHooks TextLiteral method stubs — stored as pointers in @textlit_methods
   but never called from the test harness (module body never runs). */
extern "C" void  RTHooks__TextLitInfo(void *, void *)             { }
extern "C" long  RTHooks__TextLitGetChar(void *, long)            { return 0; }
extern "C" long  RTHooks__TextLitGetWideChar(void *, long)        { return 0; }
extern "C" void  RTHooks__TextLitGetChars(void *, void *, long)   { }
extern "C" void  RTHooks__TextLitGetWideChars(void *, void *, long) { }
