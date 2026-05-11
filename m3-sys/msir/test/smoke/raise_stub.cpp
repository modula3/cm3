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
   Not GC-managed; safe for short-lived harness tests. */
extern "C" void *RTHooks__AllocateTracedRef(void *typeDescr) {
    /* TC_dataSize is at byte offset 32 of the TypeCell. */
    long *tc = (long *)typeDescr;
    long dataSize = tc[4];  /* byte 32 / sizeof(long) = index 4 */
    if (dataSize <= 0) dataSize = 8;
    char *mem = (char *)calloc(1, (size_t)(dataSize + 8));
    return mem + 8;  /* skip fake header */
}

/* Stub for RTHooks__AllocateTracedObj: allocates vtable-ptr + dataSize bytes,
   then mirrors RTAllocator.InitObj by storing OTC_defaultMethods as the vtable.
   TC_dataSize is at byte 32 (long index 4).
   OTC_defaultMethods is at byte 136 (long index 17). */
extern "C" void *RTHooks__AllocateTracedObj(void *typeDescr) {
    long *tc = (long *)typeDescr;
    long dataSize = tc[4];          /* TC_dataSize */
    if (dataSize < 0) dataSize = 0;
    void *defaultMethods = (void *)tc[17];  /* OTC_defaultMethods */
    size_t total = (size_t)(8 + 8 + dataSize);  /* fake-header + vtable + fields */
    char *mem = (char *)calloc(1, total);
    char *obj = mem + 8;            /* skip fake GC header */
    *(void **)obj = defaultMethods; /* InitObj: obj[0] = vtable ptr */
    return obj;
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

/* Fmt / IO stubs: called only from the M3 module body (Main__Main_M3),
   which the harness never invokes. */
extern "C" void *Fmt__Int(long, long)  { return nullptr; }
extern "C" void *Fmt__Bool(int)        { return nullptr; }
extern "C" void  IO__Put(void *, void *) { }

/* GC read barrier: a no-op in the harness — no real GC runs. */
extern "C" void  RTHooks__CheckLoadTracedRef(void *) { }

/* TYPECASE dispatch: for NIL input, returns 0 (→ first TYPECASE branch). */
extern "C" long  RTHooks__ScanTypecase(void *, void *) { return 0; }

/* TEXT concatenation — only called from module body, not harness tests. */
extern "C" void *RTHooks__Concat(void *, void *) { return nullptr; }

/* RTHooks TextLiteral method stubs — stored as pointers in @textlit_methods
   but never called from the test harness (module body never runs). */
extern "C" void  RTHooks__TextLitInfo(void *, void *)             { }
extern "C" long  RTHooks__TextLitGetChar(void *, long)            { return 0; }
extern "C" long  RTHooks__TextLitGetWideChar(void *, long)        { return 0; }
extern "C" void  RTHooks__TextLitGetChars(void *, void *, long)   { }
extern "C" void  RTHooks__TextLitGetWideChars(void *, void *, long) { }
