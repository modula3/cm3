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
