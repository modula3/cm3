#include "m3core.h"

/* wrapper for longjmp / _longjmp
   u is for underscore
   underscore version explicitly does not manipulate signal mask */

#if defined(_WIN32) || defined(__CYGWIN__) || defined(__MINGW__)
M3WRAP_RETURN_VOID(Csetjmp__ulongjmp, longjmp, (jmp_buf env, int val), (env, val))
#else
M3WRAP_RETURN_VOID(Csetjmp__ulongjmp, _longjmp, (jmp_buf env, int val), (env, val))
#endif
