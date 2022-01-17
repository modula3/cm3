#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

// This is messy.
//  - _setjmp does not work in Solaris C++; it is accidentally placed in std::
//  - sigsetjmp is a good portable idea. We should use it on all Unix platforms.
//  - sigsetjmp should be paired with siglongjmp, which is ok.
//  - sigsetjmp is not portably linkable, like with Linux/m3cc. That is a problem.
//    (Siglongjmp/longjmp linkability is no matter. We call it from C.)
//
// There is only one m3core for all backends.
// TODO: Relax "one m3core" rule. Have the setjmp call leave a function
// pointer or boolean or enum in the frame or in a global. Or inject a static
// helper into every C backend output file, i.e. call siglongjmp from C backend output.
//
// Therefore:
//   C backend gets "m3_setjmp"
//    => NT setjmp
//    => Solaris sigsetjmp
//    => else _setjmp as usual
//
//  m3cc:
//    => non-Solaris _setjmp as usual
//    => Solaris sigsetjmp, not tested
//
//  m3core:
//    => NT longjmp
//    => Solaris sigsetjmp
//    => else _longjmp as usual
//
// Wrapper for longjmp / siglongjmp specifically for use by Modula-3 exception handling.
#ifdef __sun
void __cdecl Csetjmp__m3_longjmp(Csetjmp__jmp_buf env, int val)
{
    siglongjmp(*env, val);
}
#else
void __cdecl Csetjmp__m3_longjmp(Csetjmp__jmp_buf env, int val)
{
#if defined(_WIN32) || defined(__CYGWIN__) || defined(__MINGW__) || defined(__DJGPP__)
    // No signal mask to save/restore, longjmp is the only longjmp.
    longjmp(*env, val);
#else
    _longjmp(*env, val);
#endif
}
#endif

#ifdef __cplusplus
} // extern "C"
#endif
