#include "m3core.h"

// _longjmp does not work on Solaris/C++ because it is std::_longjump.
#if !defined(__sun) || !defined(__cplusplus)

// Temporary for compat, not used.
// u is for underscore, do not save/restore signal mask.
#if defined(_WIN32) || defined(__CYGWIN__) || defined(__MINGW__)
M3WRAP_RETURN_VOID(Csetjmp__ulongjmp, longjmp, (jmp_buf env, int val), (env, val))
#else
M3WRAP_RETURN_VOID(Csetjmp__ulongjmp, _longjmp, (jmp_buf env, int val), (env, val))
#endif

#endif

#ifdef __cplusplus
extern "C" {
#endif

// This is messy.
//  - _setjmp does not work in Solaris C++; it is accidentally placed in std::
//  - sigsetjmp is a good portable idea.
//  - But sigsetjmp should be paired with siglongjmp.
//  - And siglongjmp is not portably linkable, like with Linux/m3cc.
//
// There is only one m3core for all backends.
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
//
#ifdef __sun
void __cdecl Csetjmp__m3_longjmp(sigjmp_buf env, int val)
{
    siglongjmp(env, val);
}
#else
void __cdecl Csetjmp__m3_longjmp(jmp_buf env, int val)
{
#if defined(_WIN32) || defined(__CYGWIN__) || defined(__MINGW__)
    // No signal mask to save/restore, longjmp is the only longjmp.
    longjmp(env, val);
#else
    _longjmp(env, val);
#endif
}
#endif

#ifdef __cplusplus
} // extern "C"
#endif
