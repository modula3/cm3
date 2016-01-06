/* Strongly consider requiring a different bootstrap procedure and remove this file. */

#ifdef _MSC_VER

#if 0 /* This is a problem with certain old<=>new bootstrap steps,
         but the workaround has problems too.
         We need an updated mklib to skip ?OptionsStorage.
		 Other workarounds are to bootstrap from newer cm3 and/or
		 via intermediate Visual C++. Some "large" steps, e.g. boot
		 from cm3 5.2.6 with Visual C++ 2015 do not currently work. */

#pragma warning(disable:4514) /* unused inline function removed */
#pragma warning(disable:4710) /* function not inlined */

#undef _DLL

/* Allow object/libraries compiled with older headers to link with newer runtimes.
   This is necessary for bootstrapping. At least until such time as
   our C runtime dependency is either eliminated, or exported from m3core.dll and
   m3core.lib never statically linked. Neither is trivial.
*/

/* Provide the inline printf function, which libcmt.lib users reference,
   but no longer exists in any .lib. */

#include <stdio.h>

#ifdef __cplusplus
extern "C"
{
#endif

/* Provide __imp__printf, which msvcrt.lib users reference,
   but no longer exists anywhere. */
#if _MSC_VER > 1000 /* TODO test with 4.1 and 4.2 */
__declspec(selectany)
#endif
extern int (__cdecl * const _imp__printf)(const char* , ...) = &printf;

#ifdef __cplusplus
}
#endif

#endif

/* The cm3-5.1.3 release requires the Pentium fdiv workaround to be present
   in the C runtime, but it is absent in newer runtimes. Evidence
   is that only the 4.0 compiler uses this. 2.0, 4.0, 5.0 tested; 4.1 and 4.2 not tested.

   As long as _adjust_fdiv is zero, the other functions will not be called.
*/
#ifdef _M_IX86 /* Microsoft x86 */

#ifdef __cplusplus
extern "C"
{
#endif

#pragma warning(disable:4035) /* no return value */

#if _MSC_VER < 1000 || _MSC_VER >= 1100 /* TODO test 4.1 and 4.2 */

#pragma warning(disable:4725) /* instruction may be inaccurate on some Pentiums */

int _adjust_fdiv;

__declspec(naked)
double __cdecl _adj_fdiv_m64(double b)
{
	/* The first parameter and the return value are in ST(0). */
	__asm { fdiv b }
	__asm { ret 8 }
}

__declspec(naked)
float __cdecl _adj_fdiv_m32(float b)
{
	/* The first parameter and the return value are in ST(0). */
	__asm { fdiv b }
	__asm { ret 4 }
}

#endif /* _MSC_VER < 1000 || _MSC_VER >= 1100 */ /* TODO test 4.1 and 4.2 */

#if _MSC_VER <= 1310

/* TODO find out when ftol2_sse introduced.
   I only currently have 2.0, 4.0, 5.0 (1100), 7.1 (1310), 2015 (14/1900).
   I lack 4.1, 4.2, 6, 7, 8, 9, 10, 11, 12.

   Newer compilers output a call to _ftol2_sse when casting a float or double to an int.
   Older runtimes have only _ftol or only _ftol and _ftol2.
   _ftol2_sse jmps to _ftol2 when SSE is not available.
   _ftol temporarily changes the rounding mode, _ftol2 does not (nor does _ftol2_sse).

   Another workaround would be to forward to msvcrt.dll -- have mklib
   always output such forwarders.
*/
int __cdecl _ftol(double b);
int __cdecl _ftol2(double b);

__declspec(naked)
int __cdecl _ftol2_sse(double b)
{
#if _MSC_VER <= 1100
	__asm { jmp _ftol }
#else
	__asm { jmp _ftol2 }
#endif
}

#endif /* _MSC_VER <= 1310 */ /* TODO test 6 (1200), 7 (1300), 8 (1400), 9 (1500), 10 (1600), 11 (1700), 12 (1800) */

#if _MSC_VER < 1900 /* TODO find exact versions */

/*
no workaround
Consider moving TimeWin32 back to Modula-3, and rewriting strtod in Modula-3 also.
Or improve and use the cross automation which avoids using the old libs.

The calling conventions of the new float to integer functions cannot be implemented with the older compiler.
/arch:IA32 would suppress their use.

m3core.lib.sa(TimeWin32.obj) : error LNK2019: unresolved external symbol __dtol3 referenced in function _TimeWin32__ToFileTime
m3core.lib.sa(TimeWin32.obj) : error LNK2019: unresolved external symbol __ltod3 referenced in function _TimeWin32__FromFileTime
m3core.lib.sa(dtoa.obj) : error LNK2019: unresolved external symbol __imp____fpe_flt_rounds referenced in function _m3_strtod
m3core.lib.sa(dtoa.obj) : error LNK2019: unresolved external symbol __dtoui3 referenced in function _m3_strtod


This is particularly odd, as the symbol has been around for a long time.
m3core.lib.sa(ThreadWin32C.obj) : error LNK2019: unresolved external symbol @__security_check_cookie@4 referenced in function _ThreadWin32__ProcessLive
*/

#endif

#ifdef __cplusplus
}
#endif

#endif /* _M_IX86 */
#endif /* _MSC_VER */

char cm3_libccompat; /* Ensure translation unit is not empty. */
