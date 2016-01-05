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
#if _MSC_VER < 1000 || _MSC_VER >= 1100 /* TODO test 4.1 and 4.2 */

#ifdef __cplusplus
extern "C"
{
#endif

#pragma warning(disable:4035) /* no return value */
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

#ifdef __cplusplus
}
#endif

#endif /* _MSC_VER < 1000 || _MSC_VER >= 1100 */ /* TODO test 4.1 and 4.2 */
#endif /* _M_IX86 */
#endif /* _MSC_VER */

char cm3_libccompat; /* Ensure translation unit is not empty. */
