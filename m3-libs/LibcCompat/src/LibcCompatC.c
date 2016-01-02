#ifdef _MSC_VER

#undef _DLL

/* Allow object/libraries compiled with older headers to link with newer runtimes.
   This is necessary for bootstrapping. At least until such time as
   our C runtime dependency is either eliminated, or exported from m3core.dll and
   m3core.lib never statically linked. Neither is trivial.
*/

/* Provide the inline printf function, which libcmt.lib users reference,
   but no longer exists in any .lib. */
#include <stdio.h>

/* Provide __imp__printf, which msvcrt.lib users reference,
   but no longer exists anywhere. */
__declspec(selectany) extern const int (__cdecl * _imp__printf)(const char*, ...) = &printf;

#endif
