// This file is possibly subject to porting work,
// i.e. if there are systems that are not Win32 or Posix, or
// word sizes other than 32 or 64, etc.
//
// Code should avoid depending on this stuff though too.
#include "m3core.h"

M3_EXTERN_C_BEGIN

// porting: Just Posix and Win32 or more?
#ifdef _WIN32
EXTERN_CONST int MxConfig__os_type = 1;
#else
EXTERN_CONST int MxConfig__os_type = 0;
#endif

M3_EXTERN_C_END
