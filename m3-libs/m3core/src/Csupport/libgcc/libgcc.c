/*
This code is for the configuration of
using the gcc backend, without an actual gcc installation.
Specifically without libgcc.

DI is gcc's "double integer" aka 64 bit integer.
uDI is unsigned double integer.

The backend sometimes outputs calls to functions, such
as helpers for 64 bit math.

As this is derived from 
m3-sys\m3cc\gcc\gcc\config\darwin-64.c
it is covered by GPL. A non GPL reimplementation would be nice.
Note that the C code is totally trivial.
*/

#ifdef __GNUC__
#error This code must not be compiled with gcc, that would be circular.
#endif

#ifdef _MSC_VER
typedef unsigned __int64 UINT64;
#else
typedef unsigned long long UINT64;
#endif

UINT64 __udivdi3 (UINT64 x, UINT64 y)
{ 
   return (x / y);
}

UINT64 __umoddi3 (UINT64 x, UINT64 y)
{
    return (x % y);
}

