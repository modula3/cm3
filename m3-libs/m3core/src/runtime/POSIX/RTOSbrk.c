#include "m3core.h"
#include <unistd.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
This is the historic and more portable implementation.
http://modula3.elegosoft.com/cgi-bin/cvsweb.cgi/cm3/m3-libs/m3core/src/runtime/POSIX/RTOS.m3?rev=1.1;content-type=text%2Fplain
*/

void* RTOS__GetMemory(size_t size)
{
    return sbrk(size);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
