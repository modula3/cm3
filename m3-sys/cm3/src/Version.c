#include <assert.h>
#include <string.h>

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

#ifdef __cplusplus
extern "C"
{
#endif

const char*
__cdecl
Version__Created(void); // declare for gcc/clang

// Convert:
//            1         2
//  012345678901234567890123
//  Wed Mar 17 23:52:48 2021 __TIMESTAMP__
//  Mar 18 202100:14:43      __DATE__ __TIME__;
// to:
//  2021-03-18 04:44:17

//static const char versionCreated1[] = __TIMESTAMP__;
static const char versionCreated1[] = __DATE__ __TIME__;
static char versionCreated2[] = __DATE__ __TIME__;

const char*
__cdecl
Version__Created(void)
{
    const char* month = versionCreated1;
    int i = -1;
    int j = 6;
    versionCreated2[++i] = versionCreated1[++j];
    versionCreated2[++i] = versionCreated1[++j];
    versionCreated2[++i] = versionCreated1[++j];
    versionCreated2[++i] = versionCreated1[++j];
    versionCreated2[++i] = '-';
    j = 0;
#if 1
         if (!memcmp(month, "Jan", 3)) j = 1;
    else if (!memcmp(month, "Feb", 3)) j = 2;
    else if (!memcmp(month, "Mar", 3)) j = 3;
    else if (!memcmp(month, "Apr", 3)) j = 4;
    else if (!memcmp(month, "May", 3)) j = 5;
    else if (!memcmp(month, "Jun", 3)) j = 6;
    else if (!memcmp(month, "Jul", 3)) j = 7;
    else if (!memcmp(month, "Aug", 3)) j = 8;
    else if (!memcmp(month, "Sep", 3)) j = 9;
    else if (!memcmp(month, "Oct", 3)) j = 10;
    else if (!memcmp(month, "Nov", 3)) j = 11;
    else if (!memcmp(month, "Dec", 3)) j = 12;
#else
         if (month[2] == 'l') j = 7; // Jul
    else if (month[1] == 'u') j = 6; // Jun
    else if (month[0] == 'J') j = 1; // Jan
    else if (month[0] == 'F') j = 2; // Feb
    else if (month[2] == 'y') j = 5; // May
    else if (month[3] == 'g') j = 8; // Aug
    else if (month[0] == 'M') j = 3; // Mar
    else if (month[0] == 'A') j = 4; // Apr
    else if (month[0] == 'S') j = 9; // Sep
    else if (month[0] == 'O') j = 10; // Oct
    else if (month[0] == 'N') j = 11; // Nov
    else if (month[0] == 'D') j = 12; // Dec
#endif
    assert(j);
    versionCreated2[++i] = '0' + (j / 10);
    versionCreated2[++i] = '0' + (j % 10);
    versionCreated2[++i] = '-';
    j = 3;
    versionCreated2[++i] = versionCreated1[++j];
    versionCreated2[++i] = versionCreated1[++j];
    versionCreated2[++i] = ' ';
    return versionCreated2;
}

#ifdef __cplusplus
} // extern "C"
#endif

#if 0

#include <stdio.h>

int main()
{
    printf("%s\n", versionCreated1);
    printf("%s\n", Version__Created());
    return 0;
}

#endif
