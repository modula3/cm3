// This file is possibly subject to porting work,
// i.e. if there are systems that are not Win32 or Posix, or
// word sizes other than 32 or 64, etc.
//
// Code should avoid depending on this stuff though too.
#if 0 /*for testing purposes*/
#ifndef _MSC_VER
#define __cdecl /* nothing */
typedef unsigned char BOOLEAN;
typedef int BOOL;
#define TRUE 1
#define FALSE 0
#endif
#else
#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <sys/utsname.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

BOOLEAN
__cdecl
MxConfigC__ifdef_win32(void)
{
#ifdef _WIN32
    return TRUE;
#else
    return FALSE;
#endif
}

static
void
ToUpper(char* a)
{
    char ch;
    while ((ch = *a++))
    {
        if (islower(ch))
            a[-1] = toupper(ch);
    }
}

static
char*
Concat(const char* a0,
       const char* a1,
       const char* a2,
       const char* a3,
       const char* a4)
{
    size_t n0 = strlen(a0);
    size_t n1 = strlen(a1);
    size_t n2 = strlen(a2);
    size_t n3 = strlen(a3);
    size_t n4 = strlen(a4);
    if (n0 > 0xFFF || n1 > 0xFFF || n2 > 0xFFF || n3 > 0xFFF || n4 > 0xFFF) // crudely avoid overflow
        return 0;
    char* result = (char*)malloc(n0 + n1 + n2 + n3 + n4 + 1);
    char* p = result;
    if (result)
    {
        memcpy(p, a0, n0); p += n0;
        memcpy(p, a1, n1); p += n1;
        memcpy(p, a2, n2); p += n2;
        memcpy(p, a3, n3); p += n3;
        memcpy(p, a4, n4); p += n4;
        *p++ = 0;
    }
    return result;
}

const char*
__cdecl
MxConfigC__HOST(void)
{
#ifdef M3_HOST_SKIP_UNAME
    return strdup(M3_HOST);
#elif defined(_WIN32) && !defined(__CYGWIN__)
    char name[256];

    if (GetEnvironmentVariableA("PROCESSOR_ARCHITECTURE", name, sizeof(name) - 10) > 200)
        return 0;

    ToUpper(name);

    if (strcmp(name, "X86") == 0)
        strcpy(name, "I386");
    else if (strcmp(name, "ARM") == 0)
        strcpy(name, "ARM32");
    strcat(name, "_NT");
    return _strdup(name);
#else
    // Regarding endian:
    //   Debian puts endian in uname_machine so just use that instead of
    //   manually formating "el", "eb", etc.
#if defined(_WIN64) || __INITIAL_POINTER_SIZE == 64 || defined(__LP64__) || defined(_LP64) || __WORDSIZE == 64
    /*const but clang warns/errors*/ BOOL size64 = TRUE;
#else
    /*const but clang warns/errors*/ BOOL size64 = (sizeof(void*) == 8 || sizeof(size_t) == 8 || sizeof(long) == 8);
#endif
    /*const but clang warns/errors*/ BOOL size32 = !size64;
    char* result = 0;
    // Sometimes word_size is implied, sometimes not.
    // Usually CPU{32,64}_OS but sometimes CPU_OS{32,64} e.g. for IA64.
    const char* cpu_word_size = "";
    const char* os_word_size = "";
    struct utsname u;
    const char* uname_machine = u.machine;
    const char* uname_system = u.sysname;
    BOOL uname32 = FALSE;
    BOOL uname64 = FALSE;
    BOOL aix = FALSE;
    BOOL alpha = FALSE;
    BOOL amd64 = FALSE;
    BOOL arm = FALSE;
    BOOL arm64 = FALSE;
    BOOL cygwin = FALSE;
    BOOL darwin = FALSE;
    BOOL freebsd = FALSE;
    BOOL hppa = FALSE;
    BOOL hpux = FALSE;
    BOOL ia64 = FALSE;
    BOOL interix = FALSE;
    BOOL irix = FALSE;
    BOOL linux_ = FALSE;
    BOOL mips = FALSE;
    BOOL m68k = FALSE;
    BOOL netbsd = FALSE;
    BOOL openbsd = FALSE;
    BOOL osf = FALSE;
    BOOL powerpc = FALSE;
    BOOL powerpc64 = FALSE;
    BOOL riscv32 = FALSE;
    BOOL riscv64 = FALSE;
    BOOL s390 = FALSE;
    BOOL s390x = FALSE;
    BOOL solaris = FALSE;
    BOOL sparc = FALSE;
    BOOL vax = FALSE;
    BOOL vms = FALSE;
    BOOL x86 = FALSE;
    // dragonflybsd, illumos, smartos, ultrix, etc.

    // Some systems do not nul terminate (old WSL1 bug).
    memset(&u, 0, sizeof(u));
    uname(&u);

    ToUpper(u.sysname);
    ToUpper(u.machine);

    // This is not necessarily all that work, it is just all with slightly special handling.
    // As well, none of this is used much.
    //
    // The main point ends up to do some simple translations.
    // For example i86pc on Solaris become I386.
    // x86 becomes I386 to match historical name.
    // x86-64 becomes AMD64.
    // Sparc is "sun4". sun3 is m68k.
    // Also checks for mips, alpha, etc. avoid the lose check for merely "86".
    // Consider maybe just running config.guess.
    //
    // Really, we could just use the uname data directly.
    //
    uname64 = !! strstr(uname_machine, "64");
    uname32 = !! strstr(uname_machine, "32");

    // There is short circuiting here. Less ambiguous before more ambiguous.
    // Roughly longer before shorter.
    // foo64 before foo.
    (alpha = !! strstr(uname_machine, "ALPHA")) ||

    // "x86" only means amd64 if there is "64" somewhere like "x86-64" or "x86_64".
    // A later check against size32/size64 will try to isolate x32 platforms.
    (amd64 = (uname64 && (strstr(uname_machine, "AMD64") || strstr(uname_machine, "X86")))) ||

    (s390x = !! strstr(uname_machine, "S390X")) || // 64bit, before plain s390
    (s390 = !! strstr(uname_machine, "S390")) ||   // 32bit
    (m68k = !! strstr(uname_machine, "M68K")) ||
    (hppa = (strstr(uname_machine, "HPPA") || strstr(uname_machine, "PARISC"))) ||
    (mips = !! strstr(uname_machine, "MIPS")) ||                                   // 32 or 64, big or little endian
    (powerpc64 = (uname64 && (strstr(uname_machine, "PPC64") || strstr(uname_machine, "POWERPC64")))) || // before plain powerpc
    (powerpc = (strstr(uname_machine, "PPC") || strstr(uname_machine, "POWERPC"))) ||
    (arm64 = (uname64 && (strstr(uname_machine, "AARCH64") || strstr(uname_machine, "ARM64")))) ||
    (arm = (strstr(uname_machine, "AARCH") || strstr(uname_machine, "ARM"))) ||
    (riscv64 = (uname64 && strstr(uname_machine, "RISCV64"))) ||
    (riscv32 = !! strstr(uname_machine, "RISCV32")) ||
    (sparc = (strstr(uname_machine, "SPARC") || strstr(uname_machine, "SUN4"))) ||
    (m68k = (strstr(uname_machine, "M68K") || strstr(uname_machine, "SUN3"))) ||
    (vax = !! strstr(uname_machine, "VAX")) ||
    (ia64 = (uname64 && (strstr(uname_machine, "IA64")))) ||

    // All the previous checks help to mitigate the following short check.
    (x86 = !! strstr(uname_machine, "86")) // Solaris: i86pc
    ;

    // Note the break here.
    // Short circuiting of processor is separate from OS.

    // Most of these are not used.
    // However there is short circuiting.
    (linux_ = !! strstr(uname_system, "LINUX")) ||
    (darwin = !! strstr(uname_system, "DARWIN")) ||
    (solaris = !! strstr(uname_system, "SUNOS")) ||
    (freebsd = !! strstr(uname_system, "FREEBSD")) ||
    (netbsd = !! strstr(uname_system, "NETBSD")) ||
    (openbsd = !! strstr(uname_system, "OPENBSD")) ||
    (hpux = !! strstr(uname_system, "HPUX")) ||
    (hpux = !! strstr(uname_system, "HP-UX")) ||
    (osf = !! strstr(uname_system, "OSF")) || // TODO: Test.
    (osf = !! strstr(uname_system, "TRU64")) || // TODO: Test.
    (osf = !! strstr(uname_system, "DIGITAL UNIX")) || // TODO: Test.
    (interix = !! strstr(uname_system, "INTERIX")) ||
    (cygwin = !! strstr(uname_system, "CYGWIN")) ||
    (irix = !! strstr(uname_system, "IRIX")) ||
    (aix = !! strstr(uname_system, "AIX")) ||
    (vms = !! strstr(uname_system, "VMS")) // TODO: Test. OpenVMS?
    ;

    if (hpux)
        uname_system = "HPUX"; // instead of HP-UX with the dash
    else if (solaris)
        uname_system = "SOLARIS"; // instead of SunOS
    else if (osf)
        uname_system = "OSF"; // instead of OSF1 with the 1

    // AMD64_SOLARIS is i86pc; we must use local sizeof(void*).
    if (size32 && (amd64 || x86)) // x86 => i386
    {
        uname_machine = "I386";
        x86 = TRUE;
        amd64 = FALSE;
    }
    else if (size64 && (amd64 || x86)) // x86-64 => amd64; prior clause should catch x32
    {
        x86 = FALSE;
        amd64 = TRUE;
        uname_machine = "AMD64";
    }
    else if (arm64) // aarch64 => arm64
    {
        uname_machine = "ARM64";
    }
    else if (hppa) // hppa/parisc => pa (too short?)
        uname_machine = "PA";

    // Alpha and IA64 are not always 64bits.
    // VMS, HPUX, NT offer 32bit variations, NT/Alpha64 never shipped.
    if (ia64 && (vms || hpux))
    {
        // IA64_VMS32
        // IA64_VMS64
        // IA64_HPUX32
        // IA64_HPUX64
        os_word_size = size32 ? "32" : "64";
        cpu_word_size = "";
    }
    else if (alpha && (vms /*|| nt*/))
    {
        // ALPHA32_VMS
        // ALPHA64_VMS
        // ALPHA32_NT
        // ALPHA64_NT (unreleased)
        cpu_word_size = size32 ? "32" : "64";
        os_word_size = "";
    }
    else if ((darwin && powerpc && size32) || x86 || m68k || s390 || alpha || ia64 || uname32 || amd64 || uname64 || vax || s390x)
    {
        // implicitly 32 or 64 because it is obvious and/or it is in the name,
        // Or historical names like PPC_DARWIN
        os_word_size = "";
        cpu_word_size = "";
    }
    else
    {
        os_word_size = "";
        cpu_word_size = size64 ? "64" : "32"; // {sparc,ppc,arm,pa,mips}{32,64} except PPC_DARWIN
    }

    result = Concat(uname_machine, cpu_word_size, "_", uname_system, os_word_size);
#ifdef M3_HOST
    if (result)
    {
        if (strcmp(M3_HOST, result))
            printf("%s %s\n", M3_HOST, result);
        assert(strcmp(M3_HOST, result) == 0); // Check against m3core.h
    }
#endif
    return result;
#endif
}

#ifdef __cplusplus
} // extern "C"
#endif

#if 0 /*for testing purposes*/
int main()
{
    printf("%s\n", MxConfigC__HOST());
}
#endif
