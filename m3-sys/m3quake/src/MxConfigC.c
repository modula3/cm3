// This file is possibly subject to porting work,
// i.e. if there are systems that are not Win32 or Posix, or
// word sizes other than 32 or 64, etc.
//
// Code should avoid depending on this stuff though too.
#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif
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

char*
__cdecl
MxConfigC__HOST(void)
{
#if defined(_WIN32) && !defined(__CYGWIN__)
    char name[256];

    if (GetEnvironmentVariableA("PROCESSOR_ARCHITECTURE", name, sizeof(name) - 10) > 200)
        return 0;

    ToUpper(name);

    if (strcmp(name, "X86") == 0)
        strcpy(name, "I386");
    strcat(name, "_NT");
    return _strdup(name);
#else
    // Regarding endian:
    //   Debian puts endian in uname_machine so just use that instead of
    //   manually formating "el", "eb", etc.
#if defined(_WIN64) || __INITIAL_POINTER_SIZE == 64 || defined(__LP64__) || defined(_LP64) || __WORDSIZE == 64
    const BOOL size64 = TRUE;
#else
    const BOOL size64 = (sizeof(void*) == 8 || sizeof(size_t) == 8 || sizeof(long) == 8);
#endif
    const BOOL size32 = !size64;
    char* result = 0;
    int length = 0;
    // Sometimes word_size is implied, sometimes not.
    const char* word_size = "";
    struct utsname u;
    const char* uname_machine = u.machine;
    const char* uname_system = u.sysname;
    size_t uname_machine_size = 0;
    size_t uname_system_size = 0;
    size_t word_size_size = 0;
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
    ZeroMemory(&u, sizeof(u));
    uname(&u);

    uname_system_size = strlen(u.sysname);
    uname_machine_size = strlen(u.machine);

    if (uname_system_size > 0x1000 || uname_machine_size > 0x1000) // Avoid overflow later.
        return 0;

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
    (amd64 = (uname64 && (strstr(uname_machine, "AMD64") || strstr(uname_machine, "X86")))) || // TODO: x32?

    (s390x = !! strstr(uname_machine, "S390X")) ||                                 // before plain s390
    (s390 = !! strstr(uname_machine, "S390")) ||
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

    // Most of these are not used.
    // However there is short circuiting.
    (linux_ = !! strstr(uname_system, "LINUX")) ||
    (darwin = !! strstr(uname_system, "DARWIN")) ||
    (solaris = !! strstr(uname_system, "SUNOS")) ||
    (freebsd = !! strstr(uname_system, "FREEBSD")) ||
    (netbsd = !! strstr(uname_system, "NETBSD")) ||
    (openbsd = !! strstr(uname_system, "OPENBSD")) ||
    (hpux = !! strstr(uname_system, "HPUX")) ||
    (osf = !! strstr(uname_system, "OSF")) || // TODO: Test.
    (osf = !! strstr(uname_system, "TRU64")) || // TODO: Test.
    (osf = !! strstr(uname_system, "DIGITAL UNIX")) || // TODO: Test.
    (interix = !! strstr(uname_system, "INTERIX")) ||
    (cygwin = !! strstr(uname_system, "CYGWIN")) ||
    (irix = !! strstr(uname_system, "IRIX")) ||
    (aix = !! strstr(uname_system, "AIX")) ||
    (vms = !! strstr(uname_system, "VMS")) // TODO: Test. OpenVMS?
    ;

    if (solaris)
        uname_system = "SOLARIS";

    // AMD64_SOLARIS is i86pc; we must use local sizeof(void*).
    if (x86 && size32) // x86 => i386
    {
        uname_machine = "I386";
        x86 = TRUE;
        amd64 = FALSE;
    }
    else if (x86 || amd64)  // x86-64 => amd64; this might mistreat x32
    {
        x86 = FALSE;
        amd64 = TRUE;
        uname_machine = "AMD64";
    }
    else if (hppa) // hppa/parisc => pa (too short?)
        uname_machine = "PA";

    if (vms && ia64)
        word_size = size64 ? "_64" : "_32"; // IA64 and ALPHA can be 32 or 64. Vax is always 32.
    else if (vms && !vax)
        word_size = size64 ? "64" : "32"; // IA64 and ALPHA can be 32 or 64. Vax is always 32.
    else if ((darwin && powerpc && size32) || x86 || m68k || s390 || alpha || ia64 || uname32 || amd64 || uname64)
    {
        // implicitly 32 or 64 because it is obvious and/or it is in the name,
        // Or historical names like PPC_DARWIN
        word_size = "";
    }
    else
        word_size = size64 ? "64" : "32"; // sparc32 ppc32 arm32 pa32 pa64 ppc64 mips32 mips64 etc.

    uname_system_size = strlen(uname_system);
    uname_machine_size = strlen(uname_machine);
    word_size_size = strlen(word_size);

    length = uname_system_size + uname_machine_size + word_size_size + 2;
    result = (char*)malloc(length);
    if (result)
    {
        char* p = result;
        memcpy(p, uname_machine, uname_machine_size);
        p += uname_machine_size;
        memcpy(p, word_size, word_size_size);
        p += word_size_size;
        *p++ = '_';
        memcpy(p, uname_system, uname_system_size + 1);
#ifdef M3_HOST
        assert(strcmp(M3_HOST, result) == 0); // Check against m3core.h
#endif
    }
    return result;
#endif
}

#ifdef __cplusplus
} // extern "C"
#endif
