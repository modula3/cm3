/* $Id$ */

#include <stdio.h>
#include <sys/utsname.h>

int main()
{
    struct utsname u;
    unsigned i;
    const static struct
    {
        const char* Format;
        unsigned Value;
    } Data[] = {
"(* This file was generated from " __FILE__ ". Do not edit it. *)", 0,
"", 0,
"INTERFACE Uutsname;", 0,
"", 0,
"FROM Ctypes IMPORT char, int;", 0,
"", 0,
"TYPE", 0,
"  struct_utsname_star = UNTRACED REF struct_utsname;", 0,
"  struct_utsname = BITS %u FOR RECORD", sizeof(u) * 8,
"    sysname    : ARRAY [0..%u] OF char;", sizeof(u.sysname) - 1,
"    nodename   : ARRAY [0..%u] OF char;", sizeof(u.nodename) - 1,
"    release    : ARRAY [0..%u] OF char;", sizeof(u.release) - 1,
"    version    : ARRAY [0..%u] OF char;", sizeof(u.version) - 1,
"    machine    : ARRAY [0..%u] OF char;", sizeof(u.machine) - 1,
"    domainname : ARRAY [0..%u] OF char;", sizeof(u.domainname) - 1,
"  END;", 0,
"", 0,
"<*EXTERNAL*> PROCEDURE uname (buf: struct_utsname_star): int;", 0,
"", 0,
"END Uutsname.", 0
};
    char *p[8];
    unsigned size[8];
    unsigned a;

    /* check the types, and adjacency */
    p[0] = (char*) &u;
    p[1] = u.sysname;
    p[2] = u.nodename;
    p[3] = u.release;
    p[4] = u.version;
    p[5] = u.machine;
    p[6] = u.domainname;
    p[7] = (char*) (1 + &u);

    size[0] = 0;
    size[1] = sizeof(u.sysname);
    size[2] = sizeof(u.nodename);
    size[3] = sizeof(u.release);
    size[4] = sizeof(u.version);
    size[5] = sizeof(u.machine);
    size[6] = sizeof(u.domainname);

    for (i = 1 ; i != sizeof(p) / sizeof(p[0]) ; ++i)
    {
        if (p[i] != p[i - 1] + size[i - 1])
        {
            printf("adjacency error %u\n", i);
            return 1;
        }
    }

    /* check that we got everything */
    a = sizeof(u.sysname) + sizeof(u.nodename) + sizeof(u.release) + sizeof(u.version) + sizeof(u.machine) + sizeof(u.domainname);
    if (sizeof(u) != a)
    {
        printf("ERROR %u vs. %u\n", sizeof(u), a);
        return 1;
    }
    for (i = 0 ; i != sizeof(Data)/sizeof(Data[0]) ; ++i)
    {
        printf(Data[i].Format, Data[i].Value);
        printf("\n");
    }
    return 0;
}
