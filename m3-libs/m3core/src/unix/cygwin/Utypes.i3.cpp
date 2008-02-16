/* $Id$ */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

const char* PickType(unsigned char) { return "u_char"; }
const char* PickType(unsigned short) { return "u_short"; }
const char* PickType(unsigned) { return "u_int"; }
const char* PickType(unsigned long) { return "u_long"; }
const char* PickType(int) { return "int"; }
const char* PickType(long) { return "long"; }

const char* PickType(unsigned long long)
{
    return "unsigned_long_long";
}

const char* PickType(long long)
{
    return "long_long";
}

const char* PickType(unsigned char);
const char* PickType(char);
const char* PickType(short);

int main()
{
    unsigned i;
    const static struct
    {
        const char* Format;
        const char* Value;
    } Data[] =
{
"(* Copyright (C) 1990, Digital Equipment Corporation.         *)", 0,
"(* All rights reserved.                                       *)", 0,
"(* See the file COPYRIGHT for a full description.             *)", 0,
"(*                                                            *)", 0,
"(* Last modified on Fri Apr 29 15:38:49 PDT 1994 by kalsow    *)", 0,
"(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)", 0,
"(*      modified on Mon Jan 11 14:34:58 PST 1993 by muller    *)", 0,
"", 0,
"(* $Id" "$ *)", 0,
"", 0,
"(* This file was generated from " __FILE__ ". Do not edit it. *)", 0,
"", 0,
"INTERFACE Utypes;", 0,
"", 0,
"FROM Ctypes IMPORT ", 0,
"	long, unsigned_long, int, unsigned_int, unsigned_short,", 0,
"        unsigned_char, long_long, unsigned_long_long;", 0,
"", 0,
"(*** <sys/types.h> ***)", 0,
"", 0,
"(*", 0,
" * Basic system types and major/minor device constructing/busting macros.", 0,
" *)", 0,
"", 0,
"(* major part of a device *)", 0,
"PROCEDURE major (x: dev_t): int;", 0,
"", 0,
"(* minor part of a device *)", 0,
"PROCEDURE minor (x: dev_t): int;", 0,
"", 0,
"(* make a device number *)", 0,
"PROCEDURE makedev (x, y: int): dev_t;", 0,
"", 0,
"TYPE", 0,
"  u_char  = unsigned_char;", 0,
"  u_short = unsigned_short;", 0,
"  u_int   = unsigned_int;", 0,
"  uint    = unsigned_int; (* sys V compatibility *)", 0,
"  u_long  = unsigned_long;", 0,
"  ushort  = unsigned_short; (* sys III compat *)", 0,
"", 0,
"  quad         = long_long;", 0,
"  daddr_t      = %s; ", PickType(daddr_t()),
"  caddr_t      = ADDRESS;", 0,
"  ino_t        = %s;", PickType(ino_t()),
"", 0,
"  size_t       = %s;", PickType(size_t()),
"  time_t       = %s;", PickType(time_t()),
"  dev_t        = %s;", PickType(dev_t()),
"  off_t        = %s;", PickType(off_t()),
"", 0,
"  key_t        = %s; (* sys V compatibility *)", PickType(key_t()),
"  clock_t      = %s; (* POSIX compliance *)", PickType(clock_t()),
"  mode_t       = %s; (* POSIX compliance *)", PickType(mode_t()),
"  nlink_t      = %s; (* POSIX compliance *)", PickType(nlink_t()),
"  uid_t        = %s; (* POSIX compliance *)", PickType(uid_t()),
"  pid_t        = %s; (* POSIX compliance *)", PickType(pid_t()),
"  gid_t        = %s; (* POSIX compliance *)", PickType(gid_t()),
"", 0,
"END Utypes.", 0,
};
    for (i = 0 ; i != sizeof(Data)/sizeof(Data[0]) ; ++i)
    {
        printf(Data[i].Format, Data[i].Value);
        printf("\n");
    }
    return 0;
}
