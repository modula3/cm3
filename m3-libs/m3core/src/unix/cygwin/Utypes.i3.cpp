/* $Id: Utypes.i3.cpp,v 1.8 2008-02-17 09:05:58 jkrell Exp $ */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

const char* PickType(const unsigned char&) { return "unsigned_char"; }
const char* PickType(const unsigned short&) { return "unsigned_short"; }
const char* PickType(const unsigned&) { return "unsigned_int"; }
const char* PickType(const unsigned long&) { return "unsigned_long"; }
const char* PickType(const int&) { return "int"; }
const char* PickType(const long&) { return "long"; }
const char* PickType(const unsigned long long&) { return "unsigned_long_long"; }
const char* PickType(const long long&) { return "long_long"; }

#define FieldType(struc,field) PickType(((struc*)0)->field)

const char* PickType(const unsigned char&);
const char* PickType(const char&);
const char* PickType(const short&);

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
" * Basic system types.", 0,
" *)", 0,
"", 0,
"TYPE", 0,
"", 0,
"  u_char = unsigned_char;", 0,
"  u_short = unsigned_short;", 0,
"  u_int = unsigned_int;", 0,
"  u_long = unsigned_long;", 0,
"  ino_t        = %s; (* inode -- the same for multiple hard links to the same file *)", FieldType(struct stat, st_ino),
"  size_t       = %s;", PickType(size_t()),
"  time_t       = %s;", PickType(time_t()),
"  dev_t        = %s; (* device *) ", FieldType(struct stat, st_dev),
"  off_t        = %s; (* file size or offset *)", PickType(off_t()),
"  clock_t      = %s;", PickType(clock_t()),
"  mode_t       = %s; (* mode of a file *)", FieldType(struct stat, st_mode),
"  nlink_t      = %s; (* number of links to a file *)", FieldType(struct stat, st_nlink),
"  uid_t        = %s; (* user id *)", PickType(uid_t()),
"  pid_t        = %s; (* process id *)", PickType(pid_t()),
"  gid_t        = %s; (* group id *)", PickType(gid_t()),
"", 0,
"  (* for struct stat *)", 0,
"  blkcnt_t     = %s;", FieldType(struct stat, st_blocks),
"  blksize_t     = %s;", FieldType(struct stat, st_blksize),
"", 0,
"  struct_timespec = RECORD", 0,
"    tv_sec  : %s; (* Seconds *)", FieldType(struct timespec, tv_sec),
"    tv_nsec : %s; (* Nanoseconds *)", FieldType(struct timespec, tv_nsec),
"  END;", 0,
"", 0,
"  timestruc_t = struct_timespec;",0,
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
