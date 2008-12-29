/*
Build this with and without #include "../Common/m3unix.h"
and verify the results match.
*/
#if 0
#include "../Common/m3unix.h"
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <netdb.h>

const char* PickType(const unsigned char&) { return "uint8_t"; }
const char* PickType(const unsigned short&) { return "uint16_t"; }
const char* PickType(const unsigned&) { return "uint32_t"; }
const char* PickType(const unsigned long&) { return "uint32_t"; }
const char* PickType(const unsigned long long&) { return "uint64_t"; }

const char* PickType(const char&) { return "int8_t"; }
const char* PickType(const short&) { return "int16_t"; }
const char* PickType(const int&) { return "int32_t"; }
const char* PickType(const long&) { return "int32_t"; }
const char* PickType(const long long&) { return "int64_t"; }

#define FieldType(struc,field) PickType(((struc*)0)->field)

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
"", 0,
"(* This file was generated from " __FILE__ ". Do not edit it. *)", 0,
"", 0,
"INTERFACE Utypes;", 0,
"", 0,
"IMPORT Ctypes;", 0,
"", 0,
"TYPE", 0,
"  uint8_t = Ctypes.unsigned_char;", 0,
"  uint16_t = Ctypes.unsigned_short;", 0,
"  uint32_t = Ctypes.unsigned_int;", 0,
"  uint64_t = Ctypes.unsigned_long_long;", 0,
"  int8_t = Ctypes.char;", 0,
"  int16_t = Ctypes.short;", 0,
"  int32_t = Ctypes.int;", 0,
"  int64_t = Ctypes.long_long;", 0,
"  u_char = uint8_t;", 0,
"  u_short = uint16_t;", 0,
"  u_int = uint32_t;", 0,
"  u_long = uint32_t;", 0,
"  size_t = %s;", PickType(size_t()),
"  time_t = %s;", PickType(time_t()),
"  off_t = %s;", PickType(off_t()),
"  clock_t = %s;", PickType(clock_t()),
"  uid_t = %s;", PickType(uid_t()),
"  pid_t = %s;", PickType(pid_t()),
"  gid_t = %s;", PickType(gid_t()),
"", 0,
"  socklen_t = int32_t;", 0,
"  hostent_addrtype_t = %s;", FieldType(struct hostent, h_addrtype),
"  hostent_length_t = %s;", FieldType(struct hostent, h_length),
"", 0,
"  struct_timespec = RECORD", 0,
"    tv_sec  : %s;", FieldType(struct timespec, tv_sec),
"    tv_nsec : %s;", FieldType(struct timespec, tv_nsec),
"  END;", 0,
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
