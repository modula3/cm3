#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/file.h>
#include <process.h>
#include <sys/socket.h>

int main()
{
    unsigned i;
    const static struct
    {
        const char* Format;
        unsigned Value;
    } Data[] =
{
"(* Copyright (C) 1993, Digital Equipment Corporation                  *)", 0,
"(* All rights reserved.                                               *)", 0,
"(* See the file COPYRIGHT for a full description.                     *)", 0,
"", 0,
"INTERFACE Unix;", 0,
"", 0,
"FROM Ctypes IMPORT short, int, long, const_char_star, char_star, char_star_star;", 0,
"FROM Utypes IMPORT off_t, size_t, pid_t;", 0,
"FROM Utime IMPORT struct_timeval;", 0,
"", 0,
"CONST", 0,
"  readEnd = 0;", 0,
"  writeEnd = 1;", 0,
"  MaxPathLen = 1024;", 0,
"  MSETUID = 0;", 0,
"  MSETGID = 0;", 0,
"  MSTICKY = 0;", 0,
"  MROWNER = 8_0400;", S_IRUSR,
"  MWOWNER = 8_0200;", S_IWUSR,
"  MXOWNER = 8_0100;", S_IXUSR,
"  MRGROUP = 8_0040;", S_IRGRP,
"  MWGROUP = 8_0020;", S_IWGRP,
"  MXGROUP = 8_0010;", S_IXGRP,
"  MROTHER = 8_0004;", S_IROTH,
"  MWOTHER = 8_0002;", S_IWOTH,
"  MXOTHER = 8_0001;", S_IXOTH,
"  Mrwrwrw = MROWNER + MWOWNER + MRGROUP + MWGROUP + MROTHER + MWOTHER;", 0,
"  F_OK = 16_%x;", F_OK,
"  X_OK = 16_%x;", X_OK,
"  W_OK = 16_%x;", W_OK,
"  R_OK = 16_%x;", R_OK,
"  P_NOWAIT = 16_%x;", _P_NOWAIT,
"  F_SETFD = 16_%x;", F_SETFD,
"  F_GETFL = 16_%x;", F_GETFL,
"  F_SETFL = 16_%x;", F_SETFL,
"  FIONREAD = 16_%x;", FIONREAD,
"  O_RDONLY = 16_%x;", O_RDONLY,
"  O_RDWR = 16_%x;", O_RDWR,
"  O_CREAT = 16_%x;", O_CREAT,
"  O_EXCL = 16_%x;", O_EXCL,
"  O_TRUNC = 16_%x;", O_TRUNC,
"  O_NDELAY = 16_%x;", O_NDELAY,
"  M3_NONBLOCK = 16_%x;", O_NONBLOCK,
"", 0,
"<*EXTERNAL*> PROCEDURE access (path: const_char_star; mod: int): int;", 0,
"<*EXTERNAL*> PROCEDURE chdir (path: const_char_star): int;", 0,
"<*EXTERNAL*> PROCEDURE close (d: int): int;", 0,
"<*EXTERNAL*> PROCEDURE dup2 (oldd, newd: int): int;", 0,
"<*EXTERNAL*> PROCEDURE execve (name: const_char_star;  argv, envp: char_star_star): int;", 0,
"<*EXTERNAL*> PROCEDURE spawnve (mode: int; name: const_char_star; argv, envp: char_star_star): int;", 0,
"<*EXTERNAL*> PROCEDURE exit (i: int);", 0,
"<*EXTERNAL \"_exit\"*> PROCEDURE underscore_exit (i: int);", 0,
"<*EXTERNAL*> PROCEDURE fcntl (fd, request, arg: int): int;", 0,
"<*EXTERNAL*> PROCEDURE fsync (fd: int): int;", 0,
"<*EXTERNAL*> PROCEDURE getdtablesize (): int;", 0,
"<*EXTERNAL*> PROCEDURE gethostname (name: char_star; namelen: int): int;", 0,
"<*EXTERNAL*> PROCEDURE getpagesize (): int;", 0,
"<*EXTERNAL*> PROCEDURE getcwd (pathname: char_star; size: size_t): char_star;", 0,
"<*EXTERNAL*> PROCEDURE ioctl (d, request: int; argp: ADDRESS): int;", 0,
"<*EXTERNAL*> PROCEDURE lseek (d: int; offset: off_t; whence: int): off_t;", 0,
"<*EXTERNAL*> PROCEDURE mkdir (path: const_char_star; mode: int): int;", 0,
"<*EXTERNAL*> PROCEDURE open (name: const_char_star; flags, mode: int): int;", 0,
"<*EXTERNAL*> PROCEDURE creat (name: const_char_star; mode: int): int;", 0,
"<*EXTERNAL*> PROCEDURE pipe (VAR fildes: ARRAY [0..1] OF int): int;", 0,
"<*EXTERNAL*> PROCEDURE readlink (path: const_char_star; buf: ADDRESS; bufsize: int): int;", 0,
"<*EXTERNAL*> PROCEDURE rename (from, to: const_char_star): int;", 0,
"<*EXTERNAL*> PROCEDURE rmdir (path: const_char_star): int;", 0,
"<*EXTERNAL*> PROCEDURE symlink (name1, name2: const_char_star): int;", 0,
"<*EXTERNAL*> PROCEDURE ftruncate (fd: int; length: off_t): int;", 0,
"<*EXTERNAL*> PROCEDURE unlink (path: const_char_star): int;", 0,
"<*EXTERNAL*> PROCEDURE utimes (file: const_char_star; tvp: UNTRACED REF ARRAY [0..1] OF struct_timeval): int;", 0,
"<*EXTERNAL*> PROCEDURE vfork (): int;", 0,
"", 0,
"END Unix.", 0,
"", 0,
};
    for (i = 0 ; i != sizeof(Data)/sizeof(Data[0]) ; ++i)
    {
        printf(Data[i].Format, Data[i].Value);
        printf("\n");
    }
    return 0;
}
