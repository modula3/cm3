/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#include "m3unix.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32

/* direct references to opendir on NetBSD:

$ cat 1.c
#include <stdio.h>
void opendir(void);
int main()
{ printf("%p\n", &opendir);
  return 0;
}
$ cc 1.c
/var/tmp//ccCjD8oF.o: In function `main':
1.c:(.text+0x15): warning: warning: reference to compatibility opendir(); include <dirent.h> for correct reference

*/

#define M3MODULE Udir
M3WRAP1(DIR*, opendir, const char*)
M3WRAP1(void*, readdir, DIR*)
M3WRAP1(int, closedir, DIR*)

#endif

#ifdef __cplusplus
}
#endif
