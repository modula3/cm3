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

DIR* Udir__opendir(const char* a)
{
    return opendir(a);
}

void* Udir__readdir(DIR* a)
{
    return readdir(a);
}

int Udir__closedir(DIR* a)
{
    return closedir(a);
}


#endif


#ifdef __cplusplus
}
#endif
