/* Copyright (C) 1994, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/* Last modified on Tue Dec 27 08:09:08 PST 1994 by kalsow     */
/*      modified on Thu Nov  3 15:08:20 PST 1994 by isard      */

#include "windows.h"
#include "stdio.h"
#include "memory.h"

#define MAX_COMMBLOCK (0x20000)

#define FILEMAP_SIZE (0x100000)

#ifdef M3STUBGUI
typedef int (*Proc)(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    LPSTR lpszCmdLine, int nCmdShow);
#else
typedef int (*Proc)(int argc, char *argv[], char *envp[]);
#endif

typedef VOID (WINAPI *exitproctype)(UINT code);

exitproctype exit_proc;

typedef struct {
  char   *start;
  DWORD  size;
  char   name[64];
} MemStruct;

typedef struct {
  MemStruct mem[3];
  DWORD     call;
} CallStruct;

void WINAPI fake_exit(UINT code) {
  SetFocus(NULL);
  DebugBreak();
  exit_proc(code);
}

#ifdef M3STUBGUI
void crash_stub(char *msg) {
  int *foo;
  MessageBox(NULL, msg, "STUB ERROR", MB_OK);
  foo = (int *) -99;
  *foo = -1;
}
#else
void crash_stub(char *msg) {
  int *foo;
  printf("STUB CRASH: %s\n", msg);
  foo = (int *) -99;
  *foo = -1;
}
#endif

void map_in_files(MemStruct mem, DWORD access) {
  int nfiles, i;

  nfiles = (mem.size + FILEMAP_SIZE - 1)/FILEMAP_SIZE;
  for (i=0; i<nfiles; i++) {
    int size;
    char *start;
    char manglename[64];
    HANDLE filemap;

    sprintf(manglename, "%s%d", mem.name, i);
    filemap = OpenFileMapping(access, 0, manglename);
    if (filemap == NULL) {
      crash_stub("file map open failed");
    }

    if (i < nfiles-1) size = FILEMAP_SIZE;
    else size = mem.size - (FILEMAP_SIZE * (nfiles-1));

    start = mem.start + FILEMAP_SIZE * i;

    if (MapViewOfFileEx(filemap, access, 0, 0, size, start)
        != start) {
      crash_stub("file mapping failed");
    }
  }
}

#ifdef M3STUBGUI
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                   LPSTR lpszCmdLine, int nCmdShow) {
#else
int main(int argc, char *argv[], char *envp[]) {
#endif
  int *commblock, *intptr, nlibs, nsyms, i, j;
  char commaddr[11], *ptr;
  FARPROC *addrptr, addr;
  HINSTANCE dllhandle;
  Proc proc;
  CallStruct *callstruct;

  commblock = VirtualAlloc(NULL, MAX_COMMBLOCK, MEM_COMMIT, PAGE_READWRITE);

  if (commblock == NULL) {
    crash_stub("commblock=0");
  }

  j = (int) commblock;
  for (i=9; i>=0; i--) {
    commaddr[i] = (j % 10) + '0';
    j /= 10;
  }

  commaddr[10] = 0;

  *commblock = -1;

  OutputDebugString(commaddr);

  while (*commblock == -1);

  nlibs = commblock[0];
  ptr = (char *) (commblock+1);
  addrptr = (FARPROC *) (commblock+1);

  for (i=0; i<nlibs; i++) {
    dllhandle = LoadLibrary(ptr);

    if (dllhandle == NULL) {
      crash_stub("dllhandle=0");
    }

    while (*(ptr++) != 0);

    nsyms = *(intptr = (int *) ptr);

    ptr += 4;

    for (j=0; j<nsyms; j++) {
      if (*ptr == 0) {
	addr = GetProcAddress(dllhandle, (char *) *(intptr = (int *) (ptr+1)));
	if (addr == NULL) {
	  crash_stub("dll ordinal not found");
	}
	ptr += 5;
      } else {
        if (strcmp(ptr, "ExitProcess") == 0) {
	  addr = (FARPROC) fake_exit;
	} else {
	  addr = GetProcAddress(dllhandle, ptr);
	  if (addr == NULL) {
	    crash_stub("dll name not found");
	  }
	}
	while (*(ptr++) != 0);
      }

      if ((char *) (addrptr+1) >= ptr) {
	crash_stub("names too long");
      }

      *(addrptr++) = addr;
    }
  }

  *commblock = -1;

  OutputDebugString(commaddr);

  while (*commblock == -1);

  callstruct = (CallStruct *) commblock;

  if (VirtualAlloc(callstruct->mem[2].start, 
                   callstruct->mem[2].size,
		   MEM_RESERVE, PAGE_READWRITE)
        == NULL) {
    crash_stub("bss reserve failed");
  }

  if (VirtualAlloc(callstruct->mem[2].start, 
                   callstruct->mem[2].size,
		   MEM_COMMIT, PAGE_READWRITE)
        == NULL) {
    crash_stub("bss allocation failed");
  }

  map_in_files(callstruct->mem[0], FILE_MAP_READ);
  map_in_files(callstruct->mem[1], FILE_MAP_ALL_ACCESS);

  proc = (Proc) callstruct->call;

#ifdef M3STUBGUI
  return (*proc) (hInstance, hPrevInstance, lpszCmdLine, nCmdShow);
#else
  return (*proc) (argc, argv, envp);
#endif
}
