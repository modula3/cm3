/* Copyright (C) 1994, Digital Equipment Corporation            */
/* All rights reserved.                                         */
/* See the file COPYRIGHT for a full description.               */
/*                                                              */
/* Last modified on Thu Feb 23 10:14:37 PST 1995 by kalsow      */
/*      modified on Fri May  6 14:13:21 PDT 1994 by wobber      */
/*      modified on Fri Mar 20 18:02:54 1992 by hisgen          */

#include <stdio.h>
#include "SRCstdlib.h"
#include "varrayptr.h"

#define Assert(b)  { if (!(b)) abort(); }
#define CheckNonNull(x)  { if ((x) == NULL) abort(); }

void VAPtrInit(va, nelems)
VArrayPtr *va;
int nelems;
{
int sz;
CheckNonNull(va);
Assert(nelems >= 0);
va->numAllocated = nelems;
va->highestIndex = -1;
if (nelems > 0) {
  sz = nelems * sizeof(char *);
  va->a = (char **) getmem(sz);
  bzero((char*)va->a, sz);
} else {
  va->a = NULL;  
}
} /*VAPtrInit*/

char *VAPtrGet(va, index)
VArrayPtr *va;
int index;
{
CheckNonNull(va);
if ((index < 0) || (index >= va->numAllocated)) abort();
return va->a[index];
} /*VAPtrGet*/

void VAPtrSet(va, index, value)
VArrayPtr *va;
int index;
char *value;
{
int n, sz, oldsz;
char **a;
CheckNonNull(va);
if (index < 0) abort();
if (index >= va->numAllocated) {
  if (va->numAllocated < 0) abort();  /*must have had memory smash earlier*/
  oldsz = va->numAllocated*sizeof(char *);
  if (index != 0) n = 2 * index;
  else n = 2;
  sz = n * sizeof(char *);
  a = (char **) getmem(sz);
  if (oldsz > 0) bcopy(/*from*/ (char*)va->a, /*to*/ (char*)a, oldsz);
  bzero((char*)&a[va->numAllocated], sz - oldsz);
  if (va->a != NULL) free(va->a);
  va->a = a;
  va->numAllocated = n;
}
if (index > va->highestIndex) va->highestIndex = index;
va->a[index] = value;
} /*VAPtrSet*/

int VAPtrHighIndex(va)
VArrayPtr *va;
{
CheckNonNull(va);
return va->highestIndex;
} /*VAPtrHighIndex*/

void VAPtrFree(va)
VArrayPtr *va;
{
CheckNonNull(va);
if (va->a != NULL) free(va->a);
va->a = NULL;
va->numAllocated = 0;
va->highestIndex = -1;
} /*VAPtrFree*/  

