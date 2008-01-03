/* Copyright (C) 1994, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/* Last modified on Thu Feb 23 10:13:34 PST 1995 by kalsow     */

#include <stdio.h>
#include "SRCstdlib.h"
#include "inttable.h"

typedef struct chainnode {
  struct chainnode *next;
  int key;     
  IntTableDatum data;
} ChainNode;

typedef ChainNode *PtrChainNode;

typedef struct tablerec {
  PtrChainNode *a;
  int aNumber;  /*of elements in the array a*/
} TableRec;

typedef TableRec *_PrivateIntTable;
#undef IntTable
#define IntTable _PrivateIntTable

#define HashMult -1664117991
  /*This is Round(.6125423371 * 2^32), casted to an int.  It's
    copied from TextTable.mod, which got it from Knuth*/

#define IntHash(key, n)  (abs((key)*HashMult) % (n))

#define CheckNonNull(x)  { if ((x) == NULL) abort(); }
#define Assert(b)  { if (!(b)) abort(); }

_inttablepublic IntTableNew(nelems)
int nelems;
{
IntTable t;
int sz;
if (nelems <= 0) abort();
t = (IntTable) getmem(sizeof(TableRec));
sz = nelems * sizeof(PtrChainNode);
t->a = (PtrChainNode*) getmem(sz);
bzero((char*)t->a, sz);
t->aNumber = nelems;
return (_inttablepublic) t;
} /*IntTableNew*/

int IntTableGet(t, key, data)
IntTable t;
int key;
IntTableDatum *data;
{
int h;
PtrChainNode p;
CheckNonNull(t);
CheckNonNull(data);
h = IntHash(key, t->aNumber);
p = t->a[h];
while (p != NULL) {
  if (key == p->key) {
    *data = p->data;
    return TRUE;
  } /*if*/
  p = p->next;
} /*while*/
return FALSE;
} /*IntTableGet*/

int IntTablePut(t, key, data)
IntTable t;
int key;
IntTableDatum data;
{
int h;
PtrChainNode p;
CheckNonNull(t);
h = IntHash(key, t->aNumber);
p = t->a[h];
while (p != NULL) {
  if (key == p->key) {
    p->data = data;
    return TRUE;
  } /*if*/
  p = p->next;
} /*while*/
p = (PtrChainNode) getmem(sizeof(ChainNode));
p->key = key;
p->data = data;
p->next = t->a[h];
t->a[h] = p;
return FALSE;
} /*IntTablePut*/

int IntTableDelete(t, key, data)
IntTable t;
int key;
IntTableDatum *data;
{
int h;
PtrChainNode p, prev;
CheckNonNull(t);
CheckNonNull(data);
h = IntHash(key, t->aNumber);
p = t->a[h];
prev = NULL;
while (p != NULL) {
  if (key == p->key) {
    *data = p->data;
    if (prev == NULL) {
      /*First node in chain*/
      t->a[h] = p->next;
    } else {
      prev->next = p->next;      
    } /*if*/
    free(p);
    return TRUE;
  } /*if*/
  prev = p;
  p = p->next;
} /*while*/
return FALSE;
} /*IntTableDelete*/

void IntTableFree(t)
IntTable t; 
{
int j;
PtrChainNode p, prev;
if (t == NULL) return;
Assert(t->aNumber > 0);
Assert(t->a != NULL);
for (j = 0; j < t->aNumber; j++) {
  p = t->a[j];
  while (p != NULL) {
    prev = p;
    p = p->next;
    free(prev);
  } /*while*/
} /*for*/
free(t->a);
t->aNumber = 0;
t->a = NULL;
free(t);
} /*IntTableFree*/

/*
Sketch of enumerator facility.  Legal to call Put at any point
in enumeration.  Only call of Delete that is legal is to
delete the key that was returned by the most recent call
on EnumNext.  Invariant is that the enumerator is guaranteed
to visit all keys that were present in the table at all times
during the enumeration, but keys that were added/deleted during
the enumeration may or may not be visited.

typedef struct _enum {
  int pos;
  PtChainNode next;
} Enum;

EnumInit(t,e)
{
e->pos = -1;
e->next = NULL;
}

bool EnumNext(t, e, key, datum)
returns false for end of enumeration
{
if (e->next == NULL) {
  e->pos++;
  while((e->pos < t->aNumber) && (t->a[e->pos] == NULL)) e->pos++;
  if (e->pos >= t->aNumber) return FALSE;
  cur = t->a[e->pos];
} else {
  cur = e->next;  
}
*key = 
*datum =
next = cur->next;
return TRUE;
}

*/

