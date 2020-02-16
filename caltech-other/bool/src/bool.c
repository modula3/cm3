/*************************************************************************
 *
 *  (c) 1995 California Institute of Technology
 *  Department of Computer Science
 *  Pasadena, CA 91125.
 *  All Rights Reserved
 *
 *  Author: Rajit Manohar
 *  Date: Sat Mar 25 14:10:06 PST 1995
 *  Info: an implementation of bdds
 *
 *  Updated: Sun Mar 26 16:00:32 PST 1995
 *  Update Info: added caching temporary tables across procedure calls.
 *
 *  $Id$
 *
 *************************************************************************/

#include <assert.h>
#include <stdio.h>

#define BOOL_INTERNAL_H

#include "bool.h"
#include "misc.h"

#define PTR_TO_INT(b) ((unsigned long)(b))
#define PMIN(a,b) ((PTR_TO_INT(a) < PTR_TO_INT(b)) ? a : b)
#define PMAX(a,b) ((PTR_TO_INT(a) > PTR_TO_INT(b)) ? a : b)

static bool_t *freelist = NULL;
#ifdef PROFILE
static unsigned long nfreed = 0;
static unsigned long nbools = 0;
static unsigned long extrabools = 0;
#endif

static bool_t *newbool (void)
{
  bool_t *b;
  int i;

  if (freelist == NULL) {
    MALLOC(b,bool_t,1024);
    freelist = b;
    for (i=0; i < 1023; i++) {
      CLEAR(b+i);
      (b+i)->next = b+i+1;
    }
    CLEAR(b+1023);
    (b+1023)->next = NULL;
  }
  b = freelist;
  freelist = freelist->next;
  b->next = NULL;
#ifdef PROFILE
  nbools++;
#endif
  return b;
}


/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  My own hash functions.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
static unsigned char T[] = {
  1, 87, 49, 12,176,178,102,166,121,193,  6, 84,249,230, 44,163,
  14,197,213,181,161, 85,218, 80, 64,239, 24,226,236,142, 38,200,
 110,177,104,103,141,253,255, 50, 77,101, 81, 18, 45, 96, 31,222,
  25,107,190, 70, 86,237,240, 34, 72,242, 20,214,244,227,149,235,
  97,234, 57, 22, 60,250, 82,175,208,  5,127,199,111, 62,135,248,
 174,169,211, 58, 66,154,106,195,245,171, 17,187,182,179,  0,243,
 132, 56,148, 75,128,133,158,100,130,126, 91, 13,153,246,216,219,
 119, 68,223, 78, 83, 88,201, 99,122, 11, 92, 32,136,114, 52, 10,
 138, 30, 48,183,156, 35, 61, 26,143, 74,251, 94,129,162, 63,152,
 170,  7,115,167,241,206,  3,150, 55, 59,151,220, 90, 53, 23,131,
 125,173, 15,238, 79, 95, 89, 16,105,137,225,224,217,160, 37,123,
 118, 73,  2,157, 46,116,  9,145,134,228,207,212,202,215, 69,229,
  27,188, 67,124,168,252, 42,  4, 29,108, 21,247, 19,205, 39,203,
 233, 40,186,147,198,192,155, 33,164,191, 98,204,165,180,117, 76,
 140, 36,210,172, 41, 54,159,  8,185,232,113,196,231, 47,146,120,
  51, 65, 28,144,254,221, 93,189,194,139,112, 43, 71,109,184,209
};

static triplehash_t *thash_new (unsigned long sz)
{
  triplehash_t *t;
  unsigned long i;

  MALLOC(t,triplehash_t,1);
  t->nelements = 0;
  t->nbuckets = sz;
  MALLOC(t->bucket,struct triple *,sz);
  for (i=0;i<sz;i++) t->bucket[i] = NULL;
  return t;
}

static pairhash_t *hash_new (unsigned long sz)
{
  pairhash_t *p;
  unsigned long i;

  MALLOC(p,pairhash_t,1);
  p->nelements = 0;
  p->nbuckets = sz;
  MALLOC(p->bucket,bool_t*,p->nbuckets);
  for (i=0;i<p->nbuckets;i++) p->bucket[i] = NULL;
  return p;
}

static unsigned long hash (unsigned long sz, bool_t *v1, bool_t *v2)
{
  int i;
  unsigned char h1, h2, h3, h4;
  unsigned long val;
  unsigned char *c;

  if (sz <= (1<<8)) {
    c = (unsigned char *) &v1;
    h1 = T[*c++];
    for (i=1; i < sizeof(void*); i++,c++)
      h1 = T[h1 ^ *c];
    c = (unsigned char *) &v2;
    for (i=0; i < sizeof(void*); i++,c++)
      h1 = T[h1 ^ *c];
    val = h1;
  }
  else if (sz <= (1<<16)) {
    c = (unsigned char *) &v1;
    h1 = T[*c];
    h2 = T[ 0xff & (1+*c++) ];
    for (i=1; i < sizeof(void*); i++, c++)
      h1 = T[h1^*c], h2=T[h2^*c];
    c = (unsigned char *) &v2;
    for (i=0; i < sizeof(void*); i++, c++)
      h1 = T[h1^*c], h2=T[h2^*c];
    val = h1 | (h2 << 8);
  }
  else if (sz <= (1<<24)) {
    c = (unsigned char *) &v1;
    h1 = T[*c];
    h2 = T[ 0xff & (1+*c) ];
    h3 = T[ 0xff & (2+*c++) ];
    for (i=1; i < sizeof(void*); i++, c++)
      h1 = T[h1^*c], h2=T[h2^*c], h3=T[h3^*c];
    c = (unsigned char *) &v2;
    for (i=0; i < sizeof(void*); i++, c++)
      h1 = T[h1^*c], h2=T[h2^*c], h3=T[h3^*c];
    val = h1 | (h2 << 8) | (h3 << 16);
  }
  else {
    c = (unsigned char *) &v1;
    h1 = T[*c];
    h2 = T[ 0xff & (1+*c) ];
    h3 = T[ 0xff & (2+*c) ];
    h4 = T[ 0xff & (3+*c++) ];
    for (i=1; i < sizeof(void*); i++, c++)
      h1 = T[h1^*c], h2=T[h2^*c], h3=T[h3^*c], h4=T[h4^*c];
    c = (unsigned char *) &v2;
    for (i=0; i < sizeof(void*); i++, c++)
      h1 = T[h1^*c], h2=T[h2^*c], h3=T[h3^*c], h4=T[h4^*c];
    val = h1 | (h2 << 8) | (h3 << 16) | (h4 << 24);
  }
  return val % sz;
}

static void doublethashtable (triplehash_t *t)
{
  struct triple **r;
  struct triple *p, *u;
  unsigned long i, j;

  MALLOC(r,struct triple *,(t->nbuckets<<1));
  t->nbuckets <<=1;
  for (i=0; i < t->nbuckets; i++)
    r[i] = NULL;
  for (i=0; i < t->nbuckets/2; i++)
    for (p=t->bucket[i]; p; ) {
      j = hash (t->nbuckets, p->v1, p->v2);
      u = p;
      p = p->next;
      u->next = r[j];
      r[j] = u;
    }
  FREE (t->bucket);
  t->bucket = r;
}

static void doublehashtable (pairhash_t *p)
{
  bool_t **r;
  bool_t *t, *u;
  unsigned long i, j;

  MALLOC(r,bool_t*,(p->nbuckets<<1));
  p->nbuckets <<= 1;
  for (i=0; i < p->nbuckets; i++)
    r[i] = NULL;

  for (i=0; i < p->nbuckets/2; i++)
    for (t=p->bucket[i]; t; ) {
      j = hash (p->nbuckets, t->l, t->r);
      u = t;
      t = t->next;
      u->next = r[j];
      r[j] = u;
    }
  FREE (p->bucket);
  p->bucket = r;
}

static bool_t *thash_locate (triplehash_t *t, bool_t *v1, bool_t *v2)
{
  unsigned long i;
  struct triple *v;

  i = hash (t->nbuckets, v1, v2);
  for (v = t->bucket[i]; v; v = v->next)
    if (v->v1 == v1 && v->v2 == v2)
      return v->v3;
  return NULL;
}

static bool_t *hash_locate (pairhash_t *p, bool_t *v1, bool_t *v2)
{
  unsigned long i;
  bool_t *b;
#ifdef PROFILE
  extrabools++;
#endif
  i = hash (p->nbuckets, v1, v2);
  for (b = p->bucket[i]; b; b = b->next)
    if (b->l == v1 && b->r == v2)
      return b;
#ifdef PROFILE
  extrabools--;
#endif
  return NULL;
}

static void thash_insert (triplehash_t *t, bool_t *v1, bool_t *v2, bool_t *v3)
{
  unsigned long i;
  struct triple *tr;

  t->nelements++;
  if ((t->nelements > (t->nbuckets<<2)) && (t->nbuckets<<2) > t->nbuckets)
    doublethashtable (t);
  i = hash (t->nbuckets, v1, v2);
  MALLOC(tr,struct triple,1);
  tr->v1 = v1; tr->v2 = v2; tr->v3 = v3;
  INC_REF (v1); INC_REF (v2); INC_REF (v3);
  tr->next = t->bucket[i];
  t->bucket[i] = tr;
}

static void hash_insert (pairhash_t *p, bool_t *b)
{
  unsigned long i;

  p->nelements ++;
  if ((p->nelements > (p->nbuckets<<2)) && ((p->nbuckets<<2) > p->nbuckets))
    doublehashtable (p);
  i = hash (p->nbuckets, b->l, b->r);
  b->next = p->bucket[i];
  p->bucket[i] = b;
}

static void hash_delete (pairhash_t *p, bool_t *b0)
{
  unsigned long i;
  bool_t *prev, *b;

  i = hash (p->nbuckets, b0->l, b0->r);
  prev = NULL;
  for (b = p->bucket[i]; b; b = b->next) {
    if (b == b0) {
      if (prev == NULL)
	p->bucket[i] = p->bucket[i]->next;
      else
	prev->next = b->next;
      break;
    }
    prev = b;
  }
  p->nelements--;
}

#if 0
static void hash_free (pairhash_t *p)
{
  FREE (p->bucket);
  FREE (p);
}
#endif

static void thash_free (BOOL_T *B, triplehash_t *t)
{
  unsigned long i;
  struct triple *p, *u;

  for (i=0; i < t->nbuckets; i++)
    for (p=t->bucket[i]; p; ) {
      u = p;
      p = p->next;
      bool_free (B, u->v1);
      bool_free (B, u->v2);
      bool_free (B, u->v3);
      FREE (u);
    }
  FREE (t->bucket);
  FREE (t);
}


/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/


/*-------------------------------------------------------------------------
 * return a new BOOL_T structure
 *-----------------------------------------------------------------------*/
extern BOOL_T *bool_init (void)
{
  BOOL_T *B;
  int i;

  MALLOC(B,BOOL_T,1);
  B->roots = NULL;
  B->nvar = 0;
  B->totvar = VAR_BLOCK;
  MALLOC(B->H, pairhash_t *, VAR_BLOCK);
  for (i=0; i < VAR_BLOCK; i++)
    B->H[i] = hash_new (HASH_BLOCK);
  for (i=0; i < BOOL_MAXOP; i++)
    B->TH[i] = thash_new (128);

  B->true = newbool ();
  ASSIGN_LEAF(B->true,1);
  B->true->id = 1;
  SET_HIBIT (B->true->id);

  B->false = newbool();
  ASSIGN_LEAF(B->false,1);
  B->false->id = 0;
  SET_HIBIT (B->false->id);

  return B;
}

/*-------------------------------------------------------------------------
 * return the bdd for "true"
 *-----------------------------------------------------------------------*/
extern bool_t *bool_true (BOOL_T *B)
{
  INC_REF(B->true);
  return B->true;
}

/*-------------------------------------------------------------------------
 * return the bdd for "false"
 *-----------------------------------------------------------------------*/
extern bool_t *bool_false (BOOL_T *B)
{
  INC_REF (B->false);
  return B->false;
}

/*-------------------------------------------------------------------------
 * return the bdd for a new variable
 *-----------------------------------------------------------------------*/
extern bool_t *bool_newvar (BOOL_T *B)
{
  bool_t *b;

  if (B->nvar >= BOOL_MAXVAR) {
    fprintf (stderr, "Exceeded limit on # of variables in bool library\n");
    return NULL;
  }
  if (B->nvar == B->totvar) {
    int i;
    B->totvar += VAR_BLOCK;
    REALLOC(B->H, pairhash_t *, B->totvar);
    for (i=B->totvar-VAR_BLOCK; i < B->totvar; i++)
      B->H[i] = hash_new (HASH_BLOCK);
  }

  /* increment refcounts for "true", "false" */
  b = newbool();		/* create a new node. */
  b->id = B->nvar;		/* set its id */
  ASSIGN_LEAF (b,0);		/* make it a non-leaf node. */
  INC_REF (b);			/* increment ref counts */
  INC_REF (B->true);
  INC_REF (B->false);
  b->l = B->true;
  b->r = B->false;

  /* add to hashtable */
  hash_insert (B->H[B->nvar], b);
  B->nvar ++;
#if 0
  fprintf(stderr,"bool_newvar: id = %d\n", bool_getid(b));
#endif
  return b;
}

/*-------------------------------------------------------------------------
 * return bdd for old variable
 *-----------------------------------------------------------------------*/
extern bool_t *bool_var (BOOL_T *B, bool_var_t v)
{
  bool_t *b;

  if (v >= B->nvar)
    return NULL;
  else {
    b = hash_locate (B->H[v], B->true, B->false);
    if (!b) {
      b = newbool();
      b->id = v;
      b->l = B->true;
      b->r = B->false;
      ASSIGN_LEAF (b,0);	/* make it a non-leaf node. */
      INC_REF (B->true);
      INC_REF (B->false);
      hash_insert (B->H[v], b);
    }
    INC_REF (b);
    return b;
  }
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  Okay, here goes . . .
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

static triplehash_t *pairvisited;
   /* used to store (a,b)->c stuff */

/*========================================================================*/
static bool_t *_bool_and (BOOL_T *B, bool_t *b1, bool_t *b2)
{
  bool_t *l, *r;
  bool_t *b;

  if (b1 == b2) { INC_REF (b1); return b1; } /* & is idempotent */
  if (b1 == B->false) { INC_REF (B->false); return b1; } /* false is a zero */
  if (b2 == B->false) { INC_REF (B->false); return b2; } /* and is symmetric */
  if (b1 == B->true) { INC_REF (b2); return b2; }
  if (b2 == B->true) { INC_REF (b1); return b1; }

  if (b1->id > b2->id) {
    l = b1;
    b1 = b2;
    b2 = l;
  }
  if ((b = thash_locate (pairvisited, PMIN(b1,b2), PMAX(b1,b2)))) {
    INC_REF(b);
    return b;
  }
  if (b1->id == b2->id) {
      l = _bool_and (B, b1->l, b2->l);
      r = _bool_and (B, b1->r, b2->r);
  }
  else {
      l = _bool_and (B, b1->l, b2);
      r = _bool_and (B, b1->r, b2);
  }
  if (l == r) {
    DEC_REF (r);
    thash_insert (pairvisited,PMIN(b1,b2),PMAX(b1,b2),l);
    return l;
  }
  if ((b = hash_locate (B->H[b1->id], l, r))) {
    DEC_REF (l); DEC_REF (r);
    INC_REF (b);
    thash_insert (pairvisited,PMIN(b1,b2),PMAX(b1,b2),b);
    return b;
  }
  else {
    b = newbool();
    ASSIGN_LEAF (b,0);
    b->id = b1->id;
    b->l = l;
    b->r = r;
    hash_insert (B->H[b->id], b);
    INC_REF (b);
    thash_insert (pairvisited,PMIN(b1,b2),PMAX(b1,b2),b);
    return b;
  }
}

/*-------------------------------------------------------------------------
 * AND of two bdds
 *-----------------------------------------------------------------------*/
extern bool_t *bool_and (BOOL_T *B, bool_t *b1, bool_t *b2)
{
  bool_t *b;

  pairvisited = B->TH[BOOL_AND];
  b = _bool_and (B,b1,b2);
  return b;
}

/*========================================================================*/
static bool_t *_bool_or (BOOL_T *B, bool_t *b1, bool_t *b2)
{
  bool_t *l, *r;
  bool_t *b;

  if (b1 == b2) { INC_REF (b1); return b1; } /* or is idempotent */
  if (b1 == B->true) { INC_REF (B->true); return b1; } /* true is a zero */
  if (b2 == B->true) { INC_REF (B->true); return b2; } /* or is symmetric */
  if (b1 == B->false){ INC_REF (b2); return b2; } /* false is an id */
  if (b2 == B->false){ INC_REF (b1); return b1; } /* false is an id */

  if ((b = thash_locate (pairvisited,PMIN(b1,b2),PMAX(b1,b2)))) {
    INC_REF (b);
    return b;
  }

  if (b1->id > b2->id) {
    l = b1;
    b1 = b2;
    b2 = l;
  }
  if (b1->id == b2->id) {
      l = _bool_or (B, b1->l, b2->l);
      r = _bool_or (B, b1->r, b2->r);
  }
  else {
    l = _bool_or (B, b1->l, b2);
    r = _bool_or (B, b1->r, b2);
  }
  if (l == r) {
    DEC_REF (r);
    thash_insert (pairvisited,PMIN(b1,b2),PMAX(b1,b2),r);
    return l;
  }
  if ((b = hash_locate (B->H[b1->id], l, r))) {
    DEC_REF (l); DEC_REF (r);
    INC_REF (b);
    thash_insert (pairvisited,PMIN(b1,b2),PMAX(b1,b2),b);
    return b;
  }
  else {
    b = newbool();
    ASSIGN_LEAF (b,0);
    b->id = b1->id;
    b->l = l;
    b->r = r;
    hash_insert (B->H[b->id], b);
    INC_REF (b);
    thash_insert (pairvisited,PMIN(b1,b2),PMAX(b1,b2),b);
    return b;
  }
}

/*-------------------------------------------------------------------------
 * OR of two bdds
 *-----------------------------------------------------------------------*/
extern bool_t *bool_or (BOOL_T *B, bool_t *b1, bool_t *b2)
{
  bool_t *b;

  pairvisited = B->TH[BOOL_OR];
  b = _bool_or (B,b1,b2);
  return b;
}

/*========================================================================*/
static bool_t *_bool_xor (BOOL_T *B, bool_t *b1, bool_t *b2)
{
  int val;
  bool_t *l, *r;
  bool_t *b;

  assert(b1);
  assert(b2);

  if (b1 == b2) { INC_REF (B->false); return B->false; }
  if (b1 == B->false){ INC_REF (b2); return b2; } /* false is an id */
  if (b2 == B->false){ INC_REF (b1); return b1; } /* false is an id */

  if (ISLEAF(b1) && ISLEAF(b2)) {
    val = (b1->id ^ b2->id);
    if (val) {
      INC_REF (B->true);
      return B->true;
    }
    else {
      INC_REF (B->false);
      return B->false;
    }
  }

  if ((b = thash_locate(pairvisited,PMIN(b1,b2),PMAX(b1,b2)))) {
    INC_REF(b);
    return b;
  }

  if (b1->id > b2->id) {
    l = b1;
    b1 = b2;
    b2 = l;
  }
  if (b1->id == b2->id) {
      l = _bool_xor (B, b1->l, b2->l);
      r = _bool_xor (B, b1->r, b2->r);
  }
  else {
    l = _bool_xor (B, b1->l, b2);
    r = _bool_xor (B, b1->r, b2);
  }
  if (l == r) {
    DEC_REF (r);
    thash_insert (pairvisited,PMIN(b1,b2),PMAX(b1,b2),r);
    return l;
  }
  if ((b = hash_locate (B->H[b1->id], l, r))) {
    DEC_REF (l); DEC_REF (r);
    INC_REF (b);
    thash_insert (pairvisited,PMIN(b1,b2),PMAX(b1,b2),b);
    return b;
  }
  else {
    b = newbool();
    ASSIGN_LEAF (b,0);
    b->id = b1->id;
    b->l = l;
    b->r = r;
    hash_insert (B->H[b->id], b);
    INC_REF (b);
    thash_insert (pairvisited,PMIN(b1,b2),PMAX(b1,b2),b);
    return b;
  }
}

/*-------------------------------------------------------------------------
 * XOR of two bdds
 *-----------------------------------------------------------------------*/
extern bool_t *bool_xor (BOOL_T *B, bool_t *b1, bool_t *b2)
{
  bool_t *b;

  pairvisited = B->TH[BOOL_XOR];
  b = _bool_xor (B,b1,b2);
  return b;
}


/*========================================================================*/
static bool_t *_bool_implies (BOOL_T *B, bool_t *b1, bool_t *b2)
{
  bool_t *l, *r;
  bool_t *b;
  bool_var_t id;

  if (b1 == b2) { INC_REF (B->true); return B->true; }
  if (b1 == B->true){ INC_REF (b2); return b2; }
  if (b1 == B->false){ INC_REF (B->true); return B->true; }
  if (b2 == B->true) { INC_REF (B->true); return B->true; }

  if ((b = thash_locate (pairvisited,b1,b2))) {
    INC_REF(b);
    return b;
  }

  if (b1->id == b2->id) {
    id = b1->id;
    l = _bool_implies (B, b1->l, b2->l);
    r = _bool_implies (B, b1->r, b2->r);
  }
  else if (b1->id > b2->id) {
    id = b2->id;
    l = _bool_implies (B, b1, b2->l);
    r = _bool_implies (B, b1, b2->r);
  }
  else {
    id = b1->id;
    l = _bool_implies (B, b1->l, b2);
    r = _bool_implies (B, b1->r, b2);
  }
  if (l == r) { 
    DEC_REF (r); 
    thash_insert (pairvisited,b1,b2,r);
    return l;
  }
  if ((b = hash_locate (B->H[id], l, r))) {
    DEC_REF (l); DEC_REF (r);
    INC_REF (b);
    thash_insert (pairvisited,b1,b2,b);
    return b;
  }
  else {
    b = newbool();
    ASSIGN_LEAF (b,0);
    b->id = id;
    b->l = l;
    b->r = r;
    hash_insert (B->H[b->id], b);
    INC_REF (b);
    thash_insert (pairvisited,b1,b2,b);
    return b;
  }
}

/*-------------------------------------------------------------------------
 * bdd of (b1 => b2)
 *-----------------------------------------------------------------------*/
extern bool_t *bool_implies (BOOL_T *B, bool_t *b1, bool_t *b2)
{
  bool_t *b;

  pairvisited = B->TH[BOOL_IMPLIES];
  b = _bool_implies (B,b1,b2);
  return b;
}


/*========================================================================*/
static
bool_t *_bool_not (BOOL_T *B, bool_t *b1)
{
  bool_t *b, *l, *r;

  if (b1 == B->true) { INC_REF (B->false); return B->false; }
  if (b1 == B->false) { INC_REF (B->true); return B->true; }

  if ((b = thash_locate(pairvisited,b1,B->true))) {
    INC_REF(b);
    return b;
  }

  l = _bool_not (B, b1->l);
  r = _bool_not (B, b1->r);
  if ((b = hash_locate (B->H[b1->id], l, r))) {
    DEC_REF (l); DEC_REF (r);
    INC_REF (b);
    thash_insert (pairvisited,b1,B->true,b);
    return b;
  }
  else {
    b = newbool();
    ASSIGN_LEAF (b,0);
    b->id = b1->id;
    b->l = l;
    b->r = r;
    hash_insert (B->H[b->id], b);
    INC_REF (b);
    thash_insert (pairvisited,b1,B->true,b);
    return b;
  }
}
  
/*-------------------------------------------------------------------------
 * negation
 *-----------------------------------------------------------------------*/
extern bool_t *bool_not (BOOL_T *B, bool_t *b1)
{
  bool_t *b;

  pairvisited = B->TH[BOOL_NOT];
  b = _bool_not (B, b1);
  return b;
}

/*-------------------------------------------------------------------------
 * copy
 *-----------------------------------------------------------------------*/
extern bool_t *bool_copy (BOOL_T *B, bool_t *b)
{
  INC_REF (b);
  return b;
}

/*========================================================================*/
static bool_t *_bool_maketrue (BOOL_T *B, bool_t *b, bool_t *v)
{
  bool_t *l, *r, *b1;

  if (b->id > v->id) {
    INC_REF (b);
    return b;
  }
  else if (b->id == v->id) {
    INC_REF (b->l);
    return b->l;
  }
  else {
    if ((l = thash_locate (pairvisited, b->l, v))) {
      INC_REF (l);
    }
    else {
      l = _bool_maketrue (B, b->l, v);
    }
    if ((r = thash_locate (pairvisited, b->r, v))) {
      INC_REF (r);
    }
    else {
      r = _bool_maketrue (B, b->r, v);
    }
  }
  if (l == r) {
    DEC_REF (r);
    thash_insert (pairvisited,b,v,l);
    return l;
  }
  if ((b1 = hash_locate (B->H[b->id], l, r))) {
    DEC_REF (l); DEC_REF (r);
    INC_REF (b1);
    thash_insert (pairvisited,b,v,b1);
    return b1;
  }
  else {
    b1 = newbool();
    ASSIGN_LEAF (b1,0);
    b1->id = b->id;
    b1->l = l;
    b1->r = r;
    thash_insert (pairvisited,b,v,b1);
    hash_insert (B->H[b->id], b1);
    INC_REF (b1);
    return b1;
  }
}

/*-------------------------------------------------------------------------
 * make variable "v" true
 *-----------------------------------------------------------------------*/
bool_t *bool_maketrue (BOOL_T *B, bool_t *b1, bool_t *v)
{
  bool_t *b;
  if (v->l != B->true || v->r != B->false)
    return NULL;
  pairvisited = B->TH[BOOL_MKTRUE];
  b = _bool_maketrue (B,b1,v);
  return b;
}

/*========================================================================*/
static bool_t *_bool_makefalse (BOOL_T *B, bool_t *b, bool_t *v)
{
  bool_t *l, *r, *b1;

  if (b->id > v->id) {
    INC_REF (b);
    return b;
  }
  else if (b->id == v->id) {
    INC_REF (b->r);
    return b->r;
  }
  else {
    if ((l = thash_locate (pairvisited, b->l, v))) {
      INC_REF (l);
    }
    else {
      l = _bool_makefalse (B, b->l, v);
    }
    if ((r = thash_locate (pairvisited, b->r, v))) {
      INC_REF (r);
    }
    else {
      r = _bool_makefalse (B, b->r, v);
    }
  }
  if (l == r) {
    DEC_REF (r);
    thash_insert (pairvisited,b,v,l);
    return l;
  }
  if ((b1 = hash_locate (B->H[b->id], l, r))) {
    DEC_REF (l); DEC_REF (r);
    INC_REF (b1);
    thash_insert (pairvisited,b,v,b1);
    return b1;
  }
  else {
    b1 = newbool();
    ASSIGN_LEAF (b1,0);
    b1->id = b->id;
    b1->l = l;
    b1->r = r;
    thash_insert (pairvisited,b,v,b1);
    hash_insert (B->H[b->id], b1);
    INC_REF (b1);
    return b1;
  }
}

/*-------------------------------------------------------------------------
 * make variable "v" false
 *-----------------------------------------------------------------------*/
bool_t *bool_makefalse (BOOL_T *B, bool_t *b1, bool_t *v)
{
  bool_t *b;
  if (v->l != B->true || v->r != B->false)
    return NULL;
  pairvisited = B->TH[BOOL_MKFALSE];
  b = _bool_makefalse (B,b1,v);
  return b;
}


/*=======================================================================*/
static void msort (bool_var_t *v, bool_var_t *v1, unsigned long i, 
		       unsigned long n)
{
  unsigned long j, k, l;

  if (n == 0)
    return;
  else 
    if (n == 1)
      v1[i] = v[i];
    else {
      msort (v, v1, i, n/2);
      msort (v, v1, i+n/2, n-n/2);
      j = i; k = i+n/2;
      for (l=0; l < n; l++)
	if (j < i+n/2)
	  if (k < n+i)
	    if (v1[j] < v1[k])
	      v[l] = v1[j++];
	    else
	      v[l] = v1[k++];
	  else
	    v[l] = v1[j++];
	else
	  v[l] = v1[k++];
      for (l=0; l < n; l++)
	v1[l] = v[l];
    }
}

/*-------------------------------------------------------------------------
 * returns a list of variables
 *-----------------------------------------------------------------------*/
bool_list_t *bool_qlist (BOOL_T *B, unsigned long n, bool_var_t *v)
{
  bool_list_t *l;

  MALLOC(l,bool_list_t,1);
  l->n = n;
  MALLOC(l->v, bool_var_t, n);
  
  msort (v, l->v, 0, n);
  return l;
}


static bool_t *_bool_substitute (BOOL_T *B, bool_list_t *l1, bool_list_t *l2,
				 bool_t *b, int loc)
{
  bool_t *b1;
  bool_t *l, *r;

  while (loc < l1->n && b->id > l1->v[loc])
    loc++;
  if (loc >= l1->n) {
    INC_REF (b);
    return b;
  }
  if ((b1 = thash_locate (pairvisited, b, b)))
    INC_REF (b1);
  else {
    if (loc < l1->n && b->id == l1->v[loc]) {
      l = _bool_substitute (B, l1, l2, b->l, loc+1);
      r = _bool_substitute (B, l1, l2, b->r, loc+1);
      if ((b1 = hash_locate (B->H[l2->v[loc]], l, r))) {
	DEC_REF (l); DEC_REF (r);
	INC_REF (b1);
      }
      else {
	b1 = newbool();
	ASSIGN_LEAF (b1, 0);
	b1->id = l2->v[loc];
	b1->l = l;
	b1->r = r;
	hash_insert (B->H[b1->id], b1);
	INC_REF (b1);
      }
    }
    else {
      l = _bool_substitute (B, l1, l2, b->l, loc);
      r = _bool_substitute (B, l1, l2, b->r, loc);
      if ((b1 = hash_locate (B->H[b->id], l, r))) {
	DEC_REF (l); DEC_REF (r);
	INC_REF (b1);
      }
      else {
	b1 = newbool();
	ASSIGN_LEAF (b1, 0);
	b1->id = b->id;
	b1->l = l;
	b1->r = r;
	hash_insert (B->H[b1->id], b1);
	INC_REF (b1);
      }
    }
  }
  thash_insert (pairvisited, b, b, b1);
  return b1;
}

/*-------------------------------------------------------------------------
 * substitue l1 -> l2
 * Assumed:
 *      EVERY VARIABLE FROM "b" is in l1.
 *-----------------------------------------------------------------------*/
extern bool_t *bool_substitute (BOOL_T *B, bool_list_t *l1, bool_list_t *l2,
				bool_t *b)
{
  bool_t *b1;

  if (l1->n != l2->n)
    return NULL;
  if (l1->n == 0)
    return b;
  pairvisited = thash_new (128);
  b1 = _bool_substitute (B, l1, l2, b, 0);
  thash_free (B, pairvisited);
  return b1;
}

/*========================================================================*/  

static bool_t *_bool_exists (BOOL_T *B, bool_list_t *l1, bool_t *b, int loc)
{
  bool_t *b1, *l, *r;
  static triplehash_t *tmp;

  while (loc < l1->n  && b->id > l1->v[loc])
    loc++;
  if (loc >= l1->n) {
    INC_REF (b);
    return b;
  }
  if ((b1 = thash_locate (pairvisited, b, b))) {
    INC_REF (b1);
    return b1;
  }
  else 
    if (b->id == l1->v[loc]) {
      l = _bool_exists (B, l1, b->l, loc+1);
      r = _bool_exists (B, l1, b->r, loc+1);
      tmp = pairvisited;
      pairvisited = B->TH[BOOL_OR];
      b1 = _bool_or (B, l, r);
      pairvisited = tmp;
    }
    else {
      l = _bool_exists (B, l1, b->l, loc);
      r = _bool_exists (B, l1, b->r, loc);
      if ((b1 = hash_locate (B->H[b->id], l, r))) {
	DEC_REF (l); DEC_REF (r);
	INC_REF (b1);
      }
      else {
	b1 = newbool();
	ASSIGN_LEAF (b1, 0);
	b1->id = b->id;
	b1->l = l;
	b1->r = r;
	hash_insert (B->H[b->id], b1);
      }
    }
  thash_insert (pairvisited, b, b, b1);
  return b1;
}
      
/*-------------------------------------------------------------------------
 * existential quantification
 *-----------------------------------------------------------------------*/
extern bool_t *bool_exists (BOOL_T *B, bool_list_t *l, bool_t *b)
{
  bool_t *b1;
  pairvisited = thash_new (128);
  b1 = _bool_exists (B, l, b, 0);
  thash_free (B, pairvisited);
  return b1;
}

/*-------------------------------------------------------------------------
 * relational product. fix it later.
 *-----------------------------------------------------------------------*/
extern bool_t *bool_exists2 (BOOL_T *B, bool_list_t *l, bool_t *b1, bool_t *b2)
{
  bool_t *b3, *b4;
  b3 = bool_and (B, b1, b2);
  b4 = bool_exists (B, l, b3);
  bool_free (B, b3);
  return b4;
}

/*-------------------------------------------------------------------------
 * actually free bdds
 *-----------------------------------------------------------------------*/
extern void bool_gc (BOOL_T *B)
{
  /* free the internal caches */
  int i;

  for (i=0; i < BOOL_MAXOP; i++) {
    thash_free (B, B->TH[i]);
    B->TH[i] = thash_new (128);
  }
}

extern void bool_addroot (BOOL_T *B, bool_t *b)
{
  struct rootlist *r;
  MALLOC (r, struct rootlist, 1);
  r->next = B->roots;
  r->b = b;
  B->roots = r;
}


extern void bool_delroot (BOOL_T *B, bool_t *b)
{
  struct rootlist *r;
  r = B->roots->next;
  FREE (B->roots);
  B->roots = r;
}


/*-------------------------------------------------------------------------
 * free a bdd
 *-----------------------------------------------------------------------*/
extern void bool_free (BOOL_T *B, bool_t *b)
{
  if (b == B->true || b == B->false) return;
#if 0
  fprintf(stderr,"freeing %#x, REF() = %d\n",b, REF(b));
#endif
  if (REF(b) == 0)
    fatal_error ("Uh oh.");
  DEC_REF(b);
  if (REF(b) == 0) {
#ifdef PROFILE
    nfreed++;
#endif
    /* free it */
    hash_delete (B->H[b->id], b);
    bool_free (B, b->l);
    bool_free (B, b->r);
    b->next = freelist;
    freelist = b;
  }
}

/*-------------------------------------------------------------------------
 * print a bdd
 *-----------------------------------------------------------------------*/
extern void bool_print (bool_t *b)
{
  if (ISLEAF(b))
    fprintf (stderr,"%s", b->id & 0x01 ? "T" : "F");
  else {
    fprintf (stderr,"[%ld,", b->id);
    bool_print (b->l);
    fprintf (stderr,",");
    bool_print (b->r);
    fprintf (stderr,"]");
  }
}


/*-------------------------------------------------------------------------
 * print out bdd operating parameters
 *-----------------------------------------------------------------------*/
extern void bool_info (BOOL_T *B)
{
  printf ("memory per node: %d\n", sizeof(bool_t));
  printf ("max. num. of vars: %ld\n", (1UL<<(sizeof(bool_var_t)*8-1)));
  printf ("init. hashtable size: %d\n", HASH_BLOCK);
  printf ("var. block size: %d\n", VAR_BLOCK);
  printf ("num. of vars in use: %ld\n", B->nvar);
#ifdef PROFILE
  printf ("bools allocated: %ld\n", nbools);
  printf ("extra bools: %ld\n", extrabools);
  printf ("bools freed: %ld\n", nfreed);
  printf ("outstanding refs: %ld\n", nbools - nfreed - 2);
#endif
  printf ("\n");
}


/*------------------------------------------------------------------------
 * Return true if leaf, false otherwise
 *------------------------------------------------------------------------*/
extern int bool_isleaf (bool_t *b)
{
  return ISLEAF(b);
}

extern int bool_getid(bool_t *b)
{
  return (int)b->id;
}

/* the following routines are used by routines that understand the bool_t
   data structure and wish to traverse it */
extern bool_t *bool_r(bool_t *b)
{
  assert(!ISLEAF(b));
  INC_REF(b->r);
  return b->r;
}

extern bool_t *bool_l(bool_t *b)
{
  assert(!ISLEAF(b));
  INC_REF(b->l);
  return b->l;
}

/* this routine allows us to build a literal of a variable in a node */
extern bool_t *bool_node_var(BOOL_T *B, bool_t *b)
{
  return bool_var(B,b->id);
}

extern int bool_refs(BOOL_T *B, bool_t *b)
{ 
  return b->ref;
}
