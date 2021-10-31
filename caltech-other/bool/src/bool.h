/*************************************************************************
 *
 *  (c) 1995 California Institute of Technology
 *  Department of Computer Science
 *  Pasadena, CA 91125.
 *  All Rights Reserved
 *
 *  Author: Rajit Manohar
 *  Date: Fri Mar 24 03:33:32 PST 1995
 *  Info: package for manipulating boolean functions.
 *
 *  $Id$
 *
 *************************************************************************/

#ifndef __BOOL_H__
#define __BOOL_H__

#if __cplusplus
extern "C" {
#endif

typedef size_t bool_var_t;

struct bool_t;
struct rootlist;
struct triple;

typedef struct bool_t bool_t;
typedef struct rootlist rootlist;
typedef struct triple triple;

/* do not rearrange order of fields! */
struct bool_t {
  bool_t *l, *r;		/* left, right links */
  bool_t *next;		/* next ptr, for hashtable */
  bool_var_t id;		/* variable, if non-leaf, value if leaf. */
  unsigned int ref:14;		/* refcount */
};

#ifdef BOOL_INTERNAL_H

#define HIBIT_OFFSET (sizeof(bool_var_t)*8-1)

#define BOOL_MAXVAR  (((bool_var_t)1)<<HIBIT_OFFSET)

#define SET_HIBIT(v) (v |= ((bool_var_t)1) << HIBIT_OFFSET)

#define REFMAX  ((1<<14)-1)
 /* max value of "ref" */

#define ISLEAF(b)      (((b)->id & (((bool_var_t)1) << HIBIT_OFFSET)) ? 1 : 0)
 /* True if "b" is a leaf */

#define ASSIGN_LEAF(b,n) ((b)->id = ((b)->id & ~(((bool_var_t)1)<<HIBIT_OFFSET))|(((size_t)n) << HIBIT_OFFSET))
 /* assign to the "leaf" bit. */

#define REF(b)         ((b)->ref)
 /* value of the "ref" field */

#define INC_REF(b)     do { if ((b)->ref < REFMAX) (b)->ref++; }while(0)
 /* increment value of "ref" field */

#define DEC_REF(b)     do { if ((b)->ref < REFMAX) (b)->ref--; }while(0)
 /* decrement value of "ref" field */

#define CLEAR(b) ((b)->ref = 0, (b)->id = 0)
 /* clear all fields */

#define VAR_BLOCK 32
 /* number of variables added at a time */

#define HASH_BLOCK 32
 /* initial size of the hastable for each variable */

enum triple_operations_t {
  BOOL_AND = 0, BOOL_OR = 1, BOOL_XOR = 2, BOOL_IMPLIES = 3, BOOL_NOT = 4,
  BOOL_MKTRUE = 5, BOOL_MKFALSE = 6
  } ;

#endif

#define BOOL_MAXOP 7

typedef struct {
  size_t nelements;	/* number of elements in the hashtable */
  size_t nbuckets;	/* number of buckets */
  bool_t **bucket;		/* the buckets */
} pairhash_t;

struct triple { /* triple for hashing */
  bool_t *v1, *v2, *v3;
  triple *next;
};

typedef struct {
  size_t nbuckets;
  size_t nelements;
  triple **bucket;
} triplehash_t;			/* triple hash table */

struct rootlist {
  bool_t *b;
  rootlist *next;
}; 

typedef struct {
  size_t nvar;		/* number of variables */
  size_t totvar;		/* total number of variables */
  pairhash_t **H;		/* hashtable for each variable */
  triplehash_t *TH[BOOL_MAXOP];	/* hashtable for (a,b)->c values */
  bool_t *True, *False;
  rootlist *roots;	/* roots */
} BOOL_T;

typedef struct {
  bool_var_t *v;
  size_t n;
} bool_list_t;			/* sorted list of variables */

BOOL_T *bool_init (void);

bool_t *bool_true (BOOL_T *);
bool_t *bool_false (BOOL_T *);
bool_t *bool_newvar (BOOL_T *);
bool_t *bool_var (BOOL_T *, bool_var_t);

bool_t *bool_and (BOOL_T *, bool_t *, bool_t *);
bool_t *bool_or (BOOL_T *, bool_t *, bool_t *);
bool_t *bool_xor (BOOL_T *, bool_t *, bool_t *);
bool_t *bool_not (BOOL_T *, bool_t *);
bool_t *bool_copy  (BOOL_T *, bool_t *);
bool_t *bool_implies (BOOL_T *, bool_t *, bool_t *);

bool_t *bool_maketrue (BOOL_T *, bool_t *, bool_t *);
bool_t *bool_makefalse (BOOL_T *, bool_t *, bool_t *);

bool_t *bool_substitute (BOOL_T *, bool_list_t *, bool_list_t *,
				bool_t *);
bool_t *bool_exists (BOOL_T *, bool_list_t *, bool_t *);
bool_t *bool_exists2 (BOOL_T *, bool_list_t *, bool_t *, bool_t *);
bool_list_t *bool_qlist (BOOL_T *, size_t, bool_var_t *);

void bool_free (BOOL_T *, bool_t *);
void bool_gc (BOOL_T *);

void bool_print (bool_t *);
void bool_info (BOOL_T *B);
int bool_getid(bool_t *b);

int bool_isleaf (bool_t *b);

#define bool_topvar(b) ((b)->id)

#if __cplusplus
} /* extern "C" */
#endif

#endif
