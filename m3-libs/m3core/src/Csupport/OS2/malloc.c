/* Copyright (C) 1994, Klaus Preschern.                        */
/* All rights reserved.                                        */
/* See the file COPYRIGHT.KP for a full description.           */

#include <stdio.h>

#define MULTI_THREADED                  0
#define PRINT                           0

#define BUCKET_NIL              ((BUCKET *) 0)
#define BUCKET_SIZE             (sizeof (BUCKET))
#define SIZESTORE               (sizeof (unsigned))

typedef struct _BUCKET {
  unsigned          len;
  struct _BUCKET * next;
} BUCKET;

extern void *sbrk (int);
static void PrintInt (int num)                  /* for debugging */
  {
  printf ("%d", num);
  }

static void PrintString (char * str)
  {
  printf ("%s", str);
  }

static BUCKET * root = BUCKET_NIL;

static void * more_core (unsigned size)
  {
    /* Note: The size is always a multiple of the BUCKET size ! */
    void * coremem = sbrk (size);
    if ((int) coremem == -1) {                  /* no more mem */
      return 0;
    }
    /* The freed memory will be inserted into the BUCKET list. */
    return coremem;
  }

static void del_slot (BUCKET * prev, BUCKET * hp)
  {
    if (hp == root) {
      root = hp->next;
    } else {
      prev->next = hp->next;
    }
  }

static void * internal_malloc (unsigned size)
  {
    BUCKET *prev = BUCKET_NIL, *hp = root;
    char * base; unsigned *sizePtr;

    size += SIZESTORE;                  /* add storage for the size */
    /* size is always a multiple of the bucket size. */
    size = ((size + BUCKET_SIZE - 1) / BUCKET_SIZE) * BUCKET_SIZE;
#if PRINT
    PrintString (" rs = "); PrintInt (size); PrintString (" ");
#endif
    while (hp != BUCKET_NIL) {
      if (hp->len >= size) {
        /* We have found hole that is big enough. Use it. */
        hp->len -= size;
        base = ((char *) hp) + hp->len; /* Bite a peace off at the end. */
        if (hp->len == 0) {             /* The entire hole has been used up. */
          del_slot (prev, hp);
        }
        sizePtr  = (unsigned *) base;
        *sizePtr = size;                /* store full size in bucket */
        base    += SIZESTORE;           /* move base over size */
        return ((void *) base);
      }
      prev = hp;
      hp = hp->next;
    }

    /* The list is empty or all memory is used. Ask the system for mem. */
    base = more_core (size);
#if PRINT
    PrintString (" mcp: "); PrintInt ((int) base); PrintString (" ");
#endif
    if (base != 0) {
      sizePtr  = (unsigned *) base;
      *sizePtr = size;                  /* store full size in bucket */
      base    += SIZESTORE;             /* move base over size */
    }
    return ((void *) base);
  }

static void merge (BUCKET * hp)
  {
    /* 'hp' points to the first of a series of 3 holes that can possibly
     * all be merged together.
     */

    BUCKET *next_ptr;

    if ((next_ptr = hp->next) == BUCKET_NIL) {
      return;
    }
    if (((char *) hp) + hp->len == (char *) next_ptr) {
      hp->len += next_ptr->len;         /* Merge first and second. */
      del_slot (hp, next_ptr);
    } else {
      hp = next_ptr;
    }

    if ((next_ptr = hp->next) == BUCKET_NIL) {
      return;
    }
    if (((char *) hp) + hp->len == (char *) next_ptr) {
       hp->len += next_ptr->len;        /* Merge second (first?) and third. */
       del_slot (hp, next_ptr);
    }
  }

static void internal_free (void * ptr)
  {
    BUCKET *new_ptr, *hp = root, *prev = BUCKET_NIL;
    unsigned *sizePtr, size;

    sizePtr = ptr;
    sizePtr--;                          /* move ptr back to size */
    size = *sizePtr;                    /* get size */

    new_ptr = (BUCKET *) sizePtr;       /* this is now our new bucket */
    new_ptr->len  = size;               /* yes I know, it is already there */
    new_ptr->next = BUCKET_NIL;

    if ((hp == BUCKET_NIL) || (new_ptr < hp)) {
      /* Block to be freed goes on front of hole list. */
      new_ptr->next = hp;
      root = new_ptr;
      merge (new_ptr);
      return;
    }

    /* Block to be returned does not go to front of the hole list. */
    while ((hp != BUCKET_NIL) && (new_ptr > hp)) {
      prev = hp;
      hp   = hp->next;
    }

    if (new_ptr == hp) {        /* Attempt to free 'new_ptr' twice. */
      return;
    }
    /* Insert block after 'prev'. */
    new_ptr->next = prev->next;
    prev->next = new_ptr;
    merge (prev);
  }

void *mymalloc (unsigned nbytes)
  {
    char *ptr = NULL;
#if MULTI_THREADED
    Critical_Enter ();
#endif
      ptr = internal_malloc (nbytes);
#if PRINT
      PrintInt ((int) ptr);
      PrintString (" = malloc (");
      PrintInt (nbytes); PrintString (") \n");
#endif
#if MULTI_THREADED
    Critical_Leave ();
#endif
    return ptr;
  }

void myfree (void *ptr)
  {
    if (ptr == 0) {
      return;
    }
#if MULTI_THREADED
    Critical_Enter ();
#endif
#if PRINT
      PrintString ("free (");
      PrintInt ((int) ptr); PrintString (") \n");
#endif
      internal_free (ptr);
#if MULTI_THREADED
    Critical_Leave ();
#endif
  }
