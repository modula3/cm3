#ifndef _MYMALLOC_H
#define _MYMALLOC_H

/*************************************************************************
 *
 *  mymalloc.h
 *
 *   Debugging malloc.
 *
 *  Copyright (c) 1997 California Institute of Technology
 *  All rights reserved.
 *  Department of Computer Science
 *  Pasadena, CA 91125.
 *
 *  Author: Mika Nystrom <mika@cs.caltech.edu>
 *
 *  Permission to use, copy, modify, and distribute this software
 *  and its documentation for any purpose and without fee is hereby
 *  granted, provided that the above copyright notice appear in all
 *  copies. The California Institute of Technology makes no representations
 *  about the suitability of this software for any purpose. It is
 *  provided "as is" without express or implied warranty. Export of this
 *  software outside of the United States of America may require an
 *  export license.
 *
 *  $Id$
 *
 *************************************************************************/

#include <stdlib.h>
#include <signal.h>

/* FAIL_SLEEP is how many seconds to pause for when malloc fails.
   To turn off this feature, #define it to zero */

/* to turn on all this stuff, just #define _NDEBUG before #including
   this file */

#define _NDEBUG /* turn it all off now */

#if !defined(_NDEBUG) && !defined(_DONT_MYMALLOC) 

#ifndef FAIL_SLEEP
# define FAIL_SLEEP 30        
#endif /* !FAIL_SLEEP */

/* _MYMALLOC_MAX_UNFREE_PRINT says how many entries from the unfreed
   list mymalloc_cleanup should print */

#ifndef _MYMALLOC_MAX_UNFREE_PRINT 
# define _MYMALLOC_MAX_UNFREE_PRINT 5
#endif /* !_MYMALLOC_MAX_UNFREE_PRINT */

/* function prototypes and headers */

void *_mymalloc (size_t size, const char *file, const int line);
void *_mycalloc1 (size_t size, const char *file, const int line);
void _myfree (void *ptr, const char *file, const int line);
void *_mycalloc (size_t number, size_t size, const char *file, const int line);

/* when you need a function pointer.. */
void myfree_f(void *ptr);

void __free (void *ptr);
void *_myrealloc (void *ptr,size_t size, const char *file, const int line);
void mymalloc_cleanup(void);
void mymalloc_cleansig(int unused);
char *_mystrdup (const char *str, const char *file, const int line);
#define mymalloc(x) _mymalloc((x),__FILE__,__LINE__)
#define mycalloc1(x) _mycalloc1((x),__FILE__,__LINE__)
#define mystrdup(x) _mystrdup((x),__FILE__,__LINE__)
#define myfree(x) _myfree((x),__FILE__,__LINE__)
#define myrealloc(x,y) _myrealloc((x),(y),__FILE__,__LINE__)
#define mycalloc(x,y) _mycalloc((x),(y),__FILE__,__LINE__)

/* just in case a forgetful user tries to bypass us.. */


#ifndef __IN_MYMALLOC_C

# ifndef FORCE_MYMALLOC

# define malloc qwerty_ERROR_dont_use_malloc_or_free
# define free qwerty_ERROR_dont_use_malloc_or_free
# define realloc qwerty_ERROR_dont_use_malloc_or_free
# define strdup qwerty_ERROR_dont_use_malloc_or_free
# define calloc qwerty_ERROR_dont_use_malloc_or_free

# else /* defined(FORCE_MYMALLOC) */

# define malloc mymalloc
# define free myfree
# define strdup mystrdup
# define realloc myrealloc
# define calloc mycalloc

# endif /* FORCE_MYMALLOC */

#endif /* !IN_MYMALLOC_C */

#define mymalloc_clean_on_intr   signal(SIGINT,&mymalloc_cleansig)

#else /* _NDEBUG */
#ifndef _DONT_MYMALLOC  /* might be from _NDEBUG */
# define _DONT_MYMALLOC /* keeps mymalloc.c from being compiled */
#endif
# define mymalloc(x) malloc((x))
# define mycalloc1(x) calloc((x),1)
# define mystrdup(x) strdup((x))
# define myfree(x) free((x))
# define myrealloc(x,y) realloc((x),(y))
# define mymalloc_cleanup()  ;
# define mymalloc_clean_on_intr
# define __free(x) free(x)

#endif /* !_NDEBUG */

#endif /* !_MYMALLOC_H */

