/*
 * The author of this software is Steven Fortune.  Copyright (c) 1994 by AT&T
 * Bell Laboratories.
 * Permission to use, copy, modify, and distribute this software for any
 * purpose without fee is hereby granted, provided that this entire notice
 * is included in all copies of any software which is or includes a copy
 * or modification of this software and in all copies of the supporting
 * documentation for such software.
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
 * REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
 * OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* modified by Mika Nystrom <mika@cs.caltech.edu> */


#include <assert.h>
#include <stdio.h>
#include <strings.h>
#include "defs.h"
#include "mymalloc.h"

#if __cplusplus
extern "C" {
#endif

Site *readone(void);
Site *nextone(void);

static int setup=0;

void
mod3_start(void)
{
  setup=0;
  nsites = 0;
  sites = (Site*)memmalloc(4000 * sizeof(*sites));

  tfl=NULL;
  hfl=NULL;
  sfl=NULL;
  efl=NULL;
  
  triples=NULL;
}

void
mod3_addsite(Point p)
{
  assert(!setup);
  sites[nsites].coord = p;
  sites[nsites].sitenbr = nsites;
  nsites += 1;
  if (nsites % 4000 == 0) {
    /* let's just free this here, as it is such a large data structure */

    /* the following code is wrong anyway (two args to memmalloc) */
    /* Site *newsites = (Site*)memmalloc(sites,(nsites+4000)*sizeof*sites); */

    Site *newsites = (Site*)mymalloc((nsites+4000) * sizeof(*sites));
    bcopy(sites, newsites, nsites * sizeof(*sites));

    /* don't free first time, as the first block as allocated by memmalloc */
    if (nsites!=4000) myfree(sites);

    sites=newsites;
  }

}

/* sort sites on y, then x, coord */
static
int scomp(
const void *p1,
const void *p2)
{
    const Point *s1 = (const Point*)p1;
    const Point *s2 = (const Point*)p2;
	if(s1 -> y < s2 -> y) return(-1);
	if(s1 -> y > s2 -> y) return(1);
	if(s1 -> x < s2 -> x) return(-1);
	if(s1 -> x > s2 -> x) return(1);
	return(0);
}

void
mod3_setup(void) 
{
int i;
  setup=1;
qsort(sites, nsites, sizeof *sites, scomp);
xmin=sites[0].coord.x; 
xmax=sites[0].coord.x;
for(i=1; i<nsites; i+=1)
{	if(sites[i].coord.x < xmin) xmin = sites[i].coord.x;
	if(sites[i].coord.x > xmax) xmax = sites[i].coord.x;
}
ymin = sites[0].coord.y;
ymax = sites[nsites-1].coord.y;
}

void
mod3_delaunay(void)
{	
int c;
 int i;
Site *(*next)(void);

 assert(setup); 
triangulate = 1; 

 next = nextone;

siteidx = 0;
geominit();

 clear_triples();
voronoi(triangulate, next); 

}

void
clear_triples(void)
{
  Triple *t=triples;
  triples=NULL;

  while (t!=NULL) {
    Triple *u=t;
    t=t->next;
  }

}

int mod3_gettriple(TripleArg *t)
{
  if (!triples) 
    return 0;
  
  {
    Triple *u=triples;
    *t = triples->d;

    triples=u->next;

  }

  return 1;
}

void
mod3_voronoi(void)
{	
int c;
Site *(*next)(void);

 assert(setup); 
triangulate = 0; 

 next = nextone;

siteidx = 0;
geominit();

voronoi(triangulate, next); 
}


/* return a single in-storage site */
Site *nextone(void)
{
Site *s;
if(siteidx < nsites)
{	s = &sites[siteidx];
	siteidx += 1;
	return(s);
}
else	return NULL;
}

char *mem=NULL;

/* what we do is this...

   every block of memory allocated has a pointer at the head

   ptr -> ptr -> NULL      
    D      D
    A      A
    T      T
    A      A

   this saves half our mallocs.  when we're done we just go down
   the pointers and free everything...

   --Mika
   
   P.S. I hate C programming!!!!
 */

char *memmalloc(size_t siz)
{
  /* TODO This reduces alignment. Better to use two pointers. */
  char *res = (char*)mymalloc(siz + sizeof(char*));

  *((char **)res) = mem;
  mem=res;
  return res+sizeof(char*);
}

/* clear it all out */
void
mod3_finish(void)
{
  char *q;
  
  while (mem) {
    q=*(char**)mem;
    myfree(mem);
    mem=q;
  }
}

#if __cplusplus
} /* extern "C" */
#endif
