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

struct Site *readone();
struct Site *nextone();


static int setup=0;

mod3_start()
{
  setup=0;
  nsites = 0;
  sites = (struct Site *) memmalloc(4000*sizeof *sites);

  tfl=NULL;
  hfl=NULL;
  sfl=NULL;
  efl=NULL;
  
  triples=NULL;
}


mod3_addsite(p)
     struct Point p;
{
  assert(!setup);
  sites[nsites].coord = p;
  sites[nsites].sitenbr = nsites;
  nsites += 1;
  if (nsites % 4000 == 0) {
    /* let's just free this here, as it is such a large data structure */

    /* the following code is wrong anyway (two args to memmalloc) */
    /* struct Site *newsites = (struct Site *) memmalloc(sites,(nsites+4000)*sizeof*sites); */

    struct Site *newsites = (struct Site *) mymalloc((nsites+4000)*sizeof*sites);
    bcopy(sites,newsites,nsites*sizeof*sites);

    /* don't free first time, as the first block as allocated by memmalloc */
    if (nsites!=4000) myfree(sites);

    sites=newsites;
  }

}

/* sort sites on y, then x, coord */
int scomp(s1,s2)
struct Point *s1,*s2;
{
	if(s1 -> y < s2 -> y) return(-1);
	if(s1 -> y > s2 -> y) return(1);
	if(s1 -> x < s2 -> x) return(-1);
	if(s1 -> x > s2 -> x) return(1);
	return(0);
}

mod3_setup() 
{
int i;
  setup=1;
qsort(sites, nsites, sizeof *sites, scomp);
xmin=sites[0].coord.x; 
xmax=sites[0].coord.x;
for(i=1; i<nsites; i+=1)
{	if(sites[i].coord.x < xmin) xmin = sites[i].coord.x;
	if(sites[i].coord.x > xmax) xmax = sites[i].coord.x;
};
ymin = sites[0].coord.y;
ymax = sites[nsites-1].coord.y;
}

mod3_delaunay()
{	
int c;
 int i;
struct Site *(*next)();

 assert(setup); 
triangulate = 1; 

 next = nextone;

siteidx = 0;
geominit();

 clear_triples();
voronoi(triangulate, next); 

}

clear_triples()
{
  struct Triple *t=triples;
  triples=NULL;

  while (t!=NULL) {
    struct Triple *u=t;
    t=t->next;
  }

}

int mod3_gettriple(struct TripleArg *t)
{
  if (!triples) 
    return 0;
  
  {
    struct Triple *u=triples;
    *t = triples->d;

    triples=u->next;

  }
}

mod3_voronoi()
{	
int c;
struct Site *(*next)();

 assert(setup); 
triangulate = 0; 

 next = nextone;

siteidx = 0;
geominit();

voronoi(triangulate, next); 

}


/* return a single in-storage site */
struct Site *nextone()
{
struct Site *s;
if(siteidx < nsites)
{	s = &sites[siteidx];
	siteidx += 1;
	return(s);
}
else	return( (struct Site *)NULL);
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

char *memmalloc(siz)
     int siz;
{
  char *res=mymalloc(siz+sizeof(char *));

  *((char **)res) = mem;
  mem=res;
  return res+sizeof(char*);
}

/* clear it all out */
mod3_finish()
{
  char *q;
  
  while (mem) {
    q=*(char**)mem;
    myfree(mem);
    mem=q;
  }
}
