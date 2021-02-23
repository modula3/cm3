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
#
#include <stdio.h>
#include "defs.h"

#if __cplusplus
extern "C" {
#endif

struct Site *readone();
struct Site *nextone();

#if __cplusplus
} /* extern "C" */
#endif

int
main(
int argc,
char *argv[])
{	
int c;
struct Site *(*next)();

sorted = 0; triangulate = 0; plot = 0; debug = 0;
while((c=getopt(argc,argv,"dpst")) != EOF)
	switch(c) {
	case 'd': debug = 1;
		  break;
	case 's': sorted = 1;
		  break;
	case 't': triangulate = 1;
		  break;
	case 'p': plot = 1;
		  break;
		  };

freeinit(&sfl, sizeof *sites);
if(sorted)
{	scanf("%d %f %f %f %f", &nsites, &xmin, &xmax, &ymin, &ymax);
	next = readone;
}
else 
{	readsites();
	next = nextone;
};

siteidx = 0;
geominit();
if(plot) plotinit();

voronoi(triangulate, next); 

}

#if __cplusplus
extern "C" {
#endif

/* sort sites on y, then x, coord */
static
int scomp(
const void *p1,
const void *p2)
{
    const struct Point *s1 = (const struct Point*)p1;
    const struct Point *s2 = (const struct Point*)p2;
	if(s1 -> y < s2 -> y) return(-1);
	if(s1 -> y > s2 -> y) return(1);
	if(s1 -> x < s2 -> x) return(-1);
	if(s1 -> x > s2 -> x) return(1);
	return(0);
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
else	return NULL;
}

/* read all sites, sort, and compute xmin, xmax, ymin, ymax */
void
readsites()
{
int i;

nsites=0;
sites = (struct Site *) memmalloc(4000*sizeof *sites);
while(scanf("%f %f", &sites[nsites].coord.x, &sites[nsites].coord.y)!=EOF)
{	sites[nsites].sitenbr = nsites;
	sites[nsites].refcnt = 0;
	nsites += 1;
	if (nsites % 4000 == 0) 
		sites = (struct Site *) memrealloc(sites,(nsites+4000)*sizeof*sites);
};
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

/* read one site */
struct Site *readone()
{
struct Site *s;

s = (struct Site *) getfree(&sfl);
s -> refcnt = 0;
s -> sitenbr = siteidx;
siteidx += 1;
if(scanf("%f %f", &(s->coord.x), &(s->coord.y)) == EOF)
	return NULL;
return(s);
}

#if __cplusplus
} /* extern "C" */
#endif
