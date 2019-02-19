#
#include "defs.h"
#include "mymalloc.h"


PQinsert(he, v, offset)
struct Halfedge *he;
struct Site *v;
float 	offset;
{
struct Halfedge *last, *next;

he -> vertex = v;
he -> ystar = v -> coord.y + offset;
last = &PQhash[PQbucket(he)];
while ((next = last -> PQnext) != (struct Halfedge *) NULL &&
      (he -> ystar  > next -> ystar  ||
      (he -> ystar == next -> ystar && v -> coord.x > next->vertex->coord.x)))
	{	last = next;};
he -> PQnext = last -> PQnext; 
last -> PQnext = he;
PQcount += 1;
}

PQdelete(he)
struct Halfedge *he;
{
struct Halfedge *last;

if(he ->  vertex != (struct Site *) NULL)
{	last = &PQhash[PQbucket(he)];
	while (last -> PQnext != he) last = last -> PQnext;
	last -> PQnext = he -> PQnext;
	PQcount -= 1;
	he -> vertex = (struct Site *) NULL;
};
}

int PQbucket(he)
struct Halfedge *he;
{
int bucket;

bucket = (he->ystar - ymin)/deltay * PQhashsize;
if (bucket<0) bucket = 0;
if (bucket>=PQhashsize) bucket = PQhashsize-1 ;
if (bucket < PQmin) PQmin = bucket;
return(bucket);
}



int PQempty()
{
	return(PQcount==0);
}


struct Point PQ_min()
{
struct Point answer;

	while(PQhash[PQmin].PQnext == (struct Halfedge *)NULL) {PQmin += 1;};
	answer.x = PQhash[PQmin].PQnext -> vertex -> coord.x;
	answer.y = PQhash[PQmin].PQnext -> ystar;
	return (answer);
}

struct Halfedge *PQextractmin()
{
struct Halfedge *curr;

	curr = PQhash[PQmin].PQnext;
	PQhash[PQmin].PQnext = curr -> PQnext;
	PQcount -= 1;
	return(curr);
}


PQinitialize()
{
int i; struct Point *s;

	PQcount = 0;
	PQmin = 0;
	PQhashsize = 4 * sqrt_nsites;


	PQhash = (struct Halfedge *) memmalloc(PQhashsize * sizeof *PQhash);
	for(i=0; i<PQhashsize; i+=1) PQhash[i].PQnext = (struct Halfedge *)NULL;
}

