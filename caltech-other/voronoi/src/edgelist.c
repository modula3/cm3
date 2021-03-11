#include "defs.h"
#include "mymalloc.h"

#if __cplusplus
extern "C" {
#endif

static int ntry, totalsearch;

/*int ELhashsize;*/
void
ELinitialize(void)
{
int i;
	ELhashsize = 2 * sqrt_nsites;


	ELhash = (Halfedge**)memmalloc(sizeof *ELhash * ELhashsize);
	for(i=0; i<ELhashsize; i +=1) ELhash[i] = NULL;
	ELleftend = HEcreate(NULL, 0);
	ELrightend = HEcreate(NULL, 0);
	ELleftend -> ELleft = NULL;
	ELleftend -> ELright = ELrightend;
	ELrightend -> ELleft = ELleftend;
	ELrightend -> ELright = NULL;
	ELhash[0] = ELleftend;
	ELhash[ELhashsize-1] = ELrightend;
}

#define HALLOC 100
static int hp=0;

Halfedge *HEcreate(
Edge *e,
int pm)
{
  Halfedge *answer;
 
  if (!hfl || hp==HALLOC) {
    hfl = (Halfedge*)memmalloc(sizeof(Halfedge) * HALLOC);
    hp=0;
  }
  answer=(Halfedge *)hfl+(hp++);

	answer -> ELedge = e;
	answer -> ELpm = pm;
	answer -> PQnext = NULL;
	answer -> vertex = NULL;
	return(answer);
}

void
ELinsert(
Halfedge *lb,
Halfedge *new_)
{
	new_ -> ELleft = lb;
	new_ -> ELright = lb -> ELright;
	(lb -> ELright) -> ELleft = new_;
	lb -> ELright = new_;
}

/* Get entry from hash table, pruning any deleted nodes */
Halfedge *ELgethash(int b)
{
Halfedge *he;

	if(b<0 || b>=ELhashsize) return NULL;
	he = ELhash[b]; 
	if (he == NULL ||
	    he -> ELedge != DELETED ) return (he);

/* Hash table points to deleted half edge.  Patch as necessary. */
	ELhash[b] = NULL;
	return NULL;
}	

Halfedge *ELleftbnd(Point *p)
{
int i, bucket;
Halfedge *he;

/* Use hash table to get close to desired halfedge */
	bucket = (p->x - xmin)/deltax * ELhashsize;
	if(bucket<0) bucket =0;
	if(bucket>=ELhashsize) bucket = ELhashsize - 1;
	he = ELgethash(bucket);
	if(he == NULL)
	{   for(i=1; 1 ; i += 1)
	    {	if ((he=ELgethash(bucket-i)) != NULL) break;
		if ((he=ELgethash(bucket+i)) != NULL) break;
	    }
	totalsearch += i;
	}
	ntry += 1;
/* Now search linear list of halfedges for the corect one */
	if (he==ELleftend  || (he != ELrightend && right_of(he,p)))
	{do {he = he -> ELright;} while (he!=ELrightend && right_of(he,p));
	 he = he -> ELleft;
	}
	else 
	do {he = he -> ELleft;} while (he!=ELleftend && !right_of(he,p));

/* Update hash table and reference counts */
	if(bucket > 0 && bucket <ELhashsize-1)
	{
		ELhash[bucket] = he;
	}
	return (he);
}

	/* This delete routine can't reclaim node, since pointers from hash
   table may be present.   */
void
ELdelete(Halfedge *he)
{
	(he -> ELleft) -> ELright = he -> ELright;
	(he -> ELright) -> ELleft = he -> ELleft;
	he -> ELedge = (Edge*)DELETED;
}

Halfedge *ELright(Halfedge *he)
{
	return (he -> ELright);
}

Halfedge *ELleft(Halfedge *he)
{
	return (he -> ELleft);
}

Site *leftreg(Halfedge *he)
{
	if(he -> ELedge == NULL) return(bottomsite);
	return( he -> ELpm == le ? 
		he -> ELedge -> reg[le] : he -> ELedge -> reg[re]);
}

Site *rightreg(Halfedge *he)
{
	if(he -> ELedge == NULL) return(bottomsite);
	return( he -> ELpm == le ? 
		he -> ELedge -> reg[re] : he -> ELedge -> reg[le]);
}

#if __cplusplus
} /* extern "C" */
#endif
