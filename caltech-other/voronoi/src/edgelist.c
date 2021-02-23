#include "defs.h"
#include "mymalloc.h"

#if __cplusplus
extern "C" {
#endif

static int ntry, totalsearch;

/*int ELhashsize;*/
void
ELinitialize()
{
int i;
	ELhashsize = 2 * sqrt_nsites;


	ELhash = (struct Halfedge **) memmalloc ( sizeof *ELhash * ELhashsize);
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

struct Halfedge *HEcreate(
struct Edge *e,
int pm)
{
  struct Halfedge *answer;
 
  if (!hfl || hp==HALLOC) {
    hfl=(struct Halfedge *)memmalloc(sizeof(struct Halfedge)*HALLOC);
    hp=0;
  }
  answer=(struct Halfedge *)hfl+(hp++);

	answer -> ELedge = e;
	answer -> ELpm = pm;
	answer -> PQnext = NULL;
	answer -> vertex = NULL;
	return(answer);
}

void
ELinsert(
struct	Halfedge *lb,
struct	Halfedge *new_)
{
	new_ -> ELleft = lb;
	new_ -> ELright = lb -> ELright;
	(lb -> ELright) -> ELleft = new_;
	lb -> ELright = new_;
}

/* Get entry from hash table, pruning any deleted nodes */
struct Halfedge *ELgethash(int b)
{
struct Halfedge *he;

	if(b<0 || b>=ELhashsize) return NULL;
	he = ELhash[b]; 
	if (he == NULL ||
	    he -> ELedge != DELETED ) return (he);

/* Hash table points to deleted half edge.  Patch as necessary. */
	ELhash[b] = NULL;
	return NULL;
}	

struct Halfedge *ELleftbnd(struct Point *p)
{
int i, bucket;
struct Halfedge *he;

/* Use hash table to get close to desired halfedge */
	bucket = (p->x - xmin)/deltax * ELhashsize;
	if(bucket<0) bucket =0;
	if(bucket>=ELhashsize) bucket = ELhashsize - 1;
	he = ELgethash(bucket);
	if(he == NULL)
	{   for(i=1; 1 ; i += 1)
	    {	if ((he=ELgethash(bucket-i)) != NULL) break;
		if ((he=ELgethash(bucket+i)) != NULL) break;
	    };
	totalsearch += i;
	};
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
	};
	return (he);
}

	/* This delete routine can't reclaim node, since pointers from hash
   table may be present.   */
void
ELdelete(struct Halfedge *he)
{
	(he -> ELleft) -> ELright = he -> ELright;
	(he -> ELright) -> ELleft = he -> ELleft;
	he -> ELedge = (struct Edge *)DELETED;
}

struct Halfedge *ELright(struct Halfedge *he)
{
	return (he -> ELright);
}

struct Halfedge *ELleft(struct Halfedge *he)
{
	return (he -> ELleft);
}

struct Site *leftreg(struct Halfedge *he)
{
	if(he -> ELedge == NULL) return(bottomsite);
	return( he -> ELpm == le ? 
		he -> ELedge -> reg[le] : he -> ELedge -> reg[re]);
}

struct Site *rightreg(struct Halfedge *he)
{
	if(he -> ELedge == NULL) return(bottomsite);
	return( he -> ELpm == le ? 
		he -> ELedge -> reg[re] : he -> ELedge -> reg[le]);
}

#if __cplusplus
} /* extern "C" */
#endif
