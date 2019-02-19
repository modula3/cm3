/* $Id$ */

#ifndef NULL
#define NULL (void *)0
#endif
#define DELETED -2


extern int triangulate, sorted, plot, debug;


extern float xmin, xmax, ymin, ymax, deltax, deltay;


struct Point	{
float x,y;
};

/* structure used both for sites and for vertices */
struct Site	{
struct	Point	coord;
int		sitenbr;
};


extern struct	Site	*sites;
extern int		nsites;
extern int		siteidx;
extern int		sqrt_nsites;
extern int		nvertices;
extern struct	Site	*bottomsite;


struct TripleArg {
  int s1, s2, s3;
};

struct Triple {
  struct TripleArg d;
  struct Triple *next;
};

extern struct Triple *triples;


struct Edge	{
float		a,b,c;
struct	Site 	*ep[2];
struct	Site	*reg[2];
int		edgenbr;
};
#define le 0
#define re 1
extern int nedges;

int has_endpoint(),right_of();
struct Site *intersect();
float dist();
struct Point PQ_min();
struct Halfedge *PQextractmin();
struct Edge *bisect();

struct Halfedge {
struct Halfedge	*ELleft, *ELright;
struct Edge	*ELedge;
char		ELpm;
struct	Site	*vertex;
float		ystar;
struct	Halfedge *PQnext;
};

extern struct	Halfedge *ELleftend, *ELrightend;
extern int 	ELhashsize;
extern struct	Halfedge **ELhash;
struct	Halfedge *HEcreate(), *ELleft(), *ELright(), *ELleftbnd();
struct	Site *leftreg(), *rightreg();


extern int PQhashsize;
extern struct	Halfedge *PQhash;
struct	Halfedge *PQfind();
extern int PQcount;
extern int PQmin;
int PQempty();

char *memmalloc();

/* free lists */
extern struct Triple *tfl;
extern struct Halfedge *hfl;
extern struct Site *sfl;
extern struct Edge *efl;
