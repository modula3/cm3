/* $Id$ */

#include <stddef.h>

#if __cplusplus
extern "C" {
#endif

/* TODO This is not portable */
#define DELETED ((void*)-2)

extern int triangulate, sorted, plot, debug;

extern float xmin, xmax, ymin, ymax, deltax, deltay;

/* Forward declare structs. */
struct Edge;
struct Halfedge;
struct Point;
struct Site;
struct Triple;
struct TripleArg;

/* Typedef structs to plain names. */
typedef struct Edge Edge;
typedef struct Halfedge Halfedge;
typedef struct Point Point;
typedef struct Site Site;
typedef struct Triple Triple;
typedef struct TripleArg TripleArg;

struct Point {
    float x,y;
};

/* structure used both for sites and for vertices */
struct Site	{
Point	coord;
int		sitenbr;
};

extern Site	    *sites;
extern int		nsites;
extern int		siteidx;
extern int		sqrt_nsites;
extern int		nvertices;
extern Site	*bottomsite;

struct TripleArg {
  int s1, s2, s3;
};

struct Triple {
  TripleArg d;
  Triple *next;
};

extern Triple *triples;

struct Edge	{
float		a,b,c;
Site 	*ep[2];
Site	*reg[2];
int		edgenbr;
};

#define le 0
#define re 1
extern int nedges;

int has_endpoint(void);
int right_of(Halfedge *el, Point *p);
Site *intersect(Halfedge*, Halfedge*);
float dist(Site *s, Site *t);
Point PQ_min(void);
Halfedge *PQextractmin(void);
Edge *bisect(Site*, Site*);

struct Halfedge {
Halfedge *ELleft;
Halfedge *ELright;
Edge     *ELedge;
char      ELpm;
Site     *vertex;
float     ystar;
Halfedge *PQnext;
};

extern Halfedge *ELleftend, *ELrightend;
extern int 	ELhashsize;
extern Halfedge **ELhash;
Halfedge *ELleft(Halfedge*);
Halfedge *ELleftbnd(Point*);
Halfedge *HEcreate(Edge *e, int pm);
Halfedge *ELright(Halfedge*);
Site *leftreg(Halfedge*);
Site *rightreg(Halfedge*);

extern int PQhashsize;
extern Halfedge *PQhash;
Halfedge *PQfind(void);
extern int PQcount;
extern int PQmin;
int PQempty(void);

char *memmalloc(size_t);

/* free lists */
extern Triple *tfl;
extern Halfedge *hfl;
extern Site *sfl;
extern Edge *efl;

void out_ep(Edge*);
void PQdelete(Halfedge*);
void PQinsert(Halfedge *he, Site *v, float offset);
void ELinsert(Halfedge *lb, Halfedge *new_);
void endpoint(Edge *e, int lr, Site *s);
void ELdelete(Halfedge*);
void makevertex(Site*);
void ELinitialize(void);
void PQinitialize(void);
void voronoi(int triangulate, Site *(*nextsite)(void));
void geominit(void);
void clear_triples(void);
int PQbucket(Halfedge*);
void out_bisector(Edge*);
void out_triple(Site*, Site*, Site*);
void out_site(Site*);
void clip_line(Edge*);
void out_vertex(Site*);

#if __cplusplus
} /* extern "C" */
#endif
