/* $Id$ */

#include <stddef.h>

#if __cplusplus
extern "C" {
#endif

/* TODO This is not portable */
#define DELETED ((void*)-2)

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

int has_endpoint();
int right_of(struct Halfedge *el, struct Point *p);
struct Site *intersect(struct Halfedge*, struct Halfedge*);
float dist(struct Site *s, struct Site *t);
struct Point PQ_min();
struct Halfedge *PQextractmin();
struct Edge *bisect(struct Site*, struct Site*);

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
struct Halfedge *ELleft(struct Halfedge*);
struct Halfedge *ELleftbnd(struct Point*);
struct Halfedge *HEcreate(struct Edge *e, int pm);
struct Halfedge *ELright(struct Halfedge*);
struct Site *leftreg(struct Halfedge*);
struct Site *rightreg(struct Halfedge*);

extern int PQhashsize;
extern struct	Halfedge *PQhash;
struct	Halfedge *PQfind();
extern int PQcount;
extern int PQmin;
int PQempty();

char *memmalloc(size_t);

/* free lists */
extern struct Triple *tfl;
extern struct Halfedge *hfl;
extern struct Site *sfl;
extern struct Edge *efl;

void out_ep(struct Edge*);
void PQdelete(struct Halfedge*);
void PQinsert(struct Halfedge *he, struct Site *v, float offset);
void ELinsert(struct Halfedge *lb, struct Halfedge *new_);
void endpoint(struct Edge *e, int lr, struct Site *s);
void ELdelete(struct Halfedge*);
void makevertex(struct Site*);
void ELinitialize();
void PQinitialize();
void voronoi(int triangulate, struct Site *(*nextsite)());
void geominit();
void clear_triples();
int PQbucket(struct Halfedge*);
void out_bisector(struct Edge*);
void out_triple(struct Site*, struct Site*, struct Site*);
void out_site(struct Site*);
void clip_line(struct Edge*);
void out_vertex(struct Site*);

#if __cplusplus
} /* extern "C" */
#endif
