/* $Id$ */

#include "defs.h"

#if __cplusplus
extern "C" {
#endif

int triangulate, sorted, plot, debug;
float xmin, xmax, ymin, ymax, deltax, deltay;
struct	Site	*sites;
int		nsites;
int		siteidx;
int		sqrt_nsites;
int		nvertices;
struct	Site	*bottomsite;
struct Triple *triples;
int nedges;
struct	Halfedge *ELleftend, *ELrightend;
int 	ELhashsize;
struct	Halfedge **ELhash;
int PQhashsize;
struct	Halfedge *PQhash;
int PQcount;
int PQmin;
struct Triple *tfl;
struct Halfedge *hfl;
struct Site *sfl;
struct Edge *efl;

#if __cplusplus
} /* extern "C" */
#endif
