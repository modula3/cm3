/* $Id$ */

#include "defs.h"

#if __cplusplus
extern "C" {
#endif

int triangulate, sorted, plot, debug;
float xmin, xmax, ymin, ymax, deltax, deltay;
Site	*sites;
int		nsites;
int		siteidx;
int		sqrt_nsites;
int		nvertices;
Site	*bottomsite;
Triple *triples;
int nedges;
Halfedge *ELleftend, *ELrightend;
int 	ELhashsize;
Halfedge **ELhash;
int PQhashsize;
Halfedge *PQhash;
int PQcount;
int PQmin;
Triple *tfl;
Halfedge *hfl;
Site *sfl;
Edge *efl;

#if __cplusplus
} /* extern "C" */
#endif
