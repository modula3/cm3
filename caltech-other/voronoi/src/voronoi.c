#
#include "defs.h"


/* implicit parameters: nsites, sqrt_nsites, xmin, xmax, ymin, ymax,
   deltax, deltay (can all be estimates).
   Performance suffers if they are wrong; better to make nsites,
   deltax, and deltay too big than too small.  (?) */

voronoi(triangulate, nextsite)
int triangulate;
struct Site *(*nextsite)();
{
struct Site *newsite, *bot, *top, *temp, *p;
struct Site *v;
struct Point newintstar;
int pm;
struct Halfedge *lbnd, *rbnd, *llbnd, *rrbnd, *bisector;
struct Edge *e;


PQinitialize();
bottomsite = (*nextsite)();
out_site(bottomsite);
ELinitialize();

newsite = (*nextsite)();
while(1)
{
	if(!PQempty()) newintstar = PQ_min();

	if (newsite != (struct Site *)NULL 
	   && (PQempty() 
		 || newsite -> coord.y < newintstar.y
	 	 || (newsite->coord.y == newintstar.y 
		     && newsite->coord.x < newintstar.x)))
	{/* new site is smallest */
		out_site(newsite);
		lbnd = ELleftbnd(&(newsite->coord));
		rbnd = ELright(lbnd);
		bot = rightreg(lbnd);
		e = bisect(bot, newsite);
		bisector = HEcreate(e, le);
		ELinsert(lbnd, bisector);
		if ((p = intersect(lbnd, bisector)) != (struct Site *) NULL) 
		{	PQdelete(lbnd);
			PQinsert(lbnd, p, dist(p,newsite));
		};
		lbnd = bisector;
		bisector = HEcreate(e, re);
		ELinsert(lbnd, bisector);
		if ((p = intersect(bisector, rbnd)) != (struct Site *) NULL)
		{	PQinsert(bisector, p, dist(p,newsite));	
		};
		newsite = (*nextsite)();	
	}
	else if (!PQempty()) 
	/* intersection is smallest */
	{	lbnd = PQextractmin();
		llbnd = ELleft(lbnd);
		rbnd = ELright(lbnd);
		rrbnd = ELright(rbnd);
		bot = leftreg(lbnd);
		top = rightreg(rbnd);
		out_triple(bot, top, rightreg(lbnd));
		v = lbnd->vertex;
		makevertex(v);
		endpoint(lbnd->ELedge,lbnd->ELpm,v);
		endpoint(rbnd->ELedge,rbnd->ELpm,v);
		ELdelete(lbnd); 
		PQdelete(rbnd);
		ELdelete(rbnd); 
		pm = le;
		if (bot->coord.y > top->coord.y)
		{	temp = bot; bot = top; top = temp; pm = re;}
		e = bisect(bot, top);
		bisector = HEcreate(e, pm);
		ELinsert(llbnd, bisector);
		endpoint(e, re-pm, v);
		if((p = intersect(llbnd, bisector)) != (struct Site *) NULL)
		{	PQdelete(llbnd);
			PQinsert(llbnd, p, dist(p,bot));
		};
		if ((p = intersect(bisector, rrbnd)) != (struct Site *) NULL)
		{	PQinsert(bisector, p, dist(p,bot));
		};
	}
	else break;
};

for(lbnd=ELright(ELleftend); lbnd != ELrightend; lbnd=ELright(lbnd))
	{	e = lbnd -> ELedge;
		out_ep(e);
	};
}
