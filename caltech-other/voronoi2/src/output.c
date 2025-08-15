#
#include "defs.h"
#include "mymalloc.h"
#include <stdio.h>
/* for those who don't have Cherry's plot */
/* #include <plot.h> */
openpl(){}
line(){}
circle(){}
range(){}

static float pxmin, pxmax, pymin, pymax, cradius;

out_bisector(e)
struct Edge *e;
{
if(triangulate & plot &!debug)
	line(e->reg[0]->coord.x, e->reg[0]->coord.y, 
	     e->reg[1]->coord.x, e->reg[1]->coord.y);
if(!triangulate & !plot &!debug)
	printf("l %f %f %f", e->a, e->b, e->c);
if(debug)
	printf("line(%d) %gx+%gy=%g, bisecting %d %d\n", e->edgenbr,
	    e->a, e->b, e->c, e->reg[le]->sitenbr, e->reg[re]->sitenbr);
}


out_ep(e)
struct Edge *e;
{
if(!triangulate & plot) 
	clip_line(e);
if(!triangulate & !plot)
{	printf("e %d", e->edgenbr);
	printf(" %d ", e->ep[le] != (struct Site *)NULL ? e->ep[le]->sitenbr : -1);
	printf("%d\n", e->ep[re] != (struct Site *)NULL ? e->ep[re]->sitenbr : -1);
};
}

out_vertex(v)
struct Site *v;
{
if(!triangulate & !plot &!debug)
	printf ("v %f %f\n", v->coord.x, v->coord.y);
if(debug)
	printf("vertex(%d) at %f %f\n", v->sitenbr, v->coord.x, v->coord.y);
}


out_site(s)
struct Site *s;
{
if(!triangulate & plot & !debug)
	circle (s->coord.x, s->coord.y, cradius);
if(!triangulate & !plot & !debug)
	printf("s %f %f\n", s->coord.x, s->coord.y);
if(debug)
	printf("site (%d) at %f %f\n", s->sitenbr, s->coord.x, s->coord.y);
}

#define TALLOC 100
static int tp=0;

out_triple(s1, s2, s3)
struct Site *s1, *s2, *s3;
{
  struct Triple *t;

  if (!tfl || tp==TALLOC) {
    tfl=(struct Triple *)memmalloc(sizeof(struct Triple)*TALLOC);
    tp=0;
  }
  t=(struct Triple *)tfl+(tp++);

  t->next=triples;
  t->d.s1 = s1->sitenbr;
  t->d.s2 = s2->sitenbr;
  t->d.s3 = s3->sitenbr;

  triples = t;

}



plotinit()
{
float dx,dy,d;

dy = ymax - ymin;
dx = xmax - xmin;
d = ( dx > dy ? dx : dy) * 1.1;
pxmin = xmin - (d-dx)/2.0;
pxmax = xmax + (d-dx)/2.0;
pymin = ymin - (d-dy)/2.0;
pymax = ymax + (d-dy)/2.0;
cradius = (pxmax - pxmin)/350.0;
openpl();
range(pxmin, pymin, pxmax, pymax);
}


int clip_line(e)
struct Edge *e;
{
struct Site *s1, *s2;
float x1,x2,y1,y2;

	if(e -> a == 1.0 && e ->b >= 0.0)
	{	s1 = e -> ep[1];
		s2 = e -> ep[0];
	}
	else 
	{	s1 = e -> ep[0];
		s2 = e -> ep[1];
	};

	if(e -> a == 1.0)
	{
		y1 = pymin;
		if (s1!=(struct Site *)NULL && s1->coord.y > pymin)
			 y1 = s1->coord.y;
		if(y1>pymax) return;
		x1 = e -> c - e -> b * y1;
		y2 = pymax;
		if (s2!=(struct Site *)NULL && s2->coord.y < pymax) 
			y2 = s2->coord.y;
		if(y2<pymin) return(0);
		x2 = e -> c - e -> b * y2;
		if ((x1> pxmax & x2>pxmax) | (x1<pxmin&x2<pxmin)) return;
		if(x1> pxmax)
		{	x1 = pxmax; y1 = (e -> c - x1)/e -> b;};
		if(x1<pxmin)
		{	x1 = pxmin; y1 = (e -> c - x1)/e -> b;};
		if(x2>pxmax)
		{	x2 = pxmax; y2 = (e -> c - x2)/e -> b;};
		if(x2<pxmin)
		{	x2 = pxmin; y2 = (e -> c - x2)/e -> b;};
	}
	else
	{
		x1 = pxmin;
		if (s1!=(struct Site *)NULL && s1->coord.x > pxmin) 
			x1 = s1->coord.x;
		if(x1>pxmax) return(0);
		y1 = e -> c - e -> a * x1;
		x2 = pxmax;
		if (s2!=(struct Site *)NULL && s2->coord.x < pxmax) 
			x2 = s2->coord.x;
		if(x2<pxmin) return(0);
		y2 = e -> c - e -> a * x2;
		if ((y1> pymax & y2>pymax) | (y1<pymin&y2<pymin)) return(0);
		if(y1> pymax)
		{	y1 = pymax; x1 = (e -> c - y1)/e -> a;};
		if(y1<pymin)
		{	y1 = pymin; x1 = (e -> c - y1)/e -> a;};
		if(y2>pymax)
		{	y2 = pymax; x2 = (e -> c - y2)/e -> a;};
		if(y2<pymin)
		{	y2 = pymin; x2 = (e -> c - y2)/e -> a;};
	};
	
	line(x1,y1,x2,y2);
}
