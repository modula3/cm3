#include <algorithm.h>
#include <R2.h>
#include <Math.h>

#define MaxDistance 1.0E30

typedef Link = struct Linkstruct {
    Point p;
    Link next;
    } * ;

float min = 100.0;
Link h,z;
int N   = 0.0;

@CheckforMin
check_for_min (p1, p2)@
point p1,p2;
{
@5    if (dist(p1,p2) < min)@
@10      update_min(p1,p2);@
}
@CheckforMin

@FindClosestPair
link find_closest_pair (c,N)@
link c;
int N;
{
  link a, b, t1, t2;
  int i;
  real middle_xcoord;
  point p1, p2, p3, p4;

@5   a = c;@
@10   b = middle_of_list(c,N);@
@15   middle_xcoord = b -> p.x;@

@20   t1 = find_closest_pair(a, N div 2);@
@25   t2 = find_closest_pair(b, N - (N div 2));@

@30   c = merge_lists(t1, t2);@

@35   a = c;@
  repeat
  {
@40     if ((abs(a -> p.x - middle_xcoord) < min) && (p1 != z -> p)) @
      {
@45       check_for_min(a -> p, p1);@
@50       check_for_min(a -> p, p2);@
@55       check_for_min(a -> p, p3);@
@60       check_for_min(a -> p, p4);@
@65       p1 = p2; p2 = p3; p3 = p4; p4 = a -> p;@
      }
@70     a = a -> next;@
  }
@75   until a = z;@
@80   return c;@
}
@FindClosestPair
 
@Main
main()  /* Main program: computes closest pair in given set of points */@
{
@5  initialize_list_of_points(h,&N);@
@10  sort_by_X(h, N);@
@15  find_closest_pair(h, N);@
}
@Main
