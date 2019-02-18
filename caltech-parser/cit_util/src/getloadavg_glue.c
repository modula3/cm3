#include <stdlib.h>

int
getloadavg_glue(double *loadavg, int which) 
{
  double la[3];
  int g=getloadavg(la,3);

  if (which>=g) return -1;

  *loadavg = la[which];
  return which;
}
