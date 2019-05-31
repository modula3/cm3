#include <assert.h>
#include <stdlib.h>

#if __cplusplus
extern "C" {
#endif

int
getloadavg_glue(double *loadavg, int which) 
{
#if _WIN32
  assert (0);
  return -1;
#else
  double la[3];
  int g=getloadavg(la,3);

  if (which>=g) return -1;

  *loadavg = la[which];
  return which;
#endif
}

#if __cplusplus
} /* extern "C" */
#endif
