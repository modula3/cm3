#ifdef linux
# include <sys/statfs.h>
#endif

#include <sys/param.h>
#include <sys/mount.h>

/* $Id$ */

int
diskAvail(const char * path,
	  int *bsize,
	  double *total, double *avail, double *availNonSuperUser) 
{
  struct statfs buf;

  if (statfs(path,&buf)<0) return -1;

  *bsize = buf.f_bsize;
  *total = buf.f_blocks;
  *avail = buf.f_bfree;
  *availNonSuperUser = buf.f_bavail;

  return 0;
}

