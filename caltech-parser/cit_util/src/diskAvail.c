#if !_WIN32

#ifdef linux
# include <sys/statfs.h>
#endif

#include <sys/param.h>
#include <sys/mount.h>

#endif

#include <assert.h>

/* $Id$ */

#if __cplusplus
extern "C" {
#endif

int
diskAvail(const char * path,
	  int *bsize,
	  double *total, double *avail, double *availNonSuperUser) 
{
#if !_WIN32
  struct statfs buf;

  if (statfs(path,&buf)<0) return -1;

  *bsize = buf.f_bsize;
  *total = buf.f_blocks;
  *avail = buf.f_bfree;
  *availNonSuperUser = buf.f_bavail;
  return 0;

#else
  /* TODO GetDiskFreeSpaceEx and/or refactor */

  assert (0);
  *bsize = 0;
  *total = 0;
  *avail = 0;
  return -1;

#endif
}

#if __cplusplus
} /* extern "C" */
#endif
