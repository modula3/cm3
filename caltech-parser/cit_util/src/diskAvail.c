// $Id$

#if !_WIN32

// statvfs is Posix
// statfs is the historical code here, since 2019
// and does work on some systems.
// It does not always work where documented, e.g. NetBSD.
// More testing is needed here.
#if defined(__NetBSD__)
#include <sys/statvfs.h>
#elif defined(linux)
#include <sys/statfs.h>
#endif

#include <sys/param.h>
#include <sys/mount.h>

#endif

#include <assert.h>

#if __cplusplus
extern "C" {
#endif

int
diskAvail(const char * path,
	  int *bsize,
	  double *total, double *avail, double *availNonSuperUser) 
{
#if !_WIN32
  // TODO: This is Posix.
  // Check other systems (FreeBSD, OpenBSD, Cygwin, Linux, Mac, Solaris, AIX, Irix, HP-UX, OSF/1, OpenVMS, etc.)
  // Autoconf would be good here.
#if defined(__NetBSD__)
  struct statvfs buf;

  if (statvfs(path,&buf)<0) return -1;
#else
  struct statfs buf;

  if (statfs(path,&buf)<0) return -1;
#endif
  *bsize = buf.f_bsize;
  *total = buf.f_blocks;
  *avail = buf.f_bfree;
  *availNonSuperUser = buf.f_bavail;
  return 0;

#else
  // TODO GetDiskFreeSpaceEx and/or refactor

  assert (0);
  *bsize = 0;
  *total = 0;
  *avail = 0;
  return -1;

#endif
}

#if __cplusplus
} // extern "C"
#endif
