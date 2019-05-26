/* $Id$ */

#if _WIN32
#include <windows.h>

#else

#ifdef linux
# include <sys/statfs.h>
#endif

#include <sys/param.h>
#include <sys/mount.h>

#endif

int
diskAvail(const char * path, double * fraction_available)
{
#if _WIN32
  ULARGE_INTEGER availableToCaller = { 0 };
  ULARGE_INTEGER total = { 0 };
  ULARGE_INTEGER totalFree = { 0 };

  if (!GetDiskFreeSpaceExA (path, &availableToCaller, &total, &totalFree))
    return -1;

  *fraction_available = (double)availableToCaller.QuadPart / (double)total.QuadPart;

#else

  struct statfs buf;

  if (statfs(path,&buf)<0) return -1;
  else {
    const double avail = buf.f_bfree;
    const double total = buf.f_blocks;
    const double availNonSuperUser = buf.f_bavail;
    const double superPart = avail - availNonSuperUser;
    const double nonSuperTotal = total - superPart;
    *fraction_available = availNonSuperUser / nonSuperTotal;
  }

#endif

  return 0;
}
