
#include <stdarg.h>
#include <sys/types.h>
#include <errno.h>
#if __FreeBSD__ >= 2
#include <sys/sysctl.h>
#include <osreldate.h>
#endif
#include <stdio.h>
#include <sys/uio.h>

void (*RTHeapRep_Fault)(char*);
void (*RTCSRC_FinishVM)();

int uopen(const char* path, int flags, mode_t mode)
{ int result;
  result = open(path, flags, mode);
  return result;
}

int ufcntl(int fd, int request, int arg)
{ int result;
  result = fcntl(fd, request, arg);
  return result;
}
