#include <stdarg.h>
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <sys/uio.h>

static int (*RTHeapRep_Fault)(void *, int);
static void (*RTCSRC_FinishVM)();

void set_RTHeapRep_Fault(void *p) {
  RTHeapRep_Fault = p;
}

void set_RTCSRC_FinishVM(void *p) {
  RTCSRC_FinishVM = p;
}

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
