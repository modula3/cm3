#include <stdarg.h>
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <sys/uio.h>
#include <signal.h>

static int (*RTHeapRep_Fault)(void *, int);
static void (*RTCSRC_FinishVM)();

void set_RTHeapRep_Fault(void *p) {
  RTHeapRep_Fault = p;
}

void set_RTCSRC_FinishVM(void *p) {
  RTCSRC_FinishVM = p;
}

int
m3_fcntl(int fd, int request, int arg)
{
  return fcntl(fd, request, arg);
}

int
m3_open(const char* path, int flags, mode_t mode)
{
  return open(path, flags, mode);
}

int
m3_sigaction(int sig, const struct sigaction *act, struct sigaction *oact)
{
  return sigaction(sig, act, oact);
}
