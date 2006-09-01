#include <stdarg.h>
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <sys/uio.h>
#include <signal.h>

/* low-level runtime lock */
int ThreadF__inCritical = 0;

void set_RTHeapRep_Fault(void *p)
{
/* Do nothing.
  This function exists for interface compatibility with m3gc-enhanced.
*/
}

void set_RTCSRC_FinishVM(void *p)
{
/* Do nothing.
  This function exists for interface compatibility with m3gc-enhanced.
*/
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
