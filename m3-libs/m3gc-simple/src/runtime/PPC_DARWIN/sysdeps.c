
#include <stdarg.h>
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <sys/uio.h>

typedef void (*RTHEAPREP_FAULT_PROC)(char*);
typedef void (*RTCSRC_FINISHVM_PROC)();

static RTHEAPREP_FAULT_PROC fault_proc;
static RTCSRC_FINISHVM_PROC finishvm_proc;

RTHEAPREP_FAULT_PROC get_RTHEAPREP_FAULT_PROC()
{
  return fault_proc;
}

void set_RTHEAPREP_FAULT_PROC(RTHEAPREP_FAULT_PROC p)
{
  fault_proc = p;
}

RTCSRC_FINISHVM_PROC get_RTCSRC_FINISHVM_PROC()
{
  return finishvm_proc;
}

void set_RTCSRC_FINISHVM_PROC(RTCSRC_FINISHVM_PROC p)
{
  finishvm_proc = p;
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
