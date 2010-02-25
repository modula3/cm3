#ifndef __INTERIX

/* avoid empty file */

void ThreadInterix__Dummy(void)
{
}

#else

#include "m3core.h"
#include <sys/procfs.h>

static void Close(int* inoutFile)
{
    int file = *inoutFile;
    *inoutFile = -1;
    if (file >= 0)
        close(file);
}

int ThreadPThread__OpenInterixFiles(m3_pthread_t m3pthread,
                                    int *outStatusFile,
                                    int *outControlFile)
{
    long pid = getpid();
    long pthread = PTHREAD_FROM_M3(m3pthread);
    char buffer[255];
    int i = 0;
    int controlFile = -1;
    int statusFile = -1;

    *outStatusFile = -1;
    *outControlFile = -1;

    i = snprintf(buffer, sizeof(buffer), "/proc/%ld/lwp/%ld/lwpstatus", pid, pthread);
    assert(i > 0 && i < sizeof(buffer));
    if (i < 0 || i >= sizeof(buffer))
        goto InternalError;

    controlFile = open(buffer, O_RDONLY);
    if (controlFile < 0)
        goto Error;
    
    snprintf(buffer, sizeof(buffer), "/proc/%ld/lwp/%ld/lwpctl", pid, pthread);
    assert(i > 0 && i < sizeof(buffer));
    if (i < 0 || i >= sizeof(buffer))
        goto InternalError;

    statusFile = open(buffer, O_WRONLY);
    if (statusFile < 0)
        goto Error;

    *outStatusFile = statusFile;
    *outControlFile = controlFile;
    statusFile = -1;
    controlFile = -1;
    i = 0;
Exit:
    Close(&statusFile);
    Close(&controlFile);
    return i;
Error:
    i = errno;
    if (i == 0)
        goto InternalError;
    goto Exit;
InternalError:
    i = EINVAL;
    goto Exit;
}

int
ThreadPThread__SuspendThread (m3_pthread_t m3pthread,
                             int controlFile)
{
  const static PROC_CTL_WORD_TYPE stop[] = { PCSTOP };
  int i = write(controlFile, stop, sizeof(stop));
  return (i == sizeof(stop));
}

int
ThreadPThread__RestartThread (m3_pthread_t m3pthread,
                             int controlFile)
{
  const static PROC_CTL_WORD_TYPE run[] = { PCRUN, 0 };
  int i = write(controlFile, run, sizeof(run));
  return (i == sizeof(run));
}

void
ThreadPThread__ProcessStopped (m3_pthread_t mt, void *bottom, void *context,
                              void (*p)(void *start, void *limit),
                              int statusFile)
{
  lwpstatus_t lwpstatus;
  gregset_t* regs = &lwpstatus.pr_context.uc_mcontext.gregs;
  long j;
  off_t off;

  assert(context == 0);

  off = lseek(statusFile, 0, L_SET); /* seek is necessary and pread doesn't work */
  assert(off == 0);
  j = read(statusFile, &lwpstatus, sizeof(lwpstatus));
  assert(j == sizeof(lwpstatus));

  /* process the stack */

#if 0
  assert(stack_grows_down); /* See ThreadPThreadC.c */
#endif

#ifdef _X86_
  p((void*)regs->gregs[UESP], bottom);
#else
  /* unknown processor */
#endif

  /* process the registers */

#ifdef _X86_
  p(&regs->gregs, ((char *)&regs->gregs) + sizeof(regs->gregs));
#else
  /* unknown processor */
#endif
}

#endif
