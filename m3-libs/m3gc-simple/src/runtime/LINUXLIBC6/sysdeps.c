
#include <stdarg.h>
#include <sys/types.h>
#include <sys/time.h>
#include <errno.h>
#include <syscall.h>
#include <sys/file.h>
#include <sys/msg.h>
#include <sys/sem.h>
#include <sys/signal.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <sys/ipc.h>
#include <dirent.h>
#include <sys/times.h>
#include <sys/resource.h>
#include <sys/wait.h>

#if __GLIBC__ >= 2 && __GLIBC_MINOR__ >= 1
#include <asm/ipc.h>
#endif

void (*RTHeapRep_Fault)(char*);
void (*RTCSRC_FinishVM)();

int __real_adjtime(const struct timeval *delta, struct timeval *olddelta);

int __wrap_adjtime(const struct timeval *delta, struct timeval *olddelta)
{ int result;

  result = __real_adjtime(delta, olddelta);
  return result;
}

ssize_t __real_readv(int d, const struct iovec *iov, int count);

ssize_t __wrap_readv(int d, const struct iovec *iov, int count)
{ int result;

  result = __real_readv(d, iov, count);
  return result;
}

int __real_utimes(const char *file, struct timeval *tvp);

int __wrap_utimes(const char *file, struct timeval *tvp)
{ int result;

  result = __real_utimes(file, tvp);
  return result;
}

pid_t __real_wait3(union wait *status, int options, struct rusage *rusage);

pid_t __wrap_wait3(union wait *status, int options, struct rusage *rusage)
{ int result;

  result = __real_wait3(status, options, rusage);
  return result;
}

int pthread_equal (pthread_t thread1, pthread_t thread2) {
  return thread1 == thread2;
}
