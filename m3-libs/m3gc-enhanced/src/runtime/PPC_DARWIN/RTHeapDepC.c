/* Copyright (C) 1992, Digital Equipment Corporation */
/* All rights reserved. */
/* See the file COPYRIGHT for a full description. */

/*      modified on Tue Feb  2 11:15:57 PST 1993 by jdd */

/*
 * ow Sat Oct 29 14:10:19 MET 1994
 * ow Sun Nov  6 16:39:26 MET 1994
 * ow Sun Dec  4 17:58:49     1994 changes for FreeBSD 2.0
 *
 * I just tried to check the calls implemented for Ultrix,
 * (almost) nothing else has been changed or added.
 * So be careful to use any other system calls, some are
 * probably missing.
 */
/* This file implements wrappers for almost all Ultrix system calls
   that take pointers as arguments.  These wrappers allow the system
   calls to take arguments that might point to the traced heap, which
   may be VM-protected in the Ultrix implementation of the collector.
   The wrappers read and write the referents of all pointers about to
   passed to the system call, which ensures that the pages are not
   protected when the call is made.

   Each wrapper is a critical section, with RT0u__inCritical non-zero,
   so that another thread cannot cause the pages to become reprotected
   before the system call is performed.

   A few system calls are not handled here, or are handled only
   partially.  This restricts the system calls that can be made from
   Modula-3, or from libraries that are passed pointers into the
   Modula-3 traced heap.  These system calls are:

   1) sigvec.  Sigvec takes 3 arguments, but passes 4 to the kernel.
      The extra argument is the address of the sigtramp code, which is
      copyrighted.  The sigtramp code appears in the the standard .o
      file that also defines sigvec, so that sigvec cannot be
      redefined here without including sigtramp.  Rewriting sigtramp
      seemed too error-prone, so sigvec is not supported here, meaning
      that sigvec cannot be called with arguments on the heap.

   2) syscall.  Implementing syscall would require a huge case
      statement, with one case per system call.  This seemed too
      error-prone, so syscall cannot take arguments that point into
      the traced heap.

   3) ioctl.  Ioctl's third argument may be a pointer, and some device
      drivers might interpret the referent as containing more
      pointers.  These second-level pointers are not handled here, so
      they must not point into the traced heap if they exist.
      Handling this problem in general is impossible, since the set of
      device drivers is open-ended.

   4) profil.  The memory referenced by the "buff" argument is updated
      after the call returns, and there is no mechanism to permanently
      unprotect it.

   5) Undocumented system calls.  There are private system calls with
      no manual pages, so it was impossible to write wrappers.

   6) audgen, whose manpage is incomprehensible.

   (Some calls in Section 2 are already wrappers for other system
   calls; it is not necessary to reimplement them here.)

   Also, longjmp must not be used from a signal handler to abnormally
   exit from a system call.

   Finally, if a system call references an object on the heap, each
   pointer must reference only one object.  Therefore, it is not
   possible to write the heap contents with a single write. */

#define MFS 1
#define NFS
#include <stdarg.h>
#include <sys/types.h>
#include <errno.h>
#include <sys/syscall.h>
#include <sys/file.h>
#include <sys/param.h>
#include <sys/sysctl.h>
#include <sys/mount.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <semaphore.h>
#include <ufs/ufs/quota.h>
#include <sys/signal.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <tzfile.h>
#include <nfs/rpcv2.h>
#include <nfs/nfsproto.h>
#include <nfs/nfs.h>
#include <ufs/ufs/ufsmount.h>

#include <string.h>
#include <unistd.h>

#if __FreeBSD_version >= 400013
#define SOCKLEN_T       socklen_t
#else
#define SOCKLEN_T       int
#endif

#ifdef   NULL
#undef   NULL
#endif
#define  NULL (void *)(0)
extern long RT0u__inCritical;
#define ENTER_CRITICAL RT0u__inCritical++
#define EXIT_CRITICAL  RT0u__inCritical--

static int (*RTHeapRep_Fault)(void *);
static void (*RTCSRC_FinishVM)();

void set_RTHeapRep_Fault(void *p) {
  RTHeapRep_Fault = p;
}

void set_RTCSRC_FinishVM(void *p) {
  RTCSRC_FinishVM = p;
}

static char RTHeapDepC__c;
#define MAKE_READABLE(x) if (x != 0) { RTHeapDepC__c = *(char*)(x); }
#define MAKE_WRITABLE(x) if (x != 0) { *(char*)(x) = RTHeapDepC__c = *(char*)(x); }

/* Unless otherwise noted, all the following wrappers have the same
   structure. */

int
accept(int s, struct sockaddr *addr, int *addrlen)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(addr);
  MAKE_WRITABLE(addrlen);
  result = syscall(SYS_accept, s, addr, addrlen);
  EXIT_CRITICAL;
  return result;
}

int
access(const char *path, int mode)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_access, path, mode);
  EXIT_CRITICAL;
  return result;
}

int
acct(const char *file)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(file);
  result = syscall(SYS_acct, file);
  EXIT_CRITICAL;
  return result;
}

int
adjtime(const struct timeval *delta, struct timeval *olddelta)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(delta);
  MAKE_WRITABLE(olddelta);
  result = syscall(SYS_adjtime, delta, olddelta);
  EXIT_CRITICAL;
  return result;
}

int
bind(int s, const struct sockaddr *name, int namelen)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name);
  result = syscall(SYS_bind, s, name, namelen);
  EXIT_CRITICAL;
  return result;
}

int chdir(const char *path)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_chdir, path);
  EXIT_CRITICAL;
  return result;
}

int
chflags(const char *path, u_long flags)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_chflags, path, flags);
  EXIT_CRITICAL;
  return result;
}

int
chmod(const char *path, mode_t mode)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_chmod, path, mode);
  EXIT_CRITICAL;
  return result;
}

int
chown(const char *path, uid_t owner, gid_t group)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_chown, path, owner, group);
  EXIT_CRITICAL;
  return result;
}

int
chroot(const char *dirname)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(dirname);
  result = syscall(SYS_chroot, dirname);
  EXIT_CRITICAL;
  return result;
}

int
connect(int s, const struct sockaddr *name, int namelen)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name);
  result = syscall(SYS_connect, s, name, namelen);
  EXIT_CRITICAL;
  return result;
}

/* execve is implemented differently since it does not return, which
   would leave RT0u__inCritical set in the parent if called in the child
   of a vfork. Many calls leave the process in an undefined state in the
   case of EFAULT, but we assume that execve is not one of these. */

int
execve(const char *path, char *const argv[], char *const envp[])
{
  int result;

  for (;;) {
    result = syscall(SYS_execve, path, argv, envp);
    if (result == -1 && errno == EFAULT) {
      MAKE_READABLE(path);
      { char * const *a; for (a = argv; *a; a++) MAKE_READABLE(*a); }
      { char * const *e; for (e = envp; *e; e++) MAKE_READABLE(*e); }
    } else {
      return result;
    }
  }
}

int
m3_fcntl(int fd, int request, int arg)
{
  int result;

  ENTER_CRITICAL;
  switch (request) {
  case F_GETLK:
    MAKE_WRITABLE(arg);
    break;
  case F_SETLK:
  case F_SETLKW:
    MAKE_READABLE(arg);
    break;
  default:
    break;
  }
  result = syscall(SYS_fcntl, fd, request, arg);
  EXIT_CRITICAL;
  return result;
}

int
fsctl (const char *path, unsigned long request, void *data,
       unsigned long option)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(data);
  result = syscall(SYS_fsctl, path, request, data, option);
  EXIT_CRITICAL;
  return result;
}

int
fstat(int fd, struct stat *buf)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = syscall(SYS_fstat, fd, buf);
  EXIT_CRITICAL;
  return result;
}

int
fstatfs(int fd, struct statfs *buf)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = syscall(SYS_fstatfs, fd, buf);
  EXIT_CRITICAL;
  return result;
}

int
futimes(int fd, const struct timeval *times)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(times);
  result = syscall(SYS_futimes, fd, times);
  EXIT_CRITICAL;
  return result;
}

int getdirentries(int fd, char *buf, int nbytes, long *basep)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  MAKE_WRITABLE(basep);
  result = syscall(SYS_getdirentries, fd, buf, nbytes, basep);
  EXIT_CRITICAL;
  return result;
}

int
getfh(const char *path, fhandle_t *fhp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(fhp);
  result = syscall(SYS_getfh, path, fhp);
  EXIT_CRITICAL;
  return result;
}

int
getfsstat(struct statfs *buf, long bufsize, int flags)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = syscall(SYS_getfsstat, buf, bufsize, flags);
  EXIT_CRITICAL;
  return result;
}

int
getgroups(int gidsetlen, gid_t *gidset)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(gidset);
  result = syscall(SYS_getgroups, gidsetlen, gidset);
  EXIT_CRITICAL;
  return result;
}

int
getitimer(int which, struct itimerval *value)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(value);
  result = syscall(SYS_getitimer, which, value);
  EXIT_CRITICAL;
  return result;
}

int
getpeername(int s, struct sockaddr *name, int *namelen)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(name);
  MAKE_WRITABLE(namelen);
  result = syscall(SYS_getpeername, s, name, namelen);
  EXIT_CRITICAL;
  return result;
}

int
getrlimit(int resource, struct rlimit *rlp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(rlp);
  result = syscall(SYS_getrlimit, resource, rlp);
  EXIT_CRITICAL;
  return result;
}

int
getrusage(int who, struct rusage *rusage)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(rusage);
  result = syscall(SYS_getrusage, who, rusage);
  EXIT_CRITICAL;
  return result;
}

int
getsockname(int s, struct sockaddr *name, int *namelen)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(name);
  MAKE_WRITABLE(namelen);
  result = syscall(SYS_getsockname, s, name, namelen);
  EXIT_CRITICAL;
  return result;
}

int
getsockopt(int s, int level, int optname, void *optval, int *optlen)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(optval);
  MAKE_WRITABLE(optlen);
  result = syscall(SYS_getsockopt, s, level, optname, optval, optlen);
  EXIT_CRITICAL;
  return result;
}

static int
_gettimeofday(struct timeval *tp, struct timezone *tzp)
{
  static int validtz = 0;
  static struct timezone cached_tz = {0};
  struct timeval localtv;

  if (tp == NULL) {
    if (tzp == NULL)
      return	(0);
    tp = &localtv;
  }

#ifdef __ppc__
  {
    extern int __ppc_gettimeofday(struct timeval *, struct timezone *);
    extern int __commpage_gettimeofday(struct timeval *);

    if (__commpage_gettimeofday(tp)) {		/* first try commpage */
      if (__ppc_gettimeofday(tp,tzp)) {	/* if it fails, use syscall */
	return (-1);
      }
    }
  }
#else
  if (syscall (SYS_gettimeofday, tp, tzp) < 0) {
    return (-1);
  }
#endif
  if (tzp) {
    if (validtz == 0)  {
      struct tm *localtm = localtime ((time_t *)&tp->tv_sec);
      cached_tz.tz_dsttime = localtm->tm_isdst;
      cached_tz.tz_minuteswest =
	(-localtm->tm_gmtoff / SECSPERMIN) +
	(localtm->tm_isdst * MINSPERHOUR);
      validtz = 1;
    }
    tzp->tz_dsttime = cached_tz.tz_dsttime;
    tzp->tz_minuteswest = cached_tz.tz_minuteswest;
  }
  return (0);
}

int
gettimeofday(struct timeval *tp, struct timezone *tzp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(tp);
  /*
   * Some callers pass an invalid second argument
   * e.g., InitTimes in libXt
   */
  if (tzp) {
    if (RTHeapRep_Fault) RTHeapRep_Fault(tzp); /* make it readable */
    if (RTHeapRep_Fault) RTHeapRep_Fault(tzp); /* make it writable */
  }
  result = _gettimeofday(tp, tzp);
  EXIT_CRITICAL;
  return result;
}

/* ioctl must test the argp argument carefully.  It may be a pointer,
   or maybe not.  At a slight expense, we call RTHeapRep.Fault to
   unprotect the page if it's in the traced heap, but do nothing
   otherwise. */

int
ioctl(int d, unsigned long request, char *argp)
{
  int result;

  ENTER_CRITICAL;
  if (argp) {
    if (RTHeapRep_Fault) RTHeapRep_Fault(argp); /* make it readable */
    if (RTHeapRep_Fault) RTHeapRep_Fault(argp); /* make it writable */
  }
  result = syscall(SYS_ioctl, d, request, argp);
  EXIT_CRITICAL;
  return result;
}

int
ktrace(const char *tracefile, int ops, int trpoints, int pid)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(tracefile);
  result = syscall(SYS_ktrace, tracefile, ops, trpoints, pid);
  EXIT_CRITICAL;
  return result;
}

int
link(const char *name1, const char *name2)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name1);
  MAKE_READABLE(name2);
  result = syscall(SYS_link, name1, name2);
  EXIT_CRITICAL;
  return result;
}

int
lstat(const char *path, struct stat *buf)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = syscall(SYS_lstat, path, buf);
  EXIT_CRITICAL;
  return result;
}

int
mincore(caddr_t addr, size_t len, char *vec)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(vec);
  result = syscall(SYS_mincore, addr, len, vec);
  EXIT_CRITICAL;
  return result;
}

int
mkdir(const char *path, mode_t mode)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_mkdir, path, mode);
  EXIT_CRITICAL;
  return result;
}

int
mkfifo(const char *path, mode_t mode)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_mkfifo, path, mode);
  EXIT_CRITICAL;
  return result;
}

int
mknod(const char *path, mode_t mode, dev_t dev)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_mknod, path, mode, dev);
  EXIT_CRITICAL;
  return result;
}

int
mount(const char *type, const char *dir, int flags, void *data)
{
  int result;
  struct ufs_args *u_data;
  struct mfs_args *m_data;
  struct nfs_args *n_data;

  ENTER_CRITICAL;
  MAKE_READABLE(type);
  MAKE_READABLE(dir);
  if (strcmp(type, "ufs") == 0) {
    u_data = (struct ufs_args*) data;
    MAKE_READABLE(u_data);
    MAKE_READABLE(u_data->fspec);
  } else if (strcmp(type, "mfs") == 0) {
    m_data = (struct mfs_args*) data;
    MAKE_READABLE(m_data);
    MAKE_READABLE(m_data->fspec);
  } else if (strcmp(type, "nfs") == 0) {
    n_data = (struct nfs_args*) data;
    MAKE_READABLE(n_data);
    MAKE_READABLE(n_data->addr);
    MAKE_READABLE(n_data->fh);
    MAKE_READABLE(n_data->hostname);
  } else {	/* Not anything we recognize. */
    MAKE_READABLE(data);
  }
  result = syscall(SYS_mount, type, dir, flags, data);
  EXIT_CRITICAL;
  if (result != -1) {
    result = 0;
  }
  return result;
}

int msgctl(int msqid, int cmd, struct msqid_ds *buf)
{
  int result;

  ENTER_CRITICAL;
  switch (cmd) {
  case IPC_SET:
    MAKE_READABLE(buf);
    break;
  case IPC_STAT:
    MAKE_WRITABLE(buf);
    break;
  default:
    break;
  }
  result = syscall(SYS_msgctl, msqid, cmd, buf);
  EXIT_CRITICAL;
  return result;
}

int msgrcv(int msqid, void *msgp, size_t msgsz)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(msgp);
  result = syscall(SYS_msgrcv, msqid, msgp, msgsz);
  EXIT_CRITICAL;
  return result;
}

int msgsnd(int msqid, void *msgp, size_t msgsz)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(msgp);
  result = syscall(SYS_msgsnd, msqid, msgp, msgsz);
  EXIT_CRITICAL;
  return result;
}

int
m3_open(const char* path, int flags, mode_t mode)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_open, path, flags, mode);
  EXIT_CRITICAL;
  return result;
}

long
pathconf(const char *path, int name)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_pathconf, path, name);
  EXIT_CRITICAL;
  return result;
}

ssize_t
pread(int d, void *buf, size_t nbytes, off_t offset)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = syscall(SYS_pread, d, buf, nbytes, offset);
  EXIT_CRITICAL;
  return result;
}

int
profil(char *samples, int size, int offset, int scale)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(samples);
  result = syscall(SYS_profil, samples, size, offset, scale);
  EXIT_CRITICAL;
  return result;
}

ssize_t
pwrite(int d, const void *buf, size_t nbytes, off_t offset)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(buf);
  result = syscall(SYS_pwrite, d, buf, nbytes, offset);
  EXIT_CRITICAL;
  return result;
}

int
quotactl(char *path, int cmd, int id, caddr_t addr)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(path);
  switch (cmd) {
  case Q_QUOTAON:
  case Q_QUOTAOFF:
  case Q_SETUSE:
  case Q_SETQUOTA:
  case Q_GETQUOTA:
    MAKE_READABLE(addr);
    break;
  default:
    break;
  }
  result = syscall(SYS_quotactl, path, cmd, id,  addr);
  EXIT_CRITICAL;
  return result;
}

ssize_t
read(int d, void *buf, size_t nbytes)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = syscall(SYS_read, d, buf, nbytes);
  EXIT_CRITICAL;
  return result;
}

int
readlink(const char *path, char *buf, int bufsiz)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = syscall(SYS_readlink, path, buf, bufsiz);
  EXIT_CRITICAL;
  return result;
}

ssize_t
readv(int d, const struct iovec *iov, int iovcnt)
{
  int result;

  ENTER_CRITICAL;
  {
    int i;
    for (i = 0; i < iovcnt; i++) {
      MAKE_WRITABLE(iov[i].iov_base);
    }
  }
  result = syscall(SYS_readv, d, iov, iovcnt);
  EXIT_CRITICAL;
  return result;
}

ssize_t
recv(int s, void *buf, size_t len, int flags)
{
  return recvfrom(s, buf, len, flags, 0, 0);
}

ssize_t
recvfrom(int s, void *buf, size_t len, int flags, struct sockaddr *from,
	 int *fromlen)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  MAKE_WRITABLE(from);
  MAKE_WRITABLE(fromlen);
  result = syscall(SYS_recvfrom, s, buf, len, flags, from, fromlen);
  EXIT_CRITICAL;
  return result;
}

ssize_t
recvmsg(int s, struct msghdr *msg, int flags)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(msg->msg_name);
  {
    int i;
    for (i = 0; i < msg->msg_iovlen; i++) {
      if (msg->msg_iov[i].iov_len > 0) {
        MAKE_WRITABLE(msg->msg_iov[i].iov_base);
      }
    }
  }
  MAKE_WRITABLE(msg->msg_control);
  result = syscall(SYS_recvmsg, s, msg, flags);
  EXIT_CRITICAL;
  return result;
}

int
rename(const char *from, const char *to)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(from);
  MAKE_READABLE(to);
  result = syscall(SYS_rename, from, to);
  EXIT_CRITICAL;
  return result;
}

int
revoke(const char *path)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_revoke, path);
  EXIT_CRITICAL;
  return result;
}

int
rmdir(const char *path)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_rmdir, path);
  EXIT_CRITICAL;
  return result;
}

int
select(int nfds, fd_set *readfds, fd_set *writefds,
       fd_set *exceptfds, struct timeval *timeout)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(readfds);
  MAKE_WRITABLE(writefds);
  MAKE_WRITABLE(exceptfds);
  MAKE_READABLE(timeout);
  result = syscall(SYS_select, nfds, readfds, writefds, exceptfds, timeout);
  EXIT_CRITICAL;
  return result;
}

int
sem_close(sem_t *sem)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(sem);
  result = syscall(SYS_sem_close, sem);
  EXIT_CRITICAL;
  return result;
}

int
sem_post(sem_t *sem)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(sem);
  result = syscall(SYS_sem_post, sem);
  EXIT_CRITICAL;
  return result;
}

int
sem_trywait(sem_t *sem)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(sem);
  result = syscall(SYS_sem_trywait, sem);
  EXIT_CRITICAL;
  return result;
}

int
sem_wait(sem_t *sem)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(sem);
  result = syscall(SYS_sem_wait, sem);
  EXIT_CRITICAL;
  return result;
}

int
semctl(int semid, int semnum, int cmd, ...)
{
  int result;
  union semun arg;
  va_list ap;

  va_start(ap, cmd);
  arg = va_arg(ap, union semun);
  va_end(ap);

  ENTER_CRITICAL;
  switch (cmd) {

  case IPC_SET:
    MAKE_READABLE(arg.buf);
    break;

  case SETALL:
    MAKE_READABLE(arg.array);
    break;

  case IPC_STAT:
    MAKE_WRITABLE(arg.buf);
    break;

  case GETALL:
    MAKE_WRITABLE(arg.array);
    break;
  }
  result = syscall(SYS_semctl, semid, semnum, cmd, arg);
  EXIT_CRITICAL;
  return result;
}

int
semop(int semid, struct sembuf *sops, unsigned nsops)
{
  int result;

  ENTER_CRITICAL;
  {
    unsigned int i;
    for (i = 0; i < nsops; i++) {
      MAKE_READABLE(sops);
    }
  }
  result = syscall(SYS_semop, semid, sops, nsops);
  EXIT_CRITICAL;
  return result;
}

ssize_t
send(int s, const void *msg, size_t len, int flags)
{
  return sendto(s, msg, len, flags, 0, 0);
}

ssize_t
sendmsg(int s, const struct msghdr *msg, int flags)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(msg->msg_name);
  {
    int i;
    for (i = 0; i < msg->msg_iovlen; i++) {
      if (msg->msg_iov[i].iov_len > 0) {
        MAKE_READABLE(msg->msg_iov[i].iov_base);
      }
    }
  }
  MAKE_WRITABLE(msg->msg_control);
  result = syscall(SYS_sendmsg, s, msg, flags);
  EXIT_CRITICAL;
  return result;
}

ssize_t
sendto(int s, const void *msg, size_t len, int flags,
       const struct sockaddr *to, int tolen)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(msg);
  MAKE_READABLE(to);
  result = syscall(SYS_sendto, s, msg, len, flags, to, tolen);
  EXIT_CRITICAL;
  return result;
}

int
setgroups(int ngroups, const gid_t *gidset)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(gidset);
  result = syscall(SYS_setgroups, ngroups, gidset);
  EXIT_CRITICAL;
  return result;
}

int
setitimer(int which, const struct itimerval *value, struct itimerval *ovalue)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(value);
  MAKE_WRITABLE(ovalue);
  result = syscall(SYS_setitimer, which, value, ovalue);
  EXIT_CRITICAL;
  return result;
}

int
setrlimit(int resource, struct rlimit *rlp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(rlp);
  result = syscall(SYS_setrlimit, resource, rlp);
  EXIT_CRITICAL;
  return result;
}

int
setsockopt(int s, int level, int optname, const void *optval, int optlen)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(optval);
  result = syscall(SYS_setsockopt, s, level, optname, optval, optlen);
  EXIT_CRITICAL;
  return result;
}

int
settimeofday(const struct timeval *tp, const struct timezone *tzp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(tp);
  MAKE_READABLE(tzp);
  result = syscall(SYS_settimeofday, tp, tzp);
  EXIT_CRITICAL;
  return result;
}

int
m3_sigaction(int sig, const struct sigaction *act, struct sigaction *oact)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(act);
  MAKE_WRITABLE(oact);
  result = sigaction(sig, act, oact);
  EXIT_CRITICAL;
  return result;
}

int
sigaltstack(const struct sigaltstack *ss, struct sigaltstack *oss)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(ss);
  MAKE_WRITABLE(oss);
  result = syscall(SYS_sigaltstack, ss, oss);
  EXIT_CRITICAL;
  return result;
}

int
sigpending(sigset_t *set)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(set);
  result = syscall(SYS_sigpending, set);
  EXIT_CRITICAL;
  return result;
}
  
int
sigprocmask(int how, const sigset_t *set, sigset_t *oset)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(set);
  MAKE_WRITABLE(oset);
  result = syscall(SYS_sigprocmask, how, set, oset);
  EXIT_CRITICAL;
  return result;
}
  
int
sigreturn(struct sigcontext *scp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(scp);
  result = syscall(SYS_sigreturn, scp);
  EXIT_CRITICAL;
  return result;
}

int
sigwait(const sigset_t *set, int *sig)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(set);
  MAKE_WRITABLE(sig);
  result = syscall(SYS_sigwait, set, sig);
  EXIT_CRITICAL;
  return result;
}

int
socketpair(int d, int type, int protocol, int *sv)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(sv);
  result = syscall(SYS_socketpair, d, type, protocol, sv);
  EXIT_CRITICAL;
  return result;
}

int
stat(const char *path, struct stat *buf)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = syscall(SYS_stat, path, buf);
  EXIT_CRITICAL;
  return result;
}

int
statfs(const char *path, struct statfs *buf)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = syscall(SYS_statfs, path, buf);
  EXIT_CRITICAL;
  return result;
}

int
swapon(const char *special)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(special);
  result = syscall(SYS_swapon, special);
  EXIT_CRITICAL;
  return result;
}

int
symlink(const char *name1, const char *name2)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name1);
  MAKE_READABLE(name2);
  result = syscall(SYS_symlink, name1, name2);
  EXIT_CRITICAL;
  return result;
}

int
truncate(const char *path, off_t length)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_truncate, path, length);
  EXIT_CRITICAL;
  return result;
}

int
unlink(const char *path)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_unlink, path);
  EXIT_CRITICAL;
  return result;
}

int
unmount(const char *dir, int flags)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(dir);
  result = syscall(SYS_unmount, dir, flags);
  EXIT_CRITICAL;
  return result;
}

int
utimes(const char *path, const struct timeval *times)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_READABLE(times);
  result = syscall(SYS_utimes, path, times);
  EXIT_CRITICAL;
  return result;
}

pid_t
wait(int *status)
{
  return wait3(status, 0, 0);
}

pid_t
wait3(int *status, int options, struct rusage *rusage)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(status);
  MAKE_WRITABLE(rusage);
  result = syscall(SYS_wait4, -1, status, options, rusage);
  EXIT_CRITICAL;
  return result;
}

pid_t
wait4(pid_t wpid, int *status, int options, struct rusage *rusage)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(status);
  MAKE_WRITABLE(rusage);
  result = syscall(SYS_wait4, wpid, status, options, rusage);
  EXIT_CRITICAL;
  return result;
}

pid_t waitpid(pid_t pid, int *status, int options)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(status);
  result = syscall(SYS_wait4, pid, status, options, NULL);
  EXIT_CRITICAL;
  return result;
}

ssize_t
write(int fd, const void *buf, size_t nbytes)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(buf);
  result = syscall(SYS_write, fd, buf, nbytes);
  EXIT_CRITICAL;
  return result;
}

ssize_t
writev(int fd, const struct iovec *iov, int ioveclen)
{
  int result;

  ENTER_CRITICAL;
  {
    int i;
    for (i = 0; i < ioveclen; i++) {
      if (iov[i].iov_len > 0) {
        MAKE_READABLE(iov[i].iov_base);
      }
    }
  }
  result = syscall(SYS_writev, fd, iov, ioveclen);
  EXIT_CRITICAL;
  return result;
}

/* fork also requires special treatment, although it takes no
   argument.  fork crashes Ultrix if some pages are unreadable, so we
   must unprotect the heap before calling fork */
/* I don't know what happens in FreeBSD, so I just leave it in here */
pid_t fork()
{
  pid_t result;
  pid_t me = getpid();

  ENTER_CRITICAL;
  if (RTCSRC_FinishVM)  RTCSRC_FinishVM();
  result = syscall(SYS_fork);
  EXIT_CRITICAL;
  if (result == me) {
    result = 0;
  }
  return result;
}
