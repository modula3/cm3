/* Copyright (C) 1993, Digital Equipment Corporation */
/* All rights reserved. */
/* See the file COPYRIGHT for a full description. */

/* This file implements wrappers for almost all Solaris system calls that take
   pointers as arguments.  These wrappers allow the system calls to take
   arguments that might point to the traced heap, which may be VM-protected in
   the Solaris implementation of the collector.  The wrappers read and write
   the referents of all pointers about to passed to the system call, which
   ensures that the pages are not protected when the call is made.

   Each wrapper is a critical section, with RTou__inCritical non-zero, so that
   another thread cannot cause the pages to become reprotected before the
   system call is performed.

   A few system calls are not handled here, or are handled only partially.
   This restricts the system calls that can be made from Modula-3, or from
   libraries that are passed pointers into the Modula-3 traced heap.  These
   system calls are:

   1) syscall.  Implementing syscall would require a huge case statement, with
      one case per system call.  This seemed too error-prone, so syscall
      cannot take arguments that point into the traced heap.

   2) ioctl.  Ioctl's third argument may be a pointer, and some device drivers
      might interpret the referent as containing more pointers.  These
      second-level pointers are not handled here, so they must not point into
      the traced heap if they exist.  Handling this problem in general is
      impossible, since the set of device drivers is open-ended.

   3) profil.  The memory referenced by the "buff" argument is updated after
      the call returns, and there is no mechanism to permanently unprotect it.

   (Some calls in Section 2 are already wrappers for other system calls; it is
   not necessary to reimplement them here.)

   Also, longjmp must not be used from a signal handler to abnormally exit
   from a system call.

   Finally, if a system call references an object on the heap, each pointer
   must reference only one object.  Therefore, it is not possible to write the
   heap contents with a single write. */

#include <sys/types.h>
#include <errno.h>
#include <stdarg.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <sys/file.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/sem.h>
#include <signal.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <sys/mount.h>
#include <sys/reboot.h>
#include <sys/sem.h>
#include <sys/stat.h>
#include <sys/statfs.h>
#include <sys/statvfs.h>
#include <bsm/audit.h>
#include <sys/dirent.h>
#include <stropts.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/lwp.h>
#include <fcntl.h>
#include <poll.h>
#include <sys/procset.h>
#include <sys/priocntl.h>
#include <sys/shm.h>
#include <sys/fstyp.h>
#include <sys/fsid.h>
#include <sys/times.h>
#include <sys/utsname.h>
#include <ustat.h>
#include <utime.h>
#include <sys/utssys.h>
#include <sys/modctl.h>
#include <sys/processor.h>
#include <sys/acl.h>
#include <sys/wait.h>

#if defined(SYS_lwp_mutex_init)
#define SOL_VERSION	20700
#elif defined(SYS_ntp_adjtime)
#define SOL_VERSION	20600
#elif defined(SYS_install_utrap)
#define SOL_VERSION	20501
#else
#define SOL_VERSION	20500
#endif

extern int RT0u__inCritical;
#define ENTER_CRITICAL RT0u__inCritical++
#define EXIT_CRITICAL  RT0u__inCritical--

void (*RTHeapRep_Fault)();
void (*RTCSRC_FinishVM)();

static char RTHeapDepC__c;
#define MAKE_READABLE(x) if (x) { RTHeapDepC__c = *(char*)(x); }
#define MAKE_WRITABLE(x) if (x) { *(char*)(x) = RTHeapDepC__c = *(char*)(x); }

/* Unless otherwise noted, all the following wrappers have the same
   structure. */

int access(const char *path, int amode)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = _access(path, amode);
  EXIT_CRITICAL;
  return result;
}

int acct(const char *path)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = _acct(path);
  EXIT_CRITICAL;
  return result;
}

int acl(const char *pathp, int cmd, int nentries, aclent_t *aclbufp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE (pathp);
  switch (cmd) {
  case SETACL:
    MAKE_READABLE (aclbufp);
    break;
  case GETACL:
    MAKE_WRITABLE (aclbufp);
    break;
  }
  result = _acl(pathp, cmd, nentries, aclbufp);
  EXIT_CRITICAL;
  return result;
}

int adjtime(struct timeval *delta, struct timeval *olddelta)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(delta);
  MAKE_WRITABLE(olddelta);
  result = _adjtime(delta, olddelta);
  EXIT_CRITICAL;
  return result;
}

int audit(caddr_t record, int length)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(record);
  result = syscall(SYS_auditsys, BSM_AUDIT, record, length);
  EXIT_CRITICAL;
  return result;
}

int auditon(int cmd, caddr_t data, int length)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(data);
  result = syscall(SYS_auditsys, BSM_AUDITCTL, cmd, data, length);
  EXIT_CRITICAL;
  return result;
}

int auditstat(au_stat_t *stat)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(stat);
  result = syscall(SYS_auditsys, BSM_AUDITSTAT, stat);
  EXIT_CRITICAL;
  return result;
}

int audituser(caddr_t record, int length)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(record);
  result = syscall(SYS_auditsys, BSM_AUDITUSER, record, length);
  EXIT_CRITICAL;
  return result;
}

int chdir(const char *path)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = _chdir(path);
  EXIT_CRITICAL;
  return result;
}

int chmod(const char *path, mode_t mode)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = _chmod(path, mode);
  EXIT_CRITICAL;
  return result;
}

int chown(const char *path, uid_t owner, gid_t group)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = _chown(path, owner, group);
  EXIT_CRITICAL;
  return result;
}

int chroot(const char *path)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = _chroot(path);
  EXIT_CRITICAL;
  return result;
}

int clock_getres(clockid_t clock_id, struct timespec *res)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(res);
  result = __clock_getres(clock_id, res);
  EXIT_CRITICAL;
  return result;
}

int clock_gettime(clockid_t clock_id, struct timespec *tp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(tp);
  result = __clock_gettime(clock_id, tp);
  EXIT_CRITICAL;
  return result;
}

int clock_settime(clockid_t clock_id, const struct timespec *tp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(tp);
  result = __clock_settime(clock_id, tp);
  EXIT_CRITICAL;
  return result;
}

int creat(const char *path, mode_t mode)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = _creat(path, mode);
  EXIT_CRITICAL;
  return result;
}

/* execve is implemented differently since it does not return, which
   would leave RT0u__inCritical set in the parent if called in the child
   of a vfork. Many calls leave the process in an undefined state in the
   case of EFAULT, but we assume that execve is not one of these. */

int execve(const char *path, char *const argv[], char *const envp[])
{
  int result;

  while (1) {
    result = _execve(path, argv, envp);
    if (result == -1 && errno == EFAULT) {
      MAKE_READABLE(path);
      { char *const* a; for (a = argv; *a; a++) MAKE_READABLE(*a); }
      { char *const* e; for (e = envp; *e; e++) MAKE_READABLE(*e); }
    } else {
      return result;
    }
  }
}

int facl (int fildes, int cmd, int nentries, aclent_t *aclbufp)
{
  int result;

  ENTER_CRITICAL;
  switch (cmd) {
  case SETACL:
    MAKE_READABLE (aclbufp);
    break;
  case GETACL:
    MAKE_WRITABLE (aclbufp);
    break;
  }
  result = _facl(fildes, cmd, nentries, aclbufp);
  EXIT_CRITICAL;
  return result;
}

int fcntl(int fildes, int cmd, ...)
{
  int result;
  va_list args;
  caddr_t arg;

  ENTER_CRITICAL;
  va_start(args, cmd);
  arg = va_arg(args, caddr_t);
  va_end(args);
  switch (cmd) {
  case F_GETLK:
    MAKE_WRITABLE(arg);
    break;
  case F_FREESP:
  case F_SETLK:
  case F_SETLKW:
    MAKE_READABLE(arg);
    break;
  default:
    break;
  }
  result = _fcntl(fildes, cmd, arg);
  EXIT_CRITICAL;
  return result;
}

#ifdef FORK_BUGGY
/* Both fork(2) and fork1(2) require special treatment, as does vfork(2)
   below, although they take no arguments.  They are reported to cause Solaris
   to crash if some pages are unreadable, so we must unprotect the heap before
   doing the system call. */

pid_t fork(void)
{
  int result;

  ENTER_CRITICAL;
  if (RTCSRC_FinishVM) RTCSRC_FinishVM();
  result = _fork();
  EXIT_CRITICAL;
  return result;
}

pid_t fork1(void)
{
  int result;

  ENTER_CRITICAL;
  if (RTCSRC_FinishVM) RTCSRC_FinishVM();
  result = _fork1();
  EXIT_CRITICAL;
  return result;
}
#endif

int fstat(int fildes, struct stat *buf)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = _fstat(fildes, buf);
  EXIT_CRITICAL;
  return result;
}

int fstatfs(int fd, struct statfs *buf, int len, int fstyp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = _fstatfs(fd, buf, len, fstyp);
  EXIT_CRITICAL;
  return result;
}

int fstatvfs(int fildes, struct statvfs *buf)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = _fstatvfs(fildes, buf);
  EXIT_CRITICAL;
  return result;
}

int _fxstat(int version, int fildes, struct stat *buf)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = syscall(SYS_fxstat, version, fildes, buf);
  EXIT_CRITICAL;
  return result;
}

int getauid(au_id_t *auid)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(auid);
  result = syscall(SYS_auditsys, BSM_GETAUID, auid);
  EXIT_CRITICAL;
  return result;
}

/*
Seems this is already wrapper, and adding a new one messes up threads.

int getcontext(ucontext_t *ucp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(ucp);
  result = _getcontext(ucp);
  EXIT_CRITICAL;
  return result;
}
*/

int getdents(int fildes, struct dirent *buf, size_t nbyte)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = _getdents(fildes, buf, nbyte);
  EXIT_CRITICAL;
  return result;
}

int getgroups(int gidsetsize, gid_t *grouplist)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(grouplist);
  result = _getgroups(gidsetsize, grouplist);
  EXIT_CRITICAL;
  return result;
}

int getitimer(int which, struct itimerval *value)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(value);
  result = _getitimer(which, value);
  EXIT_CRITICAL;
  return result;
}

int getkernstate(au_mask_t *mask)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(mask);
  result = syscall(SYS_auditsys, BSM_GETKERNSTATE, mask);
  EXIT_CRITICAL;
  return result;
}

int getmsg(int fildes, struct strbuf *ctlptr, struct strbuf *dataptr,
	   int *flagsp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(ctlptr);
  MAKE_WRITABLE(dataptr);
  MAKE_WRITABLE(flagsp);
  result = _getmsg(fildes, ctlptr, dataptr, flagsp);
  EXIT_CRITICAL;
  return result;
}

int getpmsg(int fildes, struct strbuf *ctlptr, struct strbuf *dataptr,
	    int *bandp, int *flagsp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(ctlptr);
  MAKE_WRITABLE(dataptr);
  MAKE_WRITABLE(bandp);
  MAKE_WRITABLE(flagsp);
  result = _getpmsg(fildes, ctlptr, dataptr, bandp, flagsp);
  EXIT_CRITICAL;
  return result;
}

int getrlimit(int resource, struct rlimit *rlp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(rlp);
  result = _getrlimit(resource, rlp);
  EXIT_CRITICAL;
  return result;
}

int gettimeofday(struct timeval *tp, void *tzp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(tp);
  /* MAKE_WRITABLE(tzp); */
  /*
   * Some callers pass an invalid second argument
   * e.g., InitTimes in libXt
   */
  if (RTHeapRep_Fault) RTHeapRep_Fault(tzp); /* make it readable */
  if (RTHeapRep_Fault) RTHeapRep_Fault(tzp); /* make it writable */
  result = _gettimeofday(tp, tzp);
  EXIT_CRITICAL;
  return result;
}

int getuseraudit(au_id_t uid, au_mask_t *mask)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(mask);
  result = syscall(SYS_auditsys, BSM_GETUSERAUDIT, uid, mask);
  EXIT_CRITICAL;
  return result;
}

/* ioctl must test the argp argument carefully.  It may be a pointer,
   or maybe not.  At a slight expense, we call RTHeapRep.Fault to
   unprotect the page if it's in the traced heap, but do nothing
   otherwise. */

int ioctl(int fildes, int request, ...)
{
  int result;
  va_list args;
  int argp;

  ENTER_CRITICAL;
  va_start(args, request);
  argp = va_arg(args, int);
  va_end(args);
  if (RTHeapRep_Fault) RTHeapRep_Fault(argp); /* make it readable */
  if (RTHeapRep_Fault) RTHeapRep_Fault(argp); /* make it writable */
  result = _ioctl(fildes, request, argp);
  EXIT_CRITICAL;
  return result;
}

int lchown(const char *path, uid_t owner, gid_t group)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = _lchown(path, owner, group);
  EXIT_CRITICAL;
  return result;
}

int link(const char *existing, const char *new)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(existing);
  MAKE_READABLE(new);
  result = _link(existing, new);
  EXIT_CRITICAL;
  return result;
}

int lstat(const char *path, struct stat *buf)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = _lstat(path, buf);
  EXIT_CRITICAL;
  return result;
}

#if SOL_VERSION >= 20700
int _lwp_create(ucontext_t *contextp, unsigned int flags, lwpid_t *new_lwp)
#else
int _lwp_create(ucontext_t *contextp, unsigned long flags, lwpid_t *new_lwp)
#endif
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(contextp);
  MAKE_WRITABLE(new_lwp);
  result = __lwp_create(contextp, flags, new_lwp);
  EXIT_CRITICAL;
  return result;
}

int _lwp_cond_broadcast(lwp_cond_t *cvp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(cvp);
  result = __lwp_cond_broadcast(cvp);
  EXIT_CRITICAL;
  return result;
}

int _lwp_cond_signal(lwp_cond_t *cvp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(cvp);
  result = __lwp_cond_signal(cvp);
  EXIT_CRITICAL;
  return result;
}

int _lwp_cond_wait(lwp_cond_t *cvp, lwp_mutex_t *mp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(cvp);
  MAKE_WRITABLE(mp);
  result = __lwp_cond_wait(cvp, mp);
  EXIT_CRITICAL;
  return result;
}

int _lwp_info(struct lwpinfo *buffer)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buffer);
  result = __lwp_info(buffer);
  EXIT_CRITICAL;
  return result;
}

int _lwp_sema_post(lwp_sema_t *sema)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(sema);
  result = __lwp_sema_post(sema);
  EXIT_CRITICAL;
  return result;
}

int _lwp_sema_wait(lwp_sema_t *sema)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(sema);
  result = __lwp_sema_wait(sema);
  EXIT_CRITICAL;
  return result;
}

int _lwp_wait(lwpid_t wait_for, lwpid_t *departed_lwp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(departed_lwp);
  result = __lwp_wait(wait_for, departed_lwp);
  EXIT_CRITICAL;
  return result;
}

int _lxstat(int version, const char *path, struct stat *buf)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = syscall(SYS_lxstat, version, path, buf);
  EXIT_CRITICAL;
  return result;
}

int mincore(caddr_t addr, size_t len, char *vec)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(vec);
  result = _mincore(addr, len, vec);
  EXIT_CRITICAL;
  return result;
}

int mkdir(const char *path, mode_t mode)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = _mkdir(path, mode);
  EXIT_CRITICAL;
  return result;
}

int mknod(const char *path, mode_t mode, dev_t dev)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = _mknod(path, mode, dev);
  EXIT_CRITICAL;
  return result;
}

int modctl(int opcode, char *arg)
{
  int result;

  ENTER_CRITICAL;
  switch (opcode) {
  case MODINFO:
    MAKE_WRITABLE(arg);
    break;
  default:
    break;
  }
  result = syscall(SYS_modctl, opcode, arg);
  EXIT_CRITICAL;
  return result;
}

int mount(const char *spec, const char *dir, int mflag, ...
	  /* char *fstype, const char *dataptr,  int  datalen  */)
{
  int result;
  va_list args;
  char *fstype;
  char *dataptr;
  int datalen;

  ENTER_CRITICAL;
  va_start(args, mflag);
  fstype = va_arg(args, char*);
  dataptr = va_arg(args, char*);
  datalen = va_arg(args, int);
  va_end(args);
  MAKE_READABLE(spec);
  MAKE_READABLE(dir);
  if (mflag & MS_DATA) {
    MAKE_READABLE(fstype);
    MAKE_READABLE(dataptr);
  } else if (mflag & MS_FSS) {
    MAKE_READABLE(fstype);
  }
  result = _mount(spec, dir, mflag, fstype, dataptr, datalen);
  EXIT_CRITICAL;
  return result;
}

int msgctl(int msqid, int cmd, struct msqid_ds *buf)
{
  int result;

  ENTER_CRITICAL;
  switch (cmd) {
  case IPC_STAT:
    MAKE_WRITABLE (buf);
    break;
  case IPC_SET:
    MAKE_READABLE (buf);
    break;
  default:
    break;
  }
  result = _msgctl(msqid, cmd, buf);
  EXIT_CRITICAL;
  return result;
}

int msgrcv(int msqid, void *msgp, size_t msgsz, long msgtyp, int msgflg)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(msgp);
  result = _msgrcv(msqid, msgp, msgsz, msgtyp, msgflg);
  EXIT_CRITICAL;
  return result;
}

int msgsnd(int msqid, const void *msgp, size_t msgsz, int msgflg)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(msgp);
  result = _msgsnd(msqid, msgp, msgsz, msgflg);
  EXIT_CRITICAL;
  return result;
}

int nanosleep(const struct timespec *rqtp, struct timespec *rmtp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(rqtp);
  MAKE_WRITABLE(rmtp);
  result = __nanosleep(rqtp, rmtp);
  EXIT_CRITICAL;
  return result;
}

int nuname(struct utsname *name)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(name);
  result = _uname(name);
  EXIT_CRITICAL;
  return result;
}

int open(const char *path, int oflag, ... /* mode_t mode */)
{
  int result;
  va_list args;
  mode_t mode;

  ENTER_CRITICAL;
  va_start(args, oflag);
  mode = va_arg(args, mode_t);
  va_end(args);
  MAKE_READABLE(path);
  result = _open(path, oflag, mode);
  EXIT_CRITICAL;
  return result;
}

long pathconf(const char *path, int name)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = _pathconf(path, name);
  EXIT_CRITICAL;
  return result;
}

int poll(struct pollfd *fds, unsigned long nfds, int timeout)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(fds);
  result = _poll(fds, nfds, timeout);
  EXIT_CRITICAL;
  return result;
}

ssize_t pread(int fildes, void *buf, size_t nbyte, off_t offset)
{
  ssize_t result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = _pread(fildes, buf, nbyte, offset);
  EXIT_CRITICAL;
  return result;
}

long __priocntlset(int pc_version, procset_t *psp, int cmd, caddr_t arg)
{
  long result;

  ENTER_CRITICAL;
  MAKE_READABLE(psp);
  switch (cmd) {
  case PC_GETCID:
  case PC_GETCLINFO:
  case PC_GETPARMS:
    MAKE_WRITABLE(arg);
    break;
  case PC_SETPARMS:
    MAKE_READABLE(arg);
    break;
  default:
    break;
  }
  result = syscall(SYS_priocntlsys, pc_version, psp, cmd, arg);
  EXIT_CRITICAL;
  return result;
}    

int processor_bind(idtype_t idtype, id_t id, processorid_t processorid,
		   processorid_t *obind)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(obind);
  result = syscall(SYS_processor_bind, idtype, id, processorid, obind);
  EXIT_CRITICAL;
  return result;
}

int processor_info(processorid_t processorid, processor_info_t *infop)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(infop);
  result = syscall(SYS_processor_info, processorid, infop);
  EXIT_CRITICAL;
  return result;
}

int putmsg(int fildes, const struct strbuf *ctlptr,
          const struct strbuf *dataptr, int flags)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(ctlptr);
  MAKE_READABLE(dataptr);
  result = _putmsg(fildes, ctlptr, dataptr, flags);
  EXIT_CRITICAL;
  return result;
}

int putpmsg(int fildes, const struct strbuf *ctlptr,
	    const struct strbuf *dataptr, int band, int flags)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(ctlptr);
  MAKE_READABLE(dataptr);
  result = _putpmsg(fildes, ctlptr, dataptr, band, flags);
  EXIT_CRITICAL;
  return result;
}

ssize_t pwrite(int fildes, const void  *buf,  size_t  nbyte, off_t offset)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(buf);
  result = _pwrite(fildes, buf, nbyte, offset);
  EXIT_CRITICAL;
  return result;
}

ssize_t read(int fildes, void *buf, size_t nbyte)
{
  ssize_t result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = _read(fildes, buf, nbyte);
  EXIT_CRITICAL;
  return result;
}

#if SOL_VERSION >= 20600
int readlink(const char *path, char *buf, size_t bufsiz)
#else
int readlink(const char *path, void *buf, int bufsiz)
#endif
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = _readlink(path, buf, bufsiz);
  EXIT_CRITICAL;
  return result;
}

#if SOL_VERSION >= 20600
ssize_t readv(int fildes, const struct iovec *iov, int iovcnt)
#else
ssize_t readv(int fildes, struct iovec *iov, int iovcnt)
#endif
{
  ssize_t result;

  ENTER_CRITICAL;
  {
    int i;
    for (i = 0; i < iovcnt; i++) {
      if (iov[i].iov_len > 0) {
	MAKE_WRITABLE(iov[i].iov_base);
      }
    }
  }
  result = _readv(fildes, iov, iovcnt);
  EXIT_CRITICAL;
  return result;
}

int rename(const char *old, const char *new)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(old);
  MAKE_READABLE(new);
  result = _rename(old, new);
  EXIT_CRITICAL;
  return result;
}

int rmdir(const char *path)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = _rmdir(path);
  EXIT_CRITICAL;
  return result;
}

int semctl(int semid, int semnum, int cmd, ...)
{
  int result;
  va_list args;
  union semun {
    int val;
    struct semid_ds *buf;
    ushort *array;
  } arg;

  ENTER_CRITICAL;
  va_start(args, cmd);
  arg = va_arg(args, union semun);
  va_end(args);
  switch (cmd) {
  case SETALL:
    MAKE_READABLE(arg.array);
    break;
  case GETALL:
    MAKE_WRITABLE(arg.array);
    break;
  case IPC_SET:
    MAKE_READABLE(arg.buf);
    break;
  case IPC_STAT:
    MAKE_WRITABLE(arg.buf);
    break;
  default:
    break;
  }
  result = _semctl(semid, semnum, cmd, arg);
  EXIT_CRITICAL;
  return result;
}

int semop(int semid, struct sembuf *sops, size_t nsops)
{
  int result;

  ENTER_CRITICAL;
  {
    int i;
    for (i = 0; i < nsops; i++) {
      MAKE_READABLE(&sops[i]);
    }
  }
  result = _semop(semid, sops, nsops);
  EXIT_CRITICAL;
  return result;
}

int setauid(const au_id_t *auid)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(auid);
  result = syscall(SYS_auditsys, BSM_SETAUID, auid);
  EXIT_CRITICAL;
  return result;
}

int setcontext(ucontext_t *ucp)
{
  int result;

  while (1) {
    result = _setcontext(ucp);
    if (result && errno == EFAULT) {
      MAKE_WRITABLE(ucp);
    } else
      return result;
  }
}

int setgroups(int ngroups, const gid_t *grouplist)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(grouplist);
  result = _setgroups(ngroups, grouplist);
  EXIT_CRITICAL;
  return result;
}

#if SOL_VERSION >= 20600
int setitimer(int which, struct itimerval *value,
	      struct itimerval *ovalue)
#else
int setitimer(int which, const struct itimerval *value,
	      struct itimerval *ovalue)
#endif
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(value);
  MAKE_WRITABLE(ovalue);
  result = _setitimer(which, value, ovalue);
  EXIT_CRITICAL;
  return result;
}

int setkernstate(au_mask_t *mask)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(mask);
  result = syscall(SYS_auditsys, BSM_SETKERNSTATE, mask);
  EXIT_CRITICAL;
  return result;
}

int setrlimit(int resource, const struct rlimit *rlp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(rlp);
  result = _setrlimit(resource, rlp);
  EXIT_CRITICAL;
  return result;
}

int setuseraudit(au_id_t uid, au_mask_t *mask)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(mask);
  result = syscall(SYS_auditsys, BSM_SETUSERAUDIT, uid, mask);
  EXIT_CRITICAL;
  return result;
}

int setaudit(auditinfo_t *ai)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(ai);
  result = syscall(SYS_auditsys, BSM_SETAUDIT, ai);
  EXIT_CRITICAL;
  return result;
}

int shmctl(int shmid, int cmd, struct shmid_ds *buf)
{
  int result;

  ENTER_CRITICAL;
  switch (cmd) {
  case IPC_STAT:
  case IPC_SET:
    MAKE_WRITABLE(buf);
    break;
  default:
    break;
  }
  result = _shmctl(shmid, cmd, buf);
  EXIT_CRITICAL;
  return result;
}

int sigaction(int sig, const struct sigaction *act, struct sigaction *oact)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(act);
  MAKE_WRITABLE(oact);
  result = _sigaction(sig, act, oact);
  EXIT_CRITICAL;
  return result;
}

int sigaltstack(const stack_t *ss, stack_t *oss)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(ss);
  MAKE_WRITABLE(oss);
  result = _sigaltstack(ss, oss);
  EXIT_CRITICAL;
  return result;
}

int sigfillset(sigset_t *set)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(set);
  result = _sigfillset(set);
  EXIT_CRITICAL;
  return result;
}

int sigpending(sigset_t *set)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(set);
  result = _sigpending(set);
  EXIT_CRITICAL;
  return result;
}

int sigprocmask(int how, const sigset_t *set, sigset_t *oset)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(set);
  MAKE_WRITABLE(oset);
  result = _sigprocmask(how, set, oset);
  EXIT_CRITICAL;
  return result;
}

int sigsendset(const procset_t *psp, int sig)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(psp);
  result = _sigsendset(psp, sig);
  EXIT_CRITICAL;
  return result;
}

int sigsuspend(const sigset_t *set)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(set);
  result = _sigsuspend(set);
  EXIT_CRITICAL;
  return result;
}

int sigtimedwait(const sigset_t *set, siginfo_t *info,
		 const struct timespec *timeout)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(set);
  MAKE_WRITABLE(info);
  MAKE_READABLE(timeout);
  result = __sigtimedwait(set, info, timeout);
  EXIT_CRITICAL;
  return result;
}

int sigwait(sigset_t *set)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(set);
  result = _sigwait(set);
  EXIT_CRITICAL;
  return result;
}

int
sigwaitinfo(const sigset_t *set, siginfo_t *info)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(set);
  MAKE_WRITABLE(info);
  result = __sigtimedwait(set, info, 0);
  EXIT_CRITICAL;
  return result;
}

int stat(const char *path, struct stat *buf)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = _stat(path, buf);
  EXIT_CRITICAL;
  return result;
}

int statfs(const char *path, struct statfs *buf, int len, int fstyp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = _statfs(path, buf, len, fstyp);
  EXIT_CRITICAL;
  return result;
}

int statvfs(const char *path, struct statvfs *buf)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = _statvfs(path, buf);
  EXIT_CRITICAL;
  return result;
}

int stime(const time_t *tp)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(tp);
  result = _stime(tp);
  EXIT_CRITICAL;
  return result;
}

int symlink(const char *name1, const char *name2)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name1);
  MAKE_READABLE(name2);
  result = _symlink(name1, name2);
  EXIT_CRITICAL;
  return result;
}

int sysfs(int opcode, ...)
{
  int result;
  char *fsname;
  int fs_index;
  char *buf;
  va_list args;

  ENTER_CRITICAL;
  va_start(args, opcode);
  switch (opcode) {
  case GETFSIND:
    fsname = va_arg(args, char*);
    MAKE_READABLE(fsname);
    result = _sysfs(opcode, fsname);
    break;
  case GETFSTYP:
    fs_index = va_arg(args, int);
    buf = va_arg(args, char*);
    MAKE_WRITABLE(buf);
    result = _sysfs(opcode, fs_index, buf);
    break;
  default:
    result = _sysfs(opcode);
    break;
  }
  va_end(args);
  EXIT_CRITICAL;
  return result;
}

long sysinfo(int command, char *buf, long count)
{
  long result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = _sysinfo(command, buf, count);
  EXIT_CRITICAL;
  return result;
}  

int swapcontext(ucontext_t *oucp, ucontext_t *ucp)
{
  int result;

  while (1) {
    result = _swapcontext(oucp, ucp);
    if (result && errno == EFAULT) {
      MAKE_WRITABLE(oucp);
      MAKE_WRITABLE(ucp);
    } else
      return result;
  }
}

clock_t times(struct tms *buffer)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buffer);
  result = _times(buffer);
  EXIT_CRITICAL;
  return result;
}

int timer_create(clockid_t clock_id, struct sigevent *evp, timer_t *timerid)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(evp);
  MAKE_WRITABLE(timerid);
  result = __timer_create(clock_id, evp, timerid);
  EXIT_CRITICAL;
  return result;
}

int timer_gettime(timer_t timerid, struct itimerspec *value)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(value);
  result = __timer_gettime(timerid, value);
  EXIT_CRITICAL;
  return result;
}

int timer_settime(timer_t timerid, int flags, const struct itimerspec *value,
		  struct itimerspec *ovalue)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(value);
  MAKE_WRITABLE(ovalue);
  result = __timer_settime(timerid, flags, value, ovalue);
  EXIT_CRITICAL;
  return result;
}

int umount(const char *file)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(file);
  result = _umount(file);
  EXIT_CRITICAL;
  return result;
}

int uname(struct utsname *name)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(name);
  result = _uname(name);
  EXIT_CRITICAL;
  return result;
}

int unlink(const char *path)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = _unlink(path);
  EXIT_CRITICAL;
  return result;
}

int ustat(dev_t dev, struct ustat *buf)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = _ustat(dev, buf);
  EXIT_CRITICAL;
  return result;
}

int utime(const char *path, const struct utimbuf *times)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_READABLE(times);
  result = _utime(path, times);
  EXIT_CRITICAL;
  return result;
}

#if SOL_VERSION >= 20700
int utimes(const char *file, const struct timeval *tvp)
#else
int utimes(char *file, struct timeval *tvp)
#endif
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(file);
  MAKE_READABLE(tvp);
  result = _utimes(file, tvp);
  EXIT_CRITICAL;
  return result;
}

int utssys(char *cbuf, int mv, int type, char *outbufp)
{
  int result;

  ENTER_CRITICAL;
  switch(type) {
  case UTS_UNAME:
  case UTS_USTAT:
    MAKE_WRITABLE(cbuf);
    break;
  case UTS_FUSERS:
    MAKE_WRITABLE(cbuf);
    MAKE_WRITABLE(outbufp);
    break;
  default:
    break;
  }
  result = _utssys(cbuf, mv, type, outbufp);
  EXIT_CRITICAL;
  return result;
}

#ifdef FORK_BUGGY
/* Similarly to the fork(2) and fork1(2) system calls.  vfork(2) requires
   special treatment, although it takes no arguments.  It reportedly causes
   Solaris to crash if some pages are unreadable, so we must unprotect the
   heap before doing the system call. */
pid_t vfork(void)
{
  pid_t me;
  pid_t result;

  ENTER_CRITICAL;
  if (RTCSRC_FinishVM) RTCSRC_FinishVM();
  result = _vfork();
  /* don't EXIT_CRITICAL in the child: it's sharing the parent's address
     space */
  if (result)
    EXIT_CRITICAL;
  return result;
}
#endif

pid_t wait(int *stat_loc)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(stat_loc);
  result = _wait(stat_loc);
  EXIT_CRITICAL;
  return result;
}

int waitid(idtype_t idtype, id_t id, siginfo_t *infop, int options)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(infop);
  result = _waitid(idtype, id, infop, options);
  EXIT_CRITICAL;
  return result;
}

pid_t waitpid(pid_t pid, int *stat_loc, int options)
{
  int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(stat_loc);
  result = _waitpid(pid, stat_loc, options);
  EXIT_CRITICAL;
  return result;
}

ssize_t write(int fildes, const void *buf, size_t nbyte)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(buf);
  result = _write(fildes, buf, nbyte);
  EXIT_CRITICAL;
  return result;
}

int writev(int fildes, const struct iovec *iov, int iovcnt)
{
  int result;

  ENTER_CRITICAL;
  {
    int i;
    for (i = 0; i < iovcnt; i++) {
      if (iov[i].iov_len > 0) {
        MAKE_READABLE(iov[i].iov_base);
      }
    }
  }
  result = _writev(fildes, iov, iovcnt);
  EXIT_CRITICAL;
  return result;
}

int _xmknod(int version, const char *path, mode_t mode, dev_t dev)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_xmknod, version, path, mode, dev);
  EXIT_CRITICAL;
  return result;
}

int _xstat(int version, const char *path, struct stat *buf)
{
  int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = syscall(SYS_xstat, version, path, buf);
  EXIT_CRITICAL;
  return result;
}
