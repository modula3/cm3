/* Copyright (C) 1993, Digital Equipment Corporation */
/* All rights reserved. */
/* See the file COPYRIGHT for a full description. */

/* Last modified on Thu Jun 29 09:41:52 PDT 1995 by kalsow */
/*      modified on Wed Feb  3 11:44:06 PST 1993 by jdd */
/*      modified on Mon Feb  1 21:53:46 PST 1993 by goldberg@parc.xerox.com */

/* This is RTHeapDepC.c for SunOS running on SPARC processors, and was
   derived from the DS3100 version by David Goldberg at Xerox PARC. */

/* This file implements wrappers for almost all SunOS system calls
   that take pointers as arguments.  These wrappers allow the system
   calls to take arguments that might point to the traced heap, which
   may be VM-protected in the SunOS implementation of the collector.
   The wrappers read and write the referents of all pointers about to
   passed to the system call, which ensures that the pages are not
   protected when the call is made.

   Each wrapper is a critical section, with RTou__inCritical non-zero,
   so that another thread cannot cause the pages to become reprotected
   before the system call is performed.

   A few system calls are not handled here, or are handled only
   partially.  This restricts the system calls that can be made from
   Modula-3, or from libraries that are passed pointers into the
   Modula-3 traced heap.  These system calls are:

   1) syscall.  Implementing syscall would require a huge case
      statement, with one case per system call.  This seemed too
      error-prone, so syscall cannot take arguments that point into
      the traced heap.

   2) ioctl.  Ioctl's third argument may be a pointer, and some device
      drivers might interpret the referent as containing more
      pointers.  These second-level pointers are not handled here, so
      they must not point into the traced heap if they exist.
      Handling this problem in general is impossible, since the set of
      device drivers is open-ended.

   3) profil.  The memory referenced by the "buff" argument is updated
      after the call returns, and there is no mechanism to permanently
      unprotect it.

   4) Undocumented system calls.  If there are private system calls
      with no manual pages, it is impossible to write wrappers.

   (Some calls in Section 2, including wait, wait3, sigvec,
   sigprocmask, and sigsuspend, are already wrappers for other system
   calls; it is not necessary to reimplement them here.)

   Also, longjmp must not be used from a signal handler to abnormally
   exit from a system call.

   Finally, if a system call references an object on the heap, each
   pointer must reference only one object.  Therefore, it is not
   possible to write the heap contents with a single write. */

#include <sys/types.h>
#include <errno.h>
#include <syscall.h>
#include <sys/label.h>
#include <sys/audit.h>
#include <sys/file.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <ufs/quota.h>
#include <sys/sem.h>
#include <sys/signal.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <sys/mount.h>
#include <sys/reboot.h>
#include <sys/sem.h>

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

int accept(s, addr, addrlen)
int s;
struct sockaddr *addr;
int *addrlen;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(addr);
  MAKE_WRITABLE(addrlen);
  result = syscall(SYS_accept, s, addr, addrlen);
  EXIT_CRITICAL;
  return result;
}

int access(path, mode)
char *path;
int mode;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_access, path, mode);
  EXIT_CRITICAL;
  return result;
}

int acct(file)
char *file;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(file);
  result = syscall(SYS_acct, file);
  EXIT_CRITICAL;
  return result;
}

int adjtime(delta, olddelta)
struct timeval *delta;
struct timeval *olddelta;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(delta);
  MAKE_WRITABLE(olddelta);
  result = syscall(SYS_adjtime, delta, olddelta);
  EXIT_CRITICAL;
  return result;
}

int audit(record)
audit_record_t *record;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(record);
  result = syscall(SYS_auditsys, 2, record);
  EXIT_CRITICAL;
  return result;
}

int bind(s, name, namelen)
int s;
struct sockaddr *name;
int namelen;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name);
  result = syscall(SYS_bind, s, name, namelen);
  EXIT_CRITICAL;
  return result;
}

int chdir(path)
char *path;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_chdir, path);
  EXIT_CRITICAL;
  return result;
}

int chmod(path, mode)
char *path;
mode_t mode;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_chmod, path, mode);
  EXIT_CRITICAL;
  return result;
}

int chown(path, owner, group)
char *path;
uid_t owner;
gid_t group;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_chown, path, owner, group);
  EXIT_CRITICAL;
  return result;
}

int chroot(dirname)
char *dirname;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(dirname);
  result = syscall(SYS_chroot, dirname);
  EXIT_CRITICAL;
  return result;
}

int connect(s, name, namelen)
int s;
struct sockaddr *name;
int namelen;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name);
  result = syscall(SYS_connect, s, name, namelen);
  EXIT_CRITICAL;
  return result;
}

int creat(name, mode)
char *name;
mode_t mode;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name);
  result = syscall(SYS_creat, name, mode);
  EXIT_CRITICAL;
  return result;
}

/* execve is implemented differently since it does not return, which
   would leave RT0u__inCritical set in the parent if called in the child
   of a vfork. Many calls leave the process in an undefined state in the
   case of EFAULT, but we assume that execve is not one of these. */

int execve(name, argv, envp)
char *name;
char *argv[];
char *envp[];
{ int result;

  for (;;) {
    result = syscall(SYS_execve, name, argv, envp);
    if (result == -1 && errno == EFAULT) {
      MAKE_READABLE(name);
      { char **a; for (a = argv; *a; a++) MAKE_READABLE(*a); }
      { char **e; for (e = envp; *e; e++) MAKE_READABLE(*e); }
    } else {
      return result;
    }
  }
}

int fcntl(fd, request, arg)
int fd, request, arg;
{ int result;

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

int fstatfs(fd, buf)
int fd;
struct statfs *buf;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = syscall(SYS_fstatfs, fd, buf);
  EXIT_CRITICAL;
  return result;
}

int fstat(fd, buf)
int fd;
struct stat *buf;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = syscall(SYS_fstat, fd, buf);
  EXIT_CRITICAL;
  return result;
}

int getdents(fd, buf, nbytes)
int fd;
char *buf;
int nbytes;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = syscall(SYS_getdents, fd, buf, nbytes);
  EXIT_CRITICAL;
  return result;
}

int getdirentries(fd, buf, nbytes, basep)
int fd;
char *buf;
int nbytes;
long *basep;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  MAKE_WRITABLE(basep);
  result = syscall(SYS_getdirentries, fd, buf, nbytes, basep);
  EXIT_CRITICAL;
  return result;
}

int getdomainname(name, namelen)
char *name;
int namelen;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(name);
  result = syscall(SYS_getdomainname, name, namelen);
  EXIT_CRITICAL;
  return result;
}

int gethostname(name, namelen)
char *name;
int namelen;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(name);
  result = syscall(SYS_gethostname, name, namelen);
  EXIT_CRITICAL;
  return result;
}

int getgroups(gidsetsize, grouplist)
int gidsetsize;
int grouplist[];
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(grouplist);
  result = syscall(SYS_getgroups, gidsetsize, grouplist);
  EXIT_CRITICAL;
  return result;
}

int getitimer(which, value)
int which;
struct itimerval *value;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(value);
  result = syscall(SYS_getitimer, which, value);
  EXIT_CRITICAL;
  return result;
}

int getmsg(fd, ctlptr, dataptr, flags)
int fd;
struct strbuf *ctlptr;
struct strbuf *dataptr;
int *flags;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(ctlptr);
  MAKE_WRITABLE(dataptr);
  result = syscall(SYS_getmsg, fd, ctlptr, dataptr, flags);
  EXIT_CRITICAL;
  return result;
}

int getpeername(s, name, namelen)
int s;
struct sockaddr *name;
int *namelen;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(name);
  MAKE_WRITABLE(namelen);
  result = syscall(SYS_getpeername, s, name, namelen);
  EXIT_CRITICAL;
  return result;
}

int getrlimit(resource, rlp)
int resource;
struct rlimit *rlp;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(rlp);
  result = syscall(SYS_getrlimit, resource, rlp);
  EXIT_CRITICAL;
  return result;
}

int getrusage(who, rusage)
int who;
struct rusage *rusage;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(rusage);
  result = syscall(SYS_getrusage, who, rusage);
  EXIT_CRITICAL;
  return result;
}

int getsockname(s, name, namelen)
int s;
struct sockaddr *name;
int *namelen;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(name);
  MAKE_WRITABLE(namelen);
  result = syscall(SYS_getsockname, s, name, namelen);
  EXIT_CRITICAL;
  return result;
}

int getsockopt(s, level, optname, optval, optlen)
int s, level, optname;
char *optval;
int *optlen;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(optval);
  MAKE_WRITABLE(optlen);
  result = syscall(SYS_getsockopt, s, level, optname, optval, optlen);
  EXIT_CRITICAL;
  return result;
}

int gettimeofday(tp, tzp)
struct timeval *tp;
struct timezone *tzp;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(tp);
  MAKE_WRITABLE(tzp);
  result = syscall(SYS_gettimeofday, tp, tzp);
  EXIT_CRITICAL;
  return result;
}

/* ioctl must test the argp argument carefully.  It may be a pointer,
   or maybe not.  At a slight expense, we call RTHeapRep.Fault to
   unprotect the page if it's in the traced heap, but do nothing
   otherwise. */

int ioctl(d, request, argp)
int d, request;
char *argp;
{ int result;

  ENTER_CRITICAL;
  if (RTHeapRep_Fault) RTHeapRep_Fault(argp); /* make it readable */
  if (RTHeapRep_Fault) RTHeapRep_Fault(argp); /* make it writable */
  result = syscall(SYS_ioctl, d, request, argp);
  EXIT_CRITICAL;
  return result;
}

int link(name1, name2)
char *name1;
char *name2;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name1);
  MAKE_READABLE(name2);
  result = syscall(SYS_link, name1, name2);
  EXIT_CRITICAL;
  return result;
}

int lstat(path, buf)
char *path;
struct stat *buf;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = syscall(SYS_lstat, path, buf);
  EXIT_CRITICAL;
  return result;
}

int mctl(addr, len, function, arg)
caddr_t addr;
size_t len;
int function;
void *arg;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(arg);
  result = syscall(SYS_mctl, addr, len, function, arg);
  EXIT_CRITICAL;
  return result;
}

int mincore(addr, len, vec)
caddr_t addr;
int len;
char *vec;
{ int result;
  ENTER_CRITICAL;
  MAKE_WRITABLE(vec);
  result = syscall(SYS_mincore, addr, len, vec);
  EXIT_CRITICAL;
  return result;
}

int mkdir(path, mode)
char *path;
mode_t mode;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_mkdir, path, mode);
  EXIT_CRITICAL;
  return result;
}

int mknod(path, mode, dev)
char *path;
mode_t mode;
int dev;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_mknod, path, mode, dev);
  EXIT_CRITICAL;
  return result;
}

int mount(type, dir, flags, data)
char *type;
char *dir;
int flags;
caddr_t data;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(type);
  MAKE_READABLE(dir);
  if (flags & M_NEWTYPE) {
    MAKE_READABLE(data);
  }
  result = syscall(SYS_mount, type, dir, flags, data);
  EXIT_CRITICAL;
  if (result != -1) {
    result = 0;
  }
  return result;
}

int msgctl(msqid, cmd, buf)
int msqid, cmd;
struct msqid_ds *buf;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = syscall(SYS_msgsys, 1, msqid, cmd, buf);
  EXIT_CRITICAL;
  if (result != -1) {
    result = 0;
  }
  return result;
}

int msgrcv(msqid, msgp, msgsz, msgtyp, msgflg)
int msqid;
struct msgbuf *msgp;
int msgsz;
long msgtyp;
int msgflg;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(msgp);
  result = syscall(SYS_msgsys, 2, msqid, msgp, msgsz, msgtyp, msgflg);
  EXIT_CRITICAL;
  return result;
}

int msgsnd(msqid, msgp, msgsz, msgflg)
int msqid;
struct msgbuf *msgp;
int msgsz;
int msgflg;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(msgp);
  result = syscall(SYS_msgsys, 3, msqid, msgp, msgsz, msgflg);
  EXIT_CRITICAL;
  return result;
}

int open(path, flags, mode)
char *path;
int flags, mode;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_open, path, flags, mode);
  EXIT_CRITICAL;
  return result;
}

int pathconf(path, name)
char *path;
int name;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_pathconf, path, name);
  EXIT_CRITICAL;
  return result;
}

int poll(fds, nfds, timeout)
struct pollfd *fds;
unsigned long nfds;
int timeout;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(fds);
  result = syscall(SYS_poll, fds, nfds, timeout);
  EXIT_CRITICAL;
  return result;
}

int putmsg(fd, ctlptr, dataptr, flags)
int fd;
struct strbuf *ctlptr;
struct strbuf *dataptr;
int flags;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(ctlptr);
  MAKE_READABLE(dataptr);
  result = syscall(SYS_putmsg, fd, ctlptr, dataptr, flags);
  EXIT_CRITICAL;
  return result;
}

int quotactl(cmd, special, uid, addr)
int cmd;
char *special;
int uid;
caddr_t addr;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(special);
  switch(cmd) {
  case Q_QUOTAON:
  case Q_SETQUOTA:
  case Q_SETQLIM:
    MAKE_READABLE(addr);
    break;
  case Q_GETQUOTA:
    MAKE_WRITABLE(addr);
    break;
  default:
    break;
  }
  result = syscall(SYS_quotactl, special, uid, addr);
  EXIT_CRITICAL;
  return result;
}

int read(d, buf, nbytes)
int d;
char *buf;
int nbytes;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = syscall(SYS_read, d, buf, nbytes);
  EXIT_CRITICAL;
  return result;
}

int readlink(path, buf, bufsiz)
char *path;
char *buf;
int bufsiz;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = syscall(SYS_readlink, path, buf, bufsiz);
  EXIT_CRITICAL;
  return result;
}

int readv(d, iov, iovcnt)
int d;
struct iovec *iov;
int iovcnt;
{ int result;

  ENTER_CRITICAL;
  { int i;
    for (i = 0; i < iovcnt; i++) {
      MAKE_WRITABLE(iov[i].iov_base);
    }
  }
  result = syscall(SYS_readv, d, iov, iovcnt);
  EXIT_CRITICAL;
  return result;
}

int reboot(howto, bootargs)
int howto;
char *bootargs;
{ int result;

  ENTER_CRITICAL;
  if (howto == RB_STRING) {
    MAKE_READABLE(bootargs);
  }
  result = syscall(SYS_reboot, howto, bootargs);
  EXIT_CRITICAL;
  return result;
}

int recv(s, buf, len, flags)
int s;
char *buf;
int len, flags;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = syscall(SYS_recv, s, buf, len, flags);
  EXIT_CRITICAL;
  return result;
}

int recvfrom(s, buf, len, flags, from, fromlen)
int s;
char *buf;
int len, flags;
struct sockaddr *from;
int *fromlen;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  MAKE_WRITABLE(from);
  MAKE_WRITABLE(fromlen);
  result = syscall(SYS_recvfrom, s, buf, len, flags, from, fromlen);
  EXIT_CRITICAL;
  return result;
}

int recvmsg(s, msg, flags)
int s;
struct msghdr msg[];
int flags;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(msg->msg_name);
  { int i;
    for (i = 0; i < msg->msg_iovlen; i++) {
      if (msg->msg_iov[i].iov_len > 0) {
        MAKE_WRITABLE(msg->msg_iov[i].iov_base);
      }
    }
  }
  MAKE_WRITABLE(msg->msg_accrights);
  result = syscall(SYS_recvmsg, s, msg, flags);
  EXIT_CRITICAL;
  return result;
}

int rename(from, to)
char *from;
char *to;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(from);
  MAKE_READABLE(to);
  result = syscall(SYS_rename, from, to);
  EXIT_CRITICAL;
  return result;
}

int rmdir(path)
char *path;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_rmdir, path);
  EXIT_CRITICAL;
  return result;
}

int select(nfds, readfds, writefds, exceptfds, timeout)
fd_set *readfds;
fd_set *writefds;
fd_set *exceptfds;
struct timeval *timeout;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(readfds);
  MAKE_WRITABLE(writefds);
  MAKE_WRITABLE(exceptfds);
  MAKE_READABLE(timeout);
  result = syscall(SYS_select, nfds, readfds, writefds, exceptfds, timeout);
  EXIT_CRITICAL;
  return result;
}

int semctl(semid, semnum, cmd, arg)
int semid, semnum, cmd;
union semun {
  int val;
  struct semid_ds *buf;
  ushort *array;
} arg;
{ int result;

  ENTER_CRITICAL;
  switch (cmd) {
  case IPC_SET:
    MAKE_READABLE(arg.buf);
    break;
  case GETALL:
  case IPC_STAT:
    MAKE_WRITABLE(arg.buf);
    break;
  default:
    break;
  }
  result = syscall(SYS_semsys, 0, semid, semnum, cmd, arg);
  EXIT_CRITICAL;
  return result;
}

int semop(semid, sops, nsops)
int semid;
struct sembuf *sops[];
int nsops;
{ int result;

  ENTER_CRITICAL;
  { int i;
    for (i = 0; i < nsops; i++) {
      MAKE_READABLE(sops[i]);
    }
  }
  result = syscall(SYS_semsys, 2, semid, sops, nsops);
  EXIT_CRITICAL;
  return result;
}

int send(s, msg, len, flags)
int s;
char *msg;
int len, flags;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(msg);
  result = syscall(SYS_send, s, msg, len, flags);
  EXIT_CRITICAL;
  return result;
}

int sendmsg(s, msg, flags)
int s;
struct msghdr msg[];
int flags;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(msg->msg_name);
  { int i;
    for (i = 0; i < msg->msg_iovlen; i++) {
      if (msg->msg_iov[i].iov_len > 0) {
        MAKE_READABLE(msg->msg_iov[i].iov_base);
      }
    }
  }
  MAKE_WRITABLE(msg->msg_accrights);
  result = syscall(SYS_sendmsg, s, msg, flags);
  EXIT_CRITICAL;
  return result;
}

int sendto(s, msg, len, flags, to, tolen)
int s;
char *msg;
int len, flags;
struct sockaddr *to;
int tolen;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(msg);
  MAKE_READABLE(to);
  result = syscall(SYS_sendto, s, msg, len, flags, to, tolen);
  EXIT_CRITICAL;
  return result;
}

int setdomainname(name, namelen)
char *name;
int namelen;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name);
  result = syscall(SYS_setdomainname, name, namelen);
  EXIT_CRITICAL;
  return result;
}

int setgroups(ngroups, gidset)
int ngroups;
int *gidset;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(gidset);
  result = syscall(SYS_setgroups, ngroups, gidset);
  EXIT_CRITICAL;
  return result;
}

int sethostname(name, namelen)
char *name;
int namelen;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name);
  result = syscall(SYS_sethostname, name, namelen);
  EXIT_CRITICAL;
  return result;
}

int setitimer(which, value, ovalue)
int which;
struct itimerval *value;
struct itimerval *ovalue;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(value);
  MAKE_WRITABLE(ovalue);
  result = syscall(SYS_setitimer, which, value, ovalue);
  EXIT_CRITICAL;
  return result;
}

int setrlimit(resource, rlp)
int resource;
struct rlimit *rlp;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(rlp);
  result = syscall(SYS_setrlimit, resource, rlp);
  EXIT_CRITICAL;
  return result;
}

int setsockopt(s, level, optname, optval, optlen)
int s, level, optname;
char *optval;
int optlen;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(optval);
  result = syscall(SYS_setsockopt, s, level, optname, optval, optlen);
  EXIT_CRITICAL;
  return result;
}

int settimeofday(tp, tzp)
struct timeval *tp;
struct timezone *tzp;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(tp);
  MAKE_READABLE(tzp);
  result = syscall(SYS_settimeofday, tp, tzp);
  EXIT_CRITICAL;
  return result;
}

int setuseraudit(uid, state)
int uid;
audit_state_t *state;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(state);
  result = syscall(SYS_auditsys, 5, uid, state);
  EXIT_CRITICAL;
  return result;
}

int setaudit(state)
audit_state_t *state;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(state);
  result = syscall(SYS_auditsys, 6, state);
  EXIT_CRITICAL;
  return result;
}

int shmctl(shmid, cmd, buf)
int shmid, cmd;
struct shmid_ds *buf;
{ int result;

  ENTER_CRITICAL;
  if (cmd == IPC_STAT || cmd == IPC_SET) {
    MAKE_WRITABLE(buf);
  }
  result = syscall(SYS_shmsys, 1, shmid, cmd, buf);
  EXIT_CRITICAL;
  return result;
}

int sigpending(set)
sigset_t *set;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(set);
  result = syscall(SYS_sigpending, set);
  EXIT_CRITICAL;
  return result;
}

int sigstack(ss, oss)
struct sigstack *ss;
struct sigstack *oss;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(ss);
  MAKE_WRITABLE(oss);
  result = syscall(SYS_sigstack, ss, oss);
  EXIT_CRITICAL;
  return result;
}

int socketpair(d, type, protocol, sv)
int d, type, protocol;
int sv[2];
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(sv);
  result = syscall(SYS_socketpair, d, type, protocol, sv);
  EXIT_CRITICAL;
  return result;
}

int stat(path, buf)
char *path;
struct stat *buf;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = syscall(SYS_stat, path, buf);
  EXIT_CRITICAL;
  return result;
}

int statfs(path, buf)
char *path;
struct statfs *buf;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  MAKE_WRITABLE(buf);
  result = syscall(SYS_statfs, path, buf);
  EXIT_CRITICAL;
  return result;
}

int swapon(special)
char *special;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(special);
  result = syscall(SYS_swapon, special);
  EXIT_CRITICAL;
  return result;
}

int symlink(name1, name2)
char *name1;
char *name2;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name1);
  MAKE_READABLE(name2);
  result = syscall(SYS_symlink, name1, name2);
  EXIT_CRITICAL;
  return result;
}

int truncate(path, length)
char *path;
int length;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_truncate, path, length);
  EXIT_CRITICAL;
  return result;
}

int unmount(name)
char *name;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name);
  result = syscall(SYS_unmount, name);
  EXIT_CRITICAL;
  return result;
}

int uname(name)
struct utsname *name;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(name);
  result = syscall(SYS_uname, name);
  EXIT_CRITICAL;
  return result;
}

int unlink(path)
char *path;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(path);
  result = syscall(SYS_unlink, path);
  EXIT_CRITICAL;
  return result;
}

int ustat(dev, buf)
dev_t dev;
struct ustat *buf;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = syscall(SYS_ustat, dev, buf);
  EXIT_CRITICAL;
  return result;
}

int utimes(file, tvp)
char *file;
struct timeval *tvp;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(file);
  MAKE_READABLE(tvp);
  result = syscall(SYS_utimes, file, tvp);
  EXIT_CRITICAL;
  return result;
}

pid_t wait4(pid, statusp, options, rusage)
int pid;
int *statusp;
int options;
struct rusage *rusage;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(statusp);
  MAKE_WRITABLE(rusage);
  result = syscall(SYS_wait4, pid, statusp, options, rusage);
  EXIT_CRITICAL;
  return result;
}

int write(fd, buf, nbytes)
int fd;
char *buf;
int nbytes;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(buf);
  result = syscall(SYS_write, fd, buf, nbytes);
  EXIT_CRITICAL;
  return result;
}

int writev(fd, iov, ioveclen)
int fd;
struct iovec *iov;
int ioveclen;
{ int result;

  ENTER_CRITICAL;
  { int i;
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

