/* Copyright (C) 1992, Digital Equipment Corporation */
/* All rights reserved. */
/* See the file COPYRIGHT for a full description. */

/* Last modified on Fri Jun 21 12:45:49 PDT 1996 by heydon */
/*      modified on Thu Jun 29 09:42:09 PDT 1995 by kalsow */
/*      modified on Tue Feb  2 11:15:57 PST 1993 by jdd */

/* This is RTHeapDepC.c for Linux. */

/* This file implements wrappers for almost all Linux system calls
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
#include <sys/socketcall.h>
#include <sys/uio.h>
#include <sys/ipc.h>
#include <dirent.h>

extern int RT0u__inCritical;
#define ENTER_CRITICAL RT0u__inCritical++
#define EXIT_CRITICAL  RT0u__inCritical--

void (*RTHeapRep_Fault)(char*);
void (*RTCSRC_FinishVM)();

static char RTHeapDepC__c;
#define MAKE_READABLE(x) if (x) { RTHeapDepC__c = *(char*)(x); }
#define MAKE_WRITABLE(x) if (x) { *(char*)(x) = RTHeapDepC__c = *(char*)(x); }

/* Unless otherwise noted, all the following wrappers have the same
   structure. */

int accept(sockfd, peer, paddrlen)
int sockfd;
const struct sockaddr *peer;
int *paddrlen;
{ int result;

  unsigned long args[3];

  ENTER_CRITICAL;
  MAKE_WRITABLE(peer);
  MAKE_WRITABLE(paddrlen);

  args[0] = sockfd;
  args[1] = (unsigned long)peer;
  args[2] = (unsigned long)paddrlen;
  result = socketcall(SYS_ACCEPT, args);

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
  result = __adjtime(delta, olddelta);
  EXIT_CRITICAL;
  return result;
}

/*
int atomic_op(op, addr)
int op;
int *addr;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(addr);
  result = syscall(SYS_atomic_op, op, addr);
  EXIT_CRITICAL;
  return result;
}

int audcntl(request, argp, len, flag, audit_id)
int request;
char *argp;
int len;
int flag;
audit_ID_t audit_id;
{ int result;

  ENTER_CRITICAL;
  switch (request) {
  case GET_SYS_AMASK:
  case GET_TRUSTED_AMASK:
  case GET_PROC_AMASK:
    MAKE_WRITABLE(argp);
    break;
  case SET_SYS_AMASK:
  case SET_TRUSTED_AMASK:
  case SET_PROC_AMASK:
    MAKE_READABLE(argp);
    break;
  default:
    break;
  }
  result = syscall(SYS_audcntl, request, argp, len, flag, audit_id);
  EXIT_CRITICAL;
  return result;
}

int audgen(event, tokenp, argv)
int event;
char *tokenp, *argv[];
{ int result;

  ENTER_CRITICAL;
  
  { char *t, **a;

    for (t = tokenp, a = argv; *t; t++, a++) {
      if (A_TOKEN_PTR(*t)) {
        MAKE_READABLE(*a);
      }
    }
  }
  result = syscall(SYS_audgen, tokenp, argv);
  EXIT_CRITICAL;
  return result;
}
*/

int bind(sockfd, myaddr, addrlen)
int sockfd;
const struct sockaddr *myaddr;
int addrlen;
{ int result;

  unsigned long args[3];

  ENTER_CRITICAL;
  MAKE_READABLE(myaddr);

  args[0] = sockfd;
  args[1] = (unsigned long)myaddr;
  args[2] = addrlen;
  result = socketcall(SYS_BIND, args);

  EXIT_CRITICAL;
  return result;
}

/*
int cachectl(addr, nbytes, op)
char *addr;
int nbytes, op;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(addr);
  result = syscall(SYS_cachectl, addr, nbytes, op);
  EXIT_CRITICAL;
  return result;
}

int cacheflush(addr, nbytes, cache)
char *addr;
int nbytes, cache;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(addr);
  result = syscall(SYS_cacheflush, addr, nbytes, cache);
  EXIT_CRITICAL;
  return result;
}
*/

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

int connect(sockfd, saddr, addrlen)
int sockfd;
const struct sockaddr *saddr;
int addrlen;
{ int result;

  unsigned long args[3];

  ENTER_CRITICAL;
  MAKE_READABLE(saddr);

  args[0] = sockfd;
  args[1] = (unsigned long)saddr;
  args[2] = addrlen;
  result = socketcall(SYS_CONNECT, args);

  EXIT_CRITICAL;
  return result;
}

int creat(pathname, mode)
const char *pathname;
mode_t mode;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(pathname);
  result = syscall(SYS_creat, pathname, mode);
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

/*
int exportfs(name, rootuid, exflags)
char *name;
int rootuid, exflags;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(name);
  result = syscall(SYS_exportfs, name, rootuid, exflags);
  EXIT_CRITICAL;
  return result;
}
*/

int fcntl(int fd, int cmd, ...)
{ int result;

  va_list ap;

  long arg;

  ENTER_CRITICAL;
  switch (cmd) {
  case F_GETLK:
    va_start(ap,cmd);
    arg = va_arg(ap,long);
    MAKE_WRITABLE(arg);
    result = syscall(SYS_fcntl, fd, cmd, arg);
    va_end(ap);
    break;
  case F_SETLK:
  case F_SETLKW:
    va_start(ap,cmd);
    arg = va_arg(ap,long);
    MAKE_READABLE(arg);
    result = syscall(SYS_fcntl, fd, cmd, arg);
    va_end(ap);
    break;
  case F_SETFD:
  case F_SETFL:
  case F_DUPFD:
    va_start(ap,cmd);
    arg = va_arg(ap,long);
    result = syscall(SYS_fcntl, fd, cmd, arg);
    va_end(ap);
    break;
  default:
    result = syscall(SYS_fcntl, fd, cmd);
    break;
  }

  EXIT_CRITICAL;
  return result;
}

int fstat(fd, buf)
int fd;
struct stat *buf;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  result = _fxstat(1, fd, buf);
  EXIT_CRITICAL;
  return result;
}

int getdents(unsigned int fd, struct dirent *dirp, unsigned int count)
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(dirp);
  result = __getdents(fd, dirp, count);
  EXIT_CRITICAL;
  return result;
}

int getdirentries(fd, buf, nbytes, basep)
int fd;
char *buf;
size_t nbytes;
long *basep;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buf);
  MAKE_WRITABLE(basep);
  result = __getdirentries(fd, buf, nbytes, basep);
  EXIT_CRITICAL;
  return result;
}

/*
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
*/

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

/*
int getmnt(start, buffer, nbytes, mode, path)
int *start;
struct fs_data *buffer;
int nbytes, mode;
char *path;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(start);
  MAKE_WRITABLE(buffer);
  MAKE_READABLE(path);
  result = syscall(SYS_getmnt, start, buffer, nbytes, mode, path);
  EXIT_CRITICAL;
  return result;
}
*/

int getpeername(sockfd, addr, paddrlen)
int sockfd;
struct sockaddr *addr;
int *paddrlen;
{ int result;

  unsigned long args[3];

  ENTER_CRITICAL;
  MAKE_WRITABLE(addr);
  MAKE_WRITABLE(paddrlen);

  args[0] = sockfd;
  args[1] = (unsigned long)addr;
  args[2] = (unsigned long)paddrlen;
  result = socketcall(SYS_GETPEERNAME, args);

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

int getsockname(sockfd, addr, paddrlen)
int sockfd;
struct sockaddr *addr;
int *paddrlen;
{ int result;

  unsigned long args[3];

  ENTER_CRITICAL;
  MAKE_WRITABLE(addr);
  MAKE_WRITABLE(paddrlen);

  args[0] = sockfd;
  args[1] = (unsigned long)addr;
  args[2] = (unsigned long)paddrlen;
  result = socketcall(SYS_GETSOCKNAME, args);

  EXIT_CRITICAL;
  return result;
}

int getsockopt(fd, level, optname, optval, optlen)
int fd, level, optname;
void *optval;
int *optlen;
{ int result;

  unsigned long args[5];

  ENTER_CRITICAL;
  MAKE_WRITABLE(optval);
  MAKE_WRITABLE(optlen);

  args[0]=fd;
  args[1]=level;
  args[2]=optname;
  args[3]=(unsigned long)optval;
  args[4]=(unsigned long)optlen;
  result = (socketcall (SYS_GETSOCKOPT, args));

  EXIT_CRITICAL;
  return result;
}

/*
int getsysinfo(op, buffer, nbytes, start, arg)
unsigned op;
char *buffer;
unsigned nbytes;
int *start;
char *arg;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(buffer);
  MAKE_WRITABLE(start);
  MAKE_WRITABLE(arg);
  result = syscall(SYS_getsysinfo, op, buffer, nbytes, start, arg);
  EXIT_CRITICAL;
  return result;
}
*/

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
  result = _lxstat(1, path, buf);
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
  /** result = _xmknod(path, mode, dev);  -- WKK 9/30/96 **/
  result = _xmknod(1, path, mode, dev);
  EXIT_CRITICAL;
  return result;
}

int mount(special, name, rwflag, type, options)
char *special;
char *name;
int rwflag, type;
char *options;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(special);
  MAKE_READABLE(name);
  MAKE_READABLE(options);
  result = syscall(SYS_mount, special, name, rwflag, type, options);
  EXIT_CRITICAL;
  if (result != -1) {
    result = 0;
  }
  return result;
}

/* Taken from linux/ip.h, but normally only accessible to kernel */

struct ipc_kludge {
    struct msgbuf *msgp;
    long msgtyp;
};

#define SEMOP           1
#define SEMGET          2
#define SEMCTL          3
#define MSGSND          11
#define MSGRCV          12
#define MSGGET          13
#define MSGCTL          14
#define SHMAT           21
#define SHMDT           22
#define SHMGET          23
#define SHMCTL          24

int msgctl(msqid, cmd, buf)
int msqid, cmd;
struct msqid_ds *buf;
{ int result;

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

  result = __ipc(MSGCTL, msqid, cmd, 0, buf);
  EXIT_CRITICAL;
  return result;
}

int msgrcv(msqid, msgp, msgsz, msgtyp, msgflg)
int msqid;
struct msgbuf *msgp;
int msgsz;
long msgtyp;
int msgflg;
{ int result;

  struct ipc_kludge tmp; 

  ENTER_CRITICAL;
  MAKE_WRITABLE(msgp);

  tmp.msgp = msgp;
  tmp.msgtyp = msgtyp; 
  result = __ipc (MSGRCV, msqid, msgsz, msgflg, &tmp);

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

  result = __ipc(MSGSND, msqid, msgsz, msgflg,  msgp);

  EXIT_CRITICAL;
  return result;
}

int open(const char *filename, int flags, ...)
{ int result;

  va_list ap;

  ENTER_CRITICAL;
  MAKE_READABLE(filename);
  if (flags & O_CREAT) {
    va_start(ap,flags);
    result = syscall(SYS_open, filename, flags, va_arg(ap,mode_t));
    va_end(ap);
  } else {
    result = syscall(SYS_open, filename, flags);
  }
  EXIT_CRITICAL;
  return result;
}

/*
int quota(cmd, uid, arg, addr)
int cmd, uid, arg;
caddr_t addr;
{ int result;

  ENTER_CRITICAL;
  switch (cmd) {
  case Q_SETDLIM:
  case Q_SETDUSE:
  case Q_SETWARN:
    MAKE_READABLE(addr);
    break;
  case Q_GETDLIM:
    MAKE_WRITABLE(addr);
    break;
  default:
    break;
  }
  result = syscall(SYS_quota, cmd, uid, arg, addr);
  EXIT_CRITICAL;
  return result;
}
*/

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

int readv(d, iov, count)
int d;
const struct iovec *iov;
size_t count;
{ int result;

  ENTER_CRITICAL;
  { int i;
    for (i = 0; i < count; i++) {
      MAKE_WRITABLE(iov[i].iov_base);
    }
  }
  result = __readv(d, iov, count);
  /*****************
  result = syscall(SYS_readv, d, iov, count);
  *****************/
  EXIT_CRITICAL;
  return result;
}

int recv(sockfd, buffer, len, flags)
int sockfd;
void *buffer;
size_t len;
unsigned int flags;
{ int result;

  unsigned long args[4];

  ENTER_CRITICAL;
  MAKE_WRITABLE(buffer);

  args[0] = sockfd;
  args[1] = (unsigned long) buffer;
  args[2] = len;
  args[3] = flags;
  result = (socketcall (SYS_RECV, args));

  EXIT_CRITICAL;
  return result;
}

int recvfrom(sockfd, buffer, len, flags, to, tolen)
int sockfd;
void *buffer;
size_t len;
unsigned flags;
struct sockaddr *to;
int *tolen;
{ int result;

  unsigned long args[6];

  ENTER_CRITICAL;
  MAKE_WRITABLE(buffer);
  MAKE_WRITABLE(to);
  MAKE_WRITABLE(tolen);

  args[0] = sockfd;
  args[1] = (unsigned long) buffer;
  args[2] = len;
  args[3] = flags;
  args[4] = (unsigned long) to;
  args[5] = (unsigned long) tolen;
  result = (socketcall (SYS_RECVFROM, args));

  EXIT_CRITICAL;
  return result;
}

int recvmsg(sockfd, msg, flags)
int sockfd;
struct msghdr *msg;
unsigned flags;
{ int result;

  unsigned long args[3];

  ENTER_CRITICAL;

/*
  MAKE_WRITABLE(msg->msg_name);
  { int i;
    for (i = 0; i < msg->msg_iovlen; i++) {
      if (msg->msg_iov[i].iov_len > 0) {
        MAKE_WRITABLE(msg->msg_iov[i].iov_base);
      }
    }
  }
  MAKE_WRITABLE(msg->msg_accrights);
*/

  MAKE_WRITABLE(msg);

  args[0] = sockfd;
  args[1] = (unsigned long) msg;
  args[2] = flags;
  result = (socketcall (SYS_RECVMSG, args));

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

int select(int n, fd_set *readfds, fd_set *writefds, fd_set *exceptfds, 
    struct timeval *timeout)
{ int result;

  ENTER_CRITICAL;
  if (readfds != NULL) MAKE_WRITABLE(readfds);
  if (writefds != NULL) MAKE_WRITABLE(writefds);
  if (exceptfds != NULL) MAKE_WRITABLE(exceptfds);
  if (timeout != NULL) MAKE_WRITABLE(timeout);
  result = __select(n, readfds, writefds, exceptfds, timeout);
  EXIT_CRITICAL;
  return result;
}

int semctl(semid, semnum, cmd, arg)
int semid, cmd;
int semnum;
union semun arg;
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

  result = __ipc (SEMCTL, semid, semnum, cmd, &arg);

  EXIT_CRITICAL;
  return result;
}

int semop(semid, sops, nsops)
int semid;
struct sembuf *sops;
unsigned nsops;
{ int result;

  ENTER_CRITICAL;
  { int i;
    for (i = 0; i < nsops; i++) {
      MAKE_READABLE(sops + i);
    }
  }

  result = __ipc (SEMOP, semid, (int) nsops, 0, sops);

  EXIT_CRITICAL;
  return result;
}

int send(sockfd, buffer, len, flags)
int sockfd;
const void *buffer;
size_t len;
unsigned flags;
{ int result;

  unsigned long args[4];

  ENTER_CRITICAL;
  MAKE_READABLE(buffer);

  args[0] = sockfd;
  args[1] = (unsigned long) buffer;
  args[2] = len;
  args[3] = flags;
  result = (socketcall (SYS_SEND, args));

  EXIT_CRITICAL;
  return result;
}

int sendmsg(sockfd, msg, flags)
int sockfd;
const struct msghdr *msg;
unsigned flags;
{ int result;

  unsigned long args[3];

  ENTER_CRITICAL;

/*
  MAKE_READABLE(msg->msg_name);
  { int i;
    for (i = 0; i < msg->msg_iovlen; i++) {
      if (msg->msg_iov[i].iov_len > 0) {
        MAKE_READABLE(msg->msg_iov[i].iov_base);
      }
    }
  }
  MAKE_WRITABLE(msg->msg_accrights);
*/

  MAKE_WRITABLE(msg);

  args[0] = sockfd;
  args[1] = (unsigned long) msg;
  args[2] = flags;
  result = (socketcall (SYS_SENDMSG, args));

  EXIT_CRITICAL;
  return result;
}

int sendto(sockfd, buffer, len, flags, to, tolen)
int sockfd;
const void *buffer;
size_t len;
unsigned flags;
const struct sockaddr *to;
int tolen;
{ int result;

  unsigned long args[6];

  ENTER_CRITICAL;
  MAKE_READABLE(buffer);
  MAKE_READABLE(to);

  args[0] = sockfd;
  args[1] = (unsigned long) buffer;
  args[2] = len;
  args[3] = flags;
  args[4] = (unsigned long) to;
  args[5] = tolen;
  result = (socketcall (SYS_SENDTO, args));

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
const struct itimerval *value;
struct itimerval *ovalue;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(value);
  MAKE_WRITABLE(ovalue);
  result = syscall(SYS_setitimer, which, value, ovalue);
  EXIT_CRITICAL;
  return result;
}

/*
int setquota(special, file)
char *special;
char *file;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(special);
  MAKE_READABLE(file);
  result = syscall(SYS_setquota, special, file);
  EXIT_CRITICAL;
  return result;
}
*/

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

int setsockopt(fd, level, optname, optval, optlen)
int fd, level, optname;
const void *optval;
int optlen;
{ int result;

  unsigned long args[5];

  ENTER_CRITICAL;
  MAKE_READABLE(optval);

  args[0]=fd;
  args[1]=level;
  args[2]=optname;
  args[3]=(unsigned long)optval;
  args[4]=optlen;
  result = (socketcall (SYS_SETSOCKOPT, args));

  EXIT_CRITICAL;
  return result;
}

/*
int setsysinfo(op, buffer, nbytes, arg, flag)
unsigned op;
char *buffer;
unsigned nbytes;
unsigned arg;
unsigned flag;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(buffer);
  result = syscall(SYS_setsysinfo, op, buffer, nbytes, arg, flag);
  EXIT_CRITICAL;
  return result;
}
*/

int settimeofday(tp, tzp)
const struct timeval *tp;
const struct timezone *tzp;
{ int result;

  ENTER_CRITICAL;
  MAKE_READABLE(tp);
  MAKE_READABLE(tzp);
  result = syscall(SYS_settimeofday, tp, tzp);
  EXIT_CRITICAL;
  return result;
}

int sigaction(int signum, struct sigaction *act, struct sigaction *oldact)
{ int result;

  ENTER_CRITICAL;
  if(oldact != NULL) MAKE_WRITABLE(oldact);
  MAKE_READABLE(act);
  result = syscall(SYS_sigaction, signum, act, oldact);
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

int sigprocmask(int how, const sigset_t *set, sigset_t *oldset)
{ int result;

  ENTER_CRITICAL;
  if (oldset != NULL) MAKE_WRITABLE(oldset);
  MAKE_READABLE(set);
  result = syscall(SYS_sigprocmask, how, set, oldset);
  EXIT_CRITICAL;
  return result;
}

/*
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
*/

int socketpair(family, type, protocol, sockvec)
int family, type, protocol;
int sockvec[2];
{ int result;

  unsigned long args[4];

  ENTER_CRITICAL;
  MAKE_WRITABLE(sockvec);

  args[0] = family;
  args[1] = type;
  args[2] = protocol;
  args[3] = (unsigned long)sockvec;
  result = socketcall(SYS_SOCKETPAIR, args);

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
  result = _xstat(1, path, buf);
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

time_t time(time_t *t)
{ int result;

  ENTER_CRITICAL;
  if (t != NULL) MAKE_WRITABLE(t);
  result = syscall(SYS_time, t);
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
  result = _xustat(1, dev, buf);
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
  result = __utimes(file, tvp);
  EXIT_CRITICAL;
  return result;
}

pid_t wait(status)
union wait *status;
{
  return wait3(status, 0, 0);
}

pid_t wait3(status, options, rusage)
union wait *status;
int options;
struct rusage *rusage;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(status);
  MAKE_WRITABLE(rusage);
  result = __wait3(status, options, rusage);
  EXIT_CRITICAL;
  return result;
}

pid_t waitpid(pid, status, options)
union wait *status;
int options;
{ int result;

  ENTER_CRITICAL;
  MAKE_WRITABLE(status);
  result = syscall(SYS_waitpid, pid, status, options);
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

static int kernel_writev = 0; /* 0=unknown, 1=yes, 2=no */

int writev(fd, iov, ioveclen)
int fd;
const struct iovec *iov;
size_t ioveclen;
{ int result;

  ENTER_CRITICAL;
  { int i;
    for (i = 0; i < ioveclen; i++) {
      if (iov[i].iov_len > 0) {
        MAKE_READABLE(iov[i].iov_base);
      }
    }
  }
  /****** NOT all versions of Linux have a writev kernel call...
  result = __writev(fd, iov, ioveclen);
  **********/
  if (kernel_writev == 2) {
    result = fake_writev(fd, iov, ioveclen);
  } else if (kernel_writev == 1) {
    result = syscall(SYS_writev, fd, iov, ioveclen);
  } else {
    result = first_writev(fd, iov, ioveclen);
  }
  EXIT_CRITICAL;
  return result;
}

/* We don't know if writev() is a supported kernel call or not.
   So, we'll try the kernel call once and remember whether it
   fails. */

int first_writev(fd, iov, ioveclen)
int fd;
const struct iovec *iov;
size_t ioveclen;
{ int result;

  result = syscall(SYS_writev, fd, iov, ioveclen);
  if ((result > 0) || (errno != ENOSYS)) {
    kernel_writev = 1;  /* we've got kernel support for writev() */
  } else {
    kernel_writev = 2;  /* nope, we need the user-level hack */
    result = fake_writev(fd, iov, ioveclen);
  }
  return result;
}

/* Early versions of Linux supported writev() as a user-level library
   routine, not a kernel call.  Those versions copied the entire
   contents of the output buffer onto the caller's stack.  The
   scheme "works" on the initial Unix stack, but is prone to overflowing
   the fixed size Modula-3 stacks.  Unfortunately, the X11 library
   passes large bitmaps to the server via writev().  So, we deal
   with it here.  Note that this implementation makes multiple
   write() calls, hence it's not exactly equivalent to writev()
   when the size of the write matters (eg. on some sockets).  */

int fake_writev(fd, iov, ioveclen)
int fd;
const struct iovec *iov;
size_t ioveclen;
{
  int i, k, n = 0;
  for(i = 0; i < ioveclen; i++)
  {
    k = write(fd, iov[i].iov_base, iov[i].iov_len);
    if (k < 0) return k;
    n += k;
  }
  return n;
}

/* fork also requires special treatment, although it takes no
   argument.  fork crashes Ultrix if some pages are unreadable, so we
   must unprotect the heap before calling fork */

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
