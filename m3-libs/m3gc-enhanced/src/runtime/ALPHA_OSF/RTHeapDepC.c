/* Copyright (C) 1996, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */
/*                                                          */
/* Created by Allan Heydon.                                 */
/* Last modified on Fri Nov 15 13:54:00 PST 1996 by heydon  */

/* This is RTHeapDepC.c for Digital Unix running on Alpha processors. 
   It was adapted from the version of RTHeapDepC.c written for the
   Ultrix operating system running on MIPS processors. See the file
   "syscalls.txt" in this directory for a list of all the system
   calls that were considered when this file was created. */

/* This file implements wrappers for almost all Digital Unix system calls
   that take pointers as arguments.  These wrappers allow the system
   calls to take arguments that might point to the traced heap, which
   may be VM-protected in the Digital Unix implementation of the collector.
   The wrappers read and write the referents of all pointers about to be
   passed to the system call, which ensures that the pages are not
   protected when the call is made.

   Each wrapper is a critical section, with RT0u__inCritical non-zero,
   so that another thread cannot cause the pages to become reprotected
   before the system call is performed.

   A few system calls are not handled here, or are handled only
   partially.  This restricts the system calls that can be made from
   Modula-3, or from libraries that are passed pointers into the
   Modula-3 traced heap.  These system calls (and their system
   call numbers) are:

   + syscall (0).  Implementing syscall would require a huge case
     statement, with one case per system call.  This seemed too
     error-prone, so syscall cannot take arguments that point into
     the traced heap.

   + pipe (42). Although the pipe(2) system call takes as one of its
     arguments a pointer to an array of two file descriptors, it cannot
     be handled. The syscall(2) man page explicitly lists as one of
     its restrictions "There is no way to simulate system calls such
     as 'pipe', which return values in register r1." Hence, it is
     impossible to write a wrapper for this system call.

   + profil (44).  The memory referenced by the "short_buffer" argument
     is updated after the call returns, and there is no mechanism to
     permanently unprotect it.

   + ioctl (54).  Ioctl's third argument may be a pointer, and some device
     drivers might interpret the referent as containing more pointers.
     These second-level pointers are not handled here, so they must not
     point into the traced heap if they exist. Handling this problem in
     general is impossible, since the set of device drivers is open-ended.

   + audgen (253). This is partially implemented, but the man page
     and header file are unclear as to how all token types should
     be handled.

   + Undocumented system calls.  There are private system calls with
     no manual pages, so it was impossible to write wrappers for them.
     These are:

       obreak(17), unmount(22), chflags(34), fchflags(35),
       set_program_attributes(43), mremap(65), sstk(70),
       ovadvise(72), kmodcall(77), mincore(78), gettablesize(89),
       sigwaitprim(157), getmnt(184), msleep(215), mwakeup(216),
       utc_gettime(219), utc_adjtime(220), security(222),
       kloadcall(223), signalstack(235), priocntlsetcntlset(237),
       set_speculative(239), msfs_syscall(240), proplist_syscall(244),
       usleep_thread(251), subsys_info(255), afs_syscall(258)

   + Some system calls do not require wrappers because none of their
     arguments are pointers. These are:

       exit(1), close(6), fchdir(13), lseek(19), getpid(20),
       setuid(23), getuid(24), sync(36), kill(37), setpgid(39),
       dup(41), getgid(47), getlogin(49), reboot(55), umask(60),
       getpgrp(63), getpagesize(64), sbrk(69), setpgrp(82), dup2(90),
       fsync(95), setpriority(96), socket(97), getpriority(100),
       listen(106), plock(107), fchown(123), fchmod(124), setreuid(126),
       setregid(127), ftruncate(130), flock(131), setgid(132),
       shutdown(134), gethostid(142), sethostid(143), pid_unblock(154),
       nfssvc(158), async_daemon(163), msgget(201), semget(205),
       shmget(212), getpgid(233), getsid(234), uadmin(242), uswitch(250)

   + Some system calls do not require wrappers because I don't believe
     their pointer arguments are not interpreted. These are:

       mmap(71), munmap(73), mprotect(74), madvise(75), shmat(209),
       shmdt(211), mvalid(213), msync(217), signal(218), memcntl(260)

   + The following function calls have man pages in section 2 of the
     manual, but are no longer system calls (i.e., they have no system
     call numbers defined in syscall.h). Presumably, they are implemented
     in terms of other system calls.

       setquota, ustat, sigvec, sigblock, killpg, creat, execv,
       recv, send, sigsetmask, wait, wait3, waitpid

     (This list is not necessarily complete.)       

   Also, longjmp must not be used from a signal handler to abnormally
   exit from a system call.

   Finally, if a system call references an object on the heap, each
   pointer must reference only one object.  Therefore, it is not
   possible to write the heap contents with a single write. */

#define COMPAT_43
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <syscall.h>
#include <ustat.h>
#include <utime.h>
#include <wait.h>
#include <machine/hal_sysinfo.h>
#include <stdarg.h>

/* The following two header files must be included before
   <nfs/nfs.h> on older versions of Digital Unix. */
#include <sys/time.h>
#include <rpc/types.h>

#include <nfs/nfs.h>
#include <sys/addrconf.h>	/* for getaddressconf(2) */
#include <sys/audit.h>
#include <sys/fstyp.h>		/* for sysfs(2) */
#include <sys/fuser.h>
#include <sys/ipc.h>
#include <sys/mode.h>
#include <sys/mount.h>
#include <sys/msg.h>
#include <sys/poll.h>
#include <sys/procset.h>
#include <sys/ptrace.h>		/* for ptrace(2) */
#include <sys/resource.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/swap.h>		/* for swapctl(2) */
#include <sys/sysinfo.h>
#include <sys/systeminfo.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/utsname.h>
#include <ufs/quota.h>		/* for quotactl(2) */

extern long RT0u__inCritical;
#define ENTER_CRITICAL RT0u__inCritical++
#define EXIT_CRITICAL  RT0u__inCritical--

void (*RTHeapRep_Fault)(void *);
void (*RTCSRC_FinishVM)();

static char RTHeapDepC__c;
#define MAKE_READABLE(x) if (x) { RTHeapDepC__c = *(char*)(x); }
#define MAKE_WRITABLE(x) if (x) { *(char*)(x) = RTHeapDepC__c = *(char*)(x); }

/* Unless otherwise noted, all the following wrappers have the same
   structure:

   1) they enter the critical section using ENTER_CRITICAL,
   2) they touch the pointer arguments using MAKE_READABLE or MAKE_WRITABLE
      (depending on whether the argument is read or written),
   3) they perform the system call using syscall(2), and
   4) they exit the critical section using EXIT_CRITICAL.
*/

int accept(s, addr, addrlen)
  int s;
  struct sockaddr *addr;
  int *addrlen;
{   int result;

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
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    result = syscall(SYS_access, path, mode);
    EXIT_CRITICAL;
    return result;
}

int acct(path)
  char *path;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    result = syscall(SYS_acct, path);
    EXIT_CRITICAL;
    return result;
}

int adjtime(delta, olddelta)
  struct timeval *delta;
  struct timeval *olddelta;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(delta);
    MAKE_WRITABLE(olddelta);
    result = syscall(SYS_adjtime, delta, olddelta);
    EXIT_CRITICAL;
    return result;
}

int audcntl(request, argp, len, flag, audit_id, pid)
  int request;
  char *argp;
  int len;
  int flag;
  uid_t audit_id;
  pid_t pid;
{   int result;

    ENTER_CRITICAL;
    switch (request) {
      case GET_SYS_AMASK:
      case GET_TRUSTED_AMASK:
      case GET_PROC_AMASK:
      case GET_SITEMASK:
      case GET_HABITAT_EVENT:
#ifdef GET_SUBJ_AMASK /* not defined on old versions of Digital Unix */
      case GET_SUBJ_AMASK:
#endif
	MAKE_WRITABLE(argp);
	break;
      case SET_SYS_AMASK:
      case SET_TRUSTED_AMASK:
      case SET_PROC_AMASK:
      case SET_SITEMASK:
      case SET_HABITAT_EVENT:
	MAKE_READABLE(argp);
	break;
      case UPDEVENTS:
	MAKE_READABLE(argp);
	break;
      default:
	break;
    }
    result = syscall(SYS_audcntl, request, argp, len, flag, audit_id, pid);
    EXIT_CRITICAL;
    return result;
}

int audgen(event, tokenp, argv, userbuff, size)
  int event;
  char *tokenp, *argv[], *userbuff;
  long *size;
{   int result;
    char *t, **a;

    ENTER_CRITICAL;
  
    for (t = tokenp, a = argv; *t != '\0'; t++, a++) {
	if (A_TOKEN_PTR(*t)) {
	    MAKE_READABLE(*a);
	}
    }
    MAKE_WRITABLE(size);
    if (size != NULL) MAKE_WRITABLE(userbuff);
    result = syscall(SYS_audgen, tokenp, argv);
    EXIT_CRITICAL;
    return result;
}

int bind(s, address, address_len)
  int s;
  const struct sockaddr *address;
  int address_len;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(address);
    result = syscall(SYS_bind, s, address, address_len);
    EXIT_CRITICAL;
    return result;
}

int chdir(path)
  const char *path;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    result = syscall(SYS_chdir, path);
    EXIT_CRITICAL;
    return result;
}

int chmod(path, mode)
  const char *path;
  mode_t mode;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    result = syscall(SYS_chmod, path, mode);
    EXIT_CRITICAL;
    return result;
}

int chown(path, owner, group)
  const char *path;
  uid_t owner;
  gid_t group;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    result = syscall(SYS_chown, path, owner, group);
    EXIT_CRITICAL;
    return result;
}

int chroot(path)
  const char *path;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    result = syscall(SYS_chroot, path);
    EXIT_CRITICAL;
    return result;
}

int connect(socket, address, address_len)
  int socket;
  const struct sockaddr *address;
  int address_len;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(address);
    result = syscall(SYS_connect, socket, address, address_len);
    EXIT_CRITICAL;
    return result;
}

int execve(path, argv, envp)
/* execve is implemented differently since it does not return, which
   would leave RT0u__inCritical set in the parent if called in the child
   of a vfork. Many calls leave the process in an undefined state in the
   case of EFAULT, but we assume that execve is not one of these. */
  const char *path;
  char * const argv[];
  char * const envp[];
{   int result;

    while (1) {
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

int exec_with_loader(flags, loader, file, argv, envp)
  int flags;
  const char *loader;
  const char *file;
  char * const argv[];
  char * const envp[];
/* See the note describing execve. */
{   int result;

    while (1) {
	result = syscall(SYS_exec_with_loader, flags, loader, file, argv,envp);
	if (result == -1 && errno == EFAULT) {
	    MAKE_READABLE(loader);
	    MAKE_READABLE(file);
	    { char * const *a; for (a = argv; *a; a++) MAKE_READABLE(*a); }
	    { char * const *e; for (e = envp; *e; e++) MAKE_READABLE(*e); }
	} else {
	    return result;
	}
    }
}

int exportfs(access, cookie, exdata)
  int access;
  int *cookie;
  struct exportfsdata *exdata;
{   int result;

    ENTER_CRITICAL;
    if (access == EXPORTFS_READ) {
	MAKE_WRITABLE(cookie);
	MAKE_WRITABLE(exdata);
    }
    result = syscall(SYS_exportfs, access, cookie, exdata);
    EXIT_CRITICAL;
    return result;
}

int fcntl(int filedes, int request, ...)
{   int result;
    va_list args;
    struct flock *arg;

    ENTER_CRITICAL;
    va_start(args, request);
    arg = va_arg(args, struct flock *);
    va_end(args);
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
    result = syscall(SYS_fcntl, filedes, request, arg);
    EXIT_CRITICAL;
    return result;
}

pid_t fork2(int syscall_num)
/* This is a helper function for the fork(2) and vfork(2) system calls.
   Both of these calls require special treatment, although they take no
   argument. They cause Digital Unix to crash if some pages are unreadable,
   so we must unprotect the heap before doing the system call. */
{
    pid_t result;

    ENTER_CRITICAL;
    if (RTCSRC_FinishVM) RTCSRC_FinishVM();
    result = syscall(syscall_num);
    EXIT_CRITICAL;
    /* The fork(2) and vfork(2) system calls always return the process
       ID of the child process. But the library calls are supposed to
       return 0 to the child. Hence, we compare the syscall result to
       our own process ID; if they are equal, we are in the child, so
       we return 0. */
    if (result == getpid()) {
	result = 0;
    }
    return result;
}

pid_t fork()
{
    return fork2(SYS_fork);
}

#ifdef SYS_pre_F64_fstat
int __fstat(filedes, buffer)
  int filedes;
  void *buffer;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_pre_F64_fstat, filedes, buffer);
    EXIT_CRITICAL;
    return result;
}
int _F64_fstat(filedes, buffer)
  int filedes;
  struct stat *buffer;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_fstat, filedes, buffer);
    EXIT_CRITICAL;
    return result;
}
#else
int fstat(filedes, buffer)
  int filedes;
  struct stat *buffer;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_fstat, filedes, buffer);
    EXIT_CRITICAL;
    return result;
}
#endif

#ifdef SYS_pre_F64_fstatfs
int __fstatfs(filedes, buffer, length)
  int filedes;
  void *buffer;
  int length;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_pre_F64_fstatfs, filedes, buffer, length);
    EXIT_CRITICAL;
    return result;
}
int _F64_fstatfs(int filedes, struct statfs *buffer, ...)
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_fstatfs, filedes, buffer);
    EXIT_CRITICAL;
    return result;
}
#else
int fstatfs(filedes, buffer, length)
  int filedes;
  struct statfs *buffer;
  int length;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_fstatfs, filedes, buffer, length);
    EXIT_CRITICAL;
    return result;
}
#endif

int fuser(file, flag, fuser_array, sizeof_fuser_array)
  const char *file;
  long flag;
  struct f_user fuser_array[];
  long sizeof_fuser_array;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(fuser_array);
    result = syscall(SYS_fuser, file, flag, fuser_array, sizeof_fuser_array);
    EXIT_CRITICAL;
    return result;
}

int getaddressconf(buffer, length)
  struct addressconf *buffer;
  size_t length;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_getaddressconf, buffer, length);
    EXIT_CRITICAL;
    return result;
}

int getdirentries(fd, buf, nbytes, basep)
  int fd;
  char *buf;
  int nbytes;
  long *basep;
{   int result;

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
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(name);
    result = syscall(SYS_getdomainname, name, namelen);
    EXIT_CRITICAL;
    return result;
}

int getfh(fd, fhp, exp_fd)
  int fd;
  fhandle_t *fhp;
  int exp_fd;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(fhp);
    result = syscall(SYS_getfh, fd, fhp, exp_fd);
    EXIT_CRITICAL;
    return result;
}

#ifdef SYS_pre_F64_getfsstat
int __getfsstat(buf, bufsize, flags)
  void *buf;
  long bufsize;
  int flags;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(buf);
    result = syscall(SYS_pre_F64_getfsstat, buf, bufsize, flags);
    EXIT_CRITICAL;
    return result;
}
int _F64_getfsstat(buf, bufsize, flags)
  struct statfs *buf;
  long bufsize;
  int flags;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(buf);
    result = syscall(SYS_getfsstat, buf, bufsize, flags);
    EXIT_CRITICAL;
    return result;
}
#else
int getfsstat(buf, bufsize, flags)
  struct statfs *buf;
  long bufsize;
  int flags;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(buf);
    result = syscall(SYS_getfsstat, buf, bufsize, flags);
    EXIT_CRITICAL;
    return result;
}
#endif

int gethostname(address, address_len)
  char *address;
  int address_len;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(address);
    result = syscall(SYS_gethostname, address, address_len);
    EXIT_CRITICAL;
    return result;
}

int getgroups(gidsetsize, grouplist)
  int gidsetsize;
  gid_t grouplist[];
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(grouplist);
    result = syscall(SYS_getgroups, gidsetsize, grouplist);
    EXIT_CRITICAL;
    return result;
}

int getitimer(which, value)
  int which;
  struct itimerval *value;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(value);
    result = syscall(SYS_getitimer, which, value);
    EXIT_CRITICAL;
    return result;
}

int getpeername(socket, address, address_len)
  int socket;
  struct sockaddr *address;
  int *address_len;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(address);
    MAKE_WRITABLE(address_len);
    result = syscall(SYS_getpeername, socket, address, address_len);
    EXIT_CRITICAL;
    return result;
}

int getrlimit(resource1, rlp)
  int resource1;
  struct rlimit *rlp;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(rlp);
    result = syscall(SYS_getrlimit, resource1, rlp);
    EXIT_CRITICAL;
    return result;
}

int getrusage(who, r_usage)
  int who;
  struct rusage *r_usage;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(r_usage);
    result = syscall(SYS_getrusage, who, r_usage);
    EXIT_CRITICAL;
    return result;
}

int getsockname(s, address, address_len)
  int s;
  struct sockaddr *address;
  int *address_len;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(address);
    MAKE_WRITABLE(address_len);
    result = syscall(SYS_getsockname, s, address, address_len);
    EXIT_CRITICAL;
    return result;
}

int getsockopt(socket, level, option_nam, option_value, option_len)
  int socket, level, option_nam;
  void *option_value;
  int *option_len;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(option_value);
    MAKE_WRITABLE(option_len);
    result = syscall(SYS_getsockopt, socket, level,
      option_nam, option_value, option_len);
    EXIT_CRITICAL;
    return result;
}

int getsysinfo(unsigned long op, caddr_t buffer, unsigned long nbytes, ...)
{   int result;
    va_list args;
    int *start;
    void *arg;
    unsigned long *flag;

    ENTER_CRITICAL;
    va_start(args, nbytes);
    start = va_arg(args, int *);
    arg = va_arg(args, void *);
    flag = va_arg(args, unsigned long *);
    va_end(args);
    MAKE_WRITABLE(buffer);
    MAKE_WRITABLE(start);
    MAKE_WRITABLE(arg);
    /* flag may or may not be a pointer */
    if (RTHeapRep_Fault) RTHeapRep_Fault(flag); /* make it readable */
    if (RTHeapRep_Fault) RTHeapRep_Fault(flag); /* make it writable */
    result = syscall(SYS_getsysinfo, op, buffer, nbytes, start, arg, flag);
    EXIT_CRITICAL;
    return result;
}

int gettimeofday(tp, tzp)
  struct timeval *tp;
  struct timezone *tzp;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(tp);
    MAKE_WRITABLE(tzp);
    result = syscall(SYS_gettimeofday, tp, tzp);
    EXIT_CRITICAL;
    return result;
}

int ioctl(d, request, arg)
  int d;
  unsigned long request;
  void *arg;
/* ioctl must test the 'arg' argument carefully. It may be a pointer,
   or maybe not.  At a slight expense, we call RTHeapRep.Fault to
   unprotect the page if it's in the traced heap, but do nothing
   otherwise. */
{   int result;

    ENTER_CRITICAL;
    if (RTHeapRep_Fault) RTHeapRep_Fault(arg); /* make it readable */
    if (RTHeapRep_Fault) RTHeapRep_Fault(arg); /* make it writable */
    result = syscall(SYS_ioctl, d, request, arg);
    EXIT_CRITICAL;
    return result;
}

int lchown(path, owner, group)
  const char *path;
  uid_t owner;
  gid_t group;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    result = syscall(SYS_lchown, path, owner, group);
    EXIT_CRITICAL;
    return result;
}

int link(path1, path2)
  const char *path1;
  const char *path2;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path1);
    MAKE_READABLE(path2);
    result = syscall(SYS_link, path1, path2);
    EXIT_CRITICAL;
    return result;
}

#ifdef SYS_pre_F64_lstat
int __lstat(path, buffer)
  const char *path;
  void *buffer;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_pre_F64_lstat, path, buffer);
    EXIT_CRITICAL;
    return result;
}
int _F64_lstat(path, buffer)
  const char *path;
  struct stat *buffer;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_lstat, path, buffer);
    EXIT_CRITICAL;
    return result;
}
#else
int lstat(path, buffer)
  const char *path;
  struct stat *buffer;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_lstat, path, buffer);
    EXIT_CRITICAL;
    return result;
}
#endif

int mkdir(path, mode)
  const char *path;
  mode_t mode;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    result = syscall(SYS_mkdir, path, mode);
    EXIT_CRITICAL;
    return result;
}

int mknod(const char *path, mode_t mode, ...)
{   int result;
    va_list args;
    dev_t device;

    ENTER_CRITICAL;
    va_start(args, mode);
    device = va_arg(args, dev_t);
    va_end(args);
    MAKE_READABLE(path);
    result = syscall(SYS_mknod, path, mode, device);
    EXIT_CRITICAL;
    return result;
}

int mount(type, mnt_path, mnt_flag, data)
  int type;
  char *mnt_path;
  u_long mnt_flag;
  caddr_t data;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(mnt_path);
    MAKE_READABLE(data);
    result = syscall(SYS_mount, type, mnt_path, mnt_flag, data);
    EXIT_CRITICAL;
    return result;
}

int msgctl(msqid, cmd, buf)
  int msqid, cmd;
  struct msqid_ds *buf;
{   int result;

    ENTER_CRITICAL;
    switch (cmd) {
      case IPC_STAT:
	MAKE_WRITABLE(buf);
	break;
      case IPC_SET:
	MAKE_READABLE(buf);
	break;
      default:
	break;
    }
    result = syscall(SYS_msgctl, msqid, cmd, buf);
    EXIT_CRITICAL;
    return result;
}

int msgrcv(msqid, msgp, msgsz, msgtyp, msgflg)
  int msqid;
  void *msgp;
  size_t msgsz;
  long msgtyp;
  int msgflg;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(msgp);
    result = syscall(SYS_msgrcv, msqid, msgp, msgsz, msgtyp, msgflg);
    EXIT_CRITICAL;
    return result;
}

int msgsnd(msqid, msgp, msgsz, msgflg)
  int msqid;
  const void *msgp;
  size_t msgsz;
  int msgflg;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(msgp);
    result = syscall(SYS_msgsnd, msqid, msgp, msgsz, msgflg);
    EXIT_CRITICAL;
    return result;
}

int open(const char *path, int oflag, ...)
{   int result;
    va_list args;
    mode_t mode;

    ENTER_CRITICAL;
    va_start(args, oflag);
    mode = va_arg(args, mode_t);
    va_end(args);
    MAKE_READABLE(path);
    result = syscall(SYS_open, path, oflag, mode);
    EXIT_CRITICAL;
    return result;
}

#ifdef SYS_pid_block /* not defined on old versions of Digital Unix */
int pid_block(tp, flags)
  struct timeval *tp;
  long flags;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(tp);
    result = syscall(SYS_pid_block, tp, flags);
    EXIT_CRITICAL;
    return result;
}
#endif

int poll(filedes, nfds, timeout)
  struct pollfd filedes[];
  unsigned int nfds;
  int timeout;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(filedes);
    result = syscall(SYS_poll, filedes, nfds, timeout);
    EXIT_CRITICAL;
    return result;
}

int ptrace(request, process, address, data)
  long request;
  long int process;
  ulong_t *address;
  ulong_t data;
{   int result;

    ENTER_CRITICAL;
    switch (request) {
      case PT_READ_I:
      case PT_READ_D:
      case PT_READ_U:
	MAKE_WRITABLE(address);
	break;
      case PT_WRITE_I:
      case PT_WRITE_D:
      case PT_WRITE_U:
	MAKE_READABLE(address);
	break;
    }
    result = syscall(SYS_ptrace, request, process, address, data);
    EXIT_CRITICAL;
    return result;
}

int quotactl(path, cmd, id, addr)
  char *path;
  int cmd, id;
  char *addr;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    switch (cmd) {
      case Q_GETQUOTA:
	MAKE_WRITABLE(addr);
	break;
      case Q_QUOTAON:
      case Q_SETQUOTA:
      case Q_SETUSE:
	MAKE_READABLE(addr);
	break;
    }
    result = syscall(SYS_quotactl, path, cmd, id, addr);
    EXIT_CRITICAL;
    return result;
}

ssize_t read(filedes, buffer, nbytes)
  int filedes;
  void *buffer;
  size_t nbytes;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_read, filedes, buffer, nbytes);
    EXIT_CRITICAL;
    return result;
}

int readlink(path, buffer, buf_size)
  const char *path;
  char *buffer;
  int buf_size;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_readlink, path, buffer, buf_size);
    EXIT_CRITICAL;
    return result;
}

#ifdef SYS__F64_readv
ssize_t __readv(filedes, iov, iov_count)
  int filedes;
  struct iovec *iov;
  int iov_count;
{   int result;

    ENTER_CRITICAL;
    {   int i;
	for (i = 0; i < iov_count; i++) {
	    if (iov[i].iov_len > 0) {
		MAKE_WRITABLE(iov[i].iov_base);
	    }
	}
    }
    result = syscall(SYS_readv, filedes, iov, iov_count);
    EXIT_CRITICAL;
    return result;
}

ssize_t _Ereadv(filedes, iov, iov_count)
  int filedes;
  const struct iovec *iov;
  int iov_count;
{   int result;

    ENTER_CRITICAL;
    {   int i;
	for (i = 0; i < iov_count; i++) {
	    if (iov[i].iov_len > 0) {
		MAKE_WRITABLE(iov[i].iov_base);
	    }
	}
    }
    result = __Ereadv(filedes, iov, iov_count);
    EXIT_CRITICAL;
    return result;
}
#else
ssize_t readv(filedes, iov, iov_count)
  int filedes;
  struct iovec *iov;
  int iov_count;
{   int result;

    ENTER_CRITICAL;
    {   int i;
	for (i = 0; i < iov_count; i++) {
	    if (iov[i].iov_len > 0) {
		MAKE_WRITABLE(iov[i].iov_base);
	    }
	}
    }
    result = syscall(SYS_readv, filedes, iov, iov_count);
    EXIT_CRITICAL;
    return result;
}
#endif

int recvfrom(socket, buffer, length, flags, address, address_len)
  int socket;
  void *buffer;
  int length, flags;
  struct sockaddr *address;
  int *address_len;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(buffer);
    MAKE_WRITABLE(address);
    MAKE_WRITABLE(address_len);
    result = syscall(SYS_recvfrom, socket, buffer,
      length, flags, address, address_len);
    EXIT_CRITICAL;
    return result;
}

int recvmsg(socket, message, flags)
  int socket;
  struct msghdr *message;
  int flags;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(message->msg_name);
    {   int i;
	for (i = 0; i < message->msg_iovlen; i++) {
	    if (message->msg_iov[i].iov_len > 0) {
		MAKE_WRITABLE(message->msg_iov[i].iov_base);
	    }
	}
    }
    MAKE_WRITABLE(message->msg_accrights);
    result = syscall(SYS_recvmsg, socket, message, flags);
    EXIT_CRITICAL;
    return result;
}

int rename(from, to)
  const char *from;
  const char *to;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(from);
    MAKE_READABLE(to);
    result = syscall(SYS_rename, from, to);
    EXIT_CRITICAL;
    return result;
}

int revoke(path)
  char *path;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    result = syscall(SYS_revoke, path);
    EXIT_CRITICAL;
    return result;
}

int rmdir(path)
  const char *path;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    result = syscall(SYS_rmdir, path);
    EXIT_CRITICAL;
    return result;
}

int select(nfds, readfds, writefds, exceptfds, timeout)
  int nfds;
  fd_set *readfds;
  fd_set *writefds;
  fd_set *exceptfds;
  struct timeval *timeout;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(readfds);
    MAKE_WRITABLE(writefds);
    MAKE_WRITABLE(exceptfds);
    MAKE_READABLE(timeout);
    result = syscall(SYS_select, nfds, readfds, writefds, exceptfds, timeout);
    EXIT_CRITICAL;
    return result;
}

int semctl(int semid, int semnum, int cmd, ...)
{   int result;
    va_list args;
    void *arg;

    ENTER_CRITICAL;
    va_start(args, cmd);
    arg = va_arg(args, void *);
    va_end(args);
    switch (cmd) {
      case GETALL:
	MAKE_WRITABLE(arg);
	break;
      case SETALL:
	MAKE_READABLE(arg);
	break;
      case IPC_STAT:
	MAKE_WRITABLE(arg);
	break;
      case IPC_SET:
	MAKE_READABLE(arg);
	break;
      default:
	break;
    }
    result = syscall(SYS_semctl, semid, semnum, cmd, arg);
    EXIT_CRITICAL;
    return result;
}

int semop(semid, sops, nsops)
  int semid;
  struct sembuf *sops;
  size_t nsops;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(sops);
    result = syscall(SYS_semop, semid, sops, nsops);
    EXIT_CRITICAL;
    return result;
}

int sendmsg(socket, message, flags)
  int socket;
  struct msghdr *message;
  int flags;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(message->msg_name);
    {   int i;
	for (i = 0; i < message->msg_iovlen; i++) {
	    if (message->msg_iov[i].iov_len > 0) {
		MAKE_READABLE(message->msg_iov[i].iov_base);
	    }
	}
    }
    MAKE_WRITABLE(message->msg_accrights);
    result = syscall(SYS_sendmsg, socket, message, flags);
    EXIT_CRITICAL;
    return result;
}

int sendto(socket, message_addr, length, flags, dest_addr, dest_len)
  int socket;
  const void *message_addr;
  int length, flags;
  const struct sockaddr *dest_addr;
  int dest_len;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(message_addr);
    MAKE_READABLE(dest_addr);
    result = syscall(SYS_sendto, socket, message_addr,
      length, flags, dest_addr, dest_len);
    EXIT_CRITICAL;
    return result;
}

int setdomainname(name, namelen)
  char *name;
  int namelen;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(name);
    result = syscall(SYS_setdomainname, name, namelen);
    EXIT_CRITICAL;
    return result;
}

int setgroups(gidsetsize, grouplist)
  int gidsetsize;
  gid_t grouplist[];
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(grouplist);
    result = syscall(SYS_setgroups, gidsetsize, grouplist);
    EXIT_CRITICAL;
    return result;
}

int sethostname(name, name_len)
  char *name;
  int name_len;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(name);
    result = syscall(SYS_sethostname, name, name_len);
    EXIT_CRITICAL;
    return result;
}

int setitimer(which, value, ovalue)
  int which;
  struct itimerval *value;
  struct itimerval *ovalue;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(value);
    MAKE_WRITABLE(ovalue);
    result = syscall(SYS_setitimer, which, value, ovalue);
    EXIT_CRITICAL;
    return result;
}

int setlogin(name)
  char *name;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(name);
    result = syscall(SYS_setlogin, name);
    EXIT_CRITICAL;
    return result;
}

int setrlimit(resource1, rlp)
  int resource1;
  struct rlimit *rlp;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(rlp);
    result = syscall(SYS_setrlimit, resource1, rlp);
    EXIT_CRITICAL;
    return result;
}

int setsockopt(socket, level, option_name, option_value, option_len)
  int socket, level, option_name;
  const void *option_value;
  int option_len;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(option_value);
    result = syscall(SYS_setsockopt, socket, level,
      option_name, option_value, option_len);
    EXIT_CRITICAL;
    return result;
}

int setsysinfo(unsigned long op, ...)
{   int result;
    va_list args;
    void *buffer;
    unsigned long nbytes;
    void *arg;
    unsigned long flag;

    ENTER_CRITICAL;
    va_start(args, op);
    buffer = va_arg(args, void *);
    nbytes = va_arg(args, unsigned long);
    arg = va_arg(args, void *);
    flag = va_arg(args, unsigned long);
    va_end(args);
    MAKE_READABLE(buffer);
    MAKE_READABLE(arg);
    result = syscall(SYS_setsysinfo, op, buffer, nbytes, arg, flag);
    EXIT_CRITICAL;
    return result;
}

int settimeofday(tp, tzp)
  struct timeval *tp;
  struct timezone *tzp;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(tp);
    MAKE_READABLE(tzp);
    result = syscall(SYS_settimeofday, tp, tzp);
    EXIT_CRITICAL;
    return result;
}

int shmctl(shmid, cmd, buf)
  int shmid, cmd;
  struct shmid_ds *buf;
{   int result;

    ENTER_CRITICAL;
    switch (cmd) {
      case IPC_STAT: MAKE_WRITABLE(buf); break;
      case IPC_SET:  MAKE_READABLE(buf); break;
    }
    result = syscall(SYS_shmctl, shmid, cmd, buf);
    EXIT_CRITICAL;
    return result;
}

int sigaction(signal, action, o_action)
  int signal;
  const struct sigaction *action;
  struct sigaction *o_action;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(action);
    MAKE_WRITABLE(o_action);
    result = __sigaction(signal, action, o_action);
    EXIT_CRITICAL;
    return result;
}

int sigpending(set)
  sigset_t *set;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(set);
    result = __sigpending(set);
    EXIT_CRITICAL;
    return result;
}

int sigprocmask(how, set, o_set)
  int how;
  const sigset_t *set;
  sigset_t *o_set;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(set);
    MAKE_WRITABLE(o_set);
    result = __sigprocmask(how, set, o_set);
    EXIT_CRITICAL;
    return result;
}

int sigsendset(psetp, sig)
  const procset_t *psetp;
  int sig;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(psetp);
    result = syscall(SYS_sigsendset, psetp, sig);
    EXIT_CRITICAL;
    return result;
}

int sigstack(instack, outstack)
  struct sigstack *instack;
  struct sigstack *outstack;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(instack);
    MAKE_WRITABLE(outstack);
    result = syscall(SYS_sigstack, instack, outstack);
    EXIT_CRITICAL;
    return result;
}

int sigreturn(scp)
  struct sigcontext *scp;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(scp);
    result = syscall(SYS_sigreturn, scp);
    EXIT_CRITICAL;
    return result;
}

int sigsuspend(signal_mask)
  const sigset_t *signal_mask;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(signal_mask);
    result = __sigsuspend(signal_mask);
    EXIT_CRITICAL;
    return result;
}

int socketpair(domain, type, protocol, socket_vector)
  int domain, type, protocol;
  int socket_vector[2];
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(socket_vector);
    result = syscall(SYS_socketpair, domain, type, protocol, socket_vector);
    EXIT_CRITICAL;
    return result;
}

#ifdef SYS_pre_F64_stat
int __stat(path, buffer)
  const char *path;
  void *buffer;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_pre_F64_stat, path, buffer);
    EXIT_CRITICAL;
    return result;
}
int _F64_stat(path, buffer)
  const char *path;
  struct stat *buffer;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_stat, path, buffer);
    EXIT_CRITICAL;
    return result;
}
#else
int stat(path, buffer)
  const char *path;
  struct stat *buffer;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_stat, path, buffer);
    EXIT_CRITICAL;
    return result;
}
#endif

#ifdef SYS_pre_F64_statfs
int __statfs(path, buffer, length)
  char *path;
  void *buffer;
  int length;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_pre_F64_statfs, path, buffer, length);
    EXIT_CRITICAL;
    return result;
}
int _F64_statfs(char *path, struct statfs *buffer, ...)
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_statfs, path, buffer);
    EXIT_CRITICAL;
    return result;
}
#else
int statfs(path, buffer, length)
  char *path;
  struct statfs *buffer;
  int length;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    MAKE_WRITABLE(buffer);
    result = syscall(SYS_statfs, path, buffer, length);
    EXIT_CRITICAL;
    return result;
}
#endif

int swapon(path, flags, lowat, hiwat)
  char *path;
  int flags, lowat, hiwat;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    result = syscall(SYS_swapon, path, flags, lowat, hiwat);
    EXIT_CRITICAL;
    return result;
}

int symlink(path1, path2)
  const char *path1;
  const char *path2;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path1);
    MAKE_READABLE(path2);
    result = syscall(SYS_symlink, path1, path2);
    EXIT_CRITICAL;
    return result;
}

int sysinfo(command, buf, count)
  int command;
  char *buf;
  long count;
{   int result;

    ENTER_CRITICAL;
    switch (command) {
      case SI_ARCHITECTURE:
      case SI_HOSTNAME:
      case SI_HW_PROVIDER:
      case SI_MACHINE:
      case SI_RELEASE:
      case SI_SYSNAME:
	MAKE_WRITABLE(buf);
	break;
      case SI_SET_HOSTNAME:
      case SI_SET_SYSNAME:
	MAKE_READABLE(buf);
	break;
      case SI_HW_SERIAL:
      case SI_SET_SRPC_DOMAIN:
      case SI_SRPC_DOMAIN:
	/* these commands are not supported -- do nothing */
	break;
    }
    result = syscall(SYS_sysinfo, command, buf, count);
    EXIT_CRITICAL;
    return result;
}

int sysfs(int opcode, ...)
{   int result;
    va_list args;
    char *fsname;		/* only when opcode == GETFSIND */
    int fs_index;		/* only when opcode == GETFSTYP */
    char *buf;			/* only when opcode == GETFSTYP */

    ENTER_CRITICAL;
    va_start(args, opcode);
    switch (opcode) {
      case GETFSIND:
        fsname = va_arg(args, char *);
	MAKE_READABLE(fsname);
	result = syscall(SYS_sysfs, opcode, fsname);
	break;
      case GETFSTYP:
	fs_index = va_arg(args, int);
	buf = va_arg(args, char *);
	MAKE_WRITABLE(buf);
	result = syscall(SYS_sysfs, opcode, fs_index, buf);
	break;
      case GETNFSTYP:
	result = syscall(SYS_sysfs, opcode);
	break;
    }
    va_end(args);
    EXIT_CRITICAL;
    return result;
}

int swapctl(cmd, arg)
  int cmd;
  void *arg;
{   int result, i;
    swapres_t *srp;
    swaptbl_t *stp;
    swapent_t *sep;

    ENTER_CRITICAL;
    switch (cmd) {
      case SC_ADD:
      case SC_REMOVE:
	srp = (swapres_t *)arg;
	MAKE_READABLE(srp);
	MAKE_READABLE(srp->sr_name);
	break;
      case SC_LIST:
	stp = (swaptbl_t *)arg;
	MAKE_READABLE(stp);
	MAKE_WRITABLE(stp->swt_ent);
	for (i = 0; i < stp->swt_n; i++) {
	    MAKE_WRITABLE(stp->swt_ent[i].ste_path);
	}
	break;
    }
    result = syscall(SYS_swapctl, cmd, arg);
    EXIT_CRITICAL;
    return result;
}

int table(id, index, addr, nel, lel)
  int id;
  int index;
  char *addr;
  int nel;
  u_int lel;
{   int result;

    ENTER_CRITICAL;
    if (nel > 0) {
	MAKE_WRITABLE(addr);
    } else if (nel < 0) {
	MAKE_READABLE(addr);
    }
    result = syscall(SYS_table, id, index, addr, nel, lel);
    EXIT_CRITICAL;
    return result;
}

int truncate(path, length)
  const char *path;
  off_t length;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    result = syscall(SYS_truncate, path, length);
    EXIT_CRITICAL;
    return result;
}

int uname(name)
  struct utsname *name;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(name);
    result = syscall(SYS_uname, name);
    EXIT_CRITICAL;
    return result;
}

int unlink(path)
  const char *path;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    result = syscall(SYS_unlink, path);
    EXIT_CRITICAL;
    return result;
}

int utimes(path, times)
  const char *path;
  struct timeval times[2];
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(path);
    MAKE_READABLE(times);
    result = syscall(SYS_utimes, path, times);
    EXIT_CRITICAL;
    return result;
}

pid_t wait4(process_id, status_location, options, resource_usage)
  pid_t process_id;
  union wait *status_location;
  int options;
  struct rusage *resource_usage;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(status_location);
    MAKE_WRITABLE(resource_usage);
    result = syscall(SYS_wait4, process_id, status_location,
      options, resource_usage);
    EXIT_CRITICAL;
    return result;
}

int waitid(idtype, id, infop, options)
  idtype_t idtype;
  id_t id;
  siginfo_t *infop;
  int options;
{   int result;

    ENTER_CRITICAL;
    MAKE_WRITABLE(infop);
    result = syscall(SYS_waitid, idtype, id, infop, options);
    EXIT_CRITICAL;
    return result;
}

ssize_t write(filedes, buffer, nbytes)
  int filedes;
  const void *buffer;
  size_t nbytes;
{   int result;

    ENTER_CRITICAL;
    MAKE_READABLE(buffer);
    result = syscall(SYS_write, filedes, buffer, nbytes);
    EXIT_CRITICAL;
    return result;
}

#ifdef SYS__F64_writev
ssize_t __writev(filedes, iov, iov_count)
  int filedes;
  struct iovec *iov;
  int iov_count;
{   int result;

    ENTER_CRITICAL;
    {   int i;
	for (i = 0; i < iov_count; i++) {
	    if (iov[i].iov_len > 0) {
		MAKE_READABLE(iov[i].iov_base);
	    }
	}
    }
    result = syscall(SYS_writev, filedes, iov, iov_count);
    EXIT_CRITICAL;
    return result;
}
ssize_t _Ewritev(filedes, iov, iov_count)
  int filedes;
  const struct iovec *iov;
  int iov_count;
{   int result;

    ENTER_CRITICAL;
    {   int i;
	for (i = 0; i < iov_count; i++) {
	    if (iov[i].iov_len > 0) {
		MAKE_READABLE(iov[i].iov_base);
	    }
	}
    }
    result = __Ewritev(filedes, iov, iov_count);
    EXIT_CRITICAL;
    return result;
}
#else
ssize_t writev(filedes, iov, iov_count)
  int filedes;
  struct iovec *iov;
  int iov_count;
{   int result;

    ENTER_CRITICAL;
    {   int i;
	for (i = 0; i < iov_count; i++) {
	    if (iov[i].iov_len > 0) {
		MAKE_READABLE(iov[i].iov_base);
	    }
	}
    }
    result = syscall(SYS_writev, filedes, iov, iov_count);
    EXIT_CRITICAL;
    return result;
}
#endif
