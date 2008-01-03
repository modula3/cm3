(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Wed Dec 23 09:13:19 PST 1992 by jdd                      *)

INTERFACE Usyscall;

FROM Ctypes IMPORT int;

(*** syscall.h ***)

CONST
  SYS_syscall       = 0;
  SYS_exit          = 1;
  SYS_fork          = 2;
  SYS_read          = 3;
  SYS_write         = 4;
  SYS_open          = 5;
  SYS_close         = 6;
  SYS_creat         = 8;
  SYS_link          = 9;
  SYS_unlink        = 10;
  SYS_execv         = 11;
  SYS_chdir         = 12;
  SYS_mknod         = 14;
  SYS_chmod         = 15;
  SYS_chown         = 16;
  SYS_brk           = 17;
  SYS_lseek         = 19;
  SYS_getpid        = 20;
  SYS_mount         = 21;
  SYS_umount        = 22;
  SYS_getuid        = 24;
  SYS_ptrace        = 26;
  SYS_access        = 33;
  SYS_sync          = 36;
  SYS_kill          = 37;
  SYS_stat          = 38;
  SYS_lstat         = 40;
  SYS_dup           = 41;
  SYS_pipe          = 42;
  SYS_profil        = 44;
  SYS_getgid        = 47;
  SYS_acct          = 51;
  SYS_ioctl         = 54;
  SYS_reboot        = 55;
  SYS_symlink       = 57;
  SYS_readlink      = 58;
  SYS_execve        = 59;
  SYS_umask         = 60;
  SYS_chroot        = 61;
  SYS_fstat         = 62;
  SYS_getpagesize   = 64;
  SYS_mremap        = 65;
  SYS_vfork         = 66;
  SYS_sbrk          = 69;
  SYS_sstk          = 70;
  SYS_mmap          = 71;
  SYS_vadvise       = 72;
  SYS_munmap        = 73;
  SYS_mprotect      = 74;
  SYS_madvise       = 75;
  SYS_vhangup       = 76;
  SYS_mincore       = 78;
  SYS_getgroups     = 79;
  SYS_setgroups     = 80;
  SYS_getpgrp       = 81;
  SYS_setpgrp       = 82;
  SYS_setitimer     = 83;
  SYS_wait3         = 84;
  SYS_wait          = SYS_wait3;
  SYS_swapon        = 85;
  SYS_getitimer     = 86;
  SYS_gethostname   = 87;
  SYS_sethostname   = 88;
  SYS_getdtablesize = 89;
  SYS_dup2          = 90;
  SYS_getdopt       = 91;
  SYS_fcntl         = 92;
  SYS_select        = 93;
  SYS_setdopt       = 94;
  SYS_fsync         = 95;
  SYS_setpriority   = 96;
  SYS_socket        = 97;
  SYS_connect       = 98;
  SYS_accept        = 99;
  SYS_getpriority   = 100;
  SYS_send          = 101;
  SYS_recv          = 102;
  SYS_sigreturn     = 103;
  SYS_bind          = 104;
  SYS_setsockopt    = 105;
  SYS_listen        = 106;
  SYS_sigvec        = 108;
  SYS_sigblock      = 109;
  SYS_sigsetmask    = 110;
  SYS_sigpause      = 111;
  SYS_sigstack      = 112;
  SYS_recvmsg       = 113;
  SYS_sendmsg       = 114;
  SYS_gettimeofday  = 116;
  SYS_getrusage     = 117;
  SYS_getsockopt    = 118;
  SYS_readv         = 120;
  SYS_writev        = 121;
  SYS_settimeofday  = 122;
  SYS_fchown        = 123;
  SYS_fchmod        = 124;
  SYS_recvfrom      = 125;
  SYS_setreuid      = 126;
  SYS_setregid      = 127;
  SYS_rename        = 128;
  SYS_truncate      = 129;
  SYS_ftruncate     = 130;
  SYS_flock         = 131;
  SYS_sendto        = 133;
  SYS_shutdown      = 134;
  SYS_socketpair    = 135;
  SYS_mkdir         = 136;
  SYS_rmdir         = 137;
  SYS_utimes        = 138;
  SYS_sigcleanup    = 139;
  SYS_adjtime       = 140;
  SYS_getpeername   = 141;
  SYS_gethostid     = 142;
  SYS_sethostid     = 143;
  SYS_getrlimit     = 144;
  SYS_setrlimit     = 145;
  SYS_killpg        = 146;
  SYS_setquota      = 148;
  SYS_quota         = 149;
  SYS_getsockname   = 150;
  SYS_sysmips       = 151;
  SYS_cacheflush    = 152;
  SYS_cachectl      = 153;
  SYS_atomic_op     = 155;
  SYS_nfs_svc       = 158;
  SYS_nfssvc        = 158;
  SYS_nfs_biod      = 163;
  SYS_async_daemon  = 163;
  SYS_nfs_getfh     = 164;
  SYS_getfh         = 164;
  SYS_getdirentries = 159;
  SYS_getdomainname = 165;
  SYS_setdomainname = 166;
  SYS_exportfs      = 169;
  SYS_msgctl        = 172;
  SYS_msgget        = 173;
  SYS_msgrcv        = 174;
  SYS_msgsnd        = 175;
  SYS_semctl        = 176;
  SYS_semget        = 177;
  SYS_semop         = 178;
  SYS_uname         = 179;
  SYS_shmsys        = 180;
  SYS_plock         = 181;
  SYS_lockf         = 182;
  SYS_ustat         = 183;
  SYS_getmnt        = 184;
  SYS_sigpending    = 187;
  SYS_setsid        = 188;
  SYS_waitpid       = 189;
  SYS_utc_gettime   = 233;
  SYS_utc_adjtime   = 234;
  SYS_audcntl       = 252;
  SYS_audgen        = 253;
  SYS_startcpu      = 254;
  SYS_stopcpu       = 255;
  SYS_getsysinfo    = 256;
  SYS_setsysinfo    = 257;

(* This signature provides for up to three integer arguments to
   syscall.  Feel free to add more if your system call requires it.
   Remember that not every system call returns the result that its
   wrapper returns. *)

<*EXTERNAL*>
PROCEDURE syscall (number: int;
                   arg1  : int   := 0;
                   arg2  : int   := 0;
                   arg3  : int   := 0  ): int;

END Usyscall.
