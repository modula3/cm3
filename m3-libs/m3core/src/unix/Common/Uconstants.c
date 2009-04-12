/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#include "m3unix.h"
#if defined(__OpenBSD__)
#include "context.h"
#define HAS_UCONTEXT_T
#elif defined(__linux) && defined(__i386)
#include <ucontext.h>
#define HAS_UCONTEXT_T
#else
/*#include <ucontext.h>*/
#endif
#include <signal.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#ifdef __sun
#include <sys/filio.h>
#endif
#ifdef __CYGWIN__
#include <process.h>
#include <sys/termios.h>
#endif

typedef union {
#define X(x) char a##x[x];
#include "UerrorX.h"
} CheckMax_t;
/* check that Uerror.Max=248 is enough; if you get an error here, raise it in Uerror.i3 and here */
typedef int CheckMax[248 - sizeof(CheckMax_t)];

#undef X
#define X(x) const int Uerror__##x = x;
#include "UerrorX.h"


#undef X
#define X(x) const int Uexec__##x = x; \
             const int Uexec_##x = x;
X(WNOHANG)


#undef X
#define X(x) const int Usignal__##x = x;
X(SIGINT)
X(SIGKILL)
X(SIGCHLD)
X(SIGALRM)
X(SIGHUP)
X(SIGTERM)


#undef X
#define X(x) const int Unix__##x = x;
#undef Y
#define Y(x, y) const int Unix__##x = y;

X(FIONREAD)

X(O_RDONLY)
X(O_RDWR)
X(O_CREAT)
X(O_EXCL)
X(O_TRUNC)
X(O_NONBLOCK)
X(O_APPEND)
Y(O_NDELAY, O_NONBLOCK) /* compat */
Y(M3_NONBLOCK, O_NONBLOCK) /* compat */

X(F_OK)
X(X_OK)
X(W_OK)
X(R_OK)

X(F_SETFD) /* Set close-on-exec flag */
X(F_GETFL) /* Get fd status flags */
X(F_SETFL) /* Set fd status flags */

Y(MSETUID, S_ISUID)	/* set user id on execution */
Y(MSETGID, S_ISGID)	/* set group id on execution */
Y(MSTICKY, S_ISVTX)	/* save swapped text even after use */
Y(MROWNER, S_IRUSR) /* readable by owner */
Y(MWOWNER, S_IWUSR) /* writable by owner */
Y(MXOWNER, S_IXUSR) /* executable by owner */
Y(MRGROUP, S_IRGRP) /* readable by group */
Y(MWGROUP, S_IWGRP) /* writable by group */
Y(MXGROUP, S_IXGRP) /* executable by group */
Y(MROTHER, S_IROTH) /* readable by other */
Y(MWOTHER, S_IWOTH) /* writable by other */
Y(MXOTHER, S_IXOTH) /* executable by other */

/* readable/writable by all, executable by none */
Y(Mrwrwrw, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)


#undef X
#define X(x) const int Usocket__##x = x;

X(SOCK_STREAM)
X(SOCK_DGRAM)

X(SO_REUSEADDR)
X(SO_KEEPALIVE)
X(SO_LINGER)

X(SOL_SOCKET)
X(AF_INET)
X(MSG_PEEK)


#undef X
#define X(x) const int Ustat__##x = x;
#undef Y
#define Y(x, y) const int Ustat__##x = y;

X(S_IFMT)
X(S_IFSOCK)
X(S_IFLNK)
X(S_IFREG)
X(S_IFBLK)
X(S_IFDIR)
X(S_IFCHR)
X(S_IFIFO)
X(S_ISUID)
X(S_ISGID)
X(S_ISVTX)

Y(S_IREAD, S_IRUSR)
Y(S_IWRITE, S_IWUSR)
Y(S_IEXEC, S_IXUSR)
Y(S_GREAD, S_IRGRP)
Y(S_GWRITE, S_IWGRP)
Y(S_GEXEC, S_IXGRP)
Y(S_OREAD, S_IROTH)
Y(S_OWRITE, S_IWOTH)
Y(S_OEXEC, S_IXOTH)

#ifdef HAS_STAT_FLAGS

#ifdef UF_NODUMP
X(UF_NODUMP)
#else
Y(UF_NODUMP, 0)
#endif

#ifdef UF_IMMUTABLE
X(UF_IMMUTABLE)
#else
Y(UF_IMMUTABLE, 0)
#endif

#ifdef UF_APPEND
X(UF_APPEND)
#else
Y(UF_APPEND, 0)
#endif

#ifdef UF_NOUNLINK
X(UF_NOUNLINK)
#else
Y(UF_NOUNLINK, 0)
#endif

#ifdef UF_OPAQUE
X(UF_OPAQUE)
#else
Y(UF_OPAQUE, 0)
#endif

#ifdef SF_ARCHIVED
X(SF_ARCHIVED)
#else
Y(SF_ARCHIVED, 0)
#endif

#ifdef SF_IMMUTABLE
X(SF_IMMUTABLE)
#else
Y(SF_IMMUTABLE, 0)
#endif

#ifdef SF_APPEND
X(SF_APPEND)
#else
Y(SF_APPEND, 0)
#endif

#ifdef SF_NOUNLINK
X(SF_NOUNLINK)
#else
Y(SF_NOUNLINK, 0)
#endif

#ifdef SF_SNAPSHOT
X(SF_SNAPSHOT)
#else
Y(SF_SNAPSHOT, 0)
#endif

#else

Y(UF_NODUMP, 0)
Y(UF_IMMUTABLE, 0)
Y(UF_APPEND, 0)
Y(UF_NOUNLINK, 0)
Y(UF_OPAQUE, 0)
Y(SF_ARCHIVED, 0)
Y(SF_IMMUTABLE, 0)
Y(SF_APPEND, 0)
Y(SF_NOUNLINK, 0)
Y(SF_SNAPSHOT, 0)

#endif

#undef X
#define X(x) const int Unetdb__##x = x;
X(TRY_AGAIN)
X(NO_RECOVERY)
X(NO_ADDRESS)


#undef X
#define X(x) const int Uin__##x = x;
X(IPPROTO_TCP)


#undef X
#undef Y
#define X(x) const int Utime__##x = x;
#define Y(x, y) const int Utime__##x = y;
/* Cygwin only supports real time whereas Modula-3 only uses virtual time. Lie. */
#ifdef __CYGWIN__
Y(ITIMER_VIRTUAL, ITIMER_REAL)
#else
X(ITIMER_VIRTUAL) /* virtual time intervals */
#endif


#undef X
#define X(x) const int Umman__##x = x;
X(PROT_NONE)
X(PROT_READ)
X(PROT_WRITE)

#ifdef PROT_EXEC
     X(PROT_EXEC)
#endif
#ifdef MAP_SHARED
     X(MAP_SHARED)
#endif
#ifdef MAP_PRIVATE
     X(MAP_PRIVATE)
#endif
#ifdef MAP_FIXED
     X(MAP_FIXED)
#endif
#ifdef MAP_RENAME
     X(MAP_RENAME)
#endif
#ifdef MAP_NORESERVE
     X(MAP_NORESERVE)
#endif
#ifdef MAP_HASSEMAPHORE
     X(MAP_HASSEMAPHORE)
#endif
#ifdef MAP_STACK
     X(MAP_STACK)
#endif
#ifdef MAP_NOSYNC
     X(MAP_NOSYNC)
#endif
#ifdef MAP_FILE
     X(MAP_FILE)
#endif
#ifdef MAP_ANON
     X(MAP_ANON)
#endif
#ifdef MAP_NOCORE
     X(MAP_NOCORE)
#endif

#ifdef HAS_UCONTEXT_T
#undef X
#undef Y
#define X(x) const int Uucontext__##x = x;
#define Y(x, y) const int Uucontext__##x = y;
Y(context_t_size, sizeof(ucontext_t))
#endif

#ifdef __CYGWIN__

#undef X
#define X(x) const int Usysdep__##x = x;
#undef Y
#define Y(x, y) const int Usysdep__##x = y;

Y(P_NOWAIT, _P_NOWAIT)

#undef X
#undef Y
#define X(x) const int Utermio__##x = x;
#define Y(x, y) const int Utermio__##x = y;

Y(BAUDBITS, CBAUD)
X(B75)
X(B110)
X(B300)
X(B600)
X(B1200)
X(B2400)
X(B4800)
X(B9600)
#ifdef B14400
X(B14400)
#else
Y(B14400, -1)
#endif
X(B19200)
X(B38400)
X(B57600)
X(B115200)
X(B230400)
X(CSIZE)
X(CS8)
X(CS7)
X(CS6)
X(CS5 )
X(CSTOPB)

Y(PARITYBITS, PARODD | PARENB)
Y(PARNONE, 0)
Y(PARODD, PARODD | PARENB)
Y(PAREVEN, PARENB)

X(IGNBRK)
X(IGNPAR)
X(CREAD)
X(VMIN)
X(VTIME)

X(TCSANOW)

#endif /* cygwin */
