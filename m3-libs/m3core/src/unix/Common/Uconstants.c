/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#define _FILE_OFFSET_BITS 64
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <assert.h>
#include <stddef.h>
#include <errno.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/tcp.h>
#include <pthread.h>
#include <netdb.h>
#include <netinet/in.h>

/* enable this and preprocess file to get approximate *.i3 contents, fix them in up in editor */
#if 0
#undef X
#define X(x) <* EXTERNAL Uerror_##x *> VAR #x: int;
#include "UerrorX.h"
#endif


/* check that Max is enough; if you get an error here, raise it in Uerror.i3 and here */
typedef int CheckMax[128 - sizeof(union{
#undef X
#define X(x) char a##x[x];
#include "UerrorX.h"
})];


#undef X
#define X(x) const int Uerror_##x = x;
#include "UerrorX.h"


#undef X
#define X(x) const int Uexec_##x = x;
X(WNOHANG)


#undef X
#define X(x) const int Usignal_##x = x;
X(SIGINT)
X(SIGKILL)


#undef X
#define X(x) const int Unix_##x = x;
#undef Y
#define Y(x, y) const int Unix_##x = y;

X(FIONREAD)

X(O_RDONLY)
X(O_RDWR)
X(O_CREAT)
X(O_EXCL)
X(O_TRUNC)
X(O_NONBLOCK)
Y(O_NDELAY, O_NONBLOCK) /* compat */
Y(M3_NONBLOCK, O_NONBLOCK) /* compat */

X(F_OK)
X(X_OK)
X(W_OK)
X(R_OK)

X(F_SETFD) /* Set close-on-exec flag */
X(F_GETFL) /* Get fd status flags */
X(F_SETFL) /* Set fd status flags */

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
#define X(type, x) const type Upthread_##x = x;

X(pthread_mutex_t, PTHREAD_MUTEX_INITIALIZER)
X(pthread_cond_t, PTHREAD_COND_INITIALIZER)


#undef X
#define X(x) const int Usocket_##x = x;

X(SOCK_STREAM)
X(SOCK_DGRAM)

X(SO_REUSEADDR)
X(SO_KEEPALIVE)
X(SO_LINGER)

X(SOL_SOCKET)
X(AF_INET)
X(MSG_PEEK)


#undef X
#define X(x) const int Ustat_##x = x;
#undef Y
#define Y(x, y) const int Ustat_##x = y;

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

#undef X
#define X(x) const int Unetdb_##x = x;
X(TRY_AGAIN)
X(NO_RECOVERY)
X(NO_ADDRESS)


#undef X
#define X(x) const int Uin_##x = x;
X(IPPROTO_TCP)


#undef X
#define X(x) const int Utime_##x = x;
X(ITIMER_REAL) /* real time intervals */
X(ITIMER_VIRTUAL) /* virtual time intervals */


#ifdef __CYGWIN__

#include <process.h>

#undef X
#define X(x) const int Usysdep_##x = x;
#undef Y
#define Y(x, y) const int Usysdep_##x = y;

Y(P_NOWAIT, _P_NOWAIT)

#endif
