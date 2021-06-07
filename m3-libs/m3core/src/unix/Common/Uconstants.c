/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif
#ifndef _WIN32
#include <netinet/tcp.h>
#include <netinet/in.h>
#endif
#ifdef __sun
#include <sys/filio.h>
#endif
#ifdef __CYGWIN__
#include <process.h>
#include <sys/termios.h>
#endif
#if !defined(__INTERIX) && !defined(_WIN32)
#include <net/if.h>
#endif
#ifdef __APPLE__
#include <term.h>
#include <sys/ioctl_compat.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if M3_HAS_VISIBILITY
#ifdef __APPLE__
#pragma GCC visibility push(default)
#else
#pragma GCC visibility push(protected)
#endif
#endif

#undef X
#define X(x) EXTERN_CONST int Uerror__##x = x;
#include "UerrorX.h"

// Check that Uerror.Max=248 is enough; if you get an error here, raise it in Uerror.i3 and here.
//
// FreeBSD (12.2/amd64) compiling with clang++ has some numbers between 9000 and 10000.
// Skip these. The actual user of Uerror.Max is ok with this.
//
#define M3_UERROR_MAX 248

#ifdef _LIBCPP_ERRNO_H // e.g. __cplusplus && __clang__ && __FreeBSD__
#if ENODATA > 9900 && ENODATA < 9999
#undef ENODATA
#endif

#if ENOSR > 9900 && ENOSR < 9999
#undef ENOSR
#endif

#if ENOSTR > 9900 && ENOSTR < 9999
#undef ENOSTR
#endif

#if ETIME > 9900 && ETIME < 9999
#undef ETIME
#endif
#endif // FreeBSD/clang++

typedef union {
#undef X
#define X(x) char a##x[x];
#include "UerrorX.h"
} M3UerrorCheckMax_t;
// sizeof(M3UerrorCheckMax_t) is the largest errno.
// If it is less or equal to M3_UERROR_MAX, this typedef is illegal, due to zero- or negative-sized array.
typedef int M3UerrorCheckMax[M3_UERROR_MAX - sizeof(M3UerrorCheckMax_t)];

#define const_INTEGER const_INTEGER /* inhibit m3c type that lacks const */
typedef const INTEGER const_INTEGER;

// This is messy, see CsetjmpC.c.
extern const_INTEGER m3_jmpbuf_size; // declare to avoid gcc warning
#if defined(__sun)
const_INTEGER m3_jmpbuf_size = sizeof(sigjmp_buf);
#else
const_INTEGER m3_jmpbuf_size = sizeof(jmp_buf);
#endif

#ifndef _WIN32

#undef X
#define X(x) EXTERN_CONST int Uexec__##x = x; \
             EXTERN_CONST int Uexec_##x = x;
X(WNOHANG)

#endif

#undef X
#define X(x) EXTERN_CONST int Usignal__##x = x;
X(SIGINT)
X(SIGTERM)
#ifndef _WIN32
X(SIGKILL)
X(SIGCHLD)
X(SIGALRM)
X(SIGHUP)
X(SIGSTOP)
#endif

#ifdef SIGQUIT
X(SIGQUIT)
#endif
#ifdef SIGILL
X(SIGILL)
#endif
#ifdef SIGTRAP
X(SIGTRAP)
#endif
#ifdef SIGABRT
X(SIGABRT)
#endif
#ifdef SIGEXCEPT
X(SIGEXCEPT)
#endif
#ifdef SIGFPE
X(SIGFPE)
#endif
#ifdef SIGBUS
X(SIGBUS)
#endif
#ifdef SIGSEGV
X(SIGSEGV)
#endif
#ifdef SIGSYS
X(SIGSYS)
#endif
#ifdef SIGPIPE
X(SIGPIPE)
#endif
#ifdef SIGUSR1
X(SIGUSR1)
#endif
#ifdef SIGUSR2
X(SIGUSR2)
#endif
#ifdef SIGIO
X(SIGIO)
#endif
#ifdef SIGWINCH
X(SIGWINCH)
#endif
#ifdef SIGURG
X(SIGURG)
#endif
#ifdef SIGPOLL
X(SIGPOLL)
#endif
#ifdef SIGTSTP
X(SIGTSTP)
#endif
#ifdef SIGCONT
X(SIGCONT)
#endif
#ifdef SIGTTIN
X(SIGTTIN)
#endif
#ifdef SIGTTOU
X(SIGTTOU)
#endif
#ifdef SIGVTALRM
X(SIGVTALRM)
#endif
#ifdef SIGPROF
X(SIGPROF)
#endif
#ifdef SIGXCPU
X(SIGXCPU)
#endif
#ifdef SIGXFSZ
X(SIGXFSZ)
#endif
#ifdef SIGCANCEL
X(SIGCANCEL)
#endif

#ifdef NSIG
X(NSIG)
#endif
#ifdef SIGIOT
X(SIGIOT)
#endif
#ifdef SIGCLD
X(SIGCLD)
#endif
#ifdef SA_NOCLDSTOP
X(SA_NOCLDSTOP)
#endif
#ifdef SA_RESTART
X(SA_RESTART)
#endif
#ifdef SA_RESETHAND
X(SA_RESETHAND)
#endif
#ifdef SIG_BLOCK
X(SIG_BLOCK)
#endif
#ifdef SIG_UNBLOCK
X(SIG_UNBLOCK)
#endif
#ifdef SIG_SETMASK
X(SIG_SETMASK)
#endif

#undef X
#define X(x) EXTERN_CONST int Unix__##x = x;
#undef Y
#define Y(x, y) EXTERN_CONST int Unix__##x = y;

X(FIONREAD)

X(O_RDONLY)
X(O_RDWR)
X(O_CREAT)
X(O_EXCL)
X(O_TRUNC)
X(O_APPEND)
#ifndef _WIN32
X(O_NONBLOCK)
Y(O_NDELAY, O_NONBLOCK) /* compat */
Y(M3_NONBLOCK, O_NONBLOCK) /* compat */
#endif

#ifndef _WIN32
X(F_OK)
X(X_OK)
X(W_OK)
X(R_OK)
#else
// There are no #defines in the headers but the documention gives the values.
Y(F_OK, 0) // existence only
Y(W_OK, 2) // write-only
Y(R_OK, 4) // read-only
           // X_OK not in documentation
#endif

#ifndef _WIN32

X(F_SETFD) /* Set close-on-exec flag */
X(F_GETFL) /* Get fd status flags */
X(F_SETFL) /* Set fd status flags */
X(F_DUPFD)  /* Duplicate fd */
X(F_GETFD)  /* Get close-on-exec flag */
X(F_GETOWN) /* Set owner */
X(F_SETOWN) /* Get owner */
X(F_GETLK)  /* Get file lock */
X(F_SETLK)  /* Set file lock */
X(F_SETLKW) /* Set file lock and wait */
X(FD_CLOEXEC)   /* Close file descriptor on exec() */

Y(MSETUID, S_ISUID) /* set user id on execution */
Y(MSETGID, S_ISGID) /* set group id on execution */
Y(MSTICKY, S_ISVTX) /* save swapped text even after use */
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

#endif

#undef X
#define X(x) EXTERN_CONST int Usocket__##x = x;

/* core Usocket that m3core/libm3 use */

X(SOCK_STREAM)
X(SOCK_DGRAM)

X(SO_REUSEADDR)
X(SO_KEEPALIVE)
X(SO_LINGER)

X(SOL_SOCKET)
X(MSG_PEEK)

X(AF_INET)

#ifdef AF_LOCAL
X(AF_LOCAL)
#endif
#ifdef AF_UNIX
X(AF_UNIX)
#endif
#ifdef AF_UNSPEC
X(AF_UNSPEC)
#endif

#ifdef PF_INET
X(PF_INET)
#endif
#ifdef PF_LOCAL
X(PF_LOCAL)
#endif
#ifdef PF_UNIX
X(PF_UNIX)
#endif
#ifdef PF_UNSPEC
X(PF_UNSPEC)
#endif

#ifdef AF_INET6
X(AF_INET6)
#endif

#ifdef PF_INET6
X(PF_INET6)
#endif

/* other Usocket that others might use, maybe less portable? */

#ifdef AF_AAL5
X(AF_AAL5)
#endif
#ifdef AF_APPLETALK
X(AF_APPLETALK)
#endif
#ifdef AF_AX25
X(AF_AX25)
#endif
#ifdef AF_BRIDGE
X(AF_BRIDGE)
#endif
#ifdef AF_BSC
X(AF_BSC)
#endif
#ifdef AF_BTH
X(AF_BTH)
#endif
#ifdef AF_CCITT
X(AF_CCITT)
#endif
#ifdef AF_CHAOS
X(AF_CHAOS)
#endif
#ifdef AF_DATAKIT
X(AF_DATAKIT)
#endif
#ifdef AF_DECnet
X(AF_DECnet)
#endif
#ifdef AF_DLI
X(AF_DLI)
#endif
#ifdef AF_DSS
X(AF_DSS)
#endif
#ifdef AF_ECMA
X(AF_ECMA)
#endif
#ifdef AF_HYLINK
X(AF_HYLINK)
#endif
#ifdef AF_IMPLINK
X(AF_IMPLINK)
#endif
#ifdef AF_IPX
X(AF_IPX)
#endif
#ifdef AF_IRDA
X(AF_IRDA)
#endif
#ifdef AF_ISO
X(AF_ISO)
#endif
#ifdef AF_LAT
X(AF_LAT)
#endif
#ifdef AF_MAX
X(AF_MAX)
#endif
#ifdef AF_NBS
X(AF_NBS)
#endif
#ifdef AF_NETBIOS
X(AF_NETBIOS)
#endif
#ifdef AF_NETROM
X(AF_NETROM)
#endif
#ifdef AF_NS
X(AF_NS)
#endif
#ifdef AF_OSI
X(AF_OSI)
#endif
#ifdef AF_PUP
X(AF_PUP)
#endif
#ifdef AF_SNA
X(AF_SNA)
#endif
#ifdef AF_X25
X(AF_X25)
#endif
#ifdef PF_APPLETALK
X(PF_APPLETALK)
#endif
#ifdef PF_CCITT
X(PF_CCITT)
#endif
#ifdef PF_CHAOS
X(PF_CHAOS)
#endif
#ifdef PF_DATAKIT
X(PF_DATAKIT)
#endif
#ifdef PF_DECnet
X(PF_DECnet)
#endif
#ifdef PF_DLI
X(PF_DLI)
#endif
#ifdef PF_ECMA
X(PF_ECMA)
#endif
#ifdef PF_HYLINK
X(PF_HYLINK)
#endif
#ifdef PF_IMPLINK
X(PF_IMPLINK)
#endif
#ifdef PF_ISO
X(PF_ISO)
#endif
#ifdef PF_LAT
X(PF_LAT)
#endif
#ifdef PF_MAX
X(PF_MAX)
#endif
#ifdef PF_NETBIOS
X(PF_NETBIOS)
#endif
#ifdef PF_NS
X(PF_NS)
#endif
#ifdef PF_OSI
X(PF_OSI)
#endif
#ifdef PF_PUP
X(PF_PUP)
#endif
#ifdef PF_SNA
X(PF_SNA)
#endif

#ifdef AI_PASSIVE
X(AI_PASSIVE)
#endif
#ifdef AI_CANONNAME
X(AI_CANONNAME)
#endif
#ifdef AI_NUMERIC_HOST
X(AI_NUMERIC_HOST)
#endif
#ifdef AI_ADDRCONFIG
X(AI_ADDRCONFIG)
#endif
#ifdef AI_NON_AUTHORITATIVE
X(AI_NON_AUTHORITATIVE)
#endif
#ifdef AI_SECURE
X(AI_SECURE)
#endif
#ifdef AI_RETURN_PREFERRED_NAMES
X(AI_RETURN_PREFERRED_NAMES)
#endif

X(IPPROTO_TCP)
#ifdef IPPROTO_UDP
X(IPPROTO_UDP)
#endif
#ifdef IPPROTO_RM
X(IPPROTO_RM)
#endif
#ifdef IPPROTO_PGM
X(IPPROTO_PGM)
#endif
#ifdef IPPROTO_IGMP
X(IPPROTO_IGMP)
#endif

#ifdef SOCK_RAW
X(SOCK_RAW)
#endif
#ifdef SOCK_RDM
X(SOCK_RDM)
#endif
#ifdef SOCK_SEQPACKET
X(SOCK_SEQPACKET)
#endif

#ifdef SO_TYPE
X(SO_TYPE)
#endif
#ifdef SO_ERROR
X(SO_ERROR)
#endif
#ifdef SO_DONTROUTE
X(SO_DONTROUTE)
#endif
#ifdef SO_BROADCAST
X(SO_BROADCAST)
#endif
#ifdef SO_SNDBUF
X(SO_SNDBUF)
#endif
#ifdef SO_RCVBUF
X(SO_RCVBUF)
#endif
#ifdef SO_OOBINLINE
X(SO_OOBINLINE)
#endif
#ifdef SO_NO_CHECK
X(SO_NO_CHECK)
#endif
#ifdef SO_PRIORITY
X(SO_PRIORITY)
#endif
#ifdef SO_BSDCOMPAT
X(SO_BSDCOMPAT)
#endif
#ifdef SO_ACCEPTCON
X(SO_ACCEPTCON)
#endif
#ifdef SO_CONDITIONAL_ACCEPT
X(SO_CONDITIONAL_ACCEPT)
#endif
#ifdef SO_EXCLUSIVEADDRUSE
X(SO_EXCLUSIVEADDRUSE)
#endif
#ifdef SO_PORT_SCALABILITY
X(SO_PORT_SCALABILITY)
#endif
#ifdef SO_PASSCRED
X(SO_PASSCRED)
#endif
#ifdef SO_PEERCRED
X(SO_PEERCRED)
#endif
#ifdef SO_RCVLOWAT
X(SO_RCVLOWAT)
#endif
#ifdef SO_SNDLOWAT
X(SO_SNDLOWAT)
#endif
#ifdef SO_REUSEPORT
X(SO_REUSEPORT)
#endif
#ifdef SO_RCVTIMEO
X(SO_RCVTIMEO)
#endif
#ifdef SO_SNDTIMEO
X(SO_SNDTIMEO)
#endif
#ifdef SO_SECURITY_AUTHENTICATION
X(SO_SECURITY_AUTHENTICATION)
#endif
#ifdef SO_SECURITY_ENCRYPTION_TRANSPORT
X(SO_SECURITY_ENCRYPTION_TRANSPORT)
#endif
#ifdef SO_SECURITY_ENCRYPTION_NETWORK
X(SO_SECURITY_ENCRYPTION_NETWORK)
#endif

#ifdef MSG_OOB
X(MSG_OOB)
#endif
#ifdef MSG_DONTROUTE 
X(MSG_DONTROUTE)
#endif
#ifdef MSG_CTRUNC
X(MSG_CTRUNC)
#endif
#ifdef MSG_PROXY
X(MSG_PROXY)
#endif

#ifdef SOMAXCONN
X(SOMAXCONN)
#endif


#undef X
#define X(x) EXTERN_CONST int Ustat__##x = x;
#undef Y
#define Y(x, y) EXTERN_CONST int Ustat__##x = y;

X(S_IFMT)
X(S_IFREG)
X(S_IFDIR)
X(S_IFCHR)
#ifndef _WIN32
X(S_IFIFO)
X(S_IFSOCK)
X(S_IFLNK)
X(S_IFBLK)
X(S_ISUID)
X(S_ISGID)
X(S_ISVTX)
#endif

X(S_IREAD)
X(S_IWRITE)
X(S_IEXEC)

#ifndef _WIN32

Y(S_GREAD, S_IRGRP)
Y(S_GWRITE, S_IWGRP)
Y(S_GEXEC, S_IXGRP)
Y(S_OREAD, S_IROTH)
Y(S_OWRITE, S_IWOTH)
Y(S_OEXEC, S_IXOTH)

#endif

#ifdef HAS_STAT_FLAGS

/* Super-user and owner changeable flags. */

#ifdef UF_SETTABLE
X(UF_SETTABLE) /* mask of owner changeable flags */
#else
Y(UF_SETTABLE, 0)
#endif

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

/* Super-user changeable flags. */

#ifdef SF_SETTABLE
X(SF_SETTABLE) /* mask of superuser changeable flags */
#else
Y(SF_SETTABLE, 0)
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
#define X(x) M3_STATIC_ASSERT(x); EXTERN_CONST int Unetdb__##x = x;
X(TRY_AGAIN)
X(NO_RECOVERY)
X(NO_ADDRESS)

#undef X
#define X(x) EXTERN_CONST int Uin__##x = x;
X(IPPROTO_TCP)


#undef X
#define X(x) EXTERN_CONST int Umman__##x = x;

#ifndef _WIN32
X(PROT_NONE)
X(PROT_READ)
X(PROT_WRITE)
#endif

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

#ifdef __CYGWIN__

#undef X
#define X(x) EXTERN_CONST int Usysdep__##x = x;
#undef Y
#define Y(x, y) EXTERN_CONST int Usysdep__##x = y;

Y(P_NOWAIT, _P_NOWAIT)

#undef X
#undef Y
#define X(x) EXTERN_CONST int Utermio__##x = x;
#define Y(x, y) EXTERN_CONST int Utermio__##x = y;

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

#undef X
#define X(x) EXTERN_CONST int Unix__##x = x;

#ifdef TIOCCAR
X(TIOCCAR)
#endif

#ifdef TIOCCBRK
X(TIOCCBRK)
#endif

#ifdef TIOCCDTR
X(TIOCCDTR)
#endif

#ifdef TIOCCINUSE
X(TIOCCINUSE)
#endif

#ifdef TIOCCMLB
X(TIOCCMLB)
#endif

#ifdef TIOCEXCL
X(TIOCEXCL)
#endif

#ifdef TIOCFLUSH
X(TIOCFLUSH)
#endif

#if defined(TIOCGETC) && !(defined(__linux) && defined(__alpha))
X(TIOCGETC)
#endif

#ifdef TIOCGETD
X(TIOCGETD)
#endif

#if defined(TIOCGETP) && !(defined(__linux) && defined(__alpha))
X(TIOCGETP)
#endif

#if defined(TIOCGLTC) && !(defined(__linux) && defined(__alpha))
X(TIOCGLTC)
#endif

#ifdef TIOCGPGRP
X(TIOCGPGRP)
#endif

#ifdef TIOCGWINSZ
X(TIOCGWINSZ)
#endif

#ifdef TIOCHPCL
X(TIOCHPCL)
#endif

#ifdef TIOCLBIC
X(TIOCLBIC)
#endif

#ifdef TIOCLBIS
X(TIOCLBIS)
#endif

#ifdef TIOCLGET
X(TIOCLGET)
#endif

#ifdef TIOCLSET
X(TIOCLSET)
#endif

#ifdef TIOCMASTER
X(TIOCMASTER)
#endif

#ifdef TIOCMBIC
X(TIOCMBIC)
#endif

#ifdef TIOCMBIS
X(TIOCMBIS)
#endif

#ifdef TIOCMGET
X(TIOCMGET)
#endif

#ifdef TIOCMODEM
X(TIOCMODEM)
#endif

#ifdef TIOCMODG
X(TIOCMODG)
#endif

#ifdef TIOCMODS
X(TIOCMODS)
#endif

#ifdef TIOCMSET
X(TIOCMSET)
#endif

#ifdef TIOCM_CAR
X(TIOCM_CAR)
#endif

#ifdef TIOCM_CD
X(TIOCM_CD)
#endif

#ifdef TIOCM_CTS
X(TIOCM_CTS)
#endif

#ifdef TIOCM_DSR
X(TIOCM_DSR)
#endif

#ifdef TIOCM_DTR
X(TIOCM_DTR)
#endif

#ifdef TIOCM_LE
X(TIOCM_LE)
#endif

#ifdef TIOCM_RI
X(TIOCM_RI)
#endif

#ifdef TIOCM_RNG
X(TIOCM_RNG)
#endif

#ifdef TIOCM_RTS
X(TIOCM_RTS)
#endif

#ifdef TIOCM_SR
X(TIOCM_SR)
#endif

#ifdef TIOCM_ST
X(TIOCM_ST)
#endif

#ifdef TIOCNCAR
X(TIOCNCAR)
#endif

#ifdef TIOCNMODEM
X(TIOCNMODEM)
#endif

#ifdef TIOCNOTTY
X(TIOCNOTTY)
#endif

#ifdef TIOCNXCL
X(TIOCNXCL)
#endif

#ifdef TIOCOUTQ
X(TIOCOUTQ)
#endif

#ifdef TIOCPKT
X(TIOCPKT)
#endif

#ifdef TIOCPKT_DATA
X(TIOCPKT_DATA)
#endif

#ifdef TIOCPKT_DOSTOP
X(TIOCPKT_DOSTOP)
#endif

#ifdef TIOCPKT_FLUSHREAD
X(TIOCPKT_FLUSHREAD)
#endif

#ifdef TIOCPKT_FLUSHWRITE
X(TIOCPKT_FLUSHWRITE)
#endif

#ifdef TIOCPKT_IOCTL
X(TIOCPKT_IOCTL)
#endif

#ifdef TIOCPKT_NOSTOP
X(TIOCPKT_NOSTOP)
#endif

#ifdef TIOCPKT_START
X(TIOCPKT_START)
#endif

#ifdef TIOCPKT_STOP
X(TIOCPKT_STOP)
#endif

#ifdef TIOCREMOTE
X(TIOCREMOTE)
#endif

#ifdef TIOCSBRK
X(TIOCSBRK)
#endif

#ifdef TIOCSDTR
X(TIOCSDTR)
#endif

#if defined(TIOCSETC) && !(defined(__linux) && defined(__alpha))
X(TIOCSETC)
#endif

#ifdef TIOCSETD
X(TIOCSETD)
#endif

#if defined(TIOCSETN) && !(defined(__linux) && defined(__alpha))
X(TIOCSETN)
#endif

#if defined(TIOCSETP) && !(defined(__linux) && defined(__alpha))
X(TIOCSETP)
#endif

#ifdef TIOCSINUSE
X(TIOCSINUSE)
#endif

#if defined(TIOCSLTC) && !(defined(__linux) && defined(__alpha))
X(TIOCSLTC)
#endif

#ifdef TIOCSMLB
X(TIOCSMLB)
#endif

#ifdef TIOCSPGRP
X(TIOCSPGRP)
#endif

#ifdef TIOCSTART
X(TIOCSTART)
#endif

#ifdef TIOCSTI
X(TIOCSTI)
#endif

#ifdef TIOCSTOP
X(TIOCSTOP)
#endif

#ifdef TIOCSWINSZ
X(TIOCSWINSZ)
#endif

#ifdef TIOCUCNTL
X(TIOCUCNTL)
#endif

#ifdef TIOCWONLINE
X(TIOCWONLINE)
#endif

#ifdef TERMIODISC
X(TERMIODISC)
#endif

#ifdef TIOCCONS
X(TIOCCONS)
#endif

#ifdef TIOCDCDTIMESTAMP
X(TIOCDCDTIMESTAMP)
#endif

#ifdef TIOCDRAIN
X(TIOCDRAIN)
#endif

#ifdef TIOCDSIMICROCODE
X(TIOCDSIMICROCODE)
#endif

#ifdef TIOCEXT
X(TIOCEXT)
#endif

#ifdef TIOCGDRAINWAIT
X(TIOCGDRAINWAIT)
#endif

#if defined(TIOCGETA) && defined(__APPLE__)
X(TIOCGETA)
#endif

#ifdef TIOCIXOFF
X(TIOCIXOFF)
#endif

#ifdef TIOCIXON
X(TIOCIXON)
#endif

#ifdef TIOCMGDTRWAIT
X(TIOCMGDTRWAIT)
#endif

#ifdef TIOCMSDTRWAIT
X(TIOCMSDTRWAIT)
#endif

#ifdef TIOCPTYGNAME
X(TIOCPTYGNAME)
#endif

#ifdef TIOCPTYGRANT
X(TIOCPTYGRANT)
#endif

#ifdef TIOCPTYUNLK
X(TIOCPTYUNLK)
#endif

#ifdef TIOCSCONS
X(TIOCSCONS)
#endif

#ifdef TIOCSCTTY
X(TIOCSCTTY)
#endif

#ifdef TIOCSDRAINWAIT
X(TIOCSDRAINWAIT)
#endif

#if defined(TIOCSETA) && defined(__APPLE__)
X(TIOCSETA)
#endif

#if defined(TIOCSETAF) && defined(__APPLE__)
X(TIOCSETAF)
#endif

#if defined(TIOCSETAW) && defined(__APPLE__)
X(TIOCSETAW)
#endif

#ifdef TIOCSIG
X(TIOCSIG)
#endif

#ifdef TIOCSTAT
X(TIOCSTAT)
#endif

#ifdef TIOCTIMESTAMP
X(TIOCTIMESTAMP)
#endif

#ifdef TTYDISC
X(TTYDISC)
#endif

#ifdef ALLDELAY
X(ALLDELAY)
#endif

#ifdef ANYP
X(ANYP)
#endif

#ifdef BS0
X(BS0)
#endif

#ifdef BS1
X(BS1)
#endif

#ifdef BSDELAY
X(BSDELAY)
#endif

#ifdef CBREAK
X(CBREAK)
#endif

#ifdef CR0
X(CR0)
#endif

#ifdef CR1
X(CR1)
#endif

#ifdef CR2
X(CR2)
#endif

#ifdef CR3
X(CR3)
#endif

#ifdef CRDELAY
X(CRDELAY)
#endif

#ifdef CRMOD
X(CRMOD)
#endif

#ifdef CRTBS
X(CRTBS)
#endif

#ifdef CRTERA
X(CRTERA)
#endif

#ifdef CRTKIL
X(CRTKIL)
#endif

#ifdef CTLECH
X(CTLECH)
#endif

#ifdef DECCTQ
X(DECCTQ)
#endif

#ifdef ECHO
X(ECHO)
#endif

#ifdef EVENP
X(EVENP)
#endif

#ifdef FF0
X(FF0)
#endif

#ifdef FF1
X(FF1)
#endif

#ifdef FLUSHO
X(FLUSHO)
#endif

#ifdef L001000
X(L001000)
#endif

#ifdef LCASE
X(LCASE)
#endif

#ifdef LCRTBS
X(LCRTBS)
#endif

#ifdef LCRTERA
X(LCRTERA)
#endif

#ifdef LCRTKIL
X(LCRTKIL)
#endif

#ifdef LCTLECH
X(LCTLECH)
#endif

#ifdef LDECCTQ
X(LDECCTQ)
#endif

#ifdef LFLUSHO
X(LFLUSHO)
#endif

#ifdef LITOUT
X(LITOUT)
#endif

#ifdef LLITOUT
X(LLITOUT)
#endif

#ifdef LMDMBUF
X(LMDMBUF)
#endif

#ifdef LNOFLSH
X(LNOFLSH)
#endif

#ifdef LNOHANG
X(LNOHANG)
#endif

#ifdef LPASS8
X(LPASS8)
#endif

#ifdef LPENDIN
X(LPENDIN)
#endif

#ifdef LPRTERA
X(LPRTERA)
#endif

#ifdef LTILDE
X(LTILDE)
#endif

#ifdef LTOSTOP
X(LTOSTOP)
#endif

#ifdef MDMBUF
X(MDMBUF)
#endif

#ifdef NETLDISC
X(NETLDISC)
#endif

#ifdef NL0
X(NL0)
#endif

#ifdef NL1
X(NL1)
#endif

#ifdef NL2
X(NL2)
#endif

#ifdef NL3
X(NL3)
#endif

#ifdef NLDELAY
X(NLDELAY)
#endif

#ifdef NOFLSH
X(NOFLSH)
#endif

#ifdef NOHANG
X(NOHANG)
#endif

#ifdef NTTYDISC
X(NTTYDISC)
#endif

#ifdef ODDP
X(ODDP)
#endif

#ifdef OTIOCCONS
X(OTIOCCONS)
#endif

#ifdef OTIOCGETD
X(OTIOCGETD)
#endif

#ifdef OTIOCSETD
X(OTIOCSETD)
#endif

#ifdef OTTYDISC
X(OTTYDISC)
#endif

#ifdef PASS8
X(PASS8)
#endif

#ifdef PENDIN
X(PENDIN)
#endif

#ifdef PRTERA
X(PRTERA)
#endif

#ifdef RAW
X(RAW)
#endif

#ifdef TAB0
X(TAB0)
#endif

#ifdef TAB1
X(TAB1)
#endif

#ifdef TAB2
X(TAB2)
#endif

#ifdef TANDEM
X(TANDEM)
#endif

#ifdef TBDELAY
X(TBDELAY)
#endif

#ifdef TILDE
X(TILDE)
#endif

#ifdef TIOCGSID
X(TIOCGSID)
#endif

#ifdef TOSTOP
X(TOSTOP)
#endif

#ifdef VTDELAY
X(VTDELAY)
#endif

#ifdef XTABS
X(XTABS)
#endif

#ifdef __cplusplus
}
#endif

#if M3_HAS_VISIBILITY
#pragma GCC visibility pop
#endif
