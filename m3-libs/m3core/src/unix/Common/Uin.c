#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif
#ifndef _WIN32
/* Posix says include <arpa/inet.h>, but FreeBSD 4 inet.h
   requires netinet/in.h
 */
#include <netinet/in.h>
#include <arpa/inet.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* ntohl are sometimes macros (FreeBSD 4) so we don't use M3WRAP1 */

UINT32 __cdecl Uin__ntohl(UINT32 x) { return ntohl(x); }
UINT16 __cdecl Uin__ntohs(UINT16 x) { return ntohs(x); }
UINT32 __cdecl Uin__htonl(UINT32 x) { return htonl(x); }
UINT16 __cdecl Uin__htons(UINT16 x) { return htons(x); }

#ifdef __cplusplus
} /* extern "C" */
#endif
