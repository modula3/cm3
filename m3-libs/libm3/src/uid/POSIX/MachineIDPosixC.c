/* Copyright (C) 1993, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/* Last modified on Mon Sep 20 11:46:17 PDT 1993 by kalsow     */
/*      modified on Thu Jul 15 16:23:08 PDT 1993 by swart      */

#include "m3core.h"
#include <unistd.h>
#include <netdb.h>
#include <string.h>
#include <net/if.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <stdlib.h>

#if defined(__aix) \
 || defined(__hpux) \
 || defined(__INTERIX) \
 || defined(__irix) \
 || defined(__osf__) \
 || defined(__sgi) \
 || defined(__vms)
/* osf might already work */
#error Please test/port this.
#endif

#if !(defined(__APPLE__)    \
   || defined(__CYGWIN__)   \
   || defined(__FreeBSD__)  \
   || defined(__linux__)    \
   || defined(__NetBSD__)   \
   || defined(__OpenBSD__)  \
   || defined(__sun))
#error Please test/port this.
#endif

#ifdef __sun
/* need works but keep the old version */
#endif

#if defined(__OpenBSD__) || defined(__NetBSD__) || defined(__FreeBSD__) || defined(__APPLE__)
#define HAS_GETIFADDRS
#endif

#ifdef HAS_GETIFADDRS
#include <net/if_dl.h>
#include <ifaddrs.h>
#endif

#ifdef __APPLE__
#ifndef _SIZEOF_ADDR_IFREQ
#error Apple => _SIZEOF_ADDR_IFREQ
#endif
#else
#ifdef _SIZEOF_ADDR_IFREQ
#error !Apple => !_SIZEOF_ADDR_IFREQ
#endif
#define _SIZEOF_ADDR_IFREQ(a) (sizeof(a))
#endif

#ifdef __cplusplus
extern "C" {
#endif

int/*boolean*/
__cdecl
MachineIDPosixC__CanGet(unsigned char *id)
{
    int result = { 0 };
    int s = { 0 };
#ifdef HAS_GETIFADDRS
    struct ifaddrs* if1 = { 0 };
    struct ifaddrs* if2 = { 0 };
#elif defined(__linux__) || defined(__osf__) || defined(__CYGWIN__)
    union {
        struct ifreq req[64];
        char b[4096];
    } buf;
    struct ifconf list = { 0 };
    struct ifreq* req1 = { 0 };
    struct ifreq req2 = { 0 };
    size_t i = { 0 };
#endif

    memset(id, 0, 6);

    /* try to find an ethernet hardware address */
    s = socket(PF_UNIX, SOCK_STREAM, AF_UNSPEC);
    if (s >= 0)
    {
#ifdef HAS_GETIFADDRS
        if (getifaddrs(&if1) == 0)
        {
            for (if2 = if1; (!result) && if2; if2 = if2->ifa_next)
            {
                if (if2->ifa_addr->sa_family == AF_LINK)
                {
                    struct sockaddr_dl* dl = (struct sockaddr_dl*)if2->ifa_addr;
                    unsigned char* mac = (unsigned char*)LLADDR(dl);
                    if (dl->sdl_alen == 6) /* 48 bits */
                    {
                        memcpy(id, mac, 6);
                        result = 1;
                    }
                }
            }
            freeifaddrs(if1);
        }
#elif defined(__linux__) || defined(__CYGWIN__) || defined(__osf__)
        list.ifc_len = sizeof(buf);
        list.ifc_req = (struct ifreq*)&buf;

        if (ioctl(s, SIOCGIFCONF, &list) >= 0)
        {
            for (i = 0; (!result) && (i < list.ifc_len); i += _SIZEOF_ADDR_IFREQ(*req1))
            {
                req1 = (struct ifreq*)&buf.b[i];
                memcpy(req2.ifr_name, req1->ifr_name, IFNAMSIZ);
#if defined(__linux__) || defined(__CYGWIN__)
                if (ioctl(s, SIOCGIFFLAGS, &req2) < 0)
                    continue;
                if ((req2.ifr_flags & IFF_LOOPBACK) != 0)
                    continue;
                if (ioctl(s, SIOCGIFHWADDR, &req2) < 0)
                    continue;
                memcpy(id, req2.ifr_hwaddr.sa_data, 6);
#elif defined(__osf__)
                if (ioctl(s, SIOCRPHYSADDR, &req2) < 0)
                    continue;
                memcpy(id, req2.default_pa, 6);
#else
#error unknown system
#endif
                result = 1;
            }
        }
        else
        {
            printf("ioctl SIOCGIFCONF failed %d\n", errno);
        }
#endif
        close(s);
    }

    /* lame fallback esp. for Solaris */
    if (result == 0)
    {
        char hostname[256];
        struct hostent *hostent;

        /* try using the machine's internet address */
        if (gethostname(hostname, sizeof(hostname)) == 0)
        {
            struct hostent* hostent = gethostbyname(hostname);
            if (hostent && hostent->h_length == 4)
            {
                id[2] = hostent->h_addr[0];
                id[3] = hostent->h_addr[1];
                id[4] = hostent->h_addr[2];
                id[5] = hostent->h_addr[3];
                result = 1;
            }
        }
    }

    return result;
}


#ifdef __cplusplus
} /* extern "C" */
#endif

#if 0 /* test code */

int main()
{
    unsigned char id[6] = { 0 };
    int i = { 0 };
    
    i = MachineIDPosixC__CanGet(id);
    printf("%d %02X-%02X-%02X-%02X-%02X-%02X\n", i, id[0], id[1], id[2], id[3], id[4], id[5]);
    printf("   %u.%u.%u.%u.%u.%u\n", id[0], id[1], id[2], id[3], id[4], id[5]);
    system("/sbin/ifconfig -a | grep ether");
    system("/sbin/ifconfig -a | grep addr");

    return 0;
}

#endif
