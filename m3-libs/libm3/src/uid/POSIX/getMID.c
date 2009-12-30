/* Copyright (C) 1993, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/* Last modified on Mon Sep 20 11:46:17 PDT 1993 by kalsow     */
/*      modified on Thu Jul 15 16:23:08 PDT 1993 by swart      */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef __APPLE__
#include <ifaddrs.h>
#include <sys/socket.h>
#include <net/if_dl.h>
#endif

void breakpoint()
{
    /*asm("int3");*/
}

main() {

#ifdef SIOCRPHYSADDR
    struct ifdevea req = { 0 };
#endif
    struct ifconf list = { 0 };
    int s = { 0 };
    int i = { 0 };
    union {
        struct ifreq a;
        char b[4096];
    } buf;
    struct ifreq* p = { 0 };
#ifdef __APPLE__
    struct ifaddrs* if1;
    struct ifaddrs* if2;
#endif
    memset(&buf, 0, sizeof(buf));

    s = socket(AF_UNIX, SOCK_STREAM, PF_UNSPEC);
    if (s < 0) {
       perror("socket");
       exit(1);
    }
    
 #ifdef __APPLE__
    getifaddrs(&if1);
    for (if2 = if1; if2; if2 = if2->ifa_next)
    {
        if (if2->ifa_addr->sa_family == AF_LINK)
        {
            struct sockaddr_dl* volatile dl = (struct sockaddr_dl*)if2->ifa_addr;
            unsigned char* mac = (unsigned char*)LLADDR(dl);
            if (dl->sdl_alen == 6) /* 48 bits */
            {
                printf("%s %02X:%02X:%02X:%02X:%02X:%02X\n",
                    if2->ifa_name,
                    mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);
            }
            else if (dl->sdl_alen == 8) /* 64 bits IP over firewire */
            {
                printf("%s %02X:%02X:%02X:%02X:%02X:%02X:%02X:%02X\n",
                    if2->ifa_name,
                    mac[0], mac[1], mac[2], mac[3], mac[4], mac[5], mac[6], mac[7]);
            }
            breakpoint();
        }
    }
    freeifaddrs(if1);
#endif

    list.ifc_len = sizeof(buf);
    list.ifc_req = &buf.a;

    if (-1 == ioctl(s, SIOCGIFCONF, &list))
      {perror("SIOCGIFCONF"); exit(2);}

    printf("list.ifc_len %d\n", list.ifc_len);

    for (i = 0; i < list.ifc_len; i += _SIZEOF_ADDR_IFREQ(*p)) {
      p = (struct ifreq*)&buf.b[i];
      printf("buf[%d] = %s\n", i, p->ifr_name);
#ifdef SIOCRPHYSADDR
      strcpy(req.ifr_name, p->ifr_name);
      if (-1 == ioctl(s, SIOCRPHYSADDR, &req)) {
        perror("  SIOCRPHYSADDR");
      } else {
        printf("  Device address %02x%02x%02x%02x%02x%02x\n",
	     req.default_pa[0], req.default_pa[1],
	     req.default_pa[2], req.default_pa[3],
	     req.default_pa[4], req.default_pa[5]);}
    }
#endif
  }
  exit(0);
}
