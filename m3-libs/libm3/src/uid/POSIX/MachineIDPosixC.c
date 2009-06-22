/* Copyright (C) 1993, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/* Last modified on Mon Sep 20 11:46:17 PDT 1993 by kalsow     */
/*      modified on Thu Jul 15 16:23:08 PDT 1993 by swart      */

#include <unistd.h>
#include <netdb.h>

#if defined(__linux__) || defined(__osf__) || defined(__CYGWIN__)

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <string.h>

#endif

int MachineIDPosixC__CanGet(char *id)
{
    int i;
    char hostname[128];
    struct hostent *hostent;

#if defined(__linux__) || defined(__osf__) || defined(__CYGWIN__)

    struct ifreq req;
    struct ifconf list;
    int s;
    struct ifreq buf[10];

    /* try to find an ethernet hardware address */
    s = socket(PF_UNIX, SOCK_STREAM, AF_UNSPEC);
    if (s >= 0)
    {
        list.ifc_len = sizeof buf;
        list.ifc_req = buf;

        if (ioctl(s, SIOCGIFCONF, &list) >= 0)
        {
            for (i = 0; i < list.ifc_len / sizeof(struct ifreq); i++)
            {
                strcpy(req.ifr_name, buf[i].ifr_name);
#if defined(__linux__) || defined(__CYGWIN__)
                s = ioctl(s, SIOCGIFHWADDR, &req);
#elif defined(__osf__)
                s = ioctl(s, SIOCRPHYSADDR, &req);
#endif
                if (s >= 0)
                {
                    int j;
                    for (j = 0; j < 6; j++)
                    {
#if defined(__linux__) || defined(__CYGWIN__)
                        id[j] = req.ifr_hwaddr.sa_data[j];
#elif defined(__osf__)
                        id[j] = req.default_pa[j];
#endif
                    }
                    return 1;
                }
            }
        }
    }
#endif

    /* try using the machine's internet address */
    if (gethostname(hostname, 128) == 0)
    {
        hostent = gethostbyname(hostname);
        if (hostent && hostent->h_length == 4)
        {
            id[0] = 0;
            id[1] = 0;
            id[2] = hostent->h_addr[0];
            id[3] = hostent->h_addr[1];
            id[4] = hostent->h_addr[2];
            id[5] = hostent->h_addr[3];
            return 1;
        }
    }

    for (i = 0; i < 6; i++)
        id[i] = 0;
    return 0;
}


#if 0 /* test code */

#include <stdio.h>

int main()
{
    unsigned char id[6];
    int i;
    
    i = MachineIDPosixC__CanGet((char*)id);
    printf("%d %02x%02x%02x%02x%02x%02x\n", i, id[0], id[1], id[2], id[3], id[4], id[5]);

    return 0;
}

#endif
