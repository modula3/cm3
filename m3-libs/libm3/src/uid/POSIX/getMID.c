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


main() {

    struct ifdevea req;
    struct ifconf list;
    int s, i;
    struct ifreq buf[10];

    s = socket(AF_UNIX, SOCK_STREAM, PF_UNSPEC);
    if (s < 0) {
       perror("socket");
       exit(1);
    };

    list.ifc_len = sizeof(buf);
    list.ifc_req = buf;

    if (-1 == ioctl(s, SIOCGIFCONF, &list))
      {perror("SIOCGIFCONF"); exit(2);};

    printf("list.ifc_len %d\n", list.ifc_len);

    for (i == 0; i < list.ifc_len / sizeof(struct ifreq); i++) {
      printf("buf[%d] = %s\n", i, buf[i].ifr_name);
      strcpy(req.ifr_name, buf[i].ifr_name);
      if (-1 == ioctl(s, SIOCRPHYSADDR, &req)) {
	 perror("  SIOCRPHYSADDR");
      } else {
        printf("  Device address %02x%02x%02x%02x%02x%02x\n",
	     req.default_pa[0], req.default_pa[1],
	     req.default_pa[2], req.default_pa[3],
	     req.default_pa[4], req.default_pa[5]);};
    };
    exit(0);
}
	     
