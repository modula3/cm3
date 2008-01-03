/* Copyright (C) 1994, Digital Equipment Corporation.       */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */
/*                                                          */
/* Last modified on Tue Nov  1 08:56:07 PST 1994 by kalsow  */
/*      modified on Wed Jun  2 08:40:14 PDT 1993 by mcjones */

#include <errno.h>
#include <fcntl.h>

main(argc, argv)
  int argc; char *argv[]; {
  int fd, result;
  struct flock param;

  if (argc != 2) {
    printf("usage: locktest <pathname>\n");
    exit(1);
  }
  printf("locking %s exclusive\n", argv[1]);
  fd = open(argv[1], O_RDWR|O_CREAT, 0777);
  if (fd < 0) {
    printf("error %d opening %s\n", errno, argv[1]);
    exit(1);
  }
  param.l_type = F_WRLCK;
  param.l_whence = 0;
  param.l_start = 0;
  param.l_len = 0;
  param.l_pid = 0;
  result = fcntl(fd, F_SETLK, &param);
  if (result < 0) {
    if (errno == EACCES) {
      printf("lock already held, giving up\n");
      exit(1);
    }
    printf("unexpected error %d, giving up\n", errno);
    exit(1);
  }
  printf("lock acquired, pausing\n");
  pause();
}
