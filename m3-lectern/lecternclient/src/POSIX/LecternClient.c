/* Copyright (C) 1994, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/* Last modified on Fri Mar 10 12:29:30 PST 1995 by mcjones    */

/* Send a request to an instance of Lectern, starting one if necessary. */

/* Inspired by emacsclient. */

#if defined(__INTERIX) && !defined(_REENTRANT)
#define _REENTRANT
#endif
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/stat.h>
#include <stdio.h>
#include <errno.h>
#include <sys/param.h>
#include <stdlib.h>
#include <unistd.h>

static char *argv0;

static void Abort(char *action)
{
  fprintf(stderr, "%s: ", argv0);
  perror(action);
  exit(1);
}

static void Usage(void)
{
  fprintf(
    stderr,
    "Usage: %s pathname  or:  %s -f pathname\n   or:  %s -l arg...\n",
    argv0, argv0, argv0);
  exit(1);
}

static void PutInt(int n, FILE *wr)
{
  int i;
  for (i = 0; i <= 24; i += 8) putc((n >> i) % 256, wr);
}

#define NUMSTR 50
#define STRSIZE 300
#define RETRIES 6

static void StartLectern(void)
{
  int status = system("Lectern&\n");
  if (status != 0)
  {
    fprintf(stderr, "%s: couldn't start Lectern\n", argv0);
    exit(1);
  }
}

int
main(int argc, char *argv[])
{
  int s;
  struct sockaddr_un addr;
  struct stat statbuffer;
  FILE *wr, *rd;
  char str[NUMSTR][STRSIZE];
  char pathname[MAXPATHLEN+1];
  int i, nArgs, nChars, j;

  memset(&addr, 0, sizeof(addr));

  argv0 = argv[0];

  if (argc < 2) Usage();

  if (argv[1][0] != '-') {
    if (argc != 2) Usage();
    }
  else if (strcmp(argv[1], "-l") == 0) {
    if (argc < 3) Usage();
    }
  else if (strcmp(argv[1], "-f") == 0) {
    if (argc != 3) Usage();
    rd = fopen(argv[2], "r");
    if (rd == NULL) Abort("fopen");

    for(i = 0; i < NUMSTR; i++) {
      fgets(str[i], STRSIZE, rd);
      if (feof(rd)) break;
      if (ferror(rd)) Abort("fgets");
      if (str[i][strlen(str[i])-1] != '\n') {
        fprintf(stderr, "%s -f: line longer than %d\n", argv0, STRSIZE);
        exit(1);
        }
      }

    if (!feof(rd))
      fprintf(
        stderr,
        "%s -f: linecount in excess of %d; skipping\n", argv0, NUMSTR
        );

    nArgs = i;
    }
  else Usage();

  s = socket(AF_UNIX, SOCK_STREAM, 0);
  if (s < 0) Abort("socket");

  addr.sun_family = AF_UNIX;
  sprintf(addr.sun_path, "/tmp/lectern%d", geteuid());

  for (i = 1; i < RETRIES; i++) {

    if (stat(addr.sun_path, &statbuffer) == 0) {

      if (statbuffer.st_uid != geteuid()) {
        fprintf(stderr, "Unexpected socket owner\n");
        exit(1);
        }

      if (connect(
	   s,
	   (struct sockaddr *)&addr,
	   strlen(addr.sun_path) + sizeof(addr.sun_family)) == 0) 
        break; /* connection to Lectern established */
      if (errno != ECONNREFUSED) Abort("connect");

      }
    else {if (errno != ENOENT) Abort("stat");}

    /* Lectern never started, or quit; try to restart it, or wait. */
    if (i == 1) StartLectern();
    if (i == 6) {
      fprintf(stderr, "%s: couldn't start Lectern\n", argv0);
      exit(1);
      }
    sleep(3);
    }

  wr = fdopen(s, "r+");
  if (wr == NULL) Abort("fdopen");

  if (strcmp(argv[1], "-f") == 0) {
    PutInt(nArgs, wr);
    
    for (i = 0; i < nArgs; i++) {
      nChars = strlen(str[i]) - 1;
      PutInt(nChars, wr);
      for (j = 0; j < nChars; j++) putc(str[i][j], wr);
      }
    }
  else if( strcmp(argv[1], "-l") == 0) {
    PutInt(argc - 2, wr);
    for (i = 2; i < argc; i++) {
      nChars = strlen(argv[i]);
      PutInt(nChars, wr);
      for (j = 0; j < nChars; j++) putc(argv[i][j], wr);
      }
    }
  else {
    PutInt(1, wr);
    pathname[0] = '\000';
    if (argv[1][0] != '/') {
      if (getcwd(pathname, sizeof(pathname)) == 0) {
        fprintf(stderr, "%s: %s\n", argv0, pathname);
        exit(1);
        }
      strncat(pathname, "/", MAXPATHLEN-strlen(pathname));
      }
    strncat(pathname, argv[1], MAXPATHLEN-strlen(pathname));
    nChars = strlen(pathname);
    PutInt(nChars, wr);
    for (j = 0; j < nChars; j++) putc(pathname[j], wr);
    }
  fflush(wr);

  exit(0);
}
