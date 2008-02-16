
#include "regex.h"
#include <stdlib.h>
#include <stdio.h>

regex_t *grex = NULL;
char buf[256];

char * my_re_comp(const char *s) {
  int ret;
#ifndef __FreeBSD__
  if (grex == NULL) grex = malloc(sizeof(regex_t));
#endif
  ret = regcomp (grex, s, REG_NOSUB);
  if (ret == 0) return NULL;
  sprintf(buf, "regcomp returned %d", ret);
  return buf;
}

int my_re_exec(const char *s) {
  int ret;
  ret = regexec (grex, s, 0, NULL, 0);
  if (ret == 0) return 1;
  return 0;
}
