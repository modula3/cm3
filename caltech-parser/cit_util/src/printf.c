#include <stdio.h>

#if __cplusplus
extern "C" {
#endif

void printf_prints (char *s)
{
  printf("%s\n",s);
}

#if __cplusplus
} /* extern "C" */
#endif
