/* Show the alignment of alloca. */

#include <stdio.h>
#if defined(__sun) || defined(__linux) || defined(__APPLE__)
#include <alloca.h>
#else
#include <stdlib.h>
#endif

int main()
{
  unsigned i;
  for (i = 0; i <= 65; ++i)
    printf("%x ", 0xFFF & (unsigned)(size_t)alloca(i));
  printf("\n");
  return 0;
}
