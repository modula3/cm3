/* Show the alignment of alloca. */

#include <stdlib.h>
#include <stdio.h>

int main()
{
  unsigned i;
  for (i = 0; i <= 65; ++i)
    printf("%x ", 0xFF & (unsigned)(size_t)alloca(i));
  printf("\n");
  return 0;
}
