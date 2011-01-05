#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

static int xStackGrowsDown (volatile char* a)
{
  volatile char b;
  return (&b < a);
}

int
ThreadInternal__StackGrowsDown (void)
{
  volatile char a;
  return xStackGrowsDown (&a);
}

int main()
{
    printf("stack grows %s\n", ThreadInternal__StackGrowsDown () ? "down" : "up");
    return EXIT_SUCCESS;
}
