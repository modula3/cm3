#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

int
ThreadInternal__StackGrowsDownHelper (volatile char* a)
/* Inlining could potentially reverse the relative placements,
 * leading to an incorrect result! Recursion is used to
 * help defeat inlining optimizer, though a smart compiler
 * could still inline. */
{
  volatile char b;
  return a ? (&b < a) : ThreadInternal__StackGrowsDownHelper (&b);
}

int __cdecl
ThreadInternal__StackGrowsDown (void)
{
  int a = ThreadInternal__StackGrowsDownHelper (0);
#ifdef __hppa__
  assert(!a);
#else
  assert(a);
#endif
  return a;
}

int main()
{
    printf("stack grows %s\n", ThreadInternal__StackGrowsDown () ? "down" : "up");
    return EXIT_SUCCESS;
}
