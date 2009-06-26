#include "RTSignalC.c"
#include <stdio.h>
#include <stdlib.h>

void RTSignalPrivate__MsgPCSegV (size_t a)
{
  fprintf(stderr, "%s %lx\n", __FUNCTION__, (unsigned long)a);
  exit(1);
}

void RTSignalPrivate__MsgPCAbort (size_t a)
{
  fprintf(stderr, "%s %lx\n", __FUNCTION__, (unsigned long)a);
  exit(1);
}

void RTProcess__InvokeExitors(void)
{
}

RTProcess__InterruptHandler
RTProcess__OnInterrupt(RTProcess__InterruptHandler x)
{
  return 0;
}


int main()
{
  InstallHandlers();
  *(volatile int*)0;

  return 0;
}

