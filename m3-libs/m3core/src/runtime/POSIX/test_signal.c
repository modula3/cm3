#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif
#include "RTSignalC.c"

void RTSignalPrivate__MsgPCSegV (size_t a)
{
  static const char Function[] = "RTSignalPrivate__MsgPCSegV";
  fprintf(stderr, "%s %lx\n", Function, (unsigned long)a);
  exit(1);
}

void RTSignalPrivate__MsgPCAbort (size_t a)
{
  static const char Function[] = "RTSignalPrivate__MsgPCAbort";
  fprintf(stderr, "%s %lx\n", Function, (unsigned long)a);
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

void F1(void)
{
  /*(volatile int*)0;*/
 ((void(*)())0x123)();
}

int main()
{
  InstallHandlers();
  printf("%p\n", &F1);
  F1();
  return 0;
}

