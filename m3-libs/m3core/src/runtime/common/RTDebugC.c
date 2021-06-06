#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef _WIN32

BOOLEAN __cdecl RTDebug__IsDebuggerPresent(void)
{
  return !!IsDebuggerPresent();
}

void __cdecl RTDebug__DebugBreak(void)
{
  DebugBreak();
}

#else

#include <unistd.h>
#include <signal.h>
#include <stdio.h>

static volatile char inited;
static char is_debugger_present;

static void init(void)
/* todo: test detach/reattach, and maybe make them work */
{
  struct sigaction sa;

  if (inited)
    return;
    
  sigaction(SIGTRAP, 0, &sa);
#ifdef __APPLE__
  /* experimentally derived */
  if ((sa.sa_mask == 0) != (sa.sa_flags == SA_RESTART))
    printf("warning 1 in RTDebug.init\n");
  is_debugger_present = (sa.sa_mask == 0) && (sa.sa_flags == SA_RESTART);
#else
  /* todo? */
#endif
  inited = 1;
}

BOOLEAN __cdecl RTDebug__IsDebuggerPresent(void)
{
  init();
  return !!is_debugger_present;
}

void __cdecl RTDebug__DebugBreak(void)
/* todo: this is so big/slow compared to Win32! */
{
  struct sigaction sa;

  sigaction(SIGTRAP, 0, &sa);
  signal(SIGTRAP, SIG_IGN);
  raise(SIGTRAP);
  sigaction(SIGTRAP, &sa, 0);
}

#endif

#if 0 /* test code */

int main()
{
  printf("IsDebuggerPresent %d\n", RTDebug__IsDebuggerPresent());
  RTDebug__DebugBreak();
  printf("after debugbreak (make sure it doesn't exit)\n");
  return 0;
}

#endif
