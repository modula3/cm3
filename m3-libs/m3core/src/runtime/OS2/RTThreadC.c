/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/* Last modified on Mon Nov 10              1995 by preschern  */
/*      modified on Mon Dec  6 16:14:35 PST 1993 by kalsow     */
/*      modified on Tue Jan 19 15:20:48 PST 1993 by burrows    */

/* This file implements the coroutine transfer: RTThread.Transfer */

#include <setjmp.h>
#define INCL_DOSERRORS
#define INCL_DOSDATETIME
#define INCL_DOSSEMAPHORES
#define INCL_DOSPROCESS
#include <os2emx.h>
#include <signal.h>
#include <stdio.h>

RTThread__Transfer (from, to)
jmp_buf *from, *to;
{
  if (__setjmp(*from) == 0) __longjmp (*to, 1);
}


/* global thread ID used by 'etp' */
int ThreadF__myId = 1;

/* low-level runtime lock */
int RT0u__inCritical = 0;

/* global, per-thread linked list of exception handlers */
void* RTThread__handlerStack = 0;

/*-------------------- timer for thread switching ------------------------*/

typedef VOID               (*SignalActionHandler)(INT sig);
static INT                 iInit = 0;
static SignalActionHandler sigHandlerProc = NULL;
static ULONG               TimerInterval = 200;         /* time delay */

static VOID switch_thread (int sig)
  {
  sigHandlerProc (0);                         /* call scheduler             */
  signal (sig, SIG_ACK);                      /* acknowledge signal         */
  alarm (1);                                  /* restart alarm timer        */
  }

int RTThread__StartTimer (SignalActionHandler sigHandler)
  {
  APIRET                res = 0;

  if (iInit == 0)
    {
    iInit = 1;
    sigHandlerProc = sigHandler;
    if (signal (SIGALRM, switch_thread) == SIG_ERR)
      {
      perror ("Could not set SIGALRM");
      res = -1;
      }

    if (res == 0)
      {
      alarm (1);                              /* start alarm timer          */
      }
    }

  return (res);
  }

