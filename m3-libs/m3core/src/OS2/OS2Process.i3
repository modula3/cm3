(* Copyright (C) 1995, Klaus Preschern                       *)
(* All rights reserved.                                      *)
(*                                                           *)
INTERFACE OS2Process;

FROM OS2Def IMPORT ULONG, USHORT, LONG, PCHAR, TID, PTID, PID, PPID, PVOID,
                   PCSZ;

CONST
 EXIT_THREAD  = 0;
 EXIT_PROCESS = 1;

<*EXTERNAL DosBeep*>
PROCEDURE DosBeep (ulFrequency: ULONG; ulDuration: ULONG): ULONG;
<*EXTERNAL DosExit*>
PROCEDURE DosExit (ulAction: ULONG; ulResult: ULONG);

CONST
  CREATE_READY           = 0;
  CREATE_SUSPENDED       = 1;

  STACK_SPARSE           = 0;
  STACK_COMMITTED        = 2;

  DCWA_PROCESS           = 0;
  DCWA_PROCESSTREE       = 1;

  DCWW_WAIT              = 0;
  DCWW_NOWAIT            = 1;

  DKP_PROCESSTREE        = 0;
  DKP_PROCESS            = 1;

  EXEC_SYNC              = 0;
  EXEC_ASYNC             = 1;
  EXEC_ASYNCRESULT       = 2;
  EXEC_TRACE             = 3;
  EXEC_BACKGROUND        = 4;
  EXEC_LOAD              = 5;
  EXEC_ASYNCRESULTDB     = 6;

  EXLST_ADD              = 1;
  EXLST_REMOVE           = 2;
  EXLST_EXIT             = 3;

  PRTYC_NOCHANGE         = 0;
  PRTYC_IDLETIME         = 1;
  PRTYC_REGULAR          = 2;
  PRTYC_TIMECRITICAL     = 3;
  PRTYC_FOREGROUNDSERVER = 4;

  PRTYD_MINIMUM          = (-31);
  PRTYD_MAXIMUM          = 31;

  PRTYS_PROCESS          = 0;
  PRTYS_PROCESSTREE      = 1;
  PRTYS_THREAD           = 2;

  TC_EXIT                = 0;
  TC_HARDERROR           = 1;
  TC_TRAP                = 2;
  TC_KILLPROCESS         = 3;
  TC_EXCEPTION           = 4;

TYPE
  PRESULTCODES = UNTRACED REF RESULTCODES;
  RESULTCODES = RECORD
    codeTerminate  : ULONG;
    codeResult     : ULONG;
  END;

  PTIB2 = UNTRACED REF TIB2;
  TIB2 = RECORD
    tib2_ultid: ULONG;
    tib2_ulpri: ULONG;
    tib2_version: ULONG;
    tib2_usMCCount: USHORT;
    tib2_fMCForceFlag: USHORT;
  END;

  PTIB = UNTRACED REF TIB;
  TIB = RECORD
    tib_pexchain: PVOID;
    tib_pstack: PVOID;
    tib_pstacklimit: PVOID;
    tib_ptib2: PTIB2;
    tib_version: ULONG;
    tib_ordinal: ULONG;
  END;

  PPIB = UNTRACED REF PIB;
  PIB = RECORD
    pib_ulpid: ULONG;
    pib_ulppid: ULONG;
    pib_hmte: ULONG;
    pib_pchcmd: PCHAR;
    pib_pchenv: PCHAR;
    pib_flstatus: ULONG;
    pib_ultype: ULONG;
  END;

PFNTHREAD   = PROCEDURE (ulThreadArg: ULONG);
PFNEXITLIST = PROCEDURE (ulArg: ULONG);

<* EXTERNAL DosCreateThread *>
PROCEDURE DosCreateThread (ptidThreadID: PTID; pfnThreadAddr: PFNTHREAD;
    ulThreadArg: ULONG; ulFlags: ULONG; ulStackSize: ULONG): ULONG;

<* EXTERNAL DosEnterCritSec *>
PROCEDURE DosEnterCritSec (): ULONG;

<* EXTERNAL DosExecPgm *>
PROCEDURE DosExecPgm (pObjname: PCHAR; lObjnameLength: LONG; ulFlagS: ULONG;
    pszArg: PCSZ; pszEnv: PCSZ; pReturnCodes: PRESULTCODES; pszName: PCSZ): ULONG;

<* EXTERNAL DosExitCritSec *>
PROCEDURE DosExitCritSec (): ULONG;

<* EXTERNAL DosExitList *>
PROCEDURE DosExitList (ulOrder: ULONG; pfn: PFNEXITLIST): ULONG;

<* EXTERNAL DosGetInfoBlocks *>
PROCEDURE DosGetInfoBlocks (ptib: UNTRACED REF PTIB; ppib: UNTRACED REF PPIB): ULONG;

<* EXTERNAL DosKillProcess *>
PROCEDURE DosKillProcess (ulAction: ULONG; pid: PID): ULONG;

<* EXTERNAL DosKillThread *>
PROCEDURE DosKillThread (tid: TID): ULONG;

<* EXTERNAL DosResumeThread *>
PROCEDURE DosResumeThread (tid: TID): ULONG;

<* EXTERNAL DosSetPriority *>
PROCEDURE DosSetPriority (ulScope: ULONG; ulClass: ULONG; lDelta: LONG; ulID: ULONG): ULONG;

<* EXTERNAL DosSleep *>
PROCEDURE DosSleep (ulInterval: ULONG): ULONG;

<* EXTERNAL DosSuspendThread *>
PROCEDURE DosSuspendThread (tid: TID): ULONG;

<* EXTERNAL DosWaitChild *>
PROCEDURE DosWaitChild (ulAction: ULONG; ulWait: ULONG; pReturnCodes: PRESULTCODES;
    ppidOut: PPID; pidIn: PID): ULONG;

<* EXTERNAL DosWaitThread *>
PROCEDURE DosWaitThread (ptid: PTID; ulWait: ULONG): ULONG;

END OS2Process.
