/* Copyright (C) 1994, Digital Equipment Corporation         */
/* All rights reserved.                                      */
/* See the file COPYRIGHT for a full description.            */

<*EXTERNAL InterlockedIncrement:WINAPI*>
PROCEDURE InterlockedIncrement (lpAddend: PINT32): INT32;

<*EXTERNAL InterlockedDecrement:WINAPI*>
PROCEDURE InterlockedDecrement (lpAddend: PINT32): INT32;

<*EXTERNAL InterlockedExchange:WINAPI*>
PROCEDURE InterlockedExchange (target: PINT32; value: INT32): INT32;

<*EXTERNAL InterlockedCompareExchange:WINAPI*>
PROCEDURE InterlockedCompareExchange (destination: PINT32; exchange: INT32; comparand: INT32): INT32;

  InterlockedExchange         (ord: 474  offset: 16_2662a)
