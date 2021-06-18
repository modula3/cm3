(* Copyright (C) 2018-2019 Intel Corporation *)
(* SPDX-License-Identifier: BSD-3-Clause *)
(* see the file COPYRIGHT-INTEL for more information *)

UNSAFE INTERFACE ContextC;
IMPORT Ctypes, Coroutine;
FROM CoroutineUcontext IMPORT Arg, Entry;

TYPE T = ADDRESS;
TYPE untraced_ref_integer = UNTRACED REF INTEGER; (* name for m3core.h to provide *)

<*EXTERNAL ContextC__MakeContext*>
PROCEDURE MakeContext(p      : Entry;
                      ssize  : INTEGER;
                      arg    : Arg): T;
  (* creates a new T, which must eventually be freed using DisposeContext *)

<*EXTERNAL ContextC__Current*>
PROCEDURE Current() : T;
  (* creates a new T, which must eventually be freed using DisposeContext *)

<*EXTERNAL ContextC__SwapContext*>
PROCEDURE SwapContext(from, to : T);  

<*EXTERNAL ContextC__DisposeContext*>
PROCEDURE DisposeContext(ctx : T);

<*EXTERNAL ContextC__GetCurrentCoroutine*>
PROCEDURE  GetCurrentCoroutine() : untraced_ref_integer;

<*EXTERNAL ContextC__SetCurrentCoroutine*>
PROCEDURE  SetCurrentCoroutine(c : untraced_ref_integer);

<*EXTERNAL ContextC__InitC*>
PROCEDURE  InitC(stack: Ctypes.int_star); (* must call this before any of the other routines in this interface *)

<*EXTERNAL ContextC__SetLink*>  
PROCEDURE SetLink(tgt, src : T); (* set return link of tgt to be src *)

<*EXTERNAL ContextC__GetStackBase*>
PROCEDURE GetStackBase(t : T) : ADDRESS;

<*EXTERNAL ContextC__PushContextForRun*>
PROCEDURE PushContextForRun(inhibit: Coroutine.T; t: T);
  
<*EXTERNAL ContextC__PushContextForCall*>
PROCEDURE PushContextForCall(to: Coroutine.T; myId: UNTRACED REF INTEGER; VAR me: Coroutine.T; t: T): Coroutine.T;

<*EXTERNAL ContextC__Stack*>
PROCEDURE Stack() : ADDRESS;

END ContextC.
