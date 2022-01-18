(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE ThreadPosix;

FROM Thread IMPORT Alerted;
FROM Ctypes IMPORT int;

TYPE SignalHandler1 = PROCEDURE(signo: int) RAISES {Alerted};

<*EXTERNAL ThreadPosix__setup_sigvtalrm*>
PROCEDURE setup_sigvtalrm(handler: SignalHandler1);

<*EXTERNAL ThreadPosix__allow_sigvtalrm*>
PROCEDURE allow_sigvtalrm();

<*EXTERNAL ThreadPosix__allow_othersigs*>
PROCEDURE allow_othersigs();

<*EXTERNAL ThreadPosix__disallow_signals*>
PROCEDURE disallow_signals();

<*EXTERNAL ThreadPosix__MakeContext*>
PROCEDURE MakeContext(p: PROCEDURE(); size: CARDINAL): ADDRESS;

<*EXTERNAL ThreadPosix__SwapContext*>
PROCEDURE SwapContext(from, to: ADDRESS);

<*EXTERNAL ThreadPosix__DisposeContext*>
PROCEDURE DisposeContext(VAR c: ADDRESS);

<*EXTERNAL ThreadPosix__ProcessContext*>
PROCEDURE ProcessContext(c, bottom, top: ADDRESS;
                         p: PROCEDURE(start, limit: ADDRESS));

(*---------------------------------------------------------------------------*)

<*EXTERNAL ThreadPosix__SetVirtualTimer*>
PROCEDURE SetVirtualTimer(): int;
(* Thin wrapper around setitimer. *)

(* return previous state; this is paranoia just from reading headers *)
<*EXTERNAL ThreadPosix__DisableInterrupts*>
PROCEDURE DisableInterrupts(): int;

(* return previous state; this is paranoia just from reading headers *)
<*EXTERNAL ThreadPosix__EnableInterrupts*>
PROCEDURE EnableInterrupts (old_state: int);

(*---------------------------------------------------------------------------*)

END ThreadPosix.
