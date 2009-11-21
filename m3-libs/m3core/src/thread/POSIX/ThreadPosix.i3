(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE ThreadPosix;

FROM Thread IMPORT Alerted;

TYPE SignalHandler1 = PROCEDURE(signo: INTEGER) RAISES {Alerted};
     SignalHandler3 = PROCEDURE(signo: INTEGER; info, context: ADDRESS);

<*EXTERNAL ThreadPosix__setup_sigvtalrm*>
PROCEDURE setup_sigvtalrm(handler: SignalHandler1);

<*EXTERNAL ThreadPosix__allow_sigvtalrm*>
PROCEDURE allow_sigvtalrm();

<*EXTERNAL ThreadPosix__disallow_sigvtalrm*>
PROCEDURE disallow_sigvtalrm();

<*EXTERNAL ThreadPosix__MakeContext*>
PROCEDURE MakeContext(p: PROCEDURE(); size: CARDINAL): ADDRESS;

<*EXTERNAL ThreadPosix__GetContext*>
PROCEDURE GetContext(c: ADDRESS);

<*EXTERNAL ThreadPosix__SwapContext*>
PROCEDURE SwapContext(from, to: ADDRESS);

<*EXTERNAL ThreadPosix__DisposeContext*>
PROCEDURE DisposeContext(VAR c: ADDRESS);

<*EXTERNAL ThreadPosix__ProcessContext*>
PROCEDURE ProcessContext(c, s: ADDRESS; p: PROCEDURE(start, stop: ADDRESS));

END ThreadPosix.
