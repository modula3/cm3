(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE ThreadPosix;

FROM Thread IMPORT Alerted;

TYPE SignalHandler1 = PROCEDURE(signo: INTEGER) RAISES {Alerted};
     SignalHandler3 = PROCEDURE(signo: INTEGER; info: ADDRESS(* siginfo_t *); context: ADDRESS(* ucontext_t *));

<*EXTERNAL ThreadPosix__setup_sigvtalrm*>
PROCEDURE setup_sigvtalrm(handler: SignalHandler1);

<*EXTERNAL ThreadPosix__allow_sigvtalrm*>
PROCEDURE allow_sigvtalrm();

<*EXTERNAL ThreadPosix__disallow_sigvtalrm*>
PROCEDURE disallow_sigvtalrm();

(* There is already ThreadF.Init exported by the same module that exports this interface,
   so we chose an arbitrary different name here. *)
<*EXTERNAL ThreadPosix__InitC*>
PROCEDURE InitC();

END ThreadPosix.
