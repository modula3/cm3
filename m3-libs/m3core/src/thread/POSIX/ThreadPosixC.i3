(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE ThreadPosixC;

TYPE SignalHandler1 = PROCEDURE(signo: INTEGER);
     SignalHandler3 = PROCEDURE(signo: INTEGER; info: ADDRESS(* siginfo_t* *); context: ADDRESS(* ucontext_t* *));

PROCEDURE setup_sigvtalrm(handler: SignalHandler1);

PROCEDURE allow_sigvtalrm();

PROCEDURE disallow_sigvtalrm();

PROCEDURE Init(void);

END ThreadPosixC.
