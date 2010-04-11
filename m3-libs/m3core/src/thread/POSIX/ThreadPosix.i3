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

<*EXTERNAL ThreadPosix__disallow_sigvtalrm*>
PROCEDURE disallow_sigvtalrm();

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

(*---------------------------------------------------------------------------*)

(* Model a set of integers of arbitrary size? *)

CONST FDSetSize = BITSIZE(INTEGER);

TYPE FDSet = SET OF [0 .. FDSetSize-1];
     FDS = REF ARRAY OF FDSet;

<*EXTERNAL ThreadPosix__Select*>
PROCEDURE Select(nfds: int; VAR read, write, except: FDSet; 
                 timeout: LONGREAL(*Time.T*)): int;
(* Thin wrapper around select. *)

(*---------------------------------------------------------------------------*)


END ThreadPosix.
