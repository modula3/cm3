(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*---------------------------------------------------------------------------
Path: src.dec.com!pa.dec.com!decwrl!uunet!zaphod.mps.ohio-state.edu!uakari.primate.wisc.edu!zazen!uwvax!chevre.cs.wisc.edu!miron
From: miron@chevre.cs.wisc.edu (Miron Livny)
Newsgroups: comp.lang.modula3
Subject: Problem with threads
Message-ID: <1991Sep20.132825.11499@spool.cs.wisc.edu>
Date: 20 Sep 91 13:28:25 GMT
Sender: news@spool.cs.wisc.edu (The News)
Organization: U of Wisconsin CS Dept
Lines: 88
Originator: miron@chevre.cs.wisc.edu

I have ported my simulation environment from Modula2 to Modula3.
The environment is based on processes that were implemented as co-routines   
in the Modula2 version. In Modula3 each process is represented by a thread.
The following program captures the structure of the Modula3 version
---------------------------------------------------------------------------*)

MODULE Main;
IMPORT Thread;
VAR
    simMutex : Thread.Mutex := NEW(Thread.Mutex);
    simCon   : Thread.Condition := NEW(Thread.Condition);
    procCon  : ARRAY [0..10] OF Thread.Condition ;
    process  : Thread.Closure;
    nodeId   : INTEGER;
    count    : INTEGER;

PROCEDURE Process(<*UNUSED*> SELF : Thread.Closure) : REFANY RAISES {} = 
VAR
    myNodeId : INTEGER;
BEGIN
    myNodeId := nodeId;
    Thread.Acquire(simMutex);
    LOOP
        Thread.Signal(simCon);
        Thread.Wait(simMutex,procCon[myNodeId]);
    END;
END Process;

BEGIN
    count := 0;
    Thread.Acquire(simMutex);
    FOR I :=  1 TO 10 DO
	process := NEW(Thread.Closure, apply := Process);
        procCon[I] := NEW(Thread.Condition);
	nodeId := I;
	EVAL Thread.Fork(process);
	Thread.Wait(simMutex,simCon);
    END;
    FOR j := 1 TO 10000 DO
	FOR I := 1 TO 10 DO
	    INC(count);
	    Thread.Signal(procCon[I]);
	    Thread.Wait(simMutex,simCon);
	END;
    END;
END Main.

(*---------------------------------------------------------------------------
Like the full version of the Modula3 implementation, this program
deadlocks after a random number of events. The following script
demonstrates the non-deterministic behavior of the program

chevre:1> m3 -o test test.m3
chevre:2> test
M3 runtime error: Deadlock !

Quit (core dumped)
8.3u 9.1s 0:19.75 88.6% 102+158k 2+56io 29pf+0w
chevre:3> dbx test core
dbx version 2.10.1
Type 'help' for help.
Corefile produced from file "test"
Child died at pc 0x427688 of signal : Quit
reading symbolic information ...
[using memory image in core]
(dbx) print _test__count
47102 
(dbx) quit
chevre:4> test
M3 runtime error: Deadlock !

Quit (core dumped)
24.5u 30.2s 0:56.48 97.0% 99+158k 0+56io 9pf+0w
chevre:5> dbx test core
dbx version 2.10.1
Type 'help' for help.
Corefile produced from file "test"
Child died at pc 0x427688 of signal : Quit
reading symbolic information ...
[using memory image in core]
(dbx) print _test__count
148080 
(dbx) quit

Is there anything wrong in the way I use the threads or is it a
Modula3 bug?

Thanks

Miron Livny


---------------------------------------------------------------------------*)

