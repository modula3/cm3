(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(*---------------------------------------------------------------------------
Return-Path: <nr@Princeton.EDU>
Received: by jumbo.pa.dec.com; id AA12689; Mon, 7 Oct 91 12:45:35 -0700
Received: by mts-gw.pa.dec.com; id AA11218; Mon, 7 Oct 91 12:45:32 -0700
Received: from fs.Princeton.EDU by Princeton.EDU (5.65b/2.83/princeton)
	id AA27158; Mon, 7 Oct 91 15:45:21 -0400
Received: from cs.Princeton.EDU (atomic.Princeton.EDU) by fs.Princeton.EDU (4.0/1.105)
	id AA27297; Mon, 7 Oct 91 15:45:19 EDT
Received: by cs.Princeton.EDU (5.57/1.105)
	id AA12004; Mon, 7 Oct 91 15:45:16 -0400
Date: Mon, 7 Oct 91 15:45:16 -0400
From: Norman Ramsey <nr@Princeton.EDU>
Message-Id: <9110071945.AA12004@cs.Princeton.EDU>
To: m3-request
Subject: bug in 1.6 runtime on DS3100
Cc: drh@Princeton.EDU

I had to build up a fairly complex example, but the following program
shows a problem with alerting (I think).  I have a thread ( the
``listener'') that loops forever, reading input and enqueueing it for
a second thread (the ``command loop'').  When the command loop sees
the input `quit', it calls RTMisc.Exit(0).  Now, a procedure has been
registered via RTMisc.RegisterExitor, and that procedure is to alert
the listener thread when Exit is called.  BUT for some reason when the
thread is alerted it doesn't give up its lock on Stdio.stdin, with the
result that when Stdio.Close is called, the system goes into a
select(2) system call.

I apologize in advance for the complexity of the example, but I
couldn't concoct a simpler example that would exhibit the bug.


Norman




Here's evidence:

nr@head (68) % m3 -v -g Bug26.m3
/usr/local/lib/m3/m3compiler -D/usr/local/lib/m3/Xaw -D/usr/local/lib/m3/Xt -D/u
sr/local/lib/m3/X11 -D/usr/local/lib/m3/io -D/usr/local/lib/m3/data -D/usr/local
/lib/m3/math -D/usr/local/lib/m3/misc -D/usr/local/lib/m3/ultrix-3-1 -D/usr/loca
l/lib/m3/core -D. -w3 -o 00136d000_m.c Bug26.m3
/bin/cc -G0 -I/usr/local/lib/m3 -g2 -c 00136d000_m.c
unlinking 00136d000_m.c ... done
nm -o -p 00136d000_m.o
/bin/cc -G0 -I/usr/local/lib/m3 -g2 -c m3ld00136d001.c
unlinking m3ld00136d001.c ... done
/bin/cc -G0 -o a.out m3ld00136d001.o 00136d000_m.o -L/usr/local/lib/m3/ultrix-3-
1 -lm3ultrix-3-1 -L/usr/local/lib/m3/core -lm3core -lm
unlinking m3ld00136d001.o ... done
unlinking 00136d000_m.o ... done
nr@head (69) % dbx a.out
dbx version 2.10.1
Type 'help' for help.
reading symbolic information ...
main:    Source not available
(dbx) run
this is the first line of input
You said `this is the first line of input'; say `quit' to quit.
this is the second line
You said `this is the second line'; say `quit' to quit.
quit
about to Call RTMisc.Exit(0)
called Thread.Alert(lthread);
^C
Interrupt [select.select:18 +0x8,0x4416b8]
         Source not available
(dbx) where
>  0 select.select(0x0, 0x0, 0x0, 0x0, 0x0) ["../select.s":18, 0x4416b8]
   1 _Thread__Yield(0x10042528, 0x3, 0x100028c8, 0x1001d264, 0x40da3c) ["0071000
00_m.c":6, 0x407708]
   2 _Thread__Acquire(0x0, 0x0, 0x42ea48, 0x0, 0x4126f0) ["007100000_m.c":6, 0x4
07848]
   3 _Rd__Close(0x1000d7d8, 0x0, 0x0, 0x0, 0x403d48) ["007257000_m.c":6, 0x40da3
8]
   4 _Stdio__Close(0x0, 0x10026010, 0x1, 0x10026010, 0x401dd8) [0x4126ec]
   5 _RTMisc__Exit(0x100007c0, 0x0, 0x0, 0x0, 0x0) ["007083000_m.c":12, 0x403d44
]
   6 .block15 ["Bug26.m3":88, 0x401dd4]
   7 .block14 ["Bug26.m3":88, 0x401dd4]
   8 .block11 ["Bug26.m3":88, 0x401dd4]
   9 .block10 ["Bug26.m3":88, 0x401dd4]
  10 _Bug26__CommandLoop(_cl = 0x1002602c = "P\240^A^P|") ["Bug26.m3":88, 0x401d
d4]
  11 _Thread__DetermineContext(0x8, 0x40875c, 0x1e240, 0x0, 0x0) ["007100000_m.c
":6, 0x408b1c]
  12 _Thread__InitTopContext(0x0, 0x0, 0x0, 0x0, 0x0) ["007100000_m.c":6, 0x4087
d8]
(dbx) quit
nr@head (70) %



Here's the source code:
----------------------------------------------------------------------------*)
(* bug in alerting thread on exit? *)

MODULE Bug26 EXPORTS Main;
IMPORT Fmt, List, Rd, RTMisc, Text, Thread, Wr;
FROM Stdio IMPORT stderr, stdin, stdout;


(* Queue of user input events: listener thread produces, command loop consumes
*)

VAR eventMu := Thread.NewMutex();
    events: List.T := NIL; (* TEXT for user input *)
    eventNonempty := Thread.NewCondition();

PROCEDURE AddEvent(data: REFANY := NIL) =
BEGIN
  LOCK eventMu DO
    events := List.New(data,events);
    Thread.Signal(eventNonempty);
  END;
END AddEvent;

(* definition of a listener, for listener thread *)

TYPE
  Listener = Thread.Closure OBJECT
               rd : Rd.T;
            OVERRIDES
               apply := Listen;
            END;

PROCEDURE Listen(l:Listener):REFANY RAISES {} =
BEGIN
  LOOP
    TRY
      AddEvent(Rd.GetLine(l.rd));
    EXCEPT
    | Rd.EndOfFile => Wr.PutText(stderr,"EOF ignored; alert me to quit.\n");
                      Wr.Flush(stderr);
    | Thread.Alerted =>
        Wr.PutText(stderr,
                   "listener thread caught Thread.Alerted; about to return NIL\n");
        Wr.Flush(stderr);
        RETURN NIL;
    ELSE Wr.PutText(stderr, "this can\'t happen --- bad exception in listener\n");
         Wr.Flush(stderr);
    END;
  END;
END Listen;

VAR lthread: Thread.T; (* will be assigned to input listener *)

(* procedure to alert listener when it's time to exit *)

PROCEDURE AlertListener(<*UNUSED*>n:INTEGER) =
BEGIN
  Thread.Alert(lthread);
  Wr.PutText(stderr, "called Thread.Alert(lthread);\n");
  Wr.Flush(stderr);
END AlertListener;

(* procedure for ``command loop'' thread:
     start listener
     register exit procedure
     loop, waiting for `quit'
     call RTMisc.Exit(0);
*)

PROCEDURE CommandLoop(cl:Thread.Closure):REFANY RAISES {} =
VAR e : TEXT;
BEGIN
  lthread := Thread.Fork(NEW(Listener, rd := stdin));
  EVAL RTMisc.RegisterExitor(AlertListener);
  LOOP
    LOCK eventMu DO
      WHILE events = NIL DO Thread.Wait(eventMu, eventNonempty); END;
      e := NARROW(events.first, TEXT);
      events := events.tail;
    END;
    IF Text.Equal(e, "quit") THEN
      EXIT;
    ELSE
      Wr.PutText(stdout,Fmt.F("You said `%s\'; say `quit\' to quit.\n", e));
      Wr.Flush(stdout);
    END;
  END;

  Wr.PutText(stderr, "about to Call RTMisc.Exit(0)\n"); Wr.Flush(stderr);
  RTMisc.Exit(0);
  Wr.PutText(stderr, "this can\'t happen -- command loop returned\n"); Wr.Flush
(stderr);
  RETURN NIL;
END CommandLoop;



BEGIN
  EVAL Thread.Join(Thread.Fork(NEW(Thread.Closure, apply := CommandLoop)));
  Wr.PutText(stderr, "Main program joined; will exit\n"); Wr.Flush(stderr);
END Bug26.

