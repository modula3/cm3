

MODULE ThreadExample EXPORTS Main;
IMPORT Thread;
IMPORT Fmt, IO;

(* "ThreadExample" is a simple example of a multi-threaded
    program. Driven by commands from standard input, the
    program will start multiple timers which are keeping
    track of elapsed time concurrently. *)

(*---------------------------------------- thread closure ----*)

(* Create a new <<em>>closure<</em>> object, which embodies
   the state of a thread. Each "TimeClosure" contains the
   state required for a timer thread: how long the thread
   should pause, i.e., the "time" field, and "count" to
   identify each thread.

   Thread closures override the "apply" method to associate
   the work to be done. "TimerClosure"'s work is in procedure
   "TimerApply". *)

TYPE
  TimerClosure = Thread.Closure OBJECT
    time: LONGREAL;
    count: CARDINAL;
  OVERRIDES
    apply := TimerApply;
  END;

PROCEDURE TimerApply (cl: TimerClosure): REFANY =
(* "TimerApply" contains the work of the timer threads.
   It prints out a message that it has started, waits for
   "cl.time" seconds, and then prints out a message that
   it has finished. The local variable "count" provides
   more information for matching starts and finish messages.  *)

  VAR
    count := Counter();
  BEGIN
    Print("\nStarting timer " & Fmt.Int(count) &
            " for " & Fmt.LongReal (cl.time) & " seconds.");

    Thread.Pause (cl.time);

    Print ("\nFinished timer " & Fmt.Int(count) &
            " after " & Fmt.LongReal (cl.time) & " seconds.\n");
    RETURN NIL;
  END TimerApply;

(*------------------------------------------ thread counts ---- *)

(* "timer_count" keeps track of the count for the threads created.
   "timer_count_mu", a <<em>>mutex<</em>> (or a lock) protects
   the critical sections, where multiple threads may be in contention
   of incrementing the timer. *)

VAR
  timer_count: CARDINAL := 0;
  timer_count_mu := NEW(MUTEX);

PROCEDURE Counter(): CARDINAL =
(* Return a new counter. "Counter"'s critical section (the place
   where multiple threads may be racing each other) is
   protected by a "LOCK mutex DO ... END" statement. Note that
   the mutex is automatically unlocked upon exit from the scope
   of the "LOCK" statement, so "RETURN timer_count" effectively
   unlocks timer_count on its way out. *)
  BEGIN
    LOCK timer_count_mu DO
      INC (timer_count);
      RETURN timer_count;
    END;
  END Counter;

(*--------------------------------------------- user interface ----*)

CONST
  Prompt = "thread-timer> ";

PROCEDURE Print(t: TEXT) =
(* Print "message", after appending "Prompt" to it. *)
  BEGIN
    IO.Put (t & "\n" & Prompt);
  END Print;

(* Main loop:
   "input" is read from the user, it determines how long
   the next created thread will pause; a "closure" is created
   dynamically to be passed in to "Thread.Fork" which will
   fork a new thread and run "closure.apply()". *)
VAR
  input: CARDINAL;
  closure: TimerClosure;

BEGIN
  LOOP
    IO.Put(Prompt);
    input := IO.GetInt();
    closure := NEW(TimerClosure, time := FLOAT(input, LONGREAL));
    EVAL Thread.Fork (closure);
  END;
END ThreadExample.

(* There was no need for waiting for a forked
   thread in this example. To wait for a forked thread to
   complete, you may use "Thread.Join(th)" which joins
   your current thread to "th".

|  th := Thread.Fork (cl);
|  ... do other activities ...
|  result := Thread.Join (th)

*)
