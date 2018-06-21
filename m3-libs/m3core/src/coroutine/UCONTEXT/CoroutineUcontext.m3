UNSAFE MODULE CoroutineUcontext EXPORTS Coroutine, CoroutineUcontext;
IMPORT ContextC;
IMPORT WeakRef;
IMPORT Thread;
IMPORT ThreadPThread;
IMPORT RTIO;
IMPORT Word;
IMPORT RTParams;
IMPORT RTError;
FROM Compiler IMPORT ThisFile, ThisLine;

(* Modula-3 coroutines
   Author : Mika Nystrom <mika.nystroem@intel.com>
   June, 2018

   Implements coroutines without stack back-references.  Interface is
   thus very similar to Modula-3 Thread.T.

   Some of the code will only work on AMD64 at present.

   There are assumptions that: 
   1. pointers are 64 bits 
   2. stack grows downward
   3. we rely on ThreadPThread.m3
   4. we (further) rely on pthread thread-local (global) storage

   Most of these assumptions are in the C code accompanying.
*)

CONST NoId : Id = NIL;

REVEAL
  T = BRANDED OBJECT
    context : ContextC.T;   (* ucontext_t * *)
    thread  : Thread.T;     (* for sanity checking *)
    id           := NoId;   (* Tabulate() fills this in *)
    firstcall    := TRUE;   (* first call *)
    arg     : Arg;          (* data needed for the Closure *)
    from    : T  := NIL;    (* used to pass caller to callee in Call() *)
    gcstack : ADDRESS;      (* StackState from ThreadPThread.i3 *)
    succ    : T  := NIL;    (* successor if we run off end *)
    dead    : Id := NoId;   (* notification to successor that someone died *)
    inPtr   : ADDRESS;      (* place for a pointer to myself to inhibit GC *)
    result  : REFANY := NIL;(* result of apply *)
  END;

PROCEDURE Retval(t : T) : REFANY =
  BEGIN RETURN t.result END Retval;
  
VAR
  Empty := NEW(T);                         (* dummy object, cant use NIL *)
  coArr := NEW(REF ARRAY OF WeakRef.T, 1); (* entry 0 not used *)
  coMu  := NEW(MUTEX);                     (* protect coArr and coId *)

  (* notes on garbage collection:

     A "T" is a reference held by the client.

     We desire the following behavior.

     If a T is active (i.e., running = head of its thread), it should not be
     collected.

     If a T is referenced by any active or inactive stack, it should not be
     collected.

     An inactive T must not reference itself (unless of course the client
     does so for it, which would inhibit collection).

     Corollaries of the above are that--
     an active T must have a reference to itself on its own stack, to inhibit
     collection.  An inactive T must not have a reference to itself on its own
     stack, so as not to inhibit collection.

     Any reference from an inactive T's stack to itself must go via the
     indirection of the coArr table and use its coroutine id.

     Any reference to an inactive T may go NIL at any time (via GC and WeakRef
     activity.)

     An initial coroutine is a coroutine started outside of this framework,
     i.e., a coroutine with a stack created by the threading system (or the
     C runtime).  Such a coroutine should not be collected.

     A reference from a coroutine to its successor (which inhibits collection
     of the successor) is OK.  If the successor is to be collected, it will
     eventually be collected through several rounds of GC.
  *)

PROCEDURE CreateInitialCoroutine() : T =
  VAR
    t := NEW(T,
             firstcall := FALSE,              (* since we're not a Closure *)
             arg       := NIL,                (* since firstcall is FALSE *)
             context   := ContextC.Current(),
             thread    := Thread.Self(),
             gcstack   := ThreadPThread.GetStackState(),
             succ      := NIL                 (* I will not exit *),
             inPtr     := NIL                 (* no need to inhibit *)
    );
  BEGIN
    Tabulate(t);
    <*ASSERT t.id # NIL*>
    EVAL Trace(t);

    IF DEBUG THEN
      RTIO.PutText("*** CreateInitial t=");
      RTIO.PutAddr(LOOPHOLE(t,ADDRESS));
      RTIO.PutText(" t.gcstack=");
      RTIO.PutAddr(LOOPHOLE(t.gcstack,ADDRESS));
      RTIO.PutText(" t.id^=");
      RTIO.PutInt(t.id^);
      RTIO.PutText("\n");
      RTIO.Flush();
    END;

    RETURN t
  END CreateInitialCoroutine;

  (* the whole GetCurrentCoroutine / SetCurrentCoroutine dance is really 
     only used for one purpose, namely, to ensure that we can see that 
     the initial creator (mother coroutine) has a Coroutine.T whither 
     new coroutines may return.

     I wonder if there is a simpler way.  One approach would be to
     require every thread to have a mother coroutine record created
     when the thread is created.  That could simplify a few things,
     but would require (probably) changes to Thread.T (maybe could be
     isolated to ThreadPThread only...)  It would only really hide the
     thread local (global) issues in the threading library---these
     issues dont seem to go away.
  *)
  
PROCEDURE Create(cl : Closure) : T =
  VAR
    me, t : T;
  BEGIN
    (* make sure that the current coroutine is defined, it will not be
       if this is the first/second coroutine created of this thread *)
    WITH cur = ContextC.GetCurrentCoroutine() DO
      IF LOOPHOLE(cur,INTEGER) = 0 OR cur^ = 0 THEN
        (* I do not exist yet, so I am the mother coroutine (normal thread) *)
        me := CreateInitialCoroutine();
        ContextC.SetCurrentCoroutine(me.id)
      ELSE
        LOCK coMu DO
          me := WeakRef.ToRef(coArr[cur^])
        END
      END
    END;

    (* need to get the context here *)
    <*ASSERT cl # NIL*>
    WITH arg       = NEW(Arg, cl := cl),
         ssz       = Thread.GetDefaultStackSize(), 
         ctx       = ContextC.MakeContext(Run, ssz, arg),
         stackbase = ContextC.GetStackBase(ctx),
         gcstack   = ThreadPThread.CreateStackState(stackbase,stackbase)
            (*nothing to scan yet*)
     DO
      IF DEBUG THEN
        RTIO.PutText("*** Create arg=");
        RTIO.PutAddr(LOOPHOLE(arg,ADDRESS));
        RTIO.PutText(" gcstack=");
        RTIO.PutAddr(LOOPHOLE(gcstack,ADDRESS)); RTIO.PutText("\n");
        RTIO.Flush();
      END;
      
      t := NEW(T,
               arg     := arg,
               context := ctx,
               thread  := Thread.Self(),
               gcstack := gcstack
      );

    END;
    Tabulate(t);
    EVAL Trace(t);
    t.arg.id := t.id;

    IF DEBUG THEN
      RTIO.PutText("*** Create t=");
      RTIO.PutAddr(LOOPHOLE(t,ADDRESS));
      RTIO.PutText(" t.arg=");
      RTIO.PutAddr(LOOPHOLE(t.arg,ADDRESS));
      RTIO.PutText(" t.gcstack=");
      RTIO.PutAddr(LOOPHOLE(t.gcstack,ADDRESS));
      RTIO.PutText(" t.id^=");
      RTIO.PutInt(t.id^);
      RTIO.PutText("\n");
      RTIO.Flush();
    END;

    RETURN t
  END Create;

PROCEDURE Tabulate(t : T) =
  BEGIN
    LOCK coMu DO
      FOR i := 1 TO LAST(coArr^) DO
        (* check if old tenant has been GC'd or deleted *)
        WITH tenant = WeakRef.ToRef(coArr[i]) DO
          IF tenant = NIL OR tenant = Empty THEN
            t.id := NEW(UNTRACED REF INTEGER);
            t.id^ := i;
            coArr[t.id^] := WeakRef.FromRef(t);
            RETURN
          END
        END
      END;
      (* no space, make new space *)
      WITH new = NEW(REF ARRAY OF WeakRef.T, NUMBER(coArr^)+1) DO
        SUBARRAY(new^,0,NUMBER(coArr^)) := coArr^;
        coArr := new;
        t.id := NEW(UNTRACED REF INTEGER);
        t.id^ := LAST(coArr^);
        coArr[t.id^] := WeakRef.FromRef(t)
      END
    END
  END Tabulate;

(***********************************************************************)

PROCEDURE DbgStackInfo(lab : TEXT) =
  VAR
    x    := 0;
    adr  := ADR(x);
    base := ThreadPThread.GetCurStackBase();
  BEGIN
    RTIO.PutText(lab);
    RTIO.PutText(" stack x ptr="); RTIO.PutAddr(adr);
    RTIO.PutText(" base="); RTIO.PutAddr(base);
    RTIO.PutText("\n");

    <*ASSERT Word.Minus(LOOPHOLE(adr,Word.T),LOOPHOLE(base,Word.T)) < 128*1024*1024*>

  END DbgStackInfo;
  
PROCEDURE Run(arg : Arg) =
  (* holding inCritical *)
  VAR
    inhibit : T; (* placeholder for an inhibit ptr -- when this is filled
                    in with reference to myself, I cannot be GC'd *)
    myid := arg.id^;
  BEGIN
    inhibit := WeakRef.ToRef(coArr[myid]); (* inhibit GC, inhibit = me *)
    inhibit.inPtr := ADR(inhibit);         (* remember stash *)
    
    ThreadPThread.DecInCritical();
    
    IF DEBUG THEN
      RTIO.PutText("*** Run arg="); RTIO.PutAddr(LOOPHOLE(arg,ADDRESS));
      RTIO.PutText(" arg.firstcaller="); RTIO.PutAddr(LOOPHOLE(arg.firstcaller,ADDRESS));
      RTIO.PutText(" arg.firstcaller.gcstack="); RTIO.PutAddr(arg.firstcaller.gcstack);
    
      RTIO.PutText("\n");
      DbgStackInfo("Run start");
      RTIO.Flush();
    END;
    
    <*ASSERT arg # NIL*>
    <*ASSERT arg.cl # NIL*>
    VAR
      fc := arg.firstcaller;
      res : REFANY;
    BEGIN
      arg.firstcaller := NIL;

      (* hand over control to client, inhibit is live *)
      res := arg.cl.apply(fc); (* return value? *)

      (* client leaves inhibit live owing to action of Call() *)
      <*ASSERT inhibit # NIL*>
      inhibit.result := res
    END;
    
    (* when we fall off end, we will automatically be jumped to "succ"
       (see ContextC.c, cleanup()) *)
    (* we need to set up the correct stack HERE *)

    IF DEBUG THEN
      RTIO.PutText("Run exiting gcstack="); RTIO.PutAddr(inhibit.gcstack);
      RTIO.PutText(" succ.gcstack="); RTIO.PutAddr(inhibit.succ.gcstack);
      RTIO.PutText(" succ.id^="); RTIO.PutInt(inhibit.succ.id^);
      RTIO.PutText("\n"); RTIO.Flush();
      DbgStackInfo("Run stop");
    END;
    
    ContextC.SetCurrentCoroutine(inhibit.succ.id); (* whither we are to go *)

    inhibit.succ.dead := inhibit.id; (* tell successor I am dead *)

    WITH top = ContextC.PushContext(inhibit.context) DO
      ThreadPThread.IncInCritical();
      ThreadPThread.SetCoStack(inhibit.succ.gcstack, top)
    END;
    (* holding inCritical *)
  END Run;

  (* "conservation of call flows":

     there are precisely THREE entry and exit points for coroutines
     
     either by SwapContext, on call to Call below
     
     OR
     
     from a context running off the end, which causes cleanup to be
     called 
     
     OR
     
     from jumping to a new context
     
     case 1. flow goes into SwapContext and re-emerges (with different stack)
     at same point in text
     
     case 2. flow goes off the end of Run and re-emerges in SwapContext

     case 3. flow goes into SwapContext and re-emerges at head of Run

     all three swaps must be protected by inCritical so that stack
     info in the thread header (ThreadPThread.m3) matches the actual
     stack pointer!

    *)
  
PROCEDURE Call(to : T) : T =
  VAR
    myId := ContextC.GetCurrentCoroutine();
    me : T; (* this inhibits GC if filled in *)
  BEGIN
    LOCK coMu DO
      me := WeakRef.ToRef(coArr[myId^]) (* hmm ... *)
    END;
    <*ASSERT me # NIL*>
    <*ASSERT to # NIL*>
    <*ASSERT me.thread = to.thread*>

    IF to.firstcall THEN
      (* check for first call, set arg.firstcaller *)
      to.firstcall := FALSE;
      to.arg.firstcaller := me;
    END;
    
    ContextC.SetCurrentCoroutine(to.id);
    IF to.context = NIL THEN
      RTError.Msg(ThisFile(),ThisLine(), "Coroutine client error: ",
                  "context is NIL, calling already exited coroutine?")
    END;
    ContextC.SetLink(to.context, me.context); (* set return link *)
    to.succ := me;
    to.from := me; (* in caller context *)

    IF DEBUG THEN DbgStackInfo("Call before swap") END;

    WITH top = ContextC.PushContext(me.context) DO
      (* mention to ThreadPThread that we are about to swap stacks *)
      (* when the stack disagrees with the execution context is a 
         critical section for GC *)
      ThreadPThread.IncInCritical();
      ThreadPThread.SetCoStack(to.gcstack, top)
    END;
    (* turn off gc inhibition of myself *)
    WITH myCtx = me.context,
         tgCtx = to.context DO
      IF me.inPtr # NIL THEN LOOPHOLE(me.inPtr, T) := NIL END;
      me := NIL;
      (* no stack references to me, stack is clean! *)

      (* THIS IS IT >>>>> *)
      ContextC.SwapContext(myCtx, tgCtx) (* might never return *)
      (* <<<<< THAT WAS IT *)
      
    END;

    (* I survived, so re-establish references to myself *)
    LOCK coMu DO
      me := WeakRef.ToRef(coArr[myId^])
    END;
    IF me.inPtr # NIL THEN LOOPHOLE(me.inPtr, T) := me END;
    
    ThreadPThread.DecInCritical();

    (* if we wake up here and the dead field is set, another coroutine
       exited and is notifying us to reap it,
    *)
    IF me.dead # NIL THEN
      Reap(me.dead^);
      me.dead := NIL
    END;

    IF DEBUG THEN DbgStackInfo("Call after swap") END;
    
    RETURN to.from (* in callee context *)
  END Call;

PROCEDURE Reap(id : INTEGER) =
  (* note that we still need to have the GC-driven cleanup because
     a coroutine doesn't necessarily exit through here.  It COULD be
     suspended in the middle and forgotten, only to be found by GC much
     later.  In such cases, Reap() never gets called. *)
  VAR
    dead : T;
  BEGIN
    LOCK coMu DO
      dead := WeakRef.ToRef(coArr[id])
    END;
    
    IF dead = NIL THEN RETURN END; (* hmm can this happen? *)

    (* if we get here, dead is a LIVE reference to a coroutine.

       therefore, we do not have to worry that this code runs concurrently
       with Cleanup 
    *)
    
    ThreadPThread.DisposeStack(dead.gcstack);
    ContextC.DisposeContext(dead.context);
    dead.context := NIL;

    (* note also that the WeakRef in coArr normally has to be handled
       by GC since we can't NIL it ourselves.  We could set it to
       Empty and then check for that, however, if we want... *)
    LOCK coMu DO
      coArr[dead.id^] := WeakRef.FromRef(Empty)
    END;

    DISPOSE(dead.id);
    dead.id := NIL;
    
  END Reap;
  
(***********************************************************************)
  
PROCEDURE Trace(t : T) : T =
  BEGIN
    EVAL WeakRef.FromRef(t, Cleanup);
    RETURN t
  END Trace;

PROCEDURE Cleanup(<*UNUSED*>READONLY self : WeakRef.T; ref : REFANY) =
  VAR
    dead : T := ref;
  BEGIN
    IF dead.context # NIL THEN
      ThreadPThread.DisposeStack(dead.gcstack);
      ContextC.DisposeContext(dead.context);
      dead.context := NIL;
    END;

    IF dead.id # NIL THEN
      DISPOSE(dead.id);
      dead.id := NIL;
    END;

    (* note that WeakRef.ToRef(coArr[dead.id^]) will at this point return
       NIL, so we do not need to do anything special about it *)
  END Cleanup;

VAR DEBUG := RTParams.IsPresent("debugcoroutines");

BEGIN
  ContextC.InitC()
END CoroutineUcontext.
