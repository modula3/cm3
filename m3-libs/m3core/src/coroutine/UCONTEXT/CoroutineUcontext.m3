UNSAFE MODULE CoroutineUcontext EXPORTS Coroutine, CoroutineUcontext;
IMPORT ContextC;
IMPORT WeakRef;
IMPORT Thread;
IMPORT ThreadPThread;
IMPORT RTIO;
IMPORT Word;

REVEAL
  T = BRANDED OBJECT
    context : ContextC.T; (* ucontext_t * *)
    thread  : Thread.T;
    isAlive := TRUE;
    id      : UNTRACED REF INTEGER; (* Tabulate() fills this in *)
    firstcall :=  TRUE; (* first call *)
    arg     : Arg;
    from    : T := NIL;
    gcstack : ADDRESS; (* StackState from ThreadPThread.i3 *)
    succ    : T := NIL; (* successor if we run off end *)
  END;

VAR coArr := NEW(REF ARRAY OF T, 1); (* entry 0 not used *)
    coMu  := NEW(MUTEX); (* protect coArr and coId *)

PROCEDURE CreateInitialCoroutine() : T =
  VAR
    t := NEW(T,
             firstcall := FALSE,              (* since we're not a Closure *)
             arg := NIL,                    (* since firstcall is FALSE *)
             context := ContextC.Current(),
             thread  := Thread.Self(),
             gcstack := ThreadPThread.GetStackState(),
             succ    := NIL (* should be OK *)
    );
  BEGIN
    Tabulate(t);
    <*ASSERT t.id # NIL*>
    EVAL Trace(t);

    RTIO.PutText("*** CreateInitial t=");
    RTIO.PutAddr(LOOPHOLE(t,ADDRESS));
    RTIO.PutText(" t.gcstack=");
    RTIO.PutAddr(LOOPHOLE(t.gcstack,ADDRESS));
    RTIO.PutText(" t.id^=");
    RTIO.PutInt(t.id^);
    RTIO.PutText("\n");
    RTIO.Flush();

    RETURN t
  END CreateInitialCoroutine;
  
PROCEDURE Create(cl : Closure) : T =
  VAR
    me, t : T;
  BEGIN
    (* make sure that the current coroutine is defined, it will not be
       if this is the first/second coroutine created of this thread *)
    WITH cur = ContextC.GetCurrentCoroutine() DO
      IF LOOPHOLE(cur,INTEGER) = 0 OR cur^ = 0 THEN
        
        me := CreateInitialCoroutine();

        (* the next line seems to cause a segfault in 
           RTAllocator__GetTracedRef ?? *)
        ContextC.SetCurrentCoroutine(me.id)

      ELSE
        LOCK coMu DO
          me := coArr[cur^]
        END
      END
    END;

    (* need to get the context here *)
    <*ASSERT cl # NIL*>
    WITH arg       = NEW(Arg, arg := cl, dbg := 'B'),
         ssz       = Thread.GetDefaultStackSize(), 
         ctx       = ContextC.MakeContext(Run, ssz, arg),
         stackbase = ContextC.GetStackBase(ctx),
         gcstack   = ThreadPThread.CreateStackState(stackbase,stackbase)
            (*nothing to scan yet*)
     DO
      RTIO.PutText("*** Create arg=");
      RTIO.PutAddr(LOOPHOLE(arg,ADDRESS));
      RTIO.PutText(" gcstack=");
      RTIO.PutAddr(LOOPHOLE(gcstack,ADDRESS)); RTIO.PutText("\n");
      RTIO.Flush();
      
      t := NEW(T,
               arg     := arg,
               context := ctx,
               thread  := Thread.Self(),
               gcstack := gcstack
      );
      t.arg.this := t;
    END;
    Tabulate(t);
    EVAL Trace(t);

    RTIO.PutText("*** Create t=");
    RTIO.PutAddr(LOOPHOLE(t,ADDRESS));
    RTIO.PutText(" t.arg=");
    RTIO.PutAddr(LOOPHOLE(t.arg,ADDRESS));
    RTIO.PutText(" t.arg.this=");
    RTIO.PutAddr(LOOPHOLE(t.arg.this,ADDRESS));
    RTIO.PutText(" t.gcstack=");
    RTIO.PutAddr(LOOPHOLE(t.gcstack,ADDRESS));
    RTIO.PutText(" t.id^=");
    RTIO.PutInt(t.id^);
    RTIO.PutText("\n");
    RTIO.Flush();

    RETURN t
  END Create;

PROCEDURE Tabulate(t : T) =
  BEGIN
    LOCK coMu DO
      FOR i := 1 TO LAST(coArr^) DO
        IF coArr[i] = NIL THEN
          t.id := NEW(UNTRACED REF INTEGER);
          t.id^ := i;
          coArr[t.id^] := t;
          RETURN
        END
      END;
      WITH new = NEW(REF ARRAY OF T, NUMBER(coArr^)+1) DO
        SUBARRAY(new^,0,NUMBER(coArr^)) := coArr^;
        coArr := new;
        t.id := NEW(UNTRACED REF INTEGER);
        t.id^ := LAST(coArr^);
        coArr[t.id^] := t
      END
    END
  END Tabulate;

(***********************************************************************)

PROCEDURE DbgStackInfo(lab : TEXT) =
  VAR
    x := 0;
    adr := ADR(x);
    base := ThreadPThread.GetCurStackBase();
  BEGIN
    RTIO.PutText(lab);
    RTIO.PutText(" stack x ptr="); RTIO.PutAddr(adr);
    RTIO.PutText(" base="); RTIO.PutAddr(base);
    RTIO.PutText("\n");

    <*ASSERT Word.Minus(LOOPHOLE(adr,Word.T),LOOPHOLE(base,Word.T)) < 128*1024*1024*>

  END DbgStackInfo;
  
PROCEDURE Run(arg : Arg) =
  BEGIN
    RTIO.PutText("*** Run arg="); RTIO.PutAddr(LOOPHOLE(arg,ADDRESS));
    RTIO.PutText(" arg.firstcaller="); RTIO.PutAddr(LOOPHOLE(arg.firstcaller,ADDRESS));
    RTIO.PutText(" arg.firstcaller.gcstack="); RTIO.PutAddr(arg.firstcaller.gcstack);
    
    RTIO.PutText("\n");
    DbgStackInfo("Run start");
    RTIO.Flush();
    RTIO.PutChar(arg.dbg); RTIO.PutText("\n");
    <*ASSERT arg # NIL*>
    <*ASSERT arg.arg # NIL*>
    WITH cl = NARROW(arg.arg, Closure) DO
      EVAL cl.apply(arg.firstcaller) (* return value? *)
      (* we could actually call cleanup() here instead of using the 
         default next context mechanism.  why don't we? *)
    END;

    (* when we fall off end, we will automatically be jumped to "succ" *)
    (* we need to set up the correct stack HERE *)

    (* should probably increment inCritical *)
    RTIO.PutText("Run exiting gcstack="); RTIO.PutAddr(arg.this.gcstack);
    RTIO.PutText(" succ.gcstack="); RTIO.PutAddr(arg.this.succ.gcstack);
    RTIO.PutText(" succ.id^="); RTIO.PutInt(arg.this.succ.id^);
    RTIO.PutText("\n"); RTIO.Flush();
    DbgStackInfo("Run stop");

    WITH top = ContextC.PushContext(arg.this.context) DO
      ThreadPThread.SetCoStack(arg.this.succ.gcstack, top)
    END
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

     all three swaps should probably be protected by inCritical so that
     stack info in the thread header (ThreadPThread.m3) matches the
     actual stack pointer!

    *)
  
PROCEDURE Call(to : T) : T =
  VAR
    myId := ContextC.GetCurrentCoroutine();
    me : T;
  BEGIN
    LOCK coMu DO
      me := coArr[myId^]
    END;
    <*ASSERT me # NIL*>
    <*ASSERT to # NIL*>
    <*ASSERT to.isAlive*>
    <*ASSERT me.thread = to.thread*>

    IF to.firstcall THEN
      (* check for first call, set arg.firstcaller *)
      to.firstcall := FALSE;
      to.arg.firstcaller := me;
    END;
    
    ContextC.SetCurrentCoroutine(to.id);
    ContextC.SetLink(to.context, me.context); (* set return link *)
    to.succ := me;
    to.from := me; (* in caller context *)

    (* mention to ThreadPThread that we are about to swap stacks *)

    DbgStackInfo("Call before swap");

    WITH top = ContextC.PushContext(me.context) DO
      ThreadPThread.SetCoStack(to.gcstack, top)
    END;

    ContextC.SwapContext(me.context, to.context);

    ContextC.SetCurrentCoroutine(me.id); (* this is needed for those
                                            coroutines that fall off the
                                            end and are cleaned up *)

    (* how do we clean up current stack for threads that fall off the end? *)

    DbgStackInfo("Call after swap");
    
    RETURN to.from (* in callee context *)
  END Call;

(***********************************************************************)
  
PROCEDURE IsAlive(t : T) : BOOLEAN =
  BEGIN RETURN t.isAlive END IsAlive;

  (**********************************************************************)

PROCEDURE Trace(t : T) : T =
  BEGIN
    EVAL WeakRef.FromRef(t, Cleanup);
    RETURN t
  END Trace;

PROCEDURE Cleanup(<*UNUSED*>READONLY self : WeakRef.T; ref : REFANY) =
  VAR
    dead : T := ref;
  BEGIN
    ContextC.DisposeContext(dead.context);
    dead.context := NIL;
    
    DISPOSE(dead.id);
    dead.id := NIL;
  END Cleanup;

BEGIN
  ContextC.InitC()
END CoroutineUcontext.
